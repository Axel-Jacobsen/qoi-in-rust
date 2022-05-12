use std::fmt;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::io::BufWriter;
use std::path::Path;

// i know that it is garb to do this but oh well
struct ImageData {
    png_type: png::ColorType,
    bytes: Vec<u8>,
    width: u32,
    height: u32,
    is_srgb: bool,
}

impl<'a> IntoIterator for &'a ImageData {
    type Item = PixelValue;
    type IntoIter = Box<dyn Iterator<Item = Self::Item> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        let pixel_size = get_pixel_size(self.png_type);
        Box::new(
            self.bytes
                .chunks(pixel_size)
                .map(move |chunk| match pixel_size {
                    3 => PixelValue {
                        r: chunk[0],
                        g: chunk[1],
                        b: chunk[2],
                        a: 255,
                    },
                    4 => PixelValue {
                        r: chunk[0],
                        g: chunk[1],
                        b: chunk[2],
                        a: chunk[3],
                    },
                    _ => panic!("we can't handle grayscale!"),
                }),
        )
    }
}

impl fmt::Display for ImageData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ImageData(img of size {})", self.bytes.len())
    }
}

fn get_pixel_size(color_type: png::ColorType) -> usize {
    match color_type {
        png::ColorType::GrayscaleAlpha => 2,
        png::ColorType::Grayscale => 1,
        png::ColorType::Indexed => 1,
        png::ColorType::Rgba => 4,
        png::ColorType::Rgb => 3,
    }
}

// i know that this is also garb but please be kind (for now)
fn get_encoding_file_data() -> Result<ImageData, png::DecodingError> {
    let decoder = png::Decoder::new(File::open("lenna.png")?);

    let mut reader = decoder.read_info()?;
    let mut buf = vec![0; reader.output_buffer_size()];

    let info = reader.next_frame(&mut buf)?;
    let bytes = &buf[..info.buffer_size()];

    let image_info = reader.info();
    // we dont really handle this correctly, but close enough
    let is_srgb = if let Some(_) = image_info.srgb {
        true
    } else {
        false
    };

    Ok(ImageData {
        png_type: info.color_type,
        bytes: bytes.to_vec(),
        width: image_info.width,
        height: image_info.height,
        is_srgb: is_srgb,
    })
}

fn get_decoding_file_data() -> Result<ImageData, png::DecodingError> {
    let mut decoder = File::open("out.qoi").expect("file not found in get_decoding_file_data");

    let mut header = [0; 14];
    let header_bytes_written = decoder.read(&mut header)?;
    assert!(header_bytes_written == 14);

    if header[0..4] != [0x71, 0x6F, 0x69, 0x66] {
        panic!("not a qoif file");
    }

    let width = u32::from_be_bytes(header[4..8].try_into().unwrap());
    let height = u32::from_be_bytes(header[8..12].try_into().unwrap());
    let channels = header[12];
    let is_srgb = header[13] == 0;

    // just read it all into mem like a degenerate
    let mut data_bytes: Vec<u8> = vec![];
    let _bytes_read = decoder.read_to_end(&mut data_bytes);

    Ok(ImageData {
        png_type: if channels == 3 {
            png::ColorType::Rgb
        } else {
            png::ColorType::Rgba
        },
        bytes: data_bytes,
        width: width,
        height: height,
        is_srgb: is_srgb,
    })
}

#[derive(Clone, PartialEq, Eq)]
struct PixelValue {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

fn hash_rgba(px: &PixelValue) -> usize {
    // use Wrapping? probably faster cause transparent?
    ((3_u16 * (px.r as u16)
        + 5_u16 * (px.g as u16)
        + 7_u16 * (px.b as u16)
        + 11_u16 * (px.a as u16))
        % 64) as usize
}

fn pixel_previously_seen(p: &PixelValue, prev_seen: &Vec<PixelValue>) -> Option<usize> {
    let pixel_hash = hash_rgba(p);
    if prev_seen[pixel_hash] == *p {
        Some(pixel_hash)
    } else {
        None
    }
}

fn pixel_qoi_diff(prev_pixel: &PixelValue, cur_pixel: &PixelValue) -> Option<[u8; 3]> {
    if prev_pixel.a != cur_pixel.a {
        return None;
    }
    let dr = (cur_pixel.r as i16) - (prev_pixel.r as i16);
    let dg = (cur_pixel.g as i16) - (prev_pixel.g as i16);
    let db = (cur_pixel.b as i16) - (prev_pixel.b as i16);
    let dx_range: core::ops::Range<i16> = -2..2;
    if dx_range.contains(&dr) && dx_range.contains(&dg) && dx_range.contains(&db) {
        Some([(dr + 2) as u8, (dg + 2) as u8, (db + 2) as u8])
    } else {
        None
    }
}

fn pixel_luma_diff(prev_pixel: &PixelValue, cur_pixel: &PixelValue) -> Option<[u8; 3]> {
    if prev_pixel.a != cur_pixel.a {
        return None;
    }
    let dr = (cur_pixel.r as i16) - (prev_pixel.r as i16);
    let dg = (cur_pixel.g as i16) - (prev_pixel.g as i16);
    let db = (cur_pixel.b as i16) - (prev_pixel.b as i16);
    let dr_dg = dr - dg;
    let db_dg = db - dg;
    let dg_range = -32..32;
    let drdb_range = -8..8;
    if dg_range.contains(&dg) && drdb_range.contains(&dr_dg) && drdb_range.contains(&db_dg) {
        Some([(dr + 8) as u8, (dg + 32) as u8, (db + 8) as u8])
    } else {
        None
    }
}

fn create_qoif_header(id: &ImageData) -> [u8; 14] {
    let channels = get_pixel_size(id.png_type);
    let width = id.width.to_be_bytes();
    let height = id.height.to_be_bytes();
    [
        0x71,
        0x6F,
        0x69,
        0x66,
        width[0],
        width[1],
        width[2],
        width[3],
        height[0],
        height[1],
        height[2],
        height[3],
        channels as u8,
        if id.is_srgb { 0 } else { 1 },
    ]
}

fn create_qoif_footer() -> [u8; 8] {
    [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
}

fn encode_qoip_rgb(p: &PixelValue) -> [u8; 4] {
    [0xFE, p.r, p.g, p.b]
}

fn encode_qoip_rgba(p: &PixelValue) -> [u8; 5] {
    [0xFF, p.r, p.g, p.b, p.a]
}

fn encode_run(run_length: u8) -> [u8; 1] {
    assert!(run_length < 63, "run_length greater than 62!");
    [0xC0 | run_length - 1]
}

fn encode_idx(idx: usize) -> [u8; 1] {
    assert!(idx < 64, "index must be in 0..63");
    [0x00 | idx as u8]
}

fn encode_diff(dr: u8, dg: u8, db: u8) -> [u8; 1] {
    [0x40 | dr << 4 | dg << 2 | db << 0]
}

fn encode_luma(dg: u8, drdg: u8, dbdg: u8) -> [u8; 2] {
    [0x80 | dg, drdg << 4 | dbdg << 0]
}

fn encode_qoif(png_data: ImageData) -> std::io::Result<()> {
    let out_fname = "out.qoi";
    let mut out_file = File::create(out_fname)?;

    out_file.write_all(&create_qoif_header(&png_data))?;

    let prev_pixel = PixelValue {
        r: 0,
        g: 0,
        b: 0,
        a: 255,
    };

    let mut prev_pixel_arr = vec![
        PixelValue {
            r: 0,
            g: 0,
            b: 0,
            a: 0
        };
        64
    ];

    let pixel_size = get_pixel_size(png_data.png_type);

    let mut prev_run: u8 = 0;
    for pixel in png_data.into_iter() {
        if pixel == prev_pixel && prev_run < 62 {
            // check for a run
            prev_run += 1;
        } else if prev_run > 0 {
            // handle leaving a run
            prev_run = 0;
            out_file.write_all(&encode_run(prev_run))?;
        } else if let Some(idx) = pixel_previously_seen(&pixel, &prev_pixel_arr) {
            // check for idx
            out_file.write_all(&encode_idx(idx))?;
        } else if let Some([dr, dg, db]) = pixel_qoi_diff(&prev_pixel, &pixel) {
            // check for diff
            out_file.write_all(&encode_diff(dr, dg, db))?;
        } else if let Some([dg, drdg, dbdg]) = pixel_luma_diff(&prev_pixel, &pixel) {
            // check for luma
            out_file.write_all(&encode_luma(dg, drdg, dbdg))?;
        } else {
            if pixel_size == 3 {
                out_file.write_all(&encode_qoip_rgb(&pixel))?;
            } else if pixel_size == 4 {
                out_file.write_all(&encode_qoip_rgba(&pixel))?;
            } else {
                panic!("How did you get here?!");
            }
        }
        prev_pixel_arr[hash_rgba(&pixel)] = pixel.clone();
    }
    if prev_run > 0 {
        out_file.write_all(&encode_run(prev_run))?;
    }
    out_file.write_all(&create_qoif_footer())?;
    Ok(())
}

fn grab_n<T: Clone>(itr: &mut std::slice::Iter<T>, n: u8) -> Option<Vec<T>> {
    let mut v: Vec<T> = vec![];
    for _ in 0..n {
        match itr.next() {
            Some(el) => v.push(el.clone()),
            None => return None,
        }
    }
    Some(v)
}

fn decode_qoif(qoi_data: ImageData) -> std::io::Result<()> {
    let out_fname = "lenna2.png";

    let path = Path::new(out_fname);
    let file = File::create(path)?;
    let ref mut w = BufWriter::new(file);

    let mut encoder = png::Encoder::new(w, qoi_data.width, qoi_data.height);
    encoder.set_color(qoi_data.png_type);
    encoder.set_depth(png::BitDepth::Eight);

    let mut writer = encoder.write_header()?;

    let prev_pixel = PixelValue {
        r: 0,
        g: 0,
        b: 0,
        a: 255,
    };

    let mut prev_pixel_arr = vec![
        PixelValue {
            r: 0,
            g: 0,
            b: 0,
            a: 0
        };
        64
    ];

    let pixel_size = get_pixel_size(qoi_data.png_type);

    let end_of_data = qoi_data.bytes.len() - 8;
    let mut qoi_iter = qoi_data.bytes[..end_of_data].iter();
    while let Some(&next_byte) = qoi_iter.next() {
        // check for 8-bit tags
        if next_byte == 0xFE {
            let rgb = grab_n(&mut qoi_iter, 3).unwrap();
        } else if next_byte == 0xFF {
            let rgba = grab_n(&mut qoi_iter, 4).unwrap();
        } else if next_byte >> 6 == 0b00 {
        } else if next_byte >> 6 == 0b01 {
        } else if next_byte >> 6 == 0b10 {
            grab_n(&mut qoi_iter, 1).unwrap();
        } else if next_byte >> 6 == 0b11 {
        } else {
            panic!("UNRECOGNIZED BYTE {}", next_byte);
        }
    }

    Ok(())
}

fn main() -> std::io::Result<()> {
    /*
     *PNG -> QOI -> PNG?
     */
    let image_data = get_encoding_file_data()?;
    match image_data.png_type {
        png::ColorType::Rgb => encode_qoif(image_data),
        png::ColorType::Rgba => encode_qoif(image_data),
        _ => panic!("can not process {:?} at this time", image_data.png_type),
    }?;
    let qoif_image_data = get_decoding_file_data()?;
    decode_qoif(qoif_image_data)?;
    Ok(())
}
