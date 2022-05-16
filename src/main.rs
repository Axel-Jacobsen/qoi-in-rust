use std::fmt;
use std::fs::File;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;
use std::path::Path;

/* Slow and non-idomatic */

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

fn get_pixel_index(p: &PixelValue, prev_seen: &Vec<PixelValue>) -> Option<usize> {
    let pixel_hash = hash_rgba(p);
    if prev_seen[pixel_hash] == *p {
        Some(pixel_hash)
    } else {
        None
    }
}

fn get_pixel_qoi_diff(prev_pixel: &PixelValue, cur_pixel: &PixelValue) -> Option<[u8; 3]> {
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

fn get_pixel_luma_diff(prev_pixel: &PixelValue, cur_pixel: &PixelValue) -> Option<[u8; 3]> {
    if prev_pixel.a != cur_pixel.a {
        return None;
    }
    let dr = (cur_pixel.r as i8).wrapping_sub(prev_pixel.r as i8);
    let dg = (cur_pixel.g as i8).wrapping_sub(prev_pixel.g as i8);
    let db = (cur_pixel.b as i8).wrapping_sub(prev_pixel.b as i8);
    let dr_dg = dr.wrapping_sub(dg);
    let db_dg = db.wrapping_sub(dg);
    let dg_range = -32..32;
    let drdb_range = -8..8;
    if dg_range.contains(&dg) && drdb_range.contains(&dr_dg) && drdb_range.contains(&db_dg) {
        Some([
            (dr as u8).wrapping_add(8),
            (dg as u8).wrapping_add(32),
            (db as u8).wrapping_add(8),
        ])
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
    assert!(0 < run_length && run_length < 63, "run_length invalid");
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

fn calc_qoif(png_data: ImageData) -> std::io::Result<Vec<u8>> {
    let mut out_file: Vec<u8> = vec![];

    out_file.extend(&create_qoif_header(&png_data));

    let mut prev_pixel = PixelValue {
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
            continue;
        } else if prev_run > 0 {
            // handle leaving a run
            out_file.extend(&encode_run(prev_run));
            prev_run = 0;
        }
        if let Some(idx) = get_pixel_index(&pixel, &prev_pixel_arr) {
            // check for idx
            out_file.extend(&encode_idx(idx));
        } else if let Some([dr, dg, db]) = get_pixel_qoi_diff(&prev_pixel, &pixel) {
            // check for diff
            out_file.extend(&encode_diff(dr, dg, db));
        } else if let Some([drdg, dg, dbdg]) = get_pixel_luma_diff(&prev_pixel, &pixel) {
            // check for luma
            out_file.extend(&encode_luma(dg, drdg, dbdg));
        } else {
            if pixel_size == 3 {
                out_file.extend(&encode_qoip_rgb(&pixel));
            } else if pixel_size == 4 {
                out_file.extend(&encode_qoip_rgba(&pixel));
            } else {
                panic!("How did you get here!");
            }
        }

        prev_pixel_arr[hash_rgba(&pixel)] = pixel.clone();
        prev_pixel = pixel;
    }
    if prev_run > 0 {
        out_file.extend(&encode_run(prev_run));
    }
    out_file.extend(&create_qoif_footer());
    Ok(out_file)
}

fn encode_qoif(png_data: ImageData) -> std::io::Result<()> {
    let out_fname = "out.qoi";
    let mut out_file = File::create(out_fname)?;
    let v = calc_qoif(png_data).unwrap();
    out_file.write_all(&v)
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

    let mut prev_pixel = PixelValue {
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
    let is_rgba = pixel_size == 4;

    // way to size this?
    let mut write_array: Vec<u8> = vec![];

    let end_of_data = qoi_data.bytes.len() - 8;
    let mut qoi_iter = qoi_data.bytes[..end_of_data].iter();
    while let Some(&next_byte) = qoi_iter.next() {
        // check for 8-bit tags
        if next_byte == 0xFE {
            let rgb = grab_n(&mut qoi_iter, 3).unwrap();
            write_array.push(rgb[0]);
            write_array.push(rgb[1]);
            write_array.push(rgb[2]);
            prev_pixel = PixelValue {
                r: rgb[0],
                g: rgb[1],
                b: rgb[2],
                a: 255,
            };
            prev_pixel_arr[hash_rgba(&prev_pixel)] = prev_pixel.clone();
        } else if next_byte == 0xFF {
            let rgba = grab_n(&mut qoi_iter, 4).unwrap();
            write_array.push(rgba[0]);
            write_array.push(rgba[1]);
            write_array.push(rgba[2]);
            write_array.push(rgba[3]);
            prev_pixel = PixelValue {
                r: rgba[0],
                g: rgba[1],
                b: rgba[2],
                a: rgba[3],
            };
            prev_pixel_arr[hash_rgba(&prev_pixel)] = prev_pixel.clone();
        } else if next_byte >> 6 == 0b00 {
            let idx = 0b00111111 & next_byte;
            let p = &prev_pixel_arr[idx as usize];
            write_array.push(p.r);
            write_array.push(p.g);
            write_array.push(p.b);
            if is_rgba {
                write_array.push(p.a);
            };
            prev_pixel = prev_pixel_arr[idx as usize].clone();
        } else if next_byte >> 6 == 0b01 {
            let dr = 0b00110000 & next_byte;
            let dg = 0b00001100 & next_byte;
            let db = 0b00000011 & next_byte;
            let r = prev_pixel.r.wrapping_add(dr).wrapping_sub(2);
            let g = prev_pixel.g.wrapping_add(dg).wrapping_sub(2);
            let b = prev_pixel.b.wrapping_add(db).wrapping_sub(2);
            write_array.push(r);
            write_array.push(g);
            write_array.push(b);
            if is_rgba {
                write_array.push(prev_pixel.a);
            }
            prev_pixel = PixelValue {
                r: r,
                g: g,
                b: b,
                a: prev_pixel.a,
            };
            prev_pixel_arr[hash_rgba(&prev_pixel)] = prev_pixel.clone();
        } else if next_byte >> 6 == 0b10 {
            let drdg_dbdg = qoi_iter.next().expect("guh!");
            let drdg = (0xF0 & drdg_dbdg) >> 4;
            let dbdg = 0x0F & drdg_dbdg;
            let dg = (0b00111111 & next_byte).wrapping_sub(32);
            let dr = drdg.wrapping_add(dg).wrapping_sub(8);
            let db = dbdg.wrapping_add(dg).wrapping_sub(8);
            let r = prev_pixel.r.wrapping_add(dr);
            let g = prev_pixel.g.wrapping_add(dg);
            let b = prev_pixel.b.wrapping_add(db);
            write_array.push(r);
            write_array.push(g);
            write_array.push(b);
            if is_rgba {
                write_array.push(prev_pixel.a);
            }
            prev_pixel = PixelValue {
                r: r,
                g: g,
                b: b,
                a: prev_pixel.a,
            };
            prev_pixel_arr[hash_rgba(&prev_pixel)] = prev_pixel.clone();
        } else if next_byte >> 6 == 0b11 {
            let run_length = (0b00111111 & next_byte) + 1;
            for _ in 0..run_length {
                write_array.push(prev_pixel.r);
                write_array.push(prev_pixel.g);
                write_array.push(prev_pixel.b);
                if is_rgba {
                    write_array.push(prev_pixel.a);
                };
            }
        } else {
            panic!("UNRECOGNIZED BYTE {}", next_byte);
        }
    }
    writer.write_image_data(&write_array).unwrap();

    Ok(())
}

fn write_dummy() -> std::io::Result<()> {
    let out_fname = "u.png";

    let path = Path::new(out_fname);
    let file = File::create(path)?;
    let ref mut w = BufWriter::new(file);

    let width: u8 = 255;
    let height: u8 = 255;

    let mut encoder = png::Encoder::new(w, width as u32, height as u32);
    encoder.set_color(png::ColorType::Rgb);
    encoder.set_depth(png::BitDepth::Eight);

    let mut writer = encoder.write_header()?;

    let mut write_array: Vec<u8> = vec![];

    for i in 0..width {
        for j in 0..height {
            write_array.push(127 * (j % 2) * (i % 2));
            write_array.push(85 * (j % 3) * (i % 2));
            write_array.push(63 * (j % 4) * (i % 2));
        }
    }

    writer.write_image_data(&write_array).unwrap();

    Ok(())
}

fn main() -> std::io::Result<()> {
    /*
     *PNG -> QOI -> PNG?
     */
    write_dummy().unwrap();
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

#[cfg(test)]
mod tests {
    use crate::calc_qoif;
    use crate::hash_rgba;
    use crate::ImageData;
    use crate::PixelValue;

    fn gen_image_data(v: Vec<u8>, width: u32, height: u32, is_rgba: bool) -> ImageData {
        ImageData {
            png_type: if is_rgba {
                png::ColorType::Rgba
            } else {
                png::ColorType::Rgb
            },
            bytes: v,
            width: width,
            height: height,
            is_srgb: true,
        }
    }

    #[test]
    fn basic_rgb() {
        let id = gen_image_data(vec![0x88, 0xFF, 0x88], 1, 1, false);
        let out = calc_qoif(id).unwrap();
        println!("{:?}", out);
        assert!(out.len() == 14 + 4 + 8);
        assert!(out[14] == 0xFE);
        assert!(out[15] == 0x88);
        assert!(out[16] == 0xFF);
        assert!(out[17] == 0x88);
    }

    #[test]
    fn basic_rgba() {
        let id = gen_image_data(vec![0x12, 0x34, 0x56, 0x78], 1, 1, true);
        let out = calc_qoif(id).unwrap();
        println!("{:?}", out);
        assert!(out.len() == 14 + 5 + 8);
        assert!(out[14] == 0xFF);
        assert!(out[15] == 0x12);
        assert!(out[16] == 0x34);
        assert!(out[17] == 0x56);
        assert!(out[18] == 0x78);
    }

    #[test]
    fn basic_diff() {
        let id = gen_image_data(vec![1, 1, 1], 1, 1, false);
        let out = calc_qoif(id).unwrap();
        println!("{:?}", out);
        assert!(out.len() == 14 + 1 + 8);
        assert!(out[14] == 0x40 | (1 + 2) << 4 | (1 + 2) << 2 | (1 + 2) << 0);
    }

    #[test]
    fn multi_diff() {
        let id = gen_image_data(vec![1, 1, 1, 2, 2, 2, 0, 0, 0], 3, 1, false);
        let out = calc_qoif(id).unwrap();
        println!("{:?}", out);
        assert!(out.len() == 14 + 3 + 8);
        assert!(out[14] == 0x40 | 1 + 2 << 4 | 1 + 2 << 2 | 1 + 2 << 0);
        assert!(out[15] == 0x40 | 1 + 2 << 4 | 1 + 2 << 2 | 1 + 2 << 0);
        assert!(out[16] == 0x40 | 0 << 4 | 0 << 2 | 0 << 0);
    }

    #[test]
    fn basic_index() {
        let id = gen_image_data(
            vec![10, 10, 10, 23, 255, 23, 96, 45, 33, 10, 10, 10],
            4,
            1,
            false,
        );
        let out = calc_qoif(id).unwrap();
        println!("{:?}", out);

        let repeated_px = PixelValue {
            r: 10,
            g: 10,
            b: 10,
            a: 255,
        };
        let repeated_px_idx = hash_rgba(&repeated_px);

        // luma + 2 * rgb + idx
        let expected_file_len = 14 + 2 + 2 * 4 + 1 + 8;
        assert!(
            out.len() == expected_file_len,
            "out len is {} expected {}",
            out.len(),
            expected_file_len
        );
        assert!(
            out[24] == 0x00 | repeated_px_idx as u8,
            "repeated_px_idx is {}",
            repeated_px_idx
        );
    }
}
