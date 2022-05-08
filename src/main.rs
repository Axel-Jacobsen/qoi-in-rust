#![allow(unused)]

use std::fmt;
use std::fs::File;
use std::io::Write;

// make config'able? also, this seems very verbose
const CHUNK_SIZE: usize = 1e6 as usize;

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

#[derive(Clone, PartialEq, Eq)]
struct PixelValue {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

fn hash_rgba(px: &PixelValue) -> usize {
    // use Wrapping? probably faster?
    ((3_u64 * (px.r as u64)
        + 5_u64 * (px.g as u64)
        + 7_u64 * (px.b as u64)
        + 11_u64 * (px.a as u64))
        % 64) as usize
}

// i know that this is also garb but please be kind (for now)
fn get_file_data() -> Result<ImageData, png::DecodingError> {
    let decoder = png::Decoder::new(File::open("lenna.png").unwrap());

    let mut reader = decoder.read_info()?;
    let mut buf = vec![0; reader.output_buffer_size()];

    let info = reader.next_frame(&mut buf)?;
    let bytes = &buf[..info.buffer_size()];

    let image_info = reader.info();
    let is_srgb = if let Some(srgb_type) = image_info.srgb {
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

fn get_qoif_header(id: &ImageData) -> [u8; 14] {
    let channels = get_pixel_size(id.png_type);
    let colorspace = id.is_srgb;
    let width = id.width.to_ne_bytes();
    let height = id.height.to_ne_bytes();
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

fn get_qoif_footer() -> [u8; 8] {
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

    out_file.write_all(&get_qoif_header(&png_data));

    let pixel_size = get_pixel_size(png_data.png_type);

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
            out_file.write_all(&encode_run(prev_run));
        } else if let Some(idx) = pixel_previously_seen(&pixel, &prev_pixel_arr) {
            // check for idx
            out_file.write_all(&encode_idx(idx));
        } else if let Some([dr, dg, db]) = pixel_qoi_diff(&prev_pixel, &pixel) {
            // check for diff
            out_file.write_all(&encode_diff(dr, dg, db));
        } else if let Some([dg, drdg, dbdg]) = pixel_luma_diff(&prev_pixel, &pixel) {
            // check for luma
            out_file.write_all(&encode_luma(dg, drdg, dbdg));
        } else {
            if pixel_size == 3 {
                out_file.write_all(&encode_qoip_rgb(&pixel));
            } else if pixel_size == 4 {
                out_file.write_all(&encode_qoip_rgba(&pixel));
            } else {
                panic!("How did you get here?!");
            }
        }
        prev_pixel_arr[hash_rgba(&pixel)] = pixel.clone();
    }
    out_file.write_all(&get_qoif_footer());
    Ok(())
}

fn main() {
    /*
     *PNG -> QOI -> PNG?
     */
    let image_data = get_file_data().unwrap();
    match image_data.png_type {
        png::ColorType::Rgb => encode_qoif(image_data),
        png::ColorType::Rgba => encode_qoif(image_data),
        _ => panic!("can not process {:?} at this time", image_data.png_type),
    };
}
