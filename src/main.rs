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
                    _ => PixelValue {
                        r: chunk[0],
                        g: chunk[1],
                        b: chunk[2],
                        a: chunk[3],
                    },
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

#[derive(Clone, PartialEq, Eq)]
struct PixelValue {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

fn hash_rgba(px: PixelValue) -> usize {
    ((3 * px.r + 5 * px.g + 7 * px.b + 11 * px.a) % 64) as usize
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

fn encode_qoif(png_data: ImageData) -> std::io::Result<()> {
    let out_fname = "out.qf";
    let mut out_file = File::create(out_fname)?;
    let mut write_buf = [0; CHUNK_SIZE];

    out_file.write(&get_qoif_header(&png_data));

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
            a: 255
        };
        64
    ];

    let pixel_size = get_pixel_size(png_data.png_type);

    let prev_run: Vec<PixelValue> = vec![];

    for pixel in png_data.into_iter() {
        if pixel == prev_pixel {}
        // check for index
        // check for diff
        // check for luma
        // check for run
        // default to RGB or RGBA
    }
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
