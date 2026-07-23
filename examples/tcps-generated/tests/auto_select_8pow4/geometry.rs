fn gray(binary: u16) -> u16 {
    binary ^ (binary >> 1)
}

fn ungray(mut value: u16) -> u16 {
    let mut binary = 0_u16;
    while value != 0 {
        binary ^= value;
        value >>= 1;
    }
    binary
}

fn unpack(value: u16) -> Coordinates {
    Coordinates {
        authority: (value & 7) as u8,
        readiness: ((value >> 3) & 7) as u8,
        time_budget: ((value >> 6) & 7) as u8,
        mode: ((value >> 9) & 7) as u8,
    }
}

fn cells() -> Result<Vec<Cell>, EvidenceError> {
    (0..CELL_COUNT)
        .map(|ordinal| {
            let ordinal =
                u16::try_from(ordinal).map_err(|error| EvidenceError::InvalidData {
                    identity: "tcps-8pow4-ordinal".to_owned(),
                    message: error.to_string(),
                })?;
            let gray = gray(ordinal);
            let coordinates = unpack(gray);
            coordinates.validate()?;
            Ok(Cell {
                ordinal,
                gray,
                coordinates,
            })
        })
        .collect()
}

const fn gf8_mul(mut left: u8, mut right: u8) -> u8 {
    let mut product = 0_u8;
    let mut step = 0_u8;
    while step < 3 {
        if right & 1 != 0 {
            product ^= left;
        }
        let carry = left & 4;
        left <<= 1;
        if carry != 0 {
            left ^= 0b1011;
        }
        right >>= 1;
        step += 1;
    }
    product & 7
}

fn coverage_rails() -> [Vec<Coordinates>; DIMENSIONS] {
    let strength_one = (0..WIDTH as u8)
        .map(|state| Coordinates {
            authority: state,
            readiness: state,
            time_budget: state,
            mode: state,
        })
        .collect();

    let mut strength_two = Vec::with_capacity(WIDTH.pow(2));
    for x in 0..WIDTH as u8 {
        for y in 0..WIDTH as u8 {
            strength_two.push(Coordinates {
                authority: x,
                readiness: y,
                time_budget: x ^ y,
                mode: x ^ gf8_mul(2, y),
            });
        }
    }

    let mut strength_three = Vec::with_capacity(WIDTH.pow(3));
    for x in 0..WIDTH as u8 {
        for y in 0..WIDTH as u8 {
            for z in 0..WIDTH as u8 {
                strength_three.push(Coordinates {
                    authority: x,
                    readiness: y,
                    time_budget: z,
                    mode: x ^ y ^ z,
                });
            }
        }
    }

    let strength_four = cells()
        .expect("valid exhaustive cube")
        .into_iter()
        .map(|cell| cell.coordinates)
        .collect();

    [
        strength_one,
        strength_two,
        strength_three,
        strength_four,
    ]
}
