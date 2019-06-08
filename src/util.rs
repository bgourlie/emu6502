pub fn from_u16(val: u16) -> (u8, u8) {
    let low_byte = (val & 0xff) as u8;
    let high_byte = ((val >> 8) & 0xff) as u8;
    (low_byte, high_byte)
}

pub fn to_u16(low: u8, high: u8) -> u16 {
    u16::from(low) | u16::from(high) << 8
}
