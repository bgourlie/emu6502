pub fn to_u16(low: u8, high: u8) -> u16 {
    u16::from(low) | u16::from(high) << 8
}
