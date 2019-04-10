use cfg_if::cfg_if;

cfg_if! {
    // When the `console_error_panic_hook` feature is enabled, we can call the
    // `set_panic_hook` function at least once during initialization, and then
    // we will get better error messages if our code ever panics.
    //
    // For more details see
    // https://github.com/rustwasm/console_error_panic_hook#readme
    if #[cfg(feature = "console_error_panic_hook")] {
        extern crate console_error_panic_hook;
        pub use self::console_error_panic_hook::set_once as set_panic_hook;
    } else {
        #[inline]
        pub fn set_panic_hook() {}
    }
}

pub fn set_bit(b: u8, n: u8) -> u8 {
    n | (1 << b)
}

pub fn reset_bit(b: u8, n: u8) -> u8 {
    n & (0xff ^ (1 << b))
}

pub fn test_bit(b: u8, n: u8) -> u8 {
    (n & (1 << b)) >> b
}

pub fn check_half_carry_8(a: u8, b: u8) -> bool {
    ((a.wrapping_add(b)) ^ b ^ a) & 0x10 == 0x10
}

pub fn check_half_carry_16(a: u16, b: u16) -> bool {
    ((a.wrapping_add(b)) ^ b ^ a) & 0x1000 == 0x1000
}

pub fn check_carry_8(a: u8, b: u8) -> bool {
    a.wrapping_add(b) < a
}

pub fn check_carry_16(a: u16, b: u16) -> bool {
    a.wrapping_add(b) < a
}

pub fn check_half_borrow_8(a: u8, b: u8) -> bool {
    ((a.wrapping_sub(b)) ^ b ^ a) & 0x10 == 0x10
}

pub fn check_borrow_8(a: u8, b: u8) -> bool {
    a < b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_set_bit() {
        assert_eq!(set_bit(0, 0), 1);
        assert_eq!(set_bit(1, 0), 2);
        assert_eq!(set_bit(2, 0), 4);
        assert_eq!(set_bit(3, 0), 8);
    }

    #[test]
    fn test_reset_bit() {
        assert_eq!(reset_bit(0, 0b1111), 0b1110);
        assert_eq!(reset_bit(1, 0b1111), 0b1101);
        assert_eq!(reset_bit(2, 0b1111), 0b1011);
        assert_eq!(reset_bit(3, 0b1111), 0b0111);
    }

    #[test]
    fn test_test_bit() {
        assert_eq!(test_bit(0, 0), 0);
        assert_eq!(test_bit(0, 1), 1);
        assert_eq!(test_bit(7, 0x8), 0);
        assert_eq!(test_bit(7, 0x80), 1);
        assert_eq!(test_bit(5, 0xf0), 1);
        assert_eq!(test_bit(5, 0xf), 0);
    }

    #[test]
    fn test_check_half_carry_8() {
        assert!(check_half_carry_8(0xf, 0x1));
        assert!(!check_half_carry_8(0xf, 0x0));
    }

    #[test]
    fn test_check_half_carry_16() {
        assert!(check_half_carry_16(0xfff, 0x1));
        assert!(!check_half_carry_16(0xfff, 0x0));
    }

    #[test]
    fn test_check_carry_8() {
        assert!(check_carry_8(0xff, 0x1));
        assert!(!check_carry_8(0xfe, 0x1))
    }

    #[test]
    fn test_check_carry_16() {
        assert!(check_carry_16(0xffff, 0x1));
        assert!(!check_carry_16(0xfffe, 0x1));
    }

    #[test]
    fn test_check_half_borrow_8() {
        assert!(check_half_borrow_8(0x10, 0xf));
        assert!(!check_half_borrow_8(0xff, 0x1));
    }

    #[test]
    fn test_check_borrow_8() {
        assert!(check_borrow_8(0x1, 0x2));
        assert!(!check_borrow_8(0xff, 0x1));
    }
}
