use std::{
    mem::{self, MaybeUninit},
    ptr,
};

pub struct ConsoleBuffer {
    messages: Box<[Option<String>; std::u8::MAX as usize]>,
    ptr: u8,
}

impl Default for ConsoleBuffer {
    fn default() -> Self {
        let arr = {
            let mut arr: [MaybeUninit<Option<String>>; std::u8::MAX as usize] =
                unsafe { MaybeUninit::uninit().assume_init() };

            for elem in &mut arr[..] {
                unsafe { ptr::write(elem.as_mut_ptr(), None) }
            }

            unsafe { mem::transmute::<_, [Option<String>; std::u8::MAX as usize]>(arr) }
        };
        ConsoleBuffer {
            messages: Box::new(arr),
            ptr: 0,
        }
    }
}

impl ConsoleBuffer {
    fn push<S: Into<String>>(&mut self, message: S) {
        self.messages[usize::from(self.ptr)] = Some(message.into());
        self.ptr = self.ptr.wrapping_add(1);
    }
}

pub struct Iter<'a> {
    buffer: &'a ConsoleBuffer,
    start_index: u8,
    cur_index: u8
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        self.buffer.messages[usize::from(self.cur_index)].as_ref().map(|s| &s[..])
    }
}

