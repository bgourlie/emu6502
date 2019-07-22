use std::{
    mem::{self, MaybeUninit},
    ptr,
};

const BUFFER_SIZE: u16 = 0x100;

pub struct ConsoleBuffer {
    messages: Box<[Option<String>; BUFFER_SIZE as usize]>,
    ptr: u16,
}

impl Default for ConsoleBuffer {
    fn default() -> Self {
        let arr = {
            let mut arr: [MaybeUninit<Option<String>>; BUFFER_SIZE as usize] =
                unsafe { MaybeUninit::uninit().assume_init() };

            for elem in &mut arr[..] {
                unsafe { ptr::write(elem.as_mut_ptr(), None) }
            }
            unsafe { mem::transmute::<_, [Option<String>; BUFFER_SIZE as usize]>(arr) }
        };
        ConsoleBuffer {
            messages: Box::new(arr),
            ptr: 0,
        }
    }
}

impl ConsoleBuffer {
    pub fn push<S: Into<String>>(&mut self, message: S) {
        self.messages[usize::from(self.ptr)] = Some(message.into());
        self.ptr = if self.ptr < BUFFER_SIZE - 1 {
            self.ptr + 1
        } else {
            0
        }
    }

    pub fn iter(&self) -> Iter {
        Iter {
            buffer: self,
            cur_index: u32::from(self.ptr),
            iter_count: 0,
        }
    }
}

pub struct Iter<'a> {
    buffer: &'a ConsoleBuffer,
    cur_index: u32,
    iter_count: u32,
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter_count < u32::from(BUFFER_SIZE) {
            self.iter_count += 1;
            let cur_index = self.cur_index;
            self.cur_index += 1;
            self.buffer.messages[(cur_index % u32::from(BUFFER_SIZE)) as usize]
                .as_ref()
                .map(|s| &s[..])
        } else {
            None
        }
    }
}

#[test]
fn test_console_buffer() {
    let mut buffer = ConsoleBuffer::default();
    for i in 0..256 {
        buffer.push(i.to_string());
    }

    let messages: Vec<&str> = buffer.iter().collect();

    assert_eq!(messages.len(), 256);
    for i in 0..256 {
        assert_eq!(messages[i], i.to_string().as_str());
    }

    buffer.push("256");
    let messages: Vec<&str> = buffer.iter().collect();

    assert_eq!(messages.len(), 256);
    for i in 1..257 {
        assert_eq!(messages[i - 1], i.to_string().as_str());
    }
}
