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
            cur_index: if self.ptr > 0 {
                self.ptr - 1
            } else {
                BUFFER_SIZE - 1
            },
            iter_count: 0,
        }
    }
}

pub struct Iter<'a> {
    buffer: &'a ConsoleBuffer,
    cur_index: u16,
    iter_count: u16,
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if BUFFER_SIZE > self.iter_count {
            self.iter_count += 1;
            let cur_index = self.cur_index;
            self.cur_index = if cur_index > 0 {
                cur_index - 1
            } else {
                BUFFER_SIZE - 1
            };
            self.buffer.messages[usize::from(cur_index)]
                .as_ref()
                .map(|s| &s[..])
        } else {
            None
        }
    }
}

#[test]
fn test_console_buffer_two() {
    let mut buffer = ConsoleBuffer::default();
    buffer.push("test");
    let messages: Vec<&str> = buffer.iter().collect();
    assert_eq!(messages.len(), 1);
    assert_eq!(messages[0], "test");
}

#[test]
fn test_console_buffer() {
    let mut buffer = ConsoleBuffer::default();
    for i in 0..BUFFER_SIZE {
        buffer.push(i.to_string());
    }

    let messages: Vec<&str> = buffer.iter().collect();

    assert_eq!(messages.len(), usize::from(BUFFER_SIZE));
    for i in 0..BUFFER_SIZE {
        assert_eq!(
            messages[usize::from(i)],
            (BUFFER_SIZE - 1 - i).to_string().as_str()
        );
    }

    buffer.push("256");
    let messages: Vec<&str> = buffer.iter().collect();

    assert_eq!(messages.len(), usize::from(BUFFER_SIZE));
    for i in 1..(BUFFER_SIZE + 1) {
        assert_eq!(
            messages[usize::from(i) - 1],
            usize::from(BUFFER_SIZE + 1 - i).to_string().as_str()
        );
    }
}
