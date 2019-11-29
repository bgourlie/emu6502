use crate::parser::types::Line;
use fnv::{FnvHashMap, FnvHashSet};

pub enum ResolveError<'a> {
    LabelAlreadyDefined(&'a str),
    MacroAlreadyDefined(&'a str),
}

#[derive(Default)]
pub struct Resolver<'a> {
    cur_addr: u32,
    label_map: FnvHashMap<&'a str, u32>,
    /// Tracking macro declarations in a set for now, but at some point I imagine it should hold
    /// some reference to its lines.
    macro_set: FnvHashSet<&'a str>,
}

impl<'a> Resolver<'a> {
    pub fn next_line(&mut self, line: Line<'a>) -> Result<(), ResolveError<'a>> {
        match line {
            Line::Instruction(maybe_label, _op, _opcode) => {
                if let Some(label) = maybe_label {
                    if self.label_map.contains_key(label) {
                        Err(ResolveError::LabelAlreadyDefined(label))
                    } else {
                        self.label_map.insert(label, self.cur_addr);
                        self.cur_addr += 1;
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            }
            Line::MacroStart(macro_name) => {
                if self.macro_set.contains(macro_name) {
                    Err(ResolveError::MacroAlreadyDefined(macro_name))
                } else {
                    self.macro_set.insert(macro_name);
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }

    pub fn print_macros(&self) {
        println!("LABEL DEFINITIONS:");
        for (label, addr) in self.label_map.iter() {
            println!("{:04X}: {}", addr, label);
        }

        println!("MACRO DECLARATIONS:");

        for macro_name in self.macro_set.iter() {
            println!("{}", macro_name);
        }
    }
}
