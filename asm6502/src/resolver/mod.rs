use crate::parser::types::Line;
use fnv::FnvHashMap;

// TODO: We need to determine which lines are "live" by resolving any compiler expressions (if/else)
// State machine?
// - Inside If (expression result: true/false)
// - Inside else (expression result: true/false)
// - Inside macro

#[derive(Debug)]
pub enum ResolveError<'a> {
    LabelAlreadyDefined(usize, &'a str),
    MacroAlreadyDefined(usize, &'a str),
}

pub struct Resolver<'a> {
    lines: Vec<Line<'a>>,
    line_state: Vec<bool>,
    label_map: FnvHashMap<&'a str, usize>,
    macro_map: FnvHashMap<&'a str, usize>,
}

impl<'a> Resolver<'a> {
    pub fn new(num_lines: usize) -> Resolver<'a> {
        let lines = Vec::with_capacity(num_lines);
        Resolver {
            line_state: vec![false; num_lines],
            label_map: FnvHashMap::default(),
            macro_map: FnvHashMap::default(),
            lines,
        }
    }
    pub fn resolve_line(&mut self, line: Line<'a>) -> Result<(), ResolveError<'a>> {
        let cur_line = self.lines.len();
        let result = match &line {
            Line::Instruction(maybe_label, _op, _opcode) => {
                if let Some(label) = maybe_label {
                    if self.label_map.contains_key(label) {
                        Err(ResolveError::LabelAlreadyDefined(cur_line, label))
                    } else {
                        self.label_map.insert(label, cur_line);
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            }
            Line::MacroStart(macro_name) => {
                if self.macro_map.contains_key(macro_name) {
                    Err(ResolveError::MacroAlreadyDefined(cur_line, macro_name))
                } else {
                    self.macro_map.insert(macro_name, cur_line);
                    Ok(())
                }
            }
            _ => Ok(()),
        };

        self.lines.push(line);
        result
    }

    pub fn print_macros(&self) {
        println!("LABEL DEFINITIONS ({}):", self.label_map.len());
        for (label, line) in self.label_map.iter() {
            println!("{}: {:?}", label, line);
        }

        println!("MACRO DECLARATIONS:");

        for (macro_name, line_index) in self.macro_map.iter() {
            println!("{}: {}", line_index, macro_name);
        }
    }
}
