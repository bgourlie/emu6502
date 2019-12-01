use crate::parser::types::{Expression, Line, Symbol};
use fnv::FnvHashMap;
use std::{borrow::Borrow, rc::Rc};

// TODO: We need to determine which lines are "live" by resolving any compiler expressions (if/else)
// State machine?
// - Inside If (expression result: true/false)
// - Inside else (expression result: true/false)
// - Inside macro

#[derive(Debug)]
pub enum ResolveError<'a> {
    LabelAlreadyDefined(usize, &'a str),
    MacroAlreadyDefined(usize, &'a str),
    VariableAlreadyDefined(usize, &'a str),
    SymbolNotDefined(&'a str),
}

pub struct Resolver<'a> {
    lines: Vec<Line<'a>>,
    line_state: Vec<bool>,
    variables: FnvHashMap<&'a str, Rc<Expression<'a>>>,
    label_map: FnvHashMap<&'a str, usize>,
    macro_map: FnvHashMap<&'a str, usize>,
}

impl<'a> Resolver<'a> {
    pub fn new(num_lines: usize) -> Resolver<'a> {
        let lines = Vec::with_capacity(num_lines);
        Resolver {
            lines,
            line_state: vec![false; num_lines],
            variables: FnvHashMap::default(),
            label_map: FnvHashMap::default(),
            macro_map: FnvHashMap::default(),
        }
    }
    pub fn resolve_line(&mut self, line: Line<'a>) -> Result<(), ResolveError<'a>> {
        let cur_line = self.lines.len();
        self.lines.push(line);
        let result = match &self.lines[cur_line] {
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
            Line::Equals(var, expr) => {
                // TODO: Move this up to where we determine we've entered a new scope
                if self.variables.contains_key(var) {
                    Err(ResolveError::VariableAlreadyDefined(cur_line, var))
                } else {
                    self.variables.insert(var, Rc::clone(expr));
                    Ok(())
                }
            }
            _ => Ok(()),
        };
        result
    }

    fn resolve_expr(&self, expr: Rc<Expression<'a>>) -> Result<i32, ResolveError<'a>> {
        match expr.borrow() {
            Expression::Literal(val) => Ok(i32::from(*val)),
            Expression::Symbol(Symbol::Named(name)) => {
                if let Some(expr) = self.variables.get(name) {
                    self.resolve_expr(Rc::clone(expr))
                } else {
                    Err(ResolveError::SymbolNotDefined(name))
                }
            }
            _ => Ok(0),
        }
    }

    pub fn print_macros(&self) {
        println!("LABEL DEFINITIONS ({}):", self.label_map.len());
        for (label, line) in self.label_map.iter() {
            println!("{}: {:?}", label, line);
        }

        println!("macro declarations:");
        for (macro_name, line_index) in self.macro_map.iter() {
            println!("{}: {}", line_index, macro_name);
        }

        println!("global macro declarations:");
        for (var, expr) in self.variables.iter() {
            println!("{}: {:?}", var, expr);
        }
    }
}
