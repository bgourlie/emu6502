#[cfg(test)]
mod tests;

use crate::types::{BinaryOperator, Expression, Line, Symbol, UnaryOperator};
use fnv::FnvHashMap;
use std::{borrow::Borrow, convert::TryFrom, rc::Rc};

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
    InvalidShiftOperand,
}

pub struct Resolver<'a> {
    lines: Vec<Line<'a>>,
    line_state: Vec<bool>,
    cur_addr: u16,
    variables: FnvHashMap<&'a str, i32>,
    label_map: FnvHashMap<&'a str, usize>,
    macro_map: FnvHashMap<&'a str, usize>,
}

impl<'a> Resolver<'a> {
    pub fn new(num_lines: usize) -> Resolver<'a> {
        let lines = Vec::with_capacity(num_lines);
        Resolver {
            lines,
            cur_addr: 0,
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
                if self.variables.contains_key(var) {
                    Err(ResolveError::VariableAlreadyDefined(cur_line, var))
                } else {
                    let val = self.resolve_expr(Rc::clone(expr))?;
                    self.variables.insert(var, val);
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
            Expression::CurrentAddress => Ok(i32::from(self.cur_addr)),
            Expression::Symbol(Symbol::MacroArg(_arg_num)) => unimplemented!(),
            Expression::Symbol(Symbol::Named(name)) => {
                if let Some(val) = self.variables.get(name) {
                    Ok(*val)
                } else {
                    Err(ResolveError::SymbolNotDefined(name))
                }
            }
            Expression::Grouping(expr) => self.resolve_expr(Rc::clone(expr)),
            Expression::Binary(left, operation, right) => {
                self.resolve_expr(Rc::clone(left)).and_then(|left| {
                    self.resolve_expr(Rc::clone(right))
                        .and_then(|right| match operation {
                            BinaryOperator::Equals => Ok(i32::from(left == right)),
                            BinaryOperator::NotEquals => Ok(i32::from(left != right)),
                            BinaryOperator::LessThanOrEquals => Ok(i32::from(left <= right)),
                            BinaryOperator::LessThan => Ok(i32::from(left < right)),
                            BinaryOperator::GreaterThanOrEquals => Ok(i32::from(left >= right)),
                            BinaryOperator::GreaterThan => Ok(i32::from(left > right)),
                            BinaryOperator::Subtraction => Ok(left - right),
                            BinaryOperator::Addition => Ok(left + right),
                            BinaryOperator::Multiply => Ok(left * right),
                            BinaryOperator::Or => Ok(left | right),
                            BinaryOperator::And => Ok(left & right),
                            BinaryOperator::Xor => Ok(left ^ right),
                            BinaryOperator::RightShift => u16::try_from(right)
                                .and_then(|shift_amount| Ok(left >> shift_amount))
                                .map_err(|_| ResolveError::InvalidShiftOperand),
                            BinaryOperator::LeftShift => u16::try_from(right)
                                .and_then(|shift_amount| Ok(left << shift_amount))
                                .map_err(|_| ResolveError::InvalidShiftOperand),
                        })
                })
            }
            Expression::Unary(operator, expr) => {
                self.resolve_expr(Rc::clone(expr))
                    .and_then(|val| match operator {
                        UnaryOperator::Complement => Ok(!val),
                        UnaryOperator::Negation => Ok(-val),
                        UnaryOperator::LogicalNot => Ok(i32::from(val <= 0)),
                    })
            }
            Expression::Hi(expr) => self
                .resolve_expr(Rc::clone(expr))
                .and_then(|val| Ok((val & 0xffff) >> 8)),
            Expression::Lo(expr) => self
                .resolve_expr(Rc::clone(expr))
                .and_then(|val| Ok(val & 0xff)),
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
