#[cfg(test)]
mod tests;

use crate::types::{BinaryOperator, Expression, Line, Symbol, UnaryOperator};
use fnv::FnvHashMap;
use std::{borrow::Borrow, convert::TryFrom, rc::Rc};

#[derive(Debug, PartialOrd, PartialEq)]
pub enum ResolveError<'a> {
    LabelAlreadyDefined(&'a str),
    MacroAlreadyDefined(&'a str),
    VariableAlreadyDefined(&'a str),
    SymbolNotDefined(&'a str),
    InvalidShiftOperand,
    MismatchedElse,
    MismatchedEndIf,
    InvalidMacroDefinition,
    InvalidEndMacroDefinition,
}

#[derive(Default)]
struct LivenessContext {
    if_stack: Vec<bool>,
}

impl<'a> LivenessContext {
    fn is_live(&self) -> bool {
        self.if_stack.iter().rev().all(|live| *live)
    }

    fn push(&mut self, live: bool) {
        self.if_stack.push(live);
    }

    fn pop(&mut self) -> Result<(), ResolveError<'a>> {
        self.if_stack
            .pop()
            .map(|_live| ())
            .ok_or_else(|| ResolveError::MismatchedEndIf)
    }

    fn invert_head(&mut self) -> Result<(), ResolveError<'a>> {
        self.if_stack
            .last_mut()
            .ok_or_else(|| ResolveError::MismatchedElse)
            .and_then(|val| {
                *val = !*val;
                Ok(())
            })
    }
}

#[test]
fn test_liveness_context() {
    // When the if-context is empty, we are live by default
    let mut context = LivenessContext::default();
    assert_eq!(true, context.is_live());

    // Inverting the head when the if-stack is empty should be an error indicating mismatched else
    // statement
    assert_eq!(Err(ResolveError::MismatchedElse), context.invert_head());

    // The top of the stack determines whether or not we're live
    context.push(false);
    assert_eq!(false, context.is_live());

    // If we push true, but the previous head is false, then liveness is still false
    context.push(true);
    assert_eq!(false, context.is_live());

    // We invert the head, but a previous entry in the if-stack is false
    context.invert_head().unwrap();
    assert_eq!(false, context.is_live());

    // Empty the if-stack
    context.pop().unwrap();
    context.pop().unwrap();

    // Push false, invert the head, live should be true
    context.push(false);
    context.invert_head().unwrap();
    assert_eq!(true, context.is_live());
}

#[derive(Debug, PartialEq)]
struct MacroLine<'a> {
    line_num: u16,
    line: &'a Line<'a>,
}

impl<'a> MacroLine<'a> {
    fn new(line_num: u16, line: &'a Line<'a>) -> MacroLine {
        MacroLine { line_num, line }
    }
}

#[derive(Debug)]
struct Macro<'a> {
    name: &'a str,
    invocation_count: u16,
    lines: Vec<MacroLine<'a>>,
}

impl<'a> Macro<'a> {
    fn new(name: &'a str, lines: Vec<MacroLine<'a>>) -> Macro<'a> {
        Macro {
            name,
            invocation_count: 0,
            lines,
        }
    }
}

#[derive(Default)]
struct MacroContext<'a> {
    cur_macro: Option<(&'a str, Vec<MacroLine<'a>>)>,
}

impl<'a> MacroContext<'a> {
    fn recording_macro(&self) -> bool {
        self.cur_macro.is_some()
    }

    fn add_line(&mut self, line: MacroLine<'a>) {
        self.cur_macro.as_mut().and_then(|(_macro_name, lines)| {
            lines.push(line);
            Some(())
        });
    }

    fn start_recording(&mut self, macro_name: &'a str) -> Result<(), ResolveError<'a>> {
        if self.cur_macro.is_some() {
            Err(ResolveError::InvalidMacroDefinition)
        } else {
            self.cur_macro = Some((macro_name, Vec::new()));
            Ok(())
        }
    }

    fn stop_recording(&mut self) -> Result<Macro<'a>, ResolveError<'a>> {
        self.cur_macro
            .take()
            .map(|(name, lines)| Macro::new(name, lines))
            .ok_or_else(|| ResolveError::InvalidEndMacroDefinition)
    }
}

#[derive(Default)]
pub struct Resolver<'a> {
    cur_line: u16,
    cur_addr: u16,
    liveness_context: LivenessContext,
    macro_context: MacroContext<'a>,
    variables: FnvHashMap<&'a str, i32>,
    label_map: FnvHashMap<&'a str, u16>,
    macro_map: FnvHashMap<&'a str, Macro<'a>>,
}

impl<'a> Resolver<'a> {
    fn record_label(&mut self, label: &'a str) -> Result<(), ResolveError<'a>> {
        if self.label_map.contains_key(label) {
            Err(ResolveError::LabelAlreadyDefined(label))
        } else {
            self.label_map.insert(label, self.cur_line);
            Ok(())
        }
    }

    pub fn resolve_line(&mut self, line: &'a Line<'a>) -> Result<(), (u16, ResolveError<'a>)> {
        self.cur_line += 1;

        match line {
            Line::MacroInvocationOrLabel(macro_name_or_label) => {
                if self.liveness_context.is_live() {
                    if !self.macro_context.recording_macro() {
                        if let Some(_mac) = self.macro_map.get(macro_name_or_label) {
                            // TODO: Macro invocation
                            Ok(())
                        } else {
                            self.record_label(macro_name_or_label)
                        }
                    } else {
                        self.macro_context
                            .add_line(MacroLine::new(self.cur_line, line));
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            }
            Line::Instruction(maybe_label, _op, _opcode) => {
                if self.liveness_context.is_live() {
                    if !self.macro_context.recording_macro() {
                        if let Some(label) = maybe_label {
                            self.record_label(label)
                        } else {
                            Ok(())
                        }
                    } else {
                        self.macro_context
                            .add_line(MacroLine::new(self.cur_line, line));
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            }
            Line::MacroStart(macro_name) => {
                if self.liveness_context.is_live() {
                    if self.macro_map.contains_key(macro_name) {
                        Err(ResolveError::MacroAlreadyDefined(macro_name))
                    } else {
                        self.macro_context.start_recording(macro_name)
                    }
                } else {
                    Ok(())
                }
            }
            Line::MacroEnd => {
                if self.liveness_context.is_live() {
                    self.macro_context.stop_recording().and_then(|mac| {
                        self.macro_map.insert(mac.name, mac);
                        Ok(())
                    })
                } else {
                    Ok(())
                }
            }
            Line::Equ(var, expr) => {
                if self.liveness_context.is_live() {
                    if !self.macro_context.recording_macro() {
                        if self.variables.contains_key(var) {
                            Err(ResolveError::VariableAlreadyDefined(var))
                        } else {
                            self.resolve_expr(Rc::clone(expr)).and_then(|val| {
                                self.variables.insert(var, val);
                                Ok(())
                            })
                        }
                    } else {
                        self.macro_context
                            .add_line(MacroLine::new(self.cur_line, line));
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            }
            Line::If(expr) => {
                if !self.macro_context.recording_macro() {
                    self.resolve_expr(Rc::clone(expr))
                        .and_then(|resolved| {
                            self.liveness_context.push(resolved > 0);
                            Ok(())
                        })
                        .or_else(|err| {
                            // If we fail to resolve the expression, we unconditionally push false onto the
                            // liveness context
                            self.liveness_context.push(false);
                            Err(err)
                        })
                } else {
                    self.macro_context
                        .add_line(MacroLine::new(self.cur_line, line));
                    Ok(())
                }
            }
            Line::Else => {
                if !self.macro_context.recording_macro() {
                    self.liveness_context.invert_head()
                } else {
                    self.macro_context
                        .add_line(MacroLine::new(self.cur_line, line));
                    Ok(())
                }
            }
            Line::EndIf => {
                if !self.macro_context.recording_macro() {
                    self.liveness_context.pop()
                } else {
                    self.macro_context
                        .add_line(MacroLine::new(self.cur_line, line));
                    Ok(())
                }
            }
            _ => {
                if self.liveness_context.is_live() && self.macro_context.recording_macro() {
                    self.macro_context
                        .add_line(MacroLine::new(self.cur_line, line));
                    Ok(())
                } else {
                    Ok(())
                }
            }
        }
        .map_err(|err| (self.cur_line, err))
    }

    fn resolve_expr(&self, expr: Rc<Expression<'a>>) -> Result<i32, ResolveError<'a>> {
        match expr.borrow() {
            Expression::Literal(val) => Ok(i32::from(*val)),
            Expression::CurrentAddress => Ok(i32::from(self.cur_addr)),
            Expression::Symbol(Symbol::MacroArg(_arg_num)) => unimplemented!(),
            Expression::Symbol(Symbol::Named(name)) => {
                if let Some(val) = self.variables.get(name).copied() {
                    Ok(val)
                } else if let Some(val) = self.label_map.get(name).copied() {
                    Ok(i32::from(val))
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
        println!("VARIABLE DECLARATIONS:");
        println!();
        for (var, expr) in self.variables.iter() {
            println!("{}: {:?}", var, expr);
        }

        println!();
        println!("LABEL DEFINITIONS ({}):", self.label_map.len());
        println!();
        for (label, line) in self.label_map.iter() {
            println!("{}: {:?}", label, line);
        }

        println!();
        println!("MACRO DECLARATIONS:");
        println!();
        for (macro_name, mac) in self.macro_map.iter() {
            println!("{}: {} lines", macro_name, mac.lines.len());
        }
    }
}
