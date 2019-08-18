use crate::{Span, Token};

struct Line<'a> {
    label: Option<(Span<'a>, &'a str)>,
    action: Option<Action<'a>>,
    comment: Option<(Span<'a>, &'a str)>,
}

enum Action<'a> {
    Opcode(Span<'a>, &'a [Token<'a>]),
    Directive(Span<'a>, &'a [Token<'a>]),
    Macro(Span<'a>, &'a [Token<'a>]),
}
