#![doc = include_str!("../README.md")]
#![forbid(missing_docs)]
#![forbid(unsafe_code)]

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use simple_proc_macro_error::{bail, error};
use std::{convert::identity, ops::Range};

/// Inlines a closure (Format: `|...| ...`). This assumes that the parameters
/// are available as variables in the surrounding context.
///
/// The types of the closure parameters are currently ignored.
///
/// # Example
///
/// ```rust
/// # use inline_closure::inline_closure;
/// let a = 1;
/// let b = 2;
/// let out = inline_closure!(|a, b| a + b);
/// assert_eq!(out, 3);
/// ```
#[proc_macro]
pub fn inline_closure(ts: TokenStream) -> TokenStream {
    inline_closure_(ts).unwrap_or_else(identity)
}

fn inline_closure_(mut ts: TokenStream) -> Result<TokenStream, TokenStream> {
    let ts = loop {
        let mut iter = ts.into_iter();
        match iter.next() {
            Some(TokenTree::Group(g)) if g.delimiter() != Delimiter::Bracket => {
                if let Some(tt) = iter.next() {
                    bail!("expected only a single closure (also got: {})", tt);
                }
                ts = g.stream();
            },
            Some(tt) => {
                let mut ts = TokenStream::from(tt);
                ts.extend(iter);
                break ts;
            },
            None => bail!("expected closure"),
        }
    };

    let mut span_iter = ts.clone().into_iter().map(|tt| tt.span());
    let start = span_iter.next().unwrap_or(Span::call_site());
    let end = span_iter.last().unwrap_or(start);
    let (params, closure_body) = parse_closure(ts, start..end)?;

    /// otherwise the params might not be found in scope
    fn clean(ts: TokenStream, params: &[Ident]) -> TokenStream {
        ts.into_iter()
            .map(|tt| match tt {
                TokenTree::Ident(mut i) if params.iter().any(|p| ident_eq(p, &i)) => {
                    i.set_span(Span::call_site());
                    TokenTree::Ident(i)
                },
                TokenTree::Group(g) => {
                    TokenTree::Group(Group::new(g.delimiter(), clean(g.stream(), params)))
                },
                tt => tt,
            })
            .collect()
    }

    Ok(clean(closure_body, &params))
}

/// Inlines a closure that is immediately called (Format: `(|...| ...)(...)`).
///
/// The types of the closure parameters are currently ignored.
///
/// # Example
///
/// ```rust
/// # use inline_closure::inline_closure_call;
/// let a = 1;
/// let b = 2;
/// let out = inline_closure_call!((|a: usize, b| a + b)(a * 2, b + 1));
/// assert_eq!(out, 5);
/// ```
#[proc_macro]
pub fn inline_closure_call(ts: TokenStream) -> TokenStream {
    let mut ts = ts.into_iter();
    let Some(closure) = ts.next().and_then(extract_parenthesis) else {
        return error!("expected closure in Parenthesis: `(|...| ...)`");
    };
    // a macro might add a group with delimiter None when using `$closure:expr`.
    let closure = match closure.stream().into_iter().next() {
        Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::None => g,
        _ => closure,
    };
    let Some(closure_call) = ts.next().and_then(extract_parenthesis) else {
        return error!("expected closure call: `(val1, val2, ...)`");
    };
    if let Some(tt) = ts.next() {
        return error!("expected end (got: {})", tt);
    }
    inline_closure_call_(closure, closure_call).unwrap_or_else(identity)
}

/// parse input: `(|...| ...)(...)`
/// `closure`: `|...| ...`
/// `call`: last `...`
fn inline_closure_call_(closure: Group, call: Group) -> Result<TokenStream, TokenStream> {
    let spans = closure.span()..closure.span_close();
    let (params, closure_body) = parse_closure(closure.stream(), spans)?;

    let values = parse_call_values(call, params.len())?;

    let mut out: TokenStream = params
        .into_iter()
        .zip(values)
        .map(|(param, value)| {
            [
                TokenTree::Ident(Ident::new("let", Span::call_site())),
                TokenTree::Ident(param),
                TokenTree::Punct(Punct::new('=', Spacing::Alone)),
            ]
            .into_iter()
            .chain(value)
            .chain(Some(TokenTree::Punct(Punct::new(';', Spacing::Alone))))
        })
        .flatten()
        .collect();

    out.extend(closure_body);

    Ok(TokenStream::from(TokenTree::Group(Group::new(Delimiter::Brace, out))))
}

type Param = Ident;
type ClosureBody = TokenStream;

fn parse_closure(
    ts: impl IntoIterator<Item = TokenTree>,
    full_span: Range<Span>,
) -> Result<(Vec<Param>, ClosureBody), TokenStream> {
    let mut closure = ts.into_iter();
    match closure.next() {
        Some(TokenTree::Punct(p)) if p.as_char() == '|' => (),
        Some(tt) => bail!(tt.span() => "expected '|' (got: {})", tt),
        None => bail!(full_span => "expected closure"),
    }

    let mut params = vec![];
    loop {
        match closure.next() {
            Some(TokenTree::Ident(i)) => params.push(i),
            Some(TokenTree::Punct(p)) if p.as_char() == '|' => break,
            Some(tt) => bail!(tt.span() => "expected parameter (got: {})", tt),
            None => bail!(full_span.end => "expected closing '|'"),
        };
        let mut punct = closure.next();
        if matches!(punct, Some(TokenTree::Punct(ref p)) if p.as_char() == ':') {
            let Some(_type) = closure.next() else { bail!("expected parameter type") };
            punct = closure.next();
        }
        match punct {
            Some(TokenTree::Punct(p)) if p.as_char() == ',' => continue,
            Some(TokenTree::Punct(p)) if p.as_char() == '|' => break,
            Some(tt) => bail!(tt.span() => "closure expected ',' or '|'"),
            None => bail!(full_span.end => "closure expected ',' or '|'"),
        };
    }

    let closure_body = closure.collect();
    Ok((params, closure_body))
}

fn parse_call_values(call: Group, param_count: usize) -> Result<Vec<TokenStream>, TokenStream> {
    let mut values = Vec::new();
    let mut iter = call.stream().into_iter();
    loop {
        let val: TokenStream = iter
            .by_ref()
            .take_while(
                |tt| !matches!(tt, TokenTree::Punct(p) if p.as_char() == ',' && p.spacing() == Spacing::Alone),
            )
            .collect();
        if val.is_empty() {
            break;
        }
        values.push(val);
    }

    if values.len() != param_count {
        bail!(call.span() => "closure has {param_count} parameters but got {} values", values.len())
    }

    Ok(values)
}

fn extract_parenthesis(tt: TokenTree) -> Option<Group> {
    match tt {
        TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => Some(g),
        _ => None,
    }
}

fn ident_eq(ident: &Ident, other: &Ident) -> bool {
    ident.to_string() == other.to_string()
}
