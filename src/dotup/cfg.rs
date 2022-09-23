use std::fmt::Write;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alphanumeric1, multispace0, multispace1, space1},
    combinator::map,
    multi::separated_list0,
    sequence::{delimited, preceded},
};

type Span<'s> = nom_locate::LocatedSpan<&'s str>;
type IResult<'s, I, O, E = ParseError> = nom::IResult<I, O, E>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KeyValue {
    pub key: String,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Action {
    pub location: Location,
    pub kind: String,
    pub keyvalues: Vec<KeyValue>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment {
    pub location: Location,
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GroupItem {
    Group(Group),
    Action(Action),
    Comment(Comment),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Group {
    pub location: Location,
    pub name: String,
    pub items: Vec<GroupItem>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Config {
    pub groups: Vec<Group>,
}

pub fn parse(content: &str) -> Result<Config, ParseError> {
    match config(Span::new(content)) {
        Ok((_, config)) => Ok(config),
        Err(err) => match err {
            nom::Err::Incomplete(_) => Err(ParseError::new(Default::default(), "unexpected EOF")),
            nom::Err::Error(e) | nom::Err::Failure(e) => Err(e),
        },
    }
}

pub fn format(content: &str) -> Result<String, ParseError> {
    struct Ident(usize);
    impl std::fmt::Display for Ident {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for _ in 0..self.0 {
                write!(f, "\t")?;
            }
            Ok(())
        }
    }

    fn format_action(buffer: &mut String, action: &Action, ident: usize) -> std::fmt::Result {
        write!(buffer, "{}{}", Ident(ident), action.kind)?;
        for kv in action.keyvalues.iter() {
            write!(buffer, " {}=\"{}\"", kv.key, kv.value)?;
        }
        writeln!(buffer)?;
        Ok(())
    }

    fn format_comment(buffer: &mut String, comment: &Comment, ident: usize) -> std::fmt::Result {
        for line in comment.text.lines() {
            writeln!(buffer, "{}# {}", Ident(ident), line)?;
        }
        Ok(())
    }

    fn format_group(buffer: &mut String, group: &Group, ident: usize) -> std::fmt::Result {
        writeln!(buffer, "{}group {} {{", Ident(ident), group.name)?;
        for item in group.items.iter() {
            match item {
                GroupItem::Group(group) => format_group(buffer, group, ident + 1)?,
                GroupItem::Action(action) => format_action(buffer, action, ident + 1)?,
                GroupItem::Comment(comment) => format_comment(buffer, comment, ident + 1)?,
            }
        }
        writeln!(buffer, "{}}}", Ident(ident))?;
        writeln!(buffer)?;
        Ok(())
    }

    let config = parse(content)?;
    let mut buffer = String::new();
    for group in config.groups {
        format_group(&mut buffer, &group, 0).unwrap();
    }
    assert!(parse(&buffer).is_ok());
    Ok(buffer)
}

#[derive(Debug)]
pub struct ParseError {
    location: Location,
    message: String,
}

impl ParseError {
    fn new(location: Location, expected: impl Into<String>) -> Self {
        Self {
            location,
            message: expected.into(),
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl<'s> nom::error::ParseError<Span<'s>> for ParseError {
    fn from_error_kind(input: Span<'s>, kind: nom::error::ErrorKind) -> Self {
        Self::new(location_from_span(input), format!("error kind: {kind:?}"))
    }

    fn append(_input: Span, _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn or(self, other: Self) -> Self {
        other
    }

    fn from_char(input: Span<'s>, c: char) -> Self {
        Self::new(location_from_span(input), format!("invalid character: {c}"))
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parse error at {}: {}", self.location, self.message)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

impl Location {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

impl std::cmp::PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for Location {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.line.cmp(&other.line) {
            core::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        self.column.cmp(&other.column)
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Location { line, column } = self;
        write!(f, "line {line} column {column}")
    }
}

impl KeyValue {
    #[allow(unused)]
    fn new(key: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            key: key.into(),
            value: value.into(),
        }
    }
}

fn is_value_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-' || c == '.' || c == '/'
}

fn linesep(i: Span) -> IResult<Span, Span> {
    take_while(|c: char| c.is_whitespace() && c != '\n')(i)?;
    take_while(|c: char| c == '\n')(i)?;
    take_while(char::is_whitespace)(i)
}

fn keyvalue(i: Span) -> IResult<Span, KeyValue> {
    let (i, key) = alphanumeric1(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, val) = delimited(tag("\""), take_while(is_value_char), tag("\""))(i)?;
    Ok((
        i,
        KeyValue {
            key: key.fragment().to_string(),
            value: val.fragment().to_string(),
        },
    ))
}

fn keyvalues(i: Span) -> IResult<Span, Vec<KeyValue>> {
    separated_list0(space1, keyvalue)(i)
}

fn comment(i: Span) -> IResult<Span, Comment> {
    let location = location_from_span(i);
    preceded(
        tag("#"),
        preceded(multispace0, take_while(|c: char| c != '\n')),
    )(i)
    .map(|(i, text)| {
        (
            i,
            Comment {
                location,
                text: text.to_string(),
            },
        )
    })
}

fn action(i: Span) -> IResult<Span, Action> {
    let location = location_from_span(i);
    let (i, kind) = alphanumeric1(i)?;
    let (i, keyvalues) = preceded(space1, keyvalues)(i)?;
    Ok((
        i,
        Action {
            location,
            kind: kind.to_string(),
            keyvalues,
        },
    ))
}

fn group_item(i: Span) -> IResult<Span, GroupItem> {
    alt((
        map(group, GroupItem::Group),
        map(action, GroupItem::Action),
        map(comment, GroupItem::Comment),
    ))(i)
}

fn group(i: Span) -> IResult<Span, Group> {
    let location = location_from_span(i);

    let (i, _) = tag("group")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = alphanumeric1(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("{")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, items) = separated_list0(linesep, group_item)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("}")(i)?;

    Ok((
        i,
        Group {
            location,
            name: name.to_string(),
            items,
        },
    ))
}

fn config(i: Span) -> IResult<Span, Config> {
    let mut groups = Vec::new();
    let mut parser = delimited(multispace0, group, multispace0);
    let mut curr_span = i;
    while !curr_span.is_empty() {
        match parser(curr_span) {
            Ok((i, group)) => {
                curr_span = i;
                groups.push(group);
            }
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                return Err(nom::Err::Failure(e))
            }
            Err(nom::Err::Incomplete(_)) => break,
        }
    }
    Ok((i, Config { groups }))
}

fn location_from_span(span: Span) -> Location {
    Location::new(span.location_line(), span.get_utf8_column() as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_keyvalue() {
        let input = Span::new(r#"key="value""#);
        let (rem, kv) = keyvalue(input).unwrap();
        assert!(rem.is_empty());
        assert_eq!(kv, KeyValue::new("key", "value"),);
    }

    #[test]
    fn parse_keyvalues() {
        let kvs = vec![
            KeyValue::new("key1", "value1"),
            KeyValue::new("key2", "value2"),
        ];

        let input = Span::new(r#"key1="value1"     key2="value2""#);
        let (rem, res) = keyvalues(input).unwrap();
        assert!(rem.is_empty());
        assert_eq!(res, kvs);

        let kvs = vec![
            KeyValue::new("src", "tmux/"),
            KeyValue::new("dst", ".config/tmux"),
        ];

        let input = Span::new(r#"src="tmux/" dst=".config/tmux""#);
        let (rem, res) = keyvalues(input).unwrap();
        assert!(rem.is_empty());
        assert_eq!(res, kvs);
    }
}
