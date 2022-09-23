use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{alphanumeric0, alphanumeric1, multispace0, space1},
    combinator::map,
    multi::{many0, separated_list0},
    sequence::{delimited, preceded},
};

type Span<'s> = nom_locate::LocatedSpan<&'s str>;
type IResult<'s, I, O, E = ParserError<'s>> = nom::IResult<I, O, E>;

#[derive(Debug, PartialEq, Eq)]
struct ParserError<'s> {
    location: Span<'s>,
    message: Option<String>,
}

#[derive(Debug)]
struct KeyValueParser<'s> {
    span: Span<'s>,
    kvs: Vec<KeyValue<'s>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct KeyValue<'s> {
    key: &'s str,
    value: &'s str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LinkAction {
    source: String,
    target: String,
}

enum RichAction {
    Link(LinkAction),
}

struct RichGroup {
    name: String,
    items: Vec<RichItem>,
}

enum RichItem {
    Group(RichGroup),
}

struct RichConfig {}

impl<'s> ParserError<'s> {
    fn custom(location: Span<'s>, message: impl Into<String>) -> Self {
        Self {
            location,
            message: Some(message.into()),
        }
    }

    fn missing_key(span: Span<'s>, key: &'s str) -> Self {
        Self::custom(span, format!("missing key: {key}"))
    }
}

impl<'s> From<ParserError<'s>> for nom::Err<ParserError<'s>> {
    fn from(e: ParserError<'s>) -> Self {
        Self::Failure(e)
    }
}

impl<'s> nom::error::ParseError<Span<'s>> for ParserError<'s> {
    fn from_error_kind(input: Span<'s>, kind: nom::error::ErrorKind) -> Self {
        Self::custom(input, format!("error kind: {kind:?}"))
    }

    fn append(input: Span, kind: nom::error::ErrorKind, other: Self) -> Self {
        todo!()
    }

    fn or(self, other: Self) -> Self {
        other
    }

    fn from_char(input: Span<'s>, c: char) -> Self {
        Self::custom(input, format!("invalid character: {c}"))
    }
}

impl<'s> KeyValueParser<'s> {
    fn new(span: Span<'s>, kvs: Vec<KeyValue<'s>>) -> Self {
        Self { span, kvs }
    }

    fn get(&self, key: &'static str) -> Option<&'s str> {
        self.kvs.iter().find(|kv| kv.key == key).map(|kv| kv.value)
    }

    fn expect(&self, key: &'static str) -> Result<&'s str, ParserError<'s>> {
        self.get(key)
            .ok_or(ParserError::missing_key(self.span, key))
    }
}

fn is_value_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-' || c == '.' || c == '/'
}

fn whitespace0(i: Span) -> IResult<Span, Span> {
    take_while(char::is_whitespace)(i)
}

fn whitespace1(i: Span) -> IResult<Span, Span> {
    take_while1(char::is_whitespace)(i)
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
            key: key.fragment(),
            value: val.fragment(),
        },
    ))
}

fn keyvalues(i: Span) -> IResult<Span, Vec<KeyValue>> {
    separated_list0(space1, keyvalue)(i)
}

fn link_action(i: Span) -> IResult<Span, LinkAction> {
    let (i, kvs) = preceded(tag("link"), preceded(space1, keyvalues))(i)?;
    eprintln!("{kvs:#?}");
    eprintln!("{i:?}");
    let kvparser = KeyValueParser::new(i, kvs);
    let src = kvparser.expect("src")?.to_string();
    let dst = kvparser.expect("dst")?.to_string();
    Ok((
        i,
        LinkAction {
            source: src,
            target: dst,
        },
    ))
}

fn rich_action(i: Span) -> IResult<Span, RichAction> {
    todo!()
}

fn rich_group(i: Span) -> IResult<Span, RichGroup> {
    let mut header = preceded(tag("group"), preceded(multispace0, alphanumeric1));
    let mut open_bracket = delimited(multispace0, tag("{"), multispace0);
    let mut close_bracket = preceded(multispace0, tag("}"));
    let mut body = separated_list0(linesep, rich_item);

    let (i, name) = header(i)?;
    let (i, _) = open_bracket(i)?;
    let (i, items) = body(i)?;
    let (i, _) = close_bracket(i)?;

    Ok((
        i,
        RichGroup {
            name: name.to_string(),
            items,
        },
    ))
}

fn rich_item(i: Span) -> IResult<Span, RichItem> {
    alt((map(rich_group, RichItem::Group),))(i)
}

fn config(i: Span) -> IResult<Span, RichConfig> {
    let (_, groups) = many0(rich_group)(i)?;
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_keyvalue() {
        let input = Span::new(r#"key="value""#);
        let (rem, kv) = keyvalue(input).unwrap();
        assert!(rem.is_empty());
        assert_eq!(
            kv,
            KeyValue {
                key: "key",
                value: "value"
            }
        );
    }

    #[test]
    fn parse_keyvalues() {
        let kvs = vec![
            KeyValue {
                key: "key1",
                value: "value1",
            },
            KeyValue {
                key: "key2",
                value: "value2",
            },
        ];

        let input = Span::new(r#"key1="value1"     key2="value2""#);
        let (rem, res) = keyvalues(input).unwrap();
        assert!(rem.is_empty());
        assert_eq!(res, kvs);

        let kvs = vec![
            KeyValue {
                key: "src",
                value: "tmux/",
            },
            KeyValue {
                key: "dst",
                value: ".config/tmux",
            },
        ];

        let input = Span::new(r#"src="tmux/" dst=".config/tmux""#);
        let (rem, res) = keyvalues(input).unwrap();
        assert!(rem.is_empty());
        assert_eq!(res, kvs);
    }

    #[test]
    fn parse_link_action() {
        let input = Span::new(r#"link src="tmux/" dst=".config/tmux""#);
        let (rem, res) = link_action(input).unwrap();
        assert!(rem.is_empty());
        assert_eq!(
            res,
            LinkAction {
                source: "tmux/".to_string(),
                target: ".config/tmux".to_string()
            }
        );
    }
}
