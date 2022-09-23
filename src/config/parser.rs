#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Location {
    line: u32,
    column: u32,
}

#[derive(Debug, Clone)]
struct Scanner<'s> {
    location: Location,
    content: std::str::Chars<'s>,
}

impl<'s> Scanner<'s> {
    fn new(content: &'s str) -> Self {
        Self {
            location: Default::default(),
            content: content.chars(),
        }
    }
}
