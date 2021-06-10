use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl LineColumn {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
    }

    pub fn new_zero() -> Self {
        Self {
            start: LineColumn::new(0, 0),
            end: LineColumn::new(0, 0),
        }
    }

    pub fn min_wrapping(spans: &[Span]) -> Option<Self> {
        let start = spans.iter().map(|s| s.start).min()?;
        let end = spans.iter().map(|s| s.end).max()?;
        Some(Span { start, end })
    }
}

impl fmt::Display for Span {
    fn fmt(&self, b: &mut fmt::Formatter) -> fmt::Result {
        write!(
            b,
            "{}:{}..{}:{}",
            self.start.line + 1,
            self.start.column + 1,
            self.end.line + 1,
            self.end.column + 1,
        )
    }
}
