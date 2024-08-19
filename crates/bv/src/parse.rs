use itertools::Itertools;
use num::{BigInt, Num, NumCast, ToPrimitive};
use regex::Regex;

#[derive(Debug)]
pub(crate) struct Token<'a> {
    line_index: usize,
    char_index: usize,
    slice: &'a str,
}

impl<'a> Token<'a> {
    pub(crate) fn new(line_index: usize, char_index: usize, slice: &'a str) -> Self {
        Self {
            line_index,
            char_index,
            slice,
        }
    }

    pub(crate) fn location(&self) -> TokenLocation {
        TokenLocation {
            line_number: self.line_index + 1,
            column_number: self.char_index + 1,
        }
    }

    pub(crate) fn as_str(&self) -> &'a str {
        &self.slice
    }

    pub(crate) fn parse_big_int(&self) -> Option<BigInt> {
        parse_big_int(self.as_str())
    }
}

#[derive(Debug)]
pub(crate) struct Line<'a> {
    line_index: usize,
    // invariant: non-empty
    tokens: Vec<Token<'a>>,
}

impl<'a> Line<'a> {
    pub(crate) fn tokenize(line_index: usize, raw_line: &'a str) -> Self {
        let mut tokens = vec![];
        for (is_whitespace, mut chunk) in &raw_line
            .char_indices()
            .enumerate()
            .chunk_by(|(_char_index, (_byte_index, c))| c.is_whitespace())
        {
            if !is_whitespace {
                let (char_index, first_byte_index) = {
                    let (char_index, (byte_index, _c)) = chunk.next().unwrap();
                    (char_index, byte_index)
                };
                let len = 1 + chunk.count();
                tokens.push(Token::new(
                    line_index,
                    char_index,
                    &raw_line[first_byte_index..][..len],
                ));
            }
        }
        Self { line_index, tokens }
    }

    pub(crate) fn first(&self) -> &Token<'a> {
        self.tokens.first().unwrap()
    }

    pub(crate) fn tokens(&self) -> &[Token<'a>] {
        &self.tokens
    }

    pub(crate) fn location(&self) -> LineLocation {
        LineLocation {
            line_number: self.line_index + 1,
        }
    }
}

#[derive(Debug)]
pub(crate) struct Lines<'a> {
    lines: Vec<Line<'a>>,
}

impl<'a> Lines<'a> {
    pub(crate) fn tokenize(s: &'a str) -> Self {
        Self::tokenize_with_line_index_offset(s, 0)
    }

    pub(crate) fn tokenize_with_line_index_offset(s: &'a str, line_index_offset: usize) -> Self {
        Self::tokenize_lines_with_line_index_offset(s.lines(), line_index_offset)
    }

    pub(crate) fn tokenize_lines_with_line_index_offset(
        lines: impl Iterator<Item = &'a str>,
        line_index_offset: usize,
    ) -> Self {
        Self::tokenize_lines_with_indices_and_line_index_offset(
            lines.enumerate(),
            line_index_offset,
        )
    }

    pub(crate) fn tokenize_lines_with_indices_and_line_index_offset(
        lines: impl Iterator<Item = (usize, &'a str)>,
        line_index_offset: usize,
    ) -> Self {
        let re = Regex::new(r"^\s*(#.*)?$").unwrap();
        Self::tokenize_indexed_syntax_lines(
            lines
                .filter(|(_i, raw_line)| !re.is_match(raw_line))
                .map(|(i, raw_line)| (i + line_index_offset, raw_line)),
        )
    }

    pub(crate) fn tokenize_indexed_syntax_lines(
        lines: impl Iterator<Item = (usize, &'a str)>,
    ) -> Self {
        let lines = lines
            .map(|(line_index, raw_line)| Line::tokenize(line_index, raw_line))
            .collect::<Vec<_>>();
        Self { lines }
    }

    pub(crate) fn lines(&self) -> &[Line<'a>] {
        &self.lines
    }

    pub(crate) fn parse_with<T>(
        &self,
        f: impl FnOnce(&mut LinesBuffer) -> ParseResult<T>,
    ) -> ParseResult<T> {
        let mut lines = LinesBuffer::new(self);
        let ret = f(&mut lines)?;
        lines.ensure_empty()?;
        Ok(ret)
    }

    pub(crate) fn parse<T: ParseFromLines>(&self) -> ParseResult<T> {
        self.parse_with(ParseFromLines::parse)
    }
}

#[derive(Debug)]
pub(crate) struct LineBuffer<'a> {
    line: &'a Line<'a>,
    cursor: usize,
}

impl<'a> LineBuffer<'a> {
    fn new(line: &'a Line<'a>) -> Self {
        Self { line, cursor: 0 }
    }

    pub(crate) fn location(&self) -> LineLocation {
        self.line.location()
    }

    pub(crate) fn next_token_location(&self) -> ParseResult<TokenLocation> {
        self.try_peek()
            .map(Token::location)
            .ok_or_else(|| ParseError::UnexpectedEol(self.location()))
    }

    pub(crate) fn cursor(&self) -> usize {
        self.cursor
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.cursor() == self.line.tokens().len()
    }

    pub(crate) fn advance(&mut self) -> ParseResult<()> {
        if self.is_empty() {
            Err(ParseError::UnexpectedEol(self.location()))
        } else {
            self.cursor += 1;
            Ok(())
        }
    }

    pub(crate) fn try_peek(&self) -> Option<&'a Token<'a>> {
        if self.is_empty() {
            None
        } else {
            Some(&self.line.tokens()[self.cursor()])
        }
    }

    pub(crate) fn peek(&self) -> ParseResult<&'a Token<'a>> {
        self.try_peek()
            .ok_or_else(|| ParseError::UnexpectedEol(self.location()))
    }

    pub(crate) fn next(&mut self) -> ParseResult<&Token<'a>> {
        let tok = self.peek()?;
        self.advance().unwrap();
        Ok(tok)
    }

    pub(crate) fn ensure_empty(&self) -> ParseResult<()> {
        match self.try_peek() {
            Some(tok) => Err(ParseError::LeftoverTokensInLine(tok.location())),
            None => Ok(()),
        }
    }

    pub(crate) fn match_(&mut self, s: &str) -> ParseResult<()> {
        let tok = self.next()?;
        if tok.as_str() == s {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken(tok.location()))
        }
    }

    pub(crate) fn parse<T: ParseFromLine>(&mut self) -> ParseResult<T> {
        T::parse(self)
    }

    pub(crate) fn parse_prim_int<T: NumCast>(&mut self) -> ParseResult<T> {
        let location = self.next_token_location()?;
        BigInt::parse(self)?
            .to_i128()
            .and_then(NumCast::from)
            .ok_or(ParseError::NumberOutOfRange(location))
    }
}

pub(crate) trait ParseFromLine: Sized {
    fn parse(toks: &mut LineBuffer) -> ParseResult<Self>;
}

impl<T: ParseFromLine> ParseFromLine for Box<T> {
    fn parse(toks: &mut LineBuffer) -> ParseResult<Self> {
        Ok(Box::new(toks.parse()?))
    }
}

impl<T: ParseFromLine, U: ParseFromLine> ParseFromLine for (T, U) {
    fn parse(toks: &mut LineBuffer) -> ParseResult<Self> {
        Ok((toks.parse()?, toks.parse()?))
    }
}

impl<T: ParseFromLine> ParseFromLine for Vec<T> {
    fn parse(toks: &mut LineBuffer) -> ParseResult<Self> {
        let n = toks.parse_prim_int::<usize>()?;
        let mut v = vec![];
        for _ in 0..n {
            v.push(toks.parse()?);
        }
        Ok(v)
    }
}

impl ParseFromLine for String {
    fn parse(toks: &mut LineBuffer) -> ParseResult<Self> {
        Ok(toks.next()?.as_str().to_owned())
    }
}

impl ParseFromLine for BigInt {
    fn parse(toks: &mut LineBuffer) -> ParseResult<Self> {
        let tok = toks.next()?;
        tok.parse_big_int()
            .ok_or(ParseError::InvalidNumber(tok.location()))
    }
}

fn parse_big_int(mut s: &str) -> Option<BigInt> {
    let mut positive = true;
    let mut radix = 10;
    if s.starts_with("-") || s.starts_with("~") {
        positive = false;
        s = &s[1..];
    }
    if s.starts_with("0x") {
        radix = 16;
        s = &s[2..];
    } else if s.starts_with("0b") {
        radix = 2;
        s = &s[2..];
    }
    let abs = BigInt::from_str_radix(s, radix).ok()?;
    Some(if positive { abs } else { -abs })
}

#[derive(Debug)]
pub(crate) struct LinesBuffer<'a> {
    lines: &'a Lines<'a>,
    cursor: usize,
}

impl<'a> LinesBuffer<'a> {
    fn new(lines: &'a Lines<'a>) -> Self {
        Self { lines, cursor: 0 }
    }

    fn next_line_location(&self) -> ParseResult<LineLocation> {
        self.peek_line().map(Line::location)
    }

    fn cursor(&self) -> usize {
        self.cursor
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.cursor() == self.lines.lines().len()
    }

    pub(crate) fn advance(&mut self) -> ParseResult<()> {
        if self.is_empty() {
            Err(ParseError::UnexpectedEof)
        } else {
            self.cursor += 1;
            Ok(())
        }
    }

    pub(crate) fn peek_token(&self) -> ParseResult<&'a Token<'a>> {
        self.peek_line().map(Line::first)
    }

    fn try_peek_line(&self) -> Option<&'a Line<'a>> {
        if self.is_empty() {
            None
        } else {
            Some(&self.lines.lines()[self.cursor()])
        }
    }

    fn peek_line(&self) -> ParseResult<&'a Line<'a>> {
        self.try_peek_line()
            .ok_or_else(|| ParseError::UnexpectedEof)
    }

    pub(crate) fn next(&mut self) -> ParseResult<LineBuffer<'a>> {
        let line = self.peek_line()?;
        self.advance().unwrap();
        Ok(LineBuffer::new(line))
    }

    fn ensure_empty(&self) -> ParseResult<()> {
        match self.peek_line() {
            Err(_) => Ok(()),
            Ok(line) => Err(ParseError::LeftoverLines(line.location())),
        }
    }

    pub(crate) fn parse<T: ParseFromLines>(&mut self) -> ParseResult<T> {
        T::parse(self)
    }

    pub(crate) fn parse_next_line_with<T>(
        &mut self,
        f: impl FnOnce(&mut LineBuffer) -> ParseResult<T>,
    ) -> ParseResult<T> {
        let mut toks = self.next()?;
        let ret = f(&mut toks)?;
        toks.ensure_empty()?;
        Ok(ret)
    }

    pub(crate) fn parse_next_line<T: ParseFromLine>(&mut self) -> ParseResult<T> {
        self.parse_next_line_with(ParseFromLine::parse)
    }

    pub(crate) fn parse_lines_until_with<T>(
        &mut self,
        s: &str,
        mut f: impl FnMut(&mut LineBuffer) -> ParseResult<T>,
    ) -> ParseResult<Vec<T>> {
        let mut v = vec![];
        while self.peek_token()?.as_str() != s {
            v.push(self.parse_next_line_with(&mut f)?);
        }
        self.parse_next_line_with(|toks| toks.match_(s))?;
        Ok(v)
    }

    pub(crate) fn parse_lines_until<T: ParseFromLine>(&mut self, s: &str) -> ParseResult<Vec<T>> {
        self.parse_lines_until_with(s, ParseFromLine::parse)
    }
}

pub(crate) trait ParseFromLines: Sized {
    fn parse(lines: &mut LinesBuffer) -> ParseResult<Self>;
}

pub(crate) type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Copy, Clone)]
pub(crate) enum ParseError {
    UnexpectedEof,
    UnexpectedEol(LineLocation),
    LeftoverTokensInLine(TokenLocation),
    LeftoverLines(LineLocation),
    UnexpectedToken(TokenLocation),
    InvalidNumber(TokenLocation),
    NumberOutOfRange(TokenLocation),
    Unspecified,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct LineLocation {
    line_number: usize,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct TokenLocation {
    line_number: usize,
    column_number: usize,
}
