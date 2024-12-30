use std::fmt;

use itertools::Itertools;

pub(crate) struct FileBuf {
    blocks: Vec<BlockBuf>,
}

impl FileBuf {
    pub(crate) fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub(crate) fn pretty_print_into_string(&self) -> String {
        let mut buf = String::new();
        self.pretty_print(&mut buf);
        buf
    }

    pub(crate) fn pretty_print(&self, buf: &mut String) {
        for segment in Itertools::intersperse_with(self.blocks.iter().map(Some), || None) {
            match segment {
                Some(block) => block.pretty_print(buf),
                None => buf.push('\n'),
            }
        }
    }

    pub(crate) fn push(&mut self, block: BlockBuf) {
        self.blocks.push(block)
    }
}

pub(crate) struct BlockBuf {
    lines: Vec<LineBuf>,
}

impl BlockBuf {
    pub(crate) fn new() -> Self {
        Self { lines: Vec::new() }
    }

    pub(crate) fn into_file_buf(self) -> FileBuf {
        let mut buf = FileBuf::new();
        buf.push(self);
        buf
    }

    pub(crate) fn pretty_print_into_string(&self) -> String {
        let mut buf = String::new();
        self.pretty_print(&mut buf);
        buf
    }

    fn pretty_print(&self, buf: &mut String) {
        for line in &self.lines {
            line.pretty_print(buf);
            buf.push('\n');
        }
    }

    fn push(&mut self, line: LineBuf) {
        self.lines.push(line)
    }

    pub(crate) fn push_line_with(&mut self, f: impl FnOnce(&mut LineBuf)) {
        let mut line = LineBuf::new();
        f(&mut line);
        self.push(line);
    }

    pub(crate) fn push_line<T: ToTokens>(&mut self, val: &T) {
        self.push_line_with(|line| line.to_tokens(val))
    }
}

pub(crate) struct LineBuf {
    tokens: Vec<Token>,
}

impl LineBuf {
    fn new() -> Self {
        Self { tokens: Vec::new() }
    }

    pub(crate) fn into_block_buf(self) -> BlockBuf {
        let mut buf = BlockBuf::new();
        buf.push(self);
        buf
    }

    pub(crate) fn pretty_print_into_string(&self) -> String {
        let mut buf = String::new();
        self.pretty_print(&mut buf);
        buf
    }

    fn pretty_print(&self, buf: &mut String) {
        buf.extend(Itertools::intersperse(
            self.tokens.iter().map(String::as_str),
            " ",
        ));
    }

    pub(crate) fn push(&mut self, tok: Token) {
        self.tokens.push(tok)
    }

    pub(crate) fn to_string_to_tokens<T: ToString>(&mut self, tok: T) {
        self.tokens.push(tok.to_string())
    }

    pub(crate) fn display_to_tokens<T: fmt::Display>(&mut self, v: &T) {
        self.tokens.push(format!("{}", v))
    }

    pub(crate) fn lower_hex_to_tokens<T: fmt::LowerHex>(&mut self, v: &T) {
        self.tokens.push(format!("{:#x}", v))
    }

    pub(crate) fn to_tokens<T: ToTokens + ?Sized>(&mut self, v: &T) {
        v.to_tokens(self)
    }
}

pub(crate) type Token = String;

pub(crate) trait ToTokens {
    fn to_tokens(&self, line: &mut LineBuf);

    fn pretty_print(&self) -> String {
        self.pretty_print_into_line().pretty_print_into_string()
    }

    fn pretty_print_into_line(&self) -> LineBuf {
        let mut lines = LineBuf::new();
        lines.to_tokens(self);
        lines
    }
}

impl<T: ToTokens, U: ToTokens> ToTokens for (T, U) {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.0);
        line.to_tokens(&self.1);
    }
}

impl<T: ToTokens> ToTokens for [T] {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.display_to_tokens(&self.len());
        for x in self {
            line.to_tokens(x);
        }
    }
}

impl ToTokens for str {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.push(self.to_owned())
    }
}

impl ToTokens for String {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.push(self.clone())
    }
}
