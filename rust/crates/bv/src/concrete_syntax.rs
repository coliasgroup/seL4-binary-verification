use std::collections::BTreeMap;

use crate::abstract_syntax::*;

pub(crate) mod parse;
pub(crate) mod print;

use parse::{LineBuffer, LinesBuffer, ParseError, ParseFileAsLines, ParseFromLine, ParseFromLines};
use print::{BlockBuf, FileBuf, LineBuf, ToTokens};

impl File {
    pub(crate) fn pretty_print(&self) -> String {
        let mut file = FileBuf::new();
        for (name, struct_) in &self.structs {
            file.push(struct_.pretty_print_into_block(name));
        }
        for (name, const_global) in &self.const_globals {
            file.push(const_global.pretty_print_into_block(name));
        }
        for (name, function) in &self.functions {
            file.push(function.abstract_function().pretty_print_into_block(name));
        }
        file.pretty_print_into_string()
    }
}

impl ParseFileAsLines for File {}

impl ParseFromLines for File {
    fn parse(lines: &mut LinesBuffer) -> Result<Self, ParseError> {
        let mut structs = BTreeMap::new();
        let mut const_globals = BTreeMap::new();
        let mut functions = BTreeMap::new();

        while let Ok(tok) = lines.peek_token() {
            match tok.as_str() {
                "Struct" => {
                    let (name, struct_) = Struct::parse_from_lines(lines)?;
                    structs.insert(name, struct_);
                }
                "ConstGlobalDef" => {
                    let (name, const_global) = ConstGlobal::parse_from_lines(lines)?;
                    const_globals.insert(name, const_global);
                }
                "Function" => {
                    let (name, f) = Function::parse_from_lines(lines)?;
                    functions.insert(name, f);
                }
                _ => return Err(ParseError::UnexpectedToken(tok.location())),
            }
        }

        Ok(Self {
            structs,
            const_globals,
            functions,
        })
    }
}

impl Struct {
    fn parse_from_lines(lines: &mut LinesBuffer) -> Result<(Ident, Self), ParseError> {
        let (name, size, align) = lines.parse_next_line_with(|toks| {
            toks.match_("Struct")?;
            Ok((
                toks.parse()?,
                toks.parse_prim_int()?,
                toks.parse_prim_int()?,
            ))
        })?;

        let mut fields = BTreeMap::new();

        while lines
            .peek_token()
            .map(|tok| tok.as_str() == "StructField")
            .unwrap_or(false)
        {
            let (field_name, ty, offset) = lines.parse_next_line_with(|toks| {
                toks.advance().unwrap();
                Ok((toks.parse()?, toks.parse()?, toks.parse_prim_int()?))
            })?;
            fields.insert(field_name, StructField { ty, offset });
        }

        let struct_ = Struct {
            size,
            align,
            fields,
        };

        Ok((name, struct_))
    }

    fn pretty_print_into_block(&self, name: &Ident) -> BlockBuf {
        let mut block = BlockBuf::new();
        block.push_line_with(|line| {
            line.to_tokens("Struct");
            line.to_tokens(name);
            line.display_to_tokens(&self.size);
            line.display_to_tokens(&self.align);
        });
        for (name, field) in &self.fields {
            block.push_line_with(|line| {
                line.to_tokens("StructField");
                line.to_tokens(name);
                line.to_tokens(&field.ty);
                line.display_to_tokens(&field.offset);
            });
        }
        block
    }
}
impl ConstGlobal {
    fn parse_from_lines(_lines: &mut LinesBuffer) -> Result<(Ident, Self), ParseError> {
        todo!()
    }

    fn pretty_print_into_block(&self, _name: &Ident) -> BlockBuf {
        todo!()
    }
}

impl Function {
    fn parse_from_lines(lines: &mut LinesBuffer) -> Result<(Ident, Self), ParseError> {
        let (name, input, output) = lines.parse_next_line_with(|toks| {
            toks.match_("Function")?;
            Ok((toks.parse()?, toks.parse()?, toks.parse()?))
        })?;

        let has_body = lines
            .peek_token()
            .map(|tok| tok.as_str() == "EntryPoint" || tok.parse_big_int().is_some())
            .unwrap_or(false);

        let body = if has_body {
            let mut nodes = BTreeMap::new();
            let entry_point = loop {
                if lines.peek_token()?.as_str() == "EntryPoint" {
                    break lines.parse_next_line_with(|toks| {
                        toks.advance().unwrap();
                        toks.parse()
                    })?;
                } else {
                    let (addr, node) = lines
                        .parse_next_line_with(|toks| Ok((toks.parse_prim_int()?, toks.parse()?)))?;
                    nodes.insert(addr, node);
                }
            };
            Some(FunctionBody { entry_point, nodes })
        } else {
            None
        };

        let f = Function {
            input,
            output,
            body,
        };

        Ok((name, f))
    }
}

impl<T: HasFunction> AbstractFunction<T> {
    fn pretty_print_into_block(&self, name: &Ident) -> BlockBuf {
        let mut block = BlockBuf::new();
        block.push_line_with(|line| {
            line.to_tokens("Function");
            line.to_tokens(name);
            line.to_tokens(self.input());
            line.to_tokens(self.output());
        });
        if let Some(body) = self.body_if_present() {
            for (addr, node) in body.nodes() {
                block.push_line_with(|line| {
                    line.lower_hex_to_tokens(&addr);
                    line.to_tokens(node);
                });
            }
            block.push_line_with(|line| {
                line.to_tokens("EntryPoint");
                line.to_tokens(&body.entry());
            });
        }
        block
    }
}

impl ParseFromLine for Argument {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            name: toks.parse()?,
            ty: toks.parse()?,
        })
    }
}

impl ToTokens for Argument {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.name);
        line.to_tokens(&self.ty);
    }
}

impl ParseFromLine for NodeId {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(match toks.peek()?.as_str() {
            "Ret" => {
                toks.advance().unwrap();
                Self::Ret
            }
            "Err" => {
                toks.advance().unwrap();
                Self::Err
            }
            _ => Self::Addr(toks.parse_prim_int()?),
        })
    }
}

impl ToTokens for NodeId {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::Addr(addr) => {
                line.lower_hex_to_tokens(addr);
            }
            Self::Ret => {
                line.to_tokens("Ret");
            }
            Self::Err => {
                line.to_tokens("Err");
            }
        }
    }
}

impl ParseFromLine for Node {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "Basic" => Self::Basic(toks.parse()?),
            "Cond" => Self::Cond(toks.parse()?),
            "Call" => Self::Call(toks.parse()?),
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for Node {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::Basic(inner) => {
                line.to_tokens("Basic");
                line.to_tokens(inner);
            }
            Self::Cond(inner) => {
                line.to_tokens("Cond");
                line.to_tokens(inner);
            }
            Self::Call(inner) => {
                line.to_tokens("Call");
                line.to_tokens(inner);
            }
        }
    }
}

impl ParseFromLine for BasicNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            next: toks.parse()?,
            var_updates: toks.parse()?,
        })
    }
}

impl ToTokens for BasicNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.next);
        line.to_tokens(&*self.var_updates);
    }
}

impl ParseFromLine for CondNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            left: toks.parse()?,
            right: toks.parse()?,
            expr: toks.parse()?,
        })
    }
}

impl ToTokens for CondNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.left);
        line.to_tokens(&self.right);
        line.to_tokens(&self.expr);
    }
}

impl ParseFromLine for CallNode {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            next: toks.parse()?,
            function_name: toks.parse()?,
            input: toks.parse()?,
            output: toks.parse()?,
        })
    }
}

impl ToTokens for CallNode {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.next);
        line.to_tokens(&self.function_name);
        line.to_tokens(&*self.input);
        line.to_tokens(&*self.output);
    }
}

impl ParseFromLine for VarUpdate {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        Ok(Self {
            var_name: toks.parse()?,
            ty: toks.parse()?,
            expr: toks.parse()?,
        })
    }
}

impl ToTokens for VarUpdate {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.var_name);
        line.to_tokens(&self.ty);
        line.to_tokens(&self.expr);
    }
}

impl ParseFromLine for Expr {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        let ty;
        let expr_val = match tok.as_str() {
            "Var" => {
                let name = toks.parse()?;
                ty = toks.parse()?;
                ExprValue::Var(name)
            }
            "Op" => {
                let name = toks.parse()?;
                ty = toks.parse()?;
                let exprs = toks.parse()?;
                ExprValue::Op(name, exprs)
            }
            "Num" => {
                let num = toks.parse()?;
                ty = toks.parse()?;
                ExprValue::Num(num)
            }
            "Type" => {
                ty = Type::Type;
                let ty_val = toks.parse()?;
                ExprValue::Type(ty_val)
            }
            "Symbol" => {
                let name = toks.parse()?;
                ty = toks.parse()?;
                ExprValue::Symbol(name)
            }
            "Token" => {
                let name = toks.parse()?;
                ty = toks.parse()?;
                ExprValue::Token(name)
            }
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        };
        Ok(Self::new(ty, expr_val))
    }
}

impl ToTokens for Expr {
    fn to_tokens(&self, line: &mut LineBuf) {
        let ty = &self.ty;
        match &self.value {
            ExprValue::Var(name) => {
                line.to_tokens("Var");
                line.to_tokens(name);
                line.to_tokens(ty);
            }
            ExprValue::Op(name, exprs) => {
                line.to_tokens("Op");
                line.to_tokens(name);
                line.to_tokens(ty);
                line.to_tokens(&**exprs);
            }
            ExprValue::Num(num) => {
                line.to_tokens("Num");
                line.lower_hex_to_tokens(num);
                line.to_tokens(ty);
            }
            ExprValue::Type(ty_val) => {
                line.to_tokens("Type");
                line.to_tokens(ty_val);
            }
            ExprValue::Symbol(name) => {
                line.to_tokens("Symbol");
                line.to_tokens(name);
                line.to_tokens(ty);
            }
            ExprValue::Token(name) => {
                line.to_tokens("Token");
                line.to_tokens(name);
                line.to_tokens(ty);
            }
        }
    }
}

impl ParseFromLine for Type {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Ok(match tok.as_str() {
            "Bool" => Self::Bool,
            "Mem" => Self::Mem,
            "Dom" => Self::Dom,
            "HTD" => Self::Htd,
            "PMS" => Self::Pms,
            "UNIT" => Self::Unit,
            "Type" => Self::Type,
            "Token" => Self::Token,
            "RelWrapper" => Self::RelWrapper,
            "Word" => Self::Word(toks.parse_prim_int()?),
            "WordArray" => Self::WordArray {
                length: toks.parse_prim_int()?,
                bits: toks.parse_prim_int()?,
            },
            "Array" => Self::Array(toks.parse()?, toks.parse_prim_int()?),
            "Struct" => Self::Struct(toks.parse()?),
            "Ptr" => Self::Ptr(toks.parse()?),
            _ => return Err(ParseError::UnexpectedToken(tok.location())),
        })
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, line: &mut LineBuf) {
        match self {
            Self::Bool => {
                line.to_tokens("Bool");
            }
            Self::Mem => {
                line.to_tokens("Mem");
            }
            Self::Dom => {
                line.to_tokens("Dom");
            }
            Self::Htd => {
                line.to_tokens("HTD");
            }
            Self::Pms => {
                line.to_tokens("PMS");
            }
            Self::Unit => {
                line.to_tokens("UNIT");
            }
            Self::Type => {
                line.to_tokens("Type");
            }
            Self::Token => {
                line.to_tokens("Token");
            }
            Self::RelWrapper => {
                line.to_tokens("RelWrapper");
            }
            Self::Word(num_bits) => {
                line.to_tokens("Word");
                line.display_to_tokens(num_bits);
            }
            Self::WordArray { length, bits } => {
                line.to_tokens("WordArray");
                line.display_to_tokens(length);
                line.display_to_tokens(bits);
            }
            Self::Array(ty, length) => {
                line.to_tokens("Array");
                line.to_tokens(&**ty);
                line.display_to_tokens(length);
            }
            Self::Struct(name) => {
                line.to_tokens("Struct");
                line.to_tokens(name);
            }
            Self::Ptr(ty) => {
                line.to_tokens("Ptr");
                line.to_tokens(&**ty);
            }
        }
    }
}

impl Op {
    pub(crate) fn from_op_name(s: &str) -> Option<Self> {
        Some(match s {
            "Plus" => Self::Plus,
            "Minus" => Self::Minus,
            "Times" => Self::Times,
            "Modulus" => Self::Modulus,
            "DividedBy" => Self::DividedBy,
            "BWAnd" => Self::BWAnd,
            "BWOr" => Self::BWOr,
            "BWXOR" => Self::BWXOR,
            "And" => Self::And,
            "Or" => Self::Or,
            "Implies" => Self::Implies,
            "Equals" => Self::Equals,
            "Less" => Self::Less,
            "LessEquals" => Self::LessEquals,
            "SignedLess" => Self::SignedLess,
            "SignedLessEquals" => Self::SignedLessEquals,
            "ShiftLeft" => Self::ShiftLeft,
            "ShiftRight" => Self::ShiftRight,
            "CountLeadingZeroes" => Self::CountLeadingZeroes,
            "CountTrailingZeroes" => Self::CountTrailingZeroes,
            "WordReverse" => Self::WordReverse,
            "SignedShiftRight" => Self::SignedShiftRight,
            "Not" => Self::Not,
            "BWNot" => Self::BWNot,
            "WordCast" => Self::WordCast,
            "WordCastSigned" => Self::WordCastSigned,
            "True" => Self::True,
            "False" => Self::False,
            "UnspecifiedPrecond" => Self::UnspecifiedPrecond,
            "MemUpdate" => Self::MemUpdate,
            "MemAcc" => Self::MemAcc,
            "IfThenElse" => Self::IfThenElse,
            "ArrayIndex" => Self::ArrayIndex,
            "ArrayUpdate" => Self::ArrayUpdate,
            "MemDom" => Self::MemDom,
            "PValid" => Self::PValid,
            "PWeakValid" => Self::PWeakValid,
            "PAlignValid" => Self::PAlignValid,
            "PGlobalValid" => Self::PGlobalValid,
            "PArrayValid" => Self::PArrayValid,
            "HTDUpdate" => Self::HTDUpdate,
            "WordArrayAccess" => Self::WordArrayAccess,
            "WordArrayUpdate" => Self::WordArrayUpdate,
            "TokenWordsAccess" => Self::TokenWordsAccess,
            "TokenWordsUpdate" => Self::TokenWordsUpdate,
            "ROData" => Self::ROData,
            "StackWrapper" => Self::StackWrapper,
            "EqSelectiveWrapper" => Self::EqSelectiveWrapper,
            "ToFloatingPoint" => Self::ToFloatingPoint,
            "ToFloatingPointSigned" => Self::ToFloatingPointSigned,
            "ToFloatingPointUnsigned" => Self::ToFloatingPointUnsigned,
            "FloatingPointCast" => Self::FloatingPointCast,
            _ => return None,
        })
    }

    pub(crate) fn to_op_name(&self) -> String {
        match self {
            Self::Plus => "Plus".to_string(),
            Self::Minus => "Minus".to_string(),
            Self::Times => "Times".to_string(),
            Self::Modulus => "Modulus".to_string(),
            Self::DividedBy => "DividedBy".to_string(),
            Self::BWAnd => "BWAnd".to_string(),
            Self::BWOr => "BWOr".to_string(),
            Self::BWXOR => "BWXOR".to_string(),
            Self::And => "And".to_string(),
            Self::Or => "Or".to_string(),
            Self::Implies => "Implies".to_string(),
            Self::Equals => "Equals".to_string(),
            Self::Less => "Less".to_string(),
            Self::LessEquals => "LessEquals".to_string(),
            Self::SignedLess => "SignedLess".to_string(),
            Self::SignedLessEquals => "SignedLessEquals".to_string(),
            Self::ShiftLeft => "ShiftLeft".to_string(),
            Self::ShiftRight => "ShiftRight".to_string(),
            Self::CountLeadingZeroes => "CountLeadingZeroes".to_string(),
            Self::CountTrailingZeroes => "CountTrailingZeroes".to_string(),
            Self::WordReverse => "WordReverse".to_string(),
            Self::SignedShiftRight => "SignedShiftRight".to_string(),
            Self::Not => "Not".to_string(),
            Self::BWNot => "BWNot".to_string(),
            Self::WordCast => "WordCast".to_string(),
            Self::WordCastSigned => "WordCastSigned".to_string(),
            Self::True => "True".to_string(),
            Self::False => "False".to_string(),
            Self::UnspecifiedPrecond => "UnspecifiedPrecond".to_string(),
            Self::MemUpdate => "MemUpdate".to_string(),
            Self::MemAcc => "MemAcc".to_string(),
            Self::IfThenElse => "IfThenElse".to_string(),
            Self::ArrayIndex => "ArrayIndex".to_string(),
            Self::ArrayUpdate => "ArrayUpdate".to_string(),
            Self::MemDom => "MemDom".to_string(),
            Self::PValid => "PValid".to_string(),
            Self::PWeakValid => "PWeakValid".to_string(),
            Self::PAlignValid => "PAlignValid".to_string(),
            Self::PGlobalValid => "PGlobalValid".to_string(),
            Self::PArrayValid => "PArrayValid".to_string(),
            Self::HTDUpdate => "HTDUpdate".to_string(),
            Self::WordArrayAccess => "WordArrayAccess".to_string(),
            Self::WordArrayUpdate => "WordArrayUpdate".to_string(),
            Self::TokenWordsAccess => "TokenWordsAccess".to_string(),
            Self::TokenWordsUpdate => "TokenWordsUpdate".to_string(),
            Self::ROData => "ROData".to_string(),
            Self::StackWrapper => "StackWrapper".to_string(),
            Self::EqSelectiveWrapper => "EqSelectiveWrapper".to_string(),
            Self::ToFloatingPoint => "ToFloatingPoint".to_string(),
            Self::ToFloatingPointSigned => "ToFloatingPointSigned".to_string(),
            Self::ToFloatingPointUnsigned => "ToFloatingPointUnsigned".to_string(),
            Self::FloatingPointCast => "FloatingPointCast".to_string(),
        }
    }
}

impl ParseFromLine for Op {
    fn parse(toks: &mut LineBuffer) -> Result<Self, ParseError> {
        let tok = toks.next()?;
        Self::from_op_name(tok.as_str())
            .ok_or_else(|| ParseError::InvalidOperationName(tok.location()))
    }
}

impl ToTokens for Op {
    fn to_tokens(&self, line: &mut LineBuf) {
        line.to_tokens(&self.to_op_name());
    }
}

#[cfg(test)]
mod tests {
    use crate::concrete_syntax::parse::ParseFile;

    use crate::tests::utils::*;

    use super::*;

    #[test]
    fn round_trips() {
        let mut paths = vec![];
        paths.push(graph_refine_dir().join("example/Functions.txt"));
        paths.push(graph_refine_dir().join("loop-example/CFuns-annotated.txt"));
        for opt_level in ["O1", "O2"] {
            let d = graph_refine_dir().join("loop-example").join(opt_level);
            paths.push(d.join(format!("ASM{opt_level}Funs.txt")));
            paths.push(d.join("ASM-annotated.txt"));
            paths.push(d.join("CFunDump.txt"));
        }
        paths.push(graph_refine_dir().join("loop-example/synth/Functions.txt"));
        paths.push(sel4_target_dir().join("ASMFunctions.txt"));
        paths.push(sel4_target_dir().join("CFunctions.txt"));
        paths.push(sel4_target_dir().join("functions.txt"));

        for path in paths {
            test_round_trip_path(path, File::parse_from_str, File::pretty_print);
        }
    }
}
