use ast::{
    ArraySize, BinaryOperator, Constant, DeclarationSpecifier, Declarator, DerivedDeclarator,
    Designator, Ellipsis, Expression, FloatBase, FloatFormat, FloatSuffix, Initializer,
    InitializerListItem, IntegerBase, IntegerSize, IntegerSuffix, MemberOperator,
    ParameterDeclaration, PointerQualifier, SpecifierQualifier, TypeName, TypeQualifier,
    TypeSpecifier, UnaryOperator,
};
use bindgen::callbacks::TokenKind;
use bindgen::callbacks::{ParseCallbacks, Token};
use cexpr::expr::EvalResult;
use cexpr::literal::CChar;
use env::Env;
use parser::{expression, type_name};
use regex::{Captures, Regex};
use span::Node;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Formatter, Write};
use std::rc::Rc;

#[derive(Default)]
pub struct HackBindgenContext {
    macro_define: HashMap<String, MacroItem>,
    inline_fn:
        HashMap<String, Box<dyn Fn(&[RustExpression], &HackBindgenContext) -> RustExpression>>,
}

impl Debug for HackBindgenContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HackBindgenContext")
            .field("macro_define", &self.macro_define)
            .finish()
    }
}

impl HackBindgenContext {
    pub fn define_inline_fn<
        F: Fn(&[RustExpression], &HackBindgenContext) -> RustExpression + 'static,
    >(
        &mut self,
        name: &str,
        inline_fn: F,
    ) {
        self.inline_fn.insert(name.to_string(), Box::new(inline_fn));
    }

    pub fn define_inline_fn_type(&mut self, name: &str, ty: RustType) {
        let name_str = name.to_string();
        self.define_inline_fn(name, move |args, ctx| {
            let args = args
                .iter()
                .map(|i| i.expression.clone())
                .collect::<Vec<_>>();
            RustExpression {
                type_info: Some(ty.clone()),
                expression: format!("{}({})", name_str, args.join(", ")),
            }
        });
    }
}

#[derive(Debug, Default, Clone)]
pub struct RustType {
    type_name: String,
    is_array: bool,

    // 这些修饰符用于生成过程，并不作用于最终结果
    has_const: bool,
    has_signed: bool,
    has_unsigned: bool,
}

impl RustType {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_name(name: &str) -> Self {
        Self {
            type_name: name.to_string(),
            ..Self::new()
        }
    }

    pub fn from_integer_suffix(suffix: &IntegerSuffix) -> Self {
        let type_name = match (suffix.size, suffix.unsigned) {
            (IntegerSize::Int, false) => "core::ffi::c_int",
            (IntegerSize::Int, true) => "core::ffi::c_uint",
            (IntegerSize::Long, false) => "core::ffi::c_long",
            (IntegerSize::Long, true) => "core::ffi::c_ulong",
            (IntegerSize::LongLong, false) => "core::ffi::c_longlong",
            (IntegerSize::LongLong, true) => "core::ffi::c_ulonglong",
        };
        Self {
            type_name: type_name.to_string(),
            ..Self::new()
        }
    }

    pub fn from_float_suffix(suffix: &FloatSuffix) -> Self {
        let type_name = match suffix.format {
            FloatFormat::Float => "core::ffi::c_float",
            FloatFormat::Double => "core::ffi::c_double",
            FloatFormat::LongDouble => "LongDouble",
            FloatFormat::TS18661Format(_) => "TS18661Format",
        };
        Self {
            type_name: type_name.to_string(),
            ..Self::new()
        }
    }

    pub fn from_type_name(node: &Node<TypeName>, ctx: &HackBindgenContext) -> Self {
        let mut this = Self::new();
        for i in &node.node.specifiers {
            this.with_specifier_qualifier(i);
        }
        if let Some(s) = &node.node.declarator {
            this.with_declarator(s, ctx);
        }
        this
    }

    pub fn from_parameter_declaration(node: &Node<ParameterDeclaration>) -> Self {
        let mut this = Self::new();
        for i in &node.node.specifiers {
            this.with_declaration_specifier(i);
        }
        this
    }

    pub fn with_type_specifier(&mut self, node: &Node<TypeSpecifier>) {
        match &node.node {
            TypeSpecifier::Void => self.type_name = "core::ffi::c_void".to_string(),
            TypeSpecifier::Char => {
                if self.has_unsigned {
                    self.type_name = "core::ffi::c_uchar".to_string();
                } else if self.has_signed {
                    self.type_name = "core::ffi::c_schar".to_string();
                } else {
                    self.type_name = "core::ffi::c_char".to_string();
                }
            }
            TypeSpecifier::Short => {
                if self.has_unsigned {
                    self.type_name = "core::ffi::c_ushort".to_string();
                } else {
                    self.type_name = "core::ffi::c_short".to_string();
                }
            }
            TypeSpecifier::Int => {
                if self.has_unsigned {
                    self.type_name = "core::ffi::c_uint".to_string();
                } else {
                    self.type_name = "core::ffi::c_int".to_string();
                }
            }
            TypeSpecifier::Long => {
                if self.has_unsigned {
                    self.type_name = "core::ffi::c_ulong".to_string();
                } else {
                    self.type_name = "core::ffi::c_long".to_string();
                }
            }
            TypeSpecifier::Float => self.type_name = "core::ffi::c_float".to_string(),
            TypeSpecifier::Double => self.type_name = "core::ffi::c_double".to_string(),
            TypeSpecifier::Bool => self.type_name = "bool".to_string(),
            TypeSpecifier::Struct(v) => {
                if let Some(id) = &v.node.identifier {
                    self.type_name = id.node.name.clone();
                }
            }
            TypeSpecifier::Enum(v) => {
                if let Some(id) = &v.node.identifier {
                    self.type_name = id.node.name.clone();
                }
            }
            TypeSpecifier::TypedefName(v) => self.type_name = v.node.name.clone(),
            _ => panic!(),
        }
    }

    pub fn with_type_qualifier(&mut self, node: &Node<TypeQualifier>) {
        match &node.node {
            TypeQualifier::Const => self.has_const = true,
            _ => {}
        }
    }

    pub fn with_declaration_specifier(&mut self, node: &Node<DeclarationSpecifier>) {
        match &node.node {
            DeclarationSpecifier::TypeSpecifier(p) => {
                self.with_type_specifier(p);
            }
            DeclarationSpecifier::TypeQualifier(q) => {
                self.with_type_qualifier(q);
            }
            _ => {}
        }
    }

    pub fn with_specifier_qualifier(&mut self, node: &Node<SpecifierQualifier>) {
        match &node.node {
            SpecifierQualifier::TypeSpecifier(v) => self.with_type_specifier(v),
            SpecifierQualifier::TypeQualifier(v) => self.with_type_qualifier(v),
            SpecifierQualifier::Extension(_) => {}
        }
    }

    pub fn with_declarator(&mut self, node: &Node<Declarator>, ctx: &HackBindgenContext) {
        for i in &node.node.derived {
            match &i.node {
                DerivedDeclarator::Pointer(v) => {
                    if self.has_const {
                        self.type_name = format!("*const {}", self.type_name);
                    } else {
                        self.type_name = format!("*mut {}", self.type_name);
                    }
                    for i in v {
                        match &i.node {
                            PointerQualifier::TypeQualifier(q) => match &q.node {
                                TypeQualifier::Const => self.has_const = true,
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                }
                DerivedDeclarator::Array(v) => {
                    self.is_array = true;
                    match &v.node.size {
                        ArraySize::Unknown => self.type_name = format!("[{}]", self.type_name),
                        ArraySize::VariableUnknown => {
                            self.type_name = format!("[{}]", self.type_name)
                        }
                        ArraySize::VariableExpression(v) => {
                            let len = RustExpression::from_node(v, ctx).expression;
                            self.type_name = format!("[{}; {}]", self.type_name, len);
                        }
                        ArraySize::StaticExpression(v) => {
                            let len = RustExpression::from_node(v, ctx).expression;
                            self.type_name = format!("[{}; {}]", self.type_name, len);
                        }
                    }
                }
                DerivedDeclarator::Function(v) => {
                    let mut args = v
                        .node
                        .parameters
                        .iter()
                        .map(|i| RustType::from_parameter_declaration(i).type_name)
                        .collect::<Vec<_>>();
                    if v.node.ellipsis == Ellipsis::Some {
                        args.push("...".to_string());
                    }
                    if args.len() == 1 && args[0] == "void" {
                        args.clear();
                    }
                    self.type_name = format!(
                        "Option<extern \"C\" fn({}) -> {}>",
                        args.join(", "),
                        self.type_name
                    );
                }
                _ => {}
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct RustExpression {
    type_info: Option<RustType>,
    expression: String,
}

impl RustExpression {
    pub fn from_node(node: &Node<Expression>, ctx: &HackBindgenContext) -> Self {
        let mut type_info = None;
        let mut expression = String::new();
        match &node.node {
            Expression::Identifier(v) => {
                if let Some(MacroItem::Expression(e)) = ctx.macro_define.get(&v.node.name) {
                    type_info = e.type_info.clone();
                }
                expression = v.node.name.clone();
            }
            Expression::Constant(v) => match &v.node {
                Constant::Integer(v) => {
                    let base = match &v.base {
                        IntegerBase::Decimal => "",
                        IntegerBase::Octal => "0",
                        IntegerBase::Hexadecimal => "0x",
                        IntegerBase::Binary => "0b",
                    };
                    let ty = RustType::from_integer_suffix(&v.suffix);
                    expression = format!("{}{} as {}", base, v.number, ty.type_name);
                    type_info = Some(ty);
                }
                Constant::Float(v) => {
                    let ty = RustType::from_float_suffix(&v.suffix);
                    expression = format!("{} as {}", v.number, ty.type_name);
                    if v.base == FloatBase::Hexadecimal {
                        let f = match &v.suffix.format {
                            FloatFormat::Float => "f",
                            FloatFormat::Double => "",
                            FloatFormat::LongDouble => "l",
                            FloatFormat::TS18661Format(_) => "",
                        };
                        if let Ok((_, s)) = cexpr::literal::parse(
                            format!(
                                "{}{}{}",
                                match v.base {
                                    FloatBase::Decimal => "",
                                    FloatBase::Hexadecimal => "0x",
                                },
                                v.number,
                                f,
                            )
                            .as_bytes(),
                        ) {
                            if let EvalResult::Float(s) = s {
                                expression = format!("{}{} as {}", s, f, ty.type_name);
                            }
                        }
                    } else {
                        expression = format!("{} as {}", v.number, ty.type_name);
                    }
                    type_info = Some(ty);
                }
                Constant::Character(v) => {
                    let ty = RustType::from_name("core::ffi::c_char");
                    expression = format!("{} as {}", v, ty.type_name);
                    type_info = Some(ty);
                }
            },
            Expression::StringLiteral(v) => {
                let mut v = v
                    .node
                    .iter()
                    .map(|i| match cexpr::literal::parse(i.as_bytes()) {
                        Ok((_, EvalResult::Str(v))) => v,
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>()
                    .concat();
                v.push(0);
                let ty = RustType {
                    type_name: format!("&[u8; {}]", v.len()),
                    is_array: true,
                    ..RustType::new()
                };
                expression = format!("b\"{}\"", v.escape_ascii());
                type_info = Some(ty);
            }
            Expression::GenericSelection(v) => {
                expression = format!("{:?}", v);
            }
            Expression::Member(v) => match v.node.operator.node {
                MemberOperator::Direct => {
                    let exp = RustExpression::from_node(&v.node.expression, ctx);
                    expression = format!("({}).{}", exp.expression, v.node.identifier.node.name);
                }
                MemberOperator::Indirect => {
                    let exp = RustExpression::from_node(&v.node.expression, ctx);
                    expression = format!("(*({})).{}", exp.expression, v.node.identifier.node.name);
                }
            },
            Expression::Call(v) => {
                let args = v
                    .node
                    .arguments
                    .iter()
                    .map(|i| RustExpression::from_node(i, ctx))
                    .collect::<Vec<_>>();
                if let Expression::Identifier(f) = &v.node.callee.node {
                    if let Some(inline_fn) = ctx.inline_fn.get(&f.node.name) {
                        let r = inline_fn(&args, ctx);
                        expression = r.expression;
                        type_info = r.type_info;
                    } else {
                        let args = args.into_iter().map(|i| i.expression).collect::<Vec<_>>();
                        expression = format!("{}({})", f.node.name, args.join(", "));
                    }
                } else {
                    let callee = RustExpression::from_node(&v.node.callee, ctx).expression;
                    let args = args.into_iter().map(|i| i.expression).collect::<Vec<_>>();
                    expression = format!("({})({})", callee, args.join(", "));
                };
            }
            Expression::CompoundLiteral(v) => {
                let ty = RustType::from_type_name(&v.node.type_name, ctx);
                let mut code = String::new();

                fn init_list(
                    code: &mut String,
                    pre: &str,
                    node: &Node<InitializerListItem>,
                    ctx: &HackBindgenContext,
                    index: &mut u128,
                ) {
                    let mut index_str = String::new();
                    if node.node.designation.is_empty() {
                        index_str = format!("{}", index);
                    } else {
                        if let Designator::Index(e) = &node.node.designation[0].node {
                            if let Ok((_, v)) = cexpr::literal::parse(
                                RustExpression::from_node(e, ctx).expression.as_bytes(),
                            ) {
                                match v {
                                    EvalResult::Int(s) => {
                                        *index = s.0 as u128;
                                    }
                                    EvalResult::Char(s) => {
                                        *index = match s {
                                            CChar::Char(s) => s as u128,
                                            CChar::Raw(s) => s as u128,
                                        };
                                    }
                                    _ => {}
                                }
                            }
                        }
                        let list = node
                            .node
                            .designation
                            .iter()
                            .map(|i| match &i.node {
                                Designator::Index(e) => {
                                    format!("[{}]", RustExpression::from_node(e, ctx).expression)
                                }
                                Designator::Member(e) => {
                                    format!(".{}", e.node.name)
                                }
                                Designator::Range(e) => format!(
                                    "{}..{}]",
                                    RustExpression::from_node(&e.node.from, ctx).expression,
                                    RustExpression::from_node(&e.node.to, ctx).expression
                                ),
                            })
                            .collect::<Vec<_>>();
                        index_str = list.join("");
                    }
                    match &node.node.initializer.node {
                        Initializer::Expression(e) => {
                            writeln!(
                                code,
                                "v{} = {};",
                                index_str,
                                RustExpression::from_node(e, ctx).expression
                            )
                            .unwrap();
                        }
                        Initializer::List(e) => {
                            let mut i = 0;
                            for n in e {
                                init_list(code, &format!("{}{}", pre, index_str), n, ctx, &mut i);
                            }
                        }
                    }
                }

                let mut index = 0;
                for i in &v.node.initializer_list {
                    init_list(&mut code, "v", i, ctx, &mut index);
                }
                expression = format!(
                    "unsafe {{let mut v = core::mem::zeroed::<{}>(); {} v}}",
                    ty.type_name, code
                );
                type_info = Some(ty);
            }
            Expression::SizeOfTy(v) => {
                let par_ty = RustType::from_type_name(&v.node.0, ctx);
                expression = format!("size_of::<{}>()", par_ty.type_name);
                type_info = Some(RustType::from_name("usize"));
            }
            Expression::AlignOf(v) => {
                let par_ty = RustType::from_type_name(&v.node.0, ctx);
                expression = format!("align_of::<{}>()", par_ty.type_name);
                type_info = Some(RustType::from_name("usize"));
            }
            Expression::UnaryOperator(v) => {
                let ops = RustExpression::from_node(&v.node.operand, ctx);
                type_info = ops.type_info;
                match &v.node.operator.node {
                    UnaryOperator::PostIncrement => {
                        expression = format!(
                            "{{let p = &mut ({}); let v = *p; *p += 1; v}}",
                            ops.expression
                        );
                    }
                    UnaryOperator::PostDecrement => {
                        expression = format!(
                            "{{let p = &mut ({}); let v = *p; *p -= 1; v}}",
                            ops.expression
                        );
                    }
                    UnaryOperator::PreIncrement => {
                        expression = format!("{{let p = &mut ({}); *p += 1; *p}}", ops.expression);
                    }
                    UnaryOperator::PreDecrement => {
                        expression = format!("{{let p = &mut ({}); *p -= 1; *p}}", ops.expression);
                    }
                    UnaryOperator::Address => {
                        expression = format!("(&raw const ({})).cast_mut()", ops.expression);
                        type_info = type_info.map(|i| RustType {
                            type_name: format!("*mut ({})", i.type_name),
                            ..RustType::new()
                        });
                    }
                    UnaryOperator::Indirection => {
                        expression = format!("*({})", ops.expression);
                        // TODO: impl trait Pointer { Target = T; }
                        type_info = None;
                    }
                    UnaryOperator::Plus => {
                        expression = format!("({})", ops.expression);
                    }
                    UnaryOperator::Minus => {
                        expression = format!("-({})", ops.expression);
                    }
                    UnaryOperator::Complement => {
                        expression = format!("!({})", ops.expression);
                    }
                    UnaryOperator::Negate => {
                        expression = format!("(({}) == 0) as core::ffi::c_int", ops.expression);
                        type_info = Some(RustType::from_name("bool"));
                    }
                }
            }
            Expression::Cast(v) => {
                let ops = RustExpression::from_node(&v.node.expression, ctx);
                let ty = RustType::from_type_name(&v.node.type_name, ctx);
                expression = format!("{} as {}", ops.expression, ty.type_name);
                type_info = Some(ty);
            }
            Expression::BinaryOperator(v) => {
                let lhs = RustExpression::from_node(&v.node.lhs, ctx);
                let rhs = RustExpression::from_node(&v.node.rhs, ctx);
                match &v.node.operator.node {
                    BinaryOperator::Index => {
                        expression = format!("({})[{}]", lhs.expression, rhs.expression);
                        // TODO: use <A as core::ops::Index<B>>::Output
                    }
                    BinaryOperator::Multiply => {
                        expression = format!("({}) * ({})", lhs.expression, rhs.expression);
                        // FIXME: Type Enhancement
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::Divide => {
                        expression = format!("({}) / ({})", lhs.expression, rhs.expression);
                        // FIXME: Type Enhancement
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::Modulo => {
                        expression = format!("({}) % ({})", lhs.expression, rhs.expression);
                        // FIXME: Type Enhancement
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::Plus => {
                        expression = format!("({}) + ({})", lhs.expression, rhs.expression);
                        // FIXME: Type Enhancement
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::Minus => {
                        expression = format!("({}) - ({})", lhs.expression, rhs.expression);
                        // FIXME: Type Enhancement
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::ShiftLeft => {
                        expression =
                            format!("(({}) << (({}) as i32))", lhs.expression, rhs.expression);
                        type_info = lhs.type_info;
                    }
                    BinaryOperator::ShiftRight => {
                        expression =
                            format!("(({}) >> (({}) as i32))", lhs.expression, rhs.expression);
                        type_info = lhs.type_info;
                    }
                    BinaryOperator::Less => {
                        expression = format!(
                            "(({}) < ({})) as core::ffi::c_int",
                            lhs.expression, rhs.expression
                        );
                        type_info = Some(RustType::from_name("core::ffi::c_int"));
                    }
                    BinaryOperator::Greater => {
                        expression = format!(
                            "(({}) > ({})) as core::ffi::c_int",
                            lhs.expression, rhs.expression
                        );
                        type_info = Some(RustType::from_name("core::ffi::c_int"));
                    }
                    BinaryOperator::LessOrEqual => {
                        expression = format!(
                            "(({}) <= ({})) as core::ffi::c_int",
                            lhs.expression, rhs.expression
                        );
                        type_info = Some(RustType::from_name("core::ffi::c_int"));
                    }
                    BinaryOperator::GreaterOrEqual => {
                        expression = format!(
                            "(({}) >= ({})) as core::ffi::c_int",
                            lhs.expression, rhs.expression
                        );
                        type_info = Some(RustType::from_name("core::ffi::c_int"));
                    }
                    BinaryOperator::Equals => {
                        expression = format!(
                            "(({}) == ({})) as core::ffi::c_int",
                            lhs.expression, rhs.expression
                        );
                        type_info = Some(RustType::from_name("core::ffi::c_int"));
                    }
                    BinaryOperator::NotEquals => {
                        expression = format!(
                            "(({}) != ({})) as core::ffi::c_int",
                            lhs.expression, rhs.expression
                        );
                        type_info = Some(RustType::from_name("core::ffi::c_int"));
                    }
                    BinaryOperator::BitwiseAnd => {
                        expression = format!("(({}) & ({}))", lhs.expression, rhs.expression);
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::BitwiseXor => {
                        expression = format!("(({}) ^ ({}))", lhs.expression, rhs.expression);
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::BitwiseOr => {
                        expression = format!("(({}) | ({}))", lhs.expression, rhs.expression);
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::LogicalAnd => {
                        expression = format!(
                            "((({}) as bool) && (({}) as bool)) as core::ffi::c_int",
                            lhs.expression, rhs.expression
                        );
                        type_info = Some(RustType::from_name("core::ffi::c_int"));
                    }
                    BinaryOperator::LogicalOr => {
                        expression = format!(
                            "((({}) as bool) || (({}) as bool)) as core::ffi::c_int",
                            lhs.expression, rhs.expression
                        );
                        type_info = Some(RustType::from_name("core::ffi::c_int"));
                    }
                    BinaryOperator::Assign => {
                        expression = format!(
                            "{{let p = &mut {}; *p = {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::AssignMultiply => {
                        expression = format!(
                            "{{let p = &mut {}; *p += {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::AssignDivide => {
                        expression = format!(
                            "{{let p = &mut {}; *p /= {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::AssignModulo => {
                        expression = format!(
                            "{{let p = &mut {}; *p %= {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::AssignPlus => {
                        expression = format!(
                            "{{let p = &mut {}; *p += {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::AssignMinus => {
                        expression = format!(
                            "{{let p = &mut {}; *p -= {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::AssignShiftLeft => {
                        expression = format!(
                            "{{let p = &mut {}; *p <<= ({} as i32); *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info;
                    }
                    BinaryOperator::AssignShiftRight => {
                        expression = format!(
                            "{{let p = &mut {}; *p >>= ({} as i32); *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info;
                    }
                    BinaryOperator::AssignBitwiseAnd => {
                        expression = format!(
                            "{{let p = &mut {}; *p &= {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::AssignBitwiseXor => {
                        expression = format!(
                            "{{let p = &mut {}; *p ^= {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                    BinaryOperator::AssignBitwiseOr => {
                        expression = format!(
                            "{{let p = &mut {}; *p |= {}; *p}}",
                            lhs.expression, rhs.expression
                        );
                        type_info = lhs.type_info.or(rhs.type_info);
                    }
                }
            }
            Expression::Conditional(v) => {
                let condition = RustExpression::from_node(&v.node.condition, ctx);
                let then_expression = RustExpression::from_node(&v.node.then_expression, ctx);
                let else_expression = RustExpression::from_node(&v.node.else_expression, ctx);
                expression = format!(
                    "if {} {{{}}} else {{{}}}",
                    condition.expression, then_expression.expression, else_expression.expression
                );
                type_info = then_expression.type_info.or(else_expression.type_info);
            }
            Expression::Comma(v) => {
                let mut code = v
                    .iter()
                    .map(|x| RustExpression::from_node(x, ctx))
                    .collect::<Vec<_>>();
                type_info = code.last().map(|i| i.type_info.clone()).flatten();
                expression = format!(
                    "{{{}}}",
                    code.iter()
                        .map(|i| i.expression.clone())
                        .collect::<Vec<_>>()
                        .join(";")
                );
            }
            Expression::OffsetOf(v) => {
                // TODO: offset_of!()
            }
            Expression::VaArg(v) => {
                // FIXME:
            }
            Expression::Statement(v) => {
                // FIXME:
            }
            Expression::SizeOfVal(_) => {
                // FIXME:
            }
        }
        Self {
            type_info,
            expression,
        }
    }
}

#[derive(Debug)]
pub enum MacroItem {
    Expression(RustExpression),
    TypeName(RustType),
}

#[derive(Debug, Default, Clone)]
pub struct HackBindgenCallbacks(Rc<RefCell<HackBindgenContext>>);

impl HackBindgenCallbacks {
    pub fn new(ctx: HackBindgenContext) -> Self {
        Self(Rc::new(RefCell::new(ctx)))
    }

    pub fn define(&self, name: &str, tokens: &[Token]) -> bool {
        let code = tokens
            .iter()
            .skip(1)
            .map(|i| String::from_utf8_lossy(&i.raw).into_owned())
            .collect::<Vec<_>>()
            .join(" ");
        {
            let mut env_exp = Env::with_clang();
            let mut env_type = Env::with_clang();
            if let Ok(v) = expression(&code, &mut env_exp) {
                let expr = RustExpression::from_node(&v, &self.0.borrow());
                self.0
                    .borrow_mut()
                    .macro_define
                    .insert(name.to_string(), MacroItem::Expression(expr));
                return true;
            } else if let Ok(v) = type_name(&code, &mut env_type) {
                let expr = RustType::from_type_name(&v, &self.0.borrow());
                self.0
                    .borrow_mut()
                    .macro_define
                    .insert(name.to_string(), MacroItem::TypeName(expr));
                return true;
            }
        }
        false
    }

    pub fn post_processing(&self, code: &mut String) {
        let inner = self.0.borrow_mut();
        let reg =
            Regex::new(r###"pub[ \r\n]+const[ \r\n]+(?P<NAME>.*)[ \r\n]*:[ \r\n]*.*[ \r\n]*=[ \r\n]*b"hack_bindgen_macro:(?P<ID>.*)\\0"[ \r\n]*;"###)
                .unwrap();
        let result = reg
            .replace_all(code, |x: &Captures| -> Cow<str> {
                let id = &x["ID"];
                let name = &x["NAME"];
                if let Some(item) = inner.macro_define.get(id) {
                    match item {
                        MacroItem::Expression(e) => {
                            if let Some(ty) = &e.type_info {
                                return format!(
                                    "pub const {}: {} = unsafe {{{}}};",
                                    name, ty.type_name, e.expression
                                )
                                .into();
                            } else {
                                return format!(
                                    "// pub const {}: unknown = unsafe {{{}}};",
                                    name, e.expression
                                )
                                .into();
                            }
                        }
                        MacroItem::TypeName(t) => {
                            return format!("pub type {} = {};", name, t.type_name).into()
                        }
                    }
                }
                format!("// {} unknown macro define", name).into()
            })
            .to_string();

        *code = result;
    }

    pub fn generate(&self, mut b: bindgen::Builder) -> Result<String, Box<dyn Error>> {
        b = b.raw_line("pub type uint8_t = u8;");
        b = b.raw_line("pub type int8_t = i8;");
        b = b.raw_line("pub type uint16_t = u16;");
        b = b.raw_line("pub type int16_t = i16;");
        b = b.raw_line("pub type uint32_t = u32;");
        b = b.raw_line("pub type int32_t = i32;");
        b = b.raw_line("pub type uint64_t = u64;");
        b = b.raw_line("pub type int64_t = i64;");
        b = b.raw_line("pub type uint128_t = u128;");
        b = b.raw_line("pub type int128_t = i128;");
        b = b.parse_callbacks(Box::new(self.clone()));
        let bindings = b.generate()?;
        let mut code = Vec::<u8>::new();
        bindings.write(Box::new(&mut code as &mut dyn std::io::Write))?;
        let mut code = String::from_utf8_lossy(&code).into_owned();
        self.post_processing(&mut code);
        Ok(code)
    }
}

impl ParseCallbacks for HackBindgenCallbacks {
    fn modify_macro(&self, name: &str, tokens: &mut Vec<Token>) {
        if tokens.len() > 1 {
            if self.define(name, tokens) {
                // 替换为字符串定义以便于后处理的替换查找
                tokens.truncate(1);
                tokens.push(Token {
                    kind: TokenKind::Literal,
                    raw: format!(
                        "\"{}\"",
                        format!("hack_bindgen_macro:{}", name)
                            .as_bytes()
                            .escape_ascii()
                    )
                    .into_boxed_str()
                    .into_boxed_bytes(),
                });
            }
        }
    }
}

#[test]
fn a() {
    let mut bindgen = bindgen::builder();
    bindgen = bindgen.use_core();
    bindgen = bindgen.layout_tests(false);
    bindgen = bindgen.derive_partialeq(true);
    bindgen = bindgen.merge_extern_blocks(true);

    bindgen = bindgen.clang_arg("--target=thumbv7em-none-eabihf");
    bindgen = bindgen.clang_arg("-mcpu=cortex-m4");

    bindgen = bindgen.header("test.h");

    bindgen =
        bindgen.raw_line("pub const fn BIT(n: core::ffi::c_int) -> core::ffi::c_int { 1 << n }");

    let mut ctx = HackBindgenContext::default();
    ctx.define_inline_fn_type("BIT", RustType::from_name("core::ffi::c_int"));
    let callback = HackBindgenCallbacks::new(ctx);
    let s = callback.generate(bindgen).unwrap();
    println!("{}", s);
}
