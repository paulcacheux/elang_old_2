use std::collections::HashMap;

use diagnostic::DiagnosticEngine;
use ast::{Type, Function, Param, Block, Statement, Expression, AssignOpKind, BinOpKind,
          UnOpKind};
use source::Span;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Type>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable { scopes: Vec::new() }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    pub fn insert(&mut self, name: String, ty: Type) -> Option<Type> {
        // return Some(old_ty) if name is already defined in this scope, else None

        self.scopes.last_mut().unwrap().insert(name, ty)
    }

    pub fn get(&self, name: &String) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ref ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    name: String,
    parameters: Vec<Param>,
    return_ty: Type,
}

#[derive(Debug, Clone)]
pub struct Sema<'a> {
    pub symbol_table: SymbolTable,
    pub diag_engine: &'a DiagnosticEngine<'a>,

    pub current_ret_ty: Type,
    pub loop_level: usize,
}

impl<'a> Sema<'a> {
    pub fn new(diag_engine: &'a DiagnosticEngine<'a>) -> Self {
        Sema {
            symbol_table: SymbolTable::new(),
            diag_engine: diag_engine,
            current_ret_ty: Type::Unit,
            loop_level: 0,
        }
    }

    pub fn add_io_funcs(&mut self) {
        self.symbol_table.insert(
            "print".to_string(),
            Type::Function(vec![Type::Int], Box::new(Type::Unit))
        );
        self.symbol_table.insert(
            "printchar".to_string(),
            Type::Function(vec![Type::Char], Box::new(Type::Unit))
        );
        self.symbol_table.insert(
            "println".to_string(),
            Type::Function(vec![Type::Int], Box::new(Type::Unit))
        );
        self.symbol_table.insert(
            "read".to_string(),
            Type::Function(vec![], Box::new(Type::Int))
        );
    }

    pub fn sema_function_def_begin(&mut self,
                                   name: String,
                                   params: Vec<Param>,
                                   ret_ty: Type,
                                   name_span: Span)
                                   -> FunctionInfo {
        if let Some(_) = self.symbol_table.insert(name.clone(),
                                                       Type::function_type(params.clone(),
                                                                           ret_ty.clone())) {
            self.diag_engine
                .report_sema_error(format!("{} is already defined as a function", name), name_span);
        }

        self.symbol_table.begin_scope();

        for param in params.clone() {
            if let Some(_) = self.symbol_table.insert(param.name.clone(), param.ty) {
                self.diag_engine
                    .report_sema_error(format!("{} is already defined as a parameter of this \
                                                function",
                                               param.name),
                                       param.span);
            }
        }

        self.current_ret_ty = ret_ty.clone();

        FunctionInfo {
            name: name,
            parameters: params,
            return_ty: ret_ty,
        }
    }

    pub fn sema_function_def_end(&mut self,
                                 func_info: FunctionInfo,
                                 block: Block,
                                 span: Span)
                                 -> Function {
        self.symbol_table.end_scope();
        self.current_ret_ty = Type::Unit;

        Function {
            name: func_info.name,
            parameters: func_info.parameters,
            return_ty: func_info.return_ty,
            block: block,
            span: span,
        }
    }

    pub fn sema_let_stmt(&mut self,
                         id: String,
                         ty: Option<Type>,
                         expr: Expression,
                         span: Span)
                         -> Statement {
        let expr = self.l2r_if_needed(expr);
        let expr_ty = expr.ty();
        let ty = ty.unwrap_or(expr_ty.clone());

        if ty != expr_ty {
            self.diag_engine
                .report_sema_error(format!("Mismatching types in assigment (expected: {}, \
                                            given: {})",
                                           ty,
                                           expr_ty),
                                   span);
        }

        if let Some(_) = self.symbol_table.insert(id.clone(), ty.clone()) {
            self.diag_engine.report_sema_error(format!("{} is already defined", id.clone()), span);
        }

        Statement::Let {
            identifier: id,
            ty: ty,
            expression: expr,
            span: span,
        }
    }

    pub fn sema_if_stmt(&mut self,
                        condition: Expression,
                        if_stmt: Statement,
                        else_stmt: Option<Statement>,
                        span: Span)
                        -> Statement {
        let condition = self.l2r_if_needed(condition);
        let cond_ty = condition.ty();
        if cond_ty != Type::Bool {
            self.diag_engine
                .report_sema_error(format!("If condition must be of bool type (given: {})",
                                           cond_ty),
                                   span);
        }

        Statement::If {
            condition: condition,
            if_statement: Box::new(if_stmt),
            else_statement: else_stmt.map(Box::new),
            span: span,
        }
    }

    pub fn sema_while_stmt(&mut self,
                           condition: Expression,
                           stmt: Statement,
                           span: Span)
                           -> Statement {
        let condition = self.l2r_if_needed(condition);
        let cond_ty = condition.ty();
        if cond_ty != Type::Bool {
            self.diag_engine
                .report_sema_error(format!("While condition must be of bool type (given: {})",
                                           cond_ty),
                                   span);
        }

        Statement::While {
            condition: condition,
            statement: Box::new(stmt),
            span: span,
        }
    }

    pub fn sema_break_stmt(&mut self, span: Span) -> Statement {
        if self.loop_level == 0 {
            self.diag_engine.report_sema_error("Break outside a loop".to_string(), span);
        }

        Statement::Break {
            span: span
        }
    }

    pub fn sema_continue_stmt(&mut self, span: Span) -> Statement {
        if self.loop_level == 0 {
            self.diag_engine.report_sema_error("Continue outside a loop".to_string(), span);
        }

        Statement::Continue {
            span: span
        }
    }

    pub fn sema_return_stmt(&mut self, expr: Expression, span: Span) -> Statement {
        let expr = self.l2r_if_needed(expr);
        let expr_ty = expr.ty();
        if expr_ty != self.current_ret_ty {
            self.diag_engine.report_sema_error(
                format!(
                    "Mismatching return type (expected: {}, given {})",
                    self.current_ret_ty,
                    expr_ty
                ),
                span
            );
        }

        Statement::Return {
            expression: expr,
            span: span
        }
    }

    pub fn sema_identifier(&mut self, id: String, span: Span) -> Expression {
        if let Some(ty) = self.symbol_table.get(&id) {
            Expression::Identifier {
                identifier: id,
                span: span,
                ty: Type::LVal(Box::new(ty.clone())),
            }
        } else {
            self.diag_engine.report_sema_error(format!("Undefined identifier {}", id), span)
        }
    }

    pub fn sema_assignop_expr(&mut self,
                              kind: AssignOpKind,
                              lhs: Expression,
                              rhs: Expression,
                              span: Span) -> Expression {
        let rhs = self.l2r_if_needed(rhs);
        let rhs_ty = rhs.ty();

        let res_ty = if let Type::LVal(lhs_ty) = lhs.ty() {
            if let Some(binop_kind) = kind.to_binop() {
                if let Some(res_ty) = get_binop_ty(binop_kind, &lhs_ty, &rhs_ty) {
                    res_ty
                } else {
                    self.diag_engine.report_sema_error(
                        format!("{:?} is undefined between {} and {}", binop_kind, lhs_ty, rhs_ty),
                        span
                    )
                }
            } else if *lhs_ty != rhs_ty {
                self.diag_engine.report_sema_error(
                    format!(
                        "Mismatching types in assignment (expected: {}, given: {})",
                        lhs_ty,
                        rhs_ty
                    ),
                    span
                )
            } else {
                *lhs_ty
            }
        } else {
            self.diag_engine.report_sema_error(
                "The left hand side of an assignement must be a reference".to_string(),
                span,
            )
        };

        Expression::AssignOp {
            kind: kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: span,
            ty: res_ty,
        }
    }

    pub fn sema_binop_expr(&mut self,
                           kind: BinOpKind,
                           lhs: Expression,
                           rhs: Expression,
                           span: Span) -> Expression {
        let lhs = self.l2r_if_needed(lhs);
        let rhs = self.l2r_if_needed(rhs);

        let lhs_ty = lhs.ty();
        let rhs_ty = rhs.ty();

        if let Some(ty) = get_binop_ty(kind, &lhs_ty, &rhs_ty) {
            Expression::BinOp {
                kind: kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: span,
                ty: ty,
            }
        } else {
            self.diag_engine.report_sema_error(
                format!("{:?} is undefined between {} and {}", kind, lhs_ty, rhs_ty),
                span
            )
        }
    }

    pub fn sema_unop_expr(&mut self, kind: UnOpKind, expr: Expression, span: Span) -> Expression {
        let expr = self.l2r_if_needed(expr);

        let ty = match (kind, expr.ty()) {
            (UnOpKind::Plus, Type::Int) => Type::Int,
            (UnOpKind::Plus, Type::UInt) => Type::UInt,
            (UnOpKind::Plus, Type::Double) => Type::Double,
            (UnOpKind::Minus, Type::Int) => Type::Int,
            (UnOpKind::Minus, Type::Double) => Type::Double,
            (UnOpKind::LogicalNot, Type::Bool) => Type::Bool,
            (UnOpKind::Deref, Type::Ptr(sub_ty)) => *sub_ty,
            (UnOpKind::AddressOf, ty) => Type::Ptr(Box::new(ty)),
            (kind, et) => {
                self.diag_engine.report_sema_error(
                    format!("{:?} is undefined for {}", kind, et),
                    span
                )
            }
        };

        Expression::UnOp {
            kind: kind,
            expression: Box::new(expr),
            ty: ty,
            span: span,
        }
    }

    pub fn sema_cast_expr(&mut self, expr: Expression, ty: Type, span: Span) -> Expression {
        let expr = self.l2r_if_needed(expr);
        let expr_ty = expr.ty();

        if expr_ty == ty {
            return expr;
        }

        match (expr_ty, ty.clone()) {
            (Type::Int, Type::UInt) |
            (Type::Int, Type::Double) |
            (Type::Int, Type::Bool) |
            (Type::Int, Type::Char) |
            (Type::UInt, Type::Int) |
            (Type::UInt, Type::Double) |
            (Type::UInt, Type::Bool) |
            (Type::UInt, Type::Char) |
            (Type::Double, Type::Int) |
            (Type::Double, Type::UInt) |
            (Type::Double, Type::Bool) |
            (Type::Double, Type::Char) => {},
            (src, target) => {
                self.diag_engine.report_sema_error(
                    format!("Cast between {} and {} is undefined", src, target),
                    span,
                )
            }
        }

        Expression::Cast {
            expression: Box::new(expr),
            ty: ty,
            span: span,
        }
    }

    pub fn sema_func_call(&mut self,
                          func_name: String,
                          args: Vec<Expression>,
                          span: Span) -> Expression {
        // TODO func_name must be from an expression

        let args: Vec<_> = args.into_iter().map(|param| {
            self.l2r_if_needed(param)
        }).collect();

        if let Some(ty) = self.symbol_table.get(&func_name).cloned() {
            if let Type::Function(params, ret) = ty {
                if params.len() != args.len() {
                    self.diag_engine.report_sema_error(
                        format!(
                            "Mismatching number of arguments (expected: {}, given: {})",
                            args.len(),
                            params.len()
                        ),
                        span
                    )
                }

                for (ref par, ref arg) in params.into_iter().zip(args.iter()) {
                    if *par != arg.ty() {
                        self.diag_engine.report_sema_error(
                            format!(
                                "Mismatching type in argument (expected: {}, given: {})",
                                par,
                                arg.ty()
                            ),
                            arg.span()
                        )
                    }
                }

                return Expression::FuncCall {
                    function_name: func_name,
                    arguments: args,
                    span: span,
                    ty: *ret,
                };
            } else {
                self.diag_engine.report_sema_error(
                    format!("{} is not callable (given: {})", func_name, ty),
                    span
                )
            }
        } else {
            self.diag_engine.report_sema_error(format!("{} is not defined", func_name), span)
        }
    }

    pub fn sema_id_type(&mut self, id: String, span: Span) -> Type {
        match &id[..] {
            "int" => Type::Int,
            "uint" => Type::UInt,
            "bool" => Type::Bool,
            "double" => Type::Double,
            "char" => Type::Char,
            other => {
                self.diag_engine
                    .report_sema_error(format!("\'{}\' isn't a type", other), span);
            }
        }
    }

    pub fn l2r_if_needed(&mut self, expr: Expression) -> Expression {
        let expr_ty = expr.ty();
        if let Type::LVal(sub_ty) = expr_ty {
            Expression::L2R {
                span: expr.span(),
                expression: Box::new(expr),
                ty: *sub_ty,
            }
        } else {
            expr
        }
    }
}

fn get_binop_ty(kind: BinOpKind, lhs_ty: &Type, rhs_ty: &Type) -> Option<Type> {
    enum Domain {
        Maths,
        Comp,
        Logical,
    }

    let domain = match kind {
        BinOpKind::Add |
        BinOpKind::Sub |
        BinOpKind::Mul |
        BinOpKind::Div |
        BinOpKind::Mod => Domain::Maths,
        BinOpKind::Less |
        BinOpKind::LessEq |
        BinOpKind::Greater |
        BinOpKind::GreaterEq |
        BinOpKind::Equal |
        BinOpKind::NotEqual => Domain::Comp,
        BinOpKind::LogicalOr |
        BinOpKind::LogicalAnd => Domain::Logical,
    };

    match (domain, lhs_ty, rhs_ty) {
        (Domain::Maths, &Type::Int, &Type::Int) => Some(Type::Int),
        (Domain::Maths, &Type::UInt, &Type::UInt) => Some(Type::UInt),
        (Domain::Maths, &Type::Double, &Type::Double) => Some(Type::Double),
        (Domain::Comp, &Type::Int, &Type::Int) => Some(Type::Bool),
        (Domain::Comp, &Type::UInt, &Type::UInt) => Some(Type::Bool),
        (Domain::Comp, &Type::Bool, &Type::Bool) => Some(Type::Bool),
        (Domain::Comp, &Type::Char, &Type::Char) => Some(Type::Bool),
        (Domain::Comp, &Type::Double, &Type::Double) => Some(Type::Bool),
        (Domain::Logical, &Type::Bool, &Type::Bool) => Some(Type::Bool),
        _ => None
    }
}
