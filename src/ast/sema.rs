use std::collections::HashMap;

use ast::{Type, Function, Param, Block, Statement, Expression, AssignOpKind, BinOpKind,
          UnOpKind};
use source::Span;
use error::{CodeError, SemaErrorKind, SemaError};

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
pub struct Sema {
    pub symbol_table: SymbolTable,

    pub current_ret_ty: Type,
    pub loop_level: usize,
}

impl Sema {
    pub fn new() -> Self {
        Sema {
            symbol_table: SymbolTable::new(),
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
                                   -> Result<FunctionInfo, CodeError> {
        if let Some(_) = self.symbol_table.insert(name.clone(),
                                                       Type::function_type(params.clone(),
                                                                           ret_ty.clone())) {
            return new_sema_error(SemaErrorKind::FunctionAlreadyDefined(name), name_span);
        }

        self.symbol_table.begin_scope();

        for param in params.clone() {
            if let Some(_) = self.symbol_table.insert(param.name.clone(), param.ty) {
                return new_sema_error(
                    SemaErrorKind::ParameterAlreadyDefined(param.name),
                    param.span
                )
            }
        }

        self.current_ret_ty = ret_ty.clone();

        Ok(FunctionInfo {
            name: name,
            parameters: params,
            return_ty: ret_ty,
        })
    }

    pub fn sema_function_def_end(&mut self,
                                 func_info: FunctionInfo,
                                 block: Block,
                                 span: Span)
                                 -> Result<Function, CodeError> {
        self.symbol_table.end_scope();
        self.current_ret_ty = Type::Unit;

        Ok(Function {
            name: func_info.name,
            parameters: func_info.parameters,
            return_ty: func_info.return_ty,
            block: block,
            span: span,
        })
    }

    pub fn sema_let_stmt(&mut self,
                         id: String,
                         ty: Option<Type>,
                         expr: Expression,
                         span: Span)
                         -> Result<Statement, CodeError> {
        let expr = l2r_if_needed(expr);
        let expr_ty = expr.ty();
        let ty = ty.unwrap_or(expr_ty.clone());

        if ty != expr_ty {
            return new_sema_error(SemaErrorKind::LetMismatchingTypes(ty, expr_ty), span);
        }

        if let Some(_) = self.symbol_table.insert(id.clone(), ty.clone()) {
            return new_sema_error(SemaErrorKind::LetAlreadyDefined(id.clone()), span);
        }

        Ok(Statement::Let {
            identifier: id,
            ty: ty,
            expression: expr,
            span: span,
        })
    }

    pub fn sema_if_stmt(&mut self,
                        condition: Expression,
                        if_stmt: Statement,
                        else_stmt: Option<Statement>,
                        span: Span)
                        -> Result<Statement, CodeError> {
        let condition = l2r_if_needed(condition);
        let cond_ty = condition.ty();
        if cond_ty != Type::Bool {
            return new_sema_error(SemaErrorKind::IfConditionNotBool(cond_ty), span);
        }

        Ok(Statement::If {
            condition: condition,
            if_statement: Box::new(if_stmt),
            else_statement: else_stmt.map(Box::new),
            span: span,
        })
    }

    pub fn sema_while_stmt(&mut self,
                           condition: Expression,
                           stmt: Statement,
                           span: Span)
                           -> Result<Statement, CodeError> {
        let condition = l2r_if_needed(condition);
        let cond_ty = condition.ty();
        if cond_ty != Type::Bool {
            return new_sema_error(SemaErrorKind::WhileConditionNotBool(cond_ty), span);
        }

        Ok(Statement::While {
            condition: condition,
            statement: Box::new(stmt),
            span: span,
        })
    }

    pub fn sema_break_stmt(&mut self, span: Span) -> Result<Statement, CodeError> {
        if self.loop_level == 0 {
            return new_sema_error(SemaErrorKind::BreakOutsideLoop, span);
        }

        Ok(Statement::Break {
            span: span
        })
    }

    pub fn sema_continue_stmt(&mut self, span: Span) -> Result<Statement, CodeError> {
        if self.loop_level == 0 {
            return new_sema_error(SemaErrorKind::ContinueOutsideLoop, span);
        }

        Ok(Statement::Continue {
            span: span
        })
    }

    pub fn sema_return_stmt(&mut self, expr: Expression, span: Span) -> Result<Statement, CodeError> {
        let expr = l2r_if_needed(expr);
        let expr_ty = expr.ty();
        if expr_ty != self.current_ret_ty {
            return new_sema_error(
                SemaErrorKind::ReturnMismatchingTypes(
                    self.current_ret_ty.clone(), expr_ty
                ),
                span
            )
        }

        Ok(Statement::Return {
            expression: expr,
            span: span
        })
    }

    pub fn sema_identifier(&mut self, id: String, span: Span) -> Result<Expression, CodeError> {
        if let Some(ty) = self.symbol_table.get(&id) {
            Ok(Expression::Identifier {
                identifier: id,
                span: span,
                ty: Type::LVal(Box::new(ty.clone())),
            })
        } else {
            new_sema_error(SemaErrorKind::UndefinedIdentifier(id), span)
        }
    }

    pub fn sema_assignop_expr(&mut self,
                              kind: AssignOpKind,
                              lhs: Expression,
                              rhs: Expression,
                              span: Span) -> Result<Expression, CodeError> {
        let rhs = l2r_if_needed(rhs);
        let rhs_ty = rhs.ty();

        let res_ty = if let Type::LVal(lhs_ty) = lhs.ty() {
            if let Some(binop_kind) = kind.to_binop() {
                if let Some(res_ty) = get_binop_ty(binop_kind, &lhs_ty, &rhs_ty) {
                    res_ty
                } else {
                    return new_sema_error(
                        SemaErrorKind::BinOpUndefined(
                            binop_kind,
                            *lhs_ty,
                            rhs_ty
                        ),
                        span
                    )
                }
            } else if *lhs_ty != rhs_ty {
                return new_sema_error(SemaErrorKind::AssignMismatchingTypes(*lhs_ty, rhs_ty), span);
            } else {
                *lhs_ty
            }
        } else {
            return new_sema_error(SemaErrorKind::AssignLHSNotRef, span);
        };

        Ok(Expression::AssignOp {
            kind: kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: span,
            ty: res_ty,
        })
    }

    pub fn sema_binop_expr(&mut self,
                           kind: BinOpKind,
                           lhs: Expression,
                           rhs: Expression,
                           span: Span) -> Result<Expression, CodeError> {
        let lhs = l2r_if_needed(lhs);
        let rhs = l2r_if_needed(rhs);

        let lhs_ty = lhs.ty();
        let rhs_ty = rhs.ty();

        if let Some(ty) = get_binop_ty(kind, &lhs_ty, &rhs_ty) {
            Ok(Expression::BinOp {
                kind: kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: span,
                ty: ty,
            })
        } else {
            new_sema_error(SemaErrorKind::BinOpUndefined(kind, lhs_ty, rhs_ty), span)
        }
    }

    pub fn sema_unop_expr(&mut self, kind: UnOpKind, expr: Expression, span: Span)
        -> Result<Expression, CodeError> {
        let expr = l2r_if_needed(expr);

        let ty = match (kind, expr.ty()) {
            (UnOpKind::Plus, Type::Int) => Type::Int,
            (UnOpKind::Plus, Type::UInt) => Type::UInt,
            (UnOpKind::Plus, Type::Double) => Type::Double,
            (UnOpKind::Minus, Type::Int) => Type::Int,
            (UnOpKind::Minus, Type::Double) => Type::Double,
            (UnOpKind::LogicalNot, Type::Bool) => Type::Bool,
            (UnOpKind::Deref, Type::Ptr(sub_ty)) => *sub_ty,
            (UnOpKind::AddressOf, ty) => Type::Ptr(Box::new(ty)),
            (kind, et) => return new_sema_error(SemaErrorKind::UnOpUndefined(kind, et), span),
        };

        Ok(Expression::UnOp {
            kind: kind,
            expression: Box::new(expr),
            ty: ty,
            span: span,
        })
    }

    pub fn sema_cast_expr(&mut self, expr: Expression, ty: Type, span: Span)
        -> Result<Expression, CodeError> {
        let expr = l2r_if_needed(expr);
        let expr_ty = expr.ty();

        if expr_ty == ty {
            return Ok(expr);
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
            (src, trgt) => return new_sema_error(SemaErrorKind::CastUndefined(src, trgt), span)
        }

        Ok(Expression::Cast {
            expression: Box::new(expr),
            ty: ty,
            span: span,
        })
    }

    pub fn sema_func_call(&mut self,
                          func_name: String,
                          args: Vec<Expression>,
                          span: Span) -> Result<Expression, CodeError> {
        // TODO func_name must be from an expression

        let args: Vec<_> = args.into_iter().map(|param| {
            l2r_if_needed(param)
        }).collect();

        if let Some(ty) = self.symbol_table.get(&func_name).cloned() {
            if let Type::Function(params, ret) = ty {
                if params.len() != args.len() {
                    return new_sema_error(
                        SemaErrorKind::FuncCallMismatchingLen(args.len(), params.len()), span
                    );
                }

                for (ref par, ref arg) in params.into_iter().zip(args.iter()) {
                    if *par != arg.ty() {
                        return new_sema_error(
                            SemaErrorKind::FuncCallMismatchingTypes(par.clone(), arg.ty()), span
                        );
                    }
                }

                return Ok(Expression::FuncCall {
                    function_name: func_name,
                    arguments: args,
                    span: span,
                    ty: *ret,
                });
            } else {
                return new_sema_error(SemaErrorKind::FuncCallNotCallable(func_name, ty), span);
            }
        } else {
            return new_sema_error(SemaErrorKind::UndefinedIdentifier(func_name), span);
        }
    }

    pub fn sema_id_type(&mut self, id: String, span: Span) -> Result<Type, CodeError> {
        match &id[..] {
            "int" => Ok(Type::Int),
            "uint" => Ok(Type::UInt),
            "bool" => Ok(Type::Bool),
            "double" => Ok(Type::Double),
            "char" => Ok(Type::Char),
            other => new_sema_error(SemaErrorKind::TypeUndefined(other.to_string()), span)
        }
    }
}

// utils
fn l2r_if_needed(expr: Expression) -> Expression {
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

fn new_sema_error<T>(kind: SemaErrorKind, span: Span) -> Result<T, CodeError> {
    Err(CodeError::SemaError(SemaError(kind, span)))
}
