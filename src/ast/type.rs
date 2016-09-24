enum Type {
    Unit,
    Int,
    UInt,
    Bool,
    Char,
    Double,
    Ref(Box<Type>),
}
