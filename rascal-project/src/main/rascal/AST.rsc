module AST
data Expression
        = integerLiteral(int number)
        | booleanLiteral(bool flag)
        | stringLiteral(str content)
        | interpolatedStringExp(InterpolatedString interpolatedText)
        | identifier(str name)
        | objectCreation(
            TypeRef objectType,
            list[Expression] arguments,
            list[OverrideDecl] overrides
        )
        | addExp(Expression left, Expression right)
        | subExp(Expression left, Expression right)
        | mulExp(Expression left, Expression right)
        | divExp(Expression left, Expression right)
        | modExp(Expression left, Expression right)
        | powExp(Expression left, Expression right)
        | ltExp(Expression left, Expression right)
        | gtExp(Expression left, Expression right)
        | leExp(Expression left, Expression right)
        | geExp(Expression left, Expression right)
        | eqExp(Expression left, Expression right)
        | neExp(Expression left, Expression right)
        | andExp(Expression left, Expression right)
        | orExp(Expression left, Expression right)
        | negExp(Expression operand)
        | minusExp(Expression operand)
        | ifExp(Expression condition, Expression thenBranch, Expression elseBranch)
        | memberAccess(Expression receiver, str memberName)
        | callExp(Expression function, list[Expression] arguments)
        | arrowExp(Expression left, Expression right)
        | fatArrowExp(Expression left, Expression right)

        | forInExp(Pattern pattern, Expression collection, Expression body)
        | forArrowExp(Pattern pattern, Expression collection, Expression body)
        ;

data Statement
            = stmtExp(Expression expr)
            | variableDecl(VarModifier modifier, str name, TypeRef declaredType, Expression initialValue)
            | variableDeclSinTipo(VarModifier modifier, str name, Expression initialValue)
            | variableDeclBloque(VarModifier modifier, str name, Block initialBlock)
            | variableDeclBloqueConTipo(VarModifier modifier, str name, TypeRef declaredType, Block initialBlock)
            ;

data Program = program(list[Declaration] beforeMain, MainMethod mainMethod, list[Declaration] afterMain);

data Declaration
    = declObject(ObjectDecl object)
    | declMethod(MethodAssign method)
    ;
data ObjectDecl
    = objectDecl(str name, list[TypeParam] typeParams, list[str] parents, list[ConstructorParam] constructorParams);

data TypeParam = typeParam(str name, Variance variance);

data Variance
    = covariant()
    | contravariant()
    | invariant()
    ;

data ConstructorParam = constructorParam(VarModifier modifier, str name, TypeRef declaredType);

data VarModifier
    = valMod()
    | varMod()
    ;

data TypeRef = typeRef(str name, list[TypeRef] typeArgs);

data MethodAssign
        = methodAssign(str objectName, str methodName, list[Parameter] parameters, TypeRef returnType, Block body)
        | methodAssignSinCuerpo(str objectName, str methodName, list[Parameter] parameters, TypeRef returnType)
        ;
data Parameter = parameter(str name, TypeRef paramType);

data MainMethod = mainMethod(Block body);

data Block = block(list[Statement] statements);

data Pattern
    = patternId(str name)
    | patternTuple(list[str] names)
    ;

data OverrideDecl
    = overrideBlock(str methodName, list[Parameter] parameters, TypeRef returnType, Block body)
    | overrideExp(str methodName, list[Parameter] parameters, TypeRef returnType, Expression expr)
    ;
data StringPart
    = stringText(str content)
    | stringInterpolation(Expression expr)
    ;

data InterpolatedString
    = interpolatedString(list[StringPart] parts)
    ;
