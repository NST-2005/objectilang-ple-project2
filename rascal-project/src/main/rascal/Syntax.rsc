module Syntax



lexical WhitespaceChar = [\ \t] ;
lexical Newline       = "\r\n" | "\n" | "\r" !>> [\n] ;
lexical LineComment    = @category="comment" "//" ![\n\r]* !>> ![\n\r] ;

layout Layout = (WhitespaceChar | LineComment)* !>> [\ \t] !>> "//" ;

keyword Reserved
  = "object" | "extends" | "val" | "var" | "method" | "main" | "new"
  | "override" | "if" | "then" | "else" | "for" | "and" | "or" | "neg"
  | "true" | "false" | "in"
  ;

lexical IdName     = ([a-zA-Z] [a-zA-Z0-9]*) \ Reserved ;
lexical Identifier = [a-zA-Z0-9] !<< IdName !>> [a-zA-Z0-9] ;

lexical IntLiteral     = @category="number" [0-9]+ !>> [0-9] ;
lexical BooleanLiteral = @category="keyword" "true" | @category="keyword" "false" ;
lexical CharLiteral    = "\'" [a-zA-Z0-9 ] "\'"
                        | "\'" "\\" "n" "\'"
                        | "\'" "\\" "t" "\'"
                        | "\'" "\\" "\\" "\'"
                        | "\'" "\\" "\'" "\'" ;

lexical StringLiteral  = @category="string" "\"" StringPart* "\"" ;
lexical StringPart     = StringChar | StringEscape | Interpolation ;
lexical StringChar     = ![\"$\\] ;
lexical StringEscape   = "\\" [\"\\$nt] ;
lexical Interpolation  = "${" Expression "}" | "$" Identifier ;


start syntax Program
  = program: Newline* DeclarationLine* antes MainMethod principal Newline* DeclarationLine* despues
  ;

syntax DeclarationLine = Declaration declaracion Newline* ;

syntax Declaration
  = declObject: ObjectDecl
  | declMethod: MethodAssign
  ;

syntax ObjectDecl
  = objectDecl: "object" Identifier TypeParams? ExtendsClause? ConstructorParams?
  ;

syntax TypeParams       = typeParams: "[" {TypeParam ","}+ "]" ;
syntax TypeParam        = typeParam: Variance? Identifier ;
syntax Variance         = covariant: "+" | contravariant: "-" ;
syntax ExtendsClause    = extendsClause: "extends" Identifier ;

syntax ConstructorParams = constructorParams: "(" {ConstructorParam ","}+ ")" ;
syntax ConstructorParam  = constructorParam: Modifier Identifier ":" TypeRef ;
syntax Modifier          = valMod: "val" | varMod: "var" ;

syntax MethodAssign
  = methodAssign: Identifier "." Identifier "=" "method" "(" {Parameter ","}* ")" ":" TypeRef Block?
  ;

syntax Parameter  = parameter: Identifier ":" TypeRef ;

syntax MainMethod = mainMethod: "method" "main" Block ;

syntax Block = block: "{" Newline* BlockStatements? contenido "}" ;

syntax Statement
  = stmtVar: VarDecl
  | stmtExp: Expression
  ;

syntax BlockStatements = {Statement StatementSeparator}+ instrucciones StatementSeparator? ;
syntax StatementSeparator = ";" Newline* | Newline+ ;

syntax VarDecl
  = varDeclExp:   Modifier Identifier TypeAnnotation? "=" Expression
  | varDeclBlock: Modifier Identifier TypeAnnotation? "=" Block
  ;

syntax TypeAnnotation = typeAnnotation: ":" TypeRef ;

syntax TypeRef  = typeRef: Identifier TypeArgs? ;
syntax TypeArgs = typeArgs: "[" {TypeRef ","}+ "]" ;


syntax ObjectArgs = objectArgs: "(" {Expression ","}* ")" ;
syntax ObjectBody = objectBody: "{" Newline* OverrideLine* reemplazos "}" ;

syntax OverrideLine = OverrideDecl reemplazo Newline* ;

syntax OverrideDecl
  = overrideBlock: "override" Identifier "(" {Parameter ","}* ")" ":" TypeRef "=" Block
  | overrideExp:   "override" Identifier "(" {Parameter ","}* ")" ":" TypeRef "=" Expression
  ;


syntax BaseExp
  = baseLit:      Literal
  | baseId:       Identifier
  | baseParen:    "(" Expression ")"
  | memberAccess: BaseExp "." Identifier
  | callExp:      BaseExp "(" {Expression ","}* ")"
  ;

syntax Literal
  = intLit:    IntLiteral
  | boolLit:   BooleanLiteral
  | charLit:   CharLiteral
  | stringLit: StringLiteral
  ;

syntax Expression
  = baseExp: BaseExp
  | newObj:  "new" TypeRef ObjectArgs? ObjectBody?

  > left arrowExp:    Expression "-\>" Expression
  > left fatArrowExp: Expression "=\>" Expression

  > negExp:   "neg" Expression
  | minusExp: "-" Expression

  > right powExp: Expression "**" Expression

  > left mulExp: Expression "*" Expression
  | left divExp: Expression "/" Expression
  | left modExp: Expression "%" Expression

  > left addExp: Expression "+" Expression
  | left subExp: Expression "-" Expression

  > left ltExp: Expression "\<"    Expression
  | left gtExp: Expression "\>"    Expression
  | left leExp: Expression "\<="   Expression
  | left geExp: Expression "\>="   Expression
  | left neExp: Expression "\<\>"  Expression
  | left eqExp: Expression "="     Expression

  > left andExp: Expression "and" Expression
  > left orExp:  Expression "or"  Expression

  > ifExp:       "if" Expression "then" Expression "else" Expression
  | forInExp:    "for" "(" Pattern "in" Expression ")" Expression
  | forArrowExp: "for" "(" Pattern "\<-" Expression ")" Expression
  ;

syntax Pattern
  = patternId:    Identifier
  | patternTuple: "(" {Identifier ","}+ ")"
  ;
