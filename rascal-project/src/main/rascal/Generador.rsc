module Generador

import AST;
import List;
import String;

str generarExpresion(integerLiteral(int numero)) = "<numero>";

str generarExpresion(addExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> + <generarExpresion(derecha)>)";

str generarExpresion(identifier(str nombre)) = nombre;
str generarExpresion(booleanLiteral(bool valor)) = "<valor>";
str generarExpresion(charLiteral(str valor)) =
    "\"" + escape(valor, ("\\" : "\\\\", "\"" : "\\\"", "\n" : "\\n",
        "\r" : "\\r", "\t" : "\\t")) + "\"";

str generarExpresion(ltExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> \< <generarExpresion(derecha)>)";    

str generarExpresion(stringLiteral(str texto))= "\"" + escape( texto,( 
    "\\" : "\\\\",
    "\"" : "\\\"",
    "\'" : "\\\'", 
    "\<" : "\\\<", 
    "\>" : "\\\>", 
    "\n" : "\\n", 
    "\r" : "\\r", 
    "\t" : "\\t"  
    )) + "\"";

str generarExpresion(subExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> - <generarExpresion(derecha)>)";

str generarExpresion(mulExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> * <generarExpresion(derecha)>)";
str generarExpresion(eqExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> == <generarExpresion(derecha)>)";

str generarExpresion(neExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> != <generarExpresion(derecha)>)";

str generarExpresion(andExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> && <generarExpresion(derecha)>)";

str generarExpresion(orExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> || <generarExpresion(derecha)>)";

str generarExpresion(negExp(Expression operando)) = "(!<generarExpresion(operando)>)";
str generarExpresion(
        ifExp(Expression condicion, Expression ramaVerdadera, Expression ramaFalsa))
        = "(<generarExpresion(condicion)> ? <generarExpresion(ramaVerdadera)> : <generarExpresion(ramaFalsa)>)";

str generarExpresion(divExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> / <generarExpresion(derecha)>)";

str generarExpresion(modExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> % <generarExpresion(derecha)>)";

str generarExpresion(minusExp(Expression operando)) = "(-<generarExpresion(operando)>)";
str generarDeclaracion(variableDeclSinTipo(_, str nombre, Expression valor))
            = "<nombre> = <generarExpresion(valor)>;";
str generarExpresion(gtExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> \> <generarExpresion(derecha)>)";

str generarExpresion(leExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> \<= <generarExpresion(derecha)>)";

str generarExpresion(geExp(Expression izquierda, Expression derecha))
    = "(<generarExpresion(izquierda)> \>= <generarExpresion(derecha)>)";

str generarParte(stringText(str texto)) = generarExpresion(stringLiteral(texto));

str generarParte(stringInterpolation(Expression expresion))
    = "\"\<" + generarExpresion(expresion) + "\>\"";

str generarExpresion(interpolatedStringExp(interpolatedString(list[StringPart] partes)))
    = "(" + ( "\"\"" | it + " + " + generarParte(parte) | StringPart parte <- partes ) + ")";
str generarTipo(typeRef("Int", [])) = "int";
str generarTipo(typeRef("Bool", [])) = "bool";
str generarTipo(typeRef("String", [])) = "str";
str generarTipo(typeRef("Char", [])) = "str";
str generarTipo(typeRef(str nombre, list[TypeRef] argumentos)) {
    if (size(argumentos) == 0) {
        return nombre;
    }
    list[str] tipos = [generarTipo(argumento) | TypeRef argumento <- argumentos];
    str tiposFormateados = List::intercalate(", ", tipos);
    return "<nombre><tiposFormateados>";
}

str generarDeclaracion(variableDecl(_, str nombre, TypeRef _, Expression valor))
    = "<nombre> = <generarExpresion(valor)>;";
str generarInstruccion(stmtExp(Expression expresion)) =
    generarExpresion(expresion) + ";";

default str generarInstruccion(Statement declaracion) =
    generarDeclaracion(declaracion);

str generarBloqueValor(block([*Statement instrucciones, stmtExp(Expression resultado)])) {
    str cuerpo = "";
    for (Statement instruccion <- instrucciones) {
        cuerpo += generarInstruccion(instruccion) + " ";
    }
    return "({ " + cuerpo + generarExpresion(resultado) + "; })";
}

str generarDeclaracion(variableDeclBloque(_, str nombre, Block bloque)) =
    "<nombre> = <generarBloqueValor(bloque)>;";

str generarDeclaracion(variableDeclBloqueConTipo(_, str nombre, TypeRef _, Block bloque)) =
    "<nombre> = <generarBloqueValor(bloque)>;";
str generarExpresion(powExp(Expression base, Expression exponente))
        = "objlPotencia(<generarExpresion(base)>, <generarExpresion(exponente)>)";
str generarCampo(constructorParam(_, str nombre, TypeRef tipo)) = "<generarTipo(tipo)> <nombre>";
str generarCampoGenerico(constructorParam(_, str nombre, TypeRef _)) = "value <nombre>";

list[ConstructorParam] camposDeObjeto(
    str nombre, map[str, ObjectDecl] objetos, set[str] visitados) {
    if (nombre in visitados) {
        throw "Herencia circular en <nombre>.";
    }
    if (nombre notin objetos) {
        throw "Objeto padre desconocido: <nombre>.";
    }
    ObjectDecl objeto = objetos[nombre];
    list[ConstructorParam] campos = [];
    for (str padre <- objeto.parents) {
        campos += camposDeObjeto(padre, objetos, visitados + { nombre });
    }
    return campos + objeto.constructorParams;
}

str generarObjeto(
    objectDecl(str nombre, list[TypeParam] parametrosTipo, _, _),
    map[str, ObjectDecl] objetos) {
    list[ConstructorParam] campos = camposDeObjeto(nombre, objetos, {});
    list[str] camposGenerados = [generarCampo(campo) | ConstructorParam campo <- campos];
    if (size(parametrosTipo) > 0) {
        camposGenerados = [generarCampoGenerico(campo) | ConstructorParam campo <- campos];
    }
    str parametros = List::intercalate(", ", camposGenerados);
    return "data <nombre> = objlNuevo<nombre>(<parametros>);";
}

str generarExpresion(objectCreation(typeRef(str nombre, _), list[Expression] argumentos, [])) {
    str valores = List::intercalate(", ", [generarExpresion(arg) | Expression arg <- argumentos]);
    return "objlNuevo<nombre>(<valores>)";
}

str generarExpresion(memberAccess(Expression receptor, str miembro))
    = "(<generarExpresion(receptor)>).<miembro>";
str generarParametro(parameter(str nombre, TypeRef tipo)) = "<generarTipo(tipo)> <nombre>";

str generarMetodoEnObjeto(
    methodAssign(_, str nombre, list[Parameter] parametros, TypeRef retorno, Block cuerpo),
    str objeto) {
    list[str] formales = ["<objeto> este"] +
        [generarParametro(parametro) | Parameter parametro <- parametros];
    str firma = List::intercalate(", ", formales);
    return "<generarTipo(retorno)> objlMetodo<nombre>(<firma>) = <generarBloqueValor(cuerpo)>;";
}

tuple[str, list[TypeRef]] firmaMetodo(MethodAssign metodo) =
    <metodo.methodName, [parametro.paramType | Parameter parametro <- metodo.parameters]>;

list[MethodAssign] metodosDeObjeto(str nombre, map[str, ObjectDecl] objetos, list[MethodAssign] metodos) {
    list[MethodAssign] propios = [metodo | MethodAssign metodo <- metodos, metodo.objectName == nombre];
    set[tuple[str, list[TypeRef]]] firmasPropias = {firmaMetodo(metodo) | MethodAssign metodo <- propios};
    list[MethodAssign] heredados = [
        metodo | str padre <- objetos[nombre].parents,
        MethodAssign metodo <- metodosDeObjeto(padre, objetos, metodos),
        firmaMetodo(metodo) notin firmasPropias
    ];
    return propios + heredados;
}

str generarExpresion(
    callExp(memberAccess(Expression receptor, str nombre), list[Expression] argumentos)) {
    list[str] valores = [generarExpresion(receptor)] + [generarExpresion(arg) | Expression arg <- argumentos];
    return "objlMetodo<nombre>(<List::intercalate(", ", valores)>)";
}

str generarArgumentos(list[Expression] argumentos) =
    List::intercalate(", ", [generarExpresion(arg) | Expression arg <- argumentos]);

str generarExpresion(callExp(identifier("print"), list[Expression] argumentos)) =
    "print(<generarArgumentos(argumentos)>)";

str generarExpresion(callExp(identifier("println"), list[Expression] argumentos)) =
    "println(<generarArgumentos(argumentos)>)";
