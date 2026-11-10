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
str generarTipo(TypeRef tipo) = generarTipo(tipo, {});

str generarTipo(TypeRef tipo, set[str] variables) =
    generarTipo(tipo, variables, "");

str generarTipo(TypeRef tipo, set[str] variables, str prefijo) {
    if (tipo.typeArgs != []) {
        str argumentos = List::intercalate(", ",
            [generarTipo(arg, variables, prefijo) | TypeRef arg <- tipo.typeArgs]);
        return "<tipo.name>[<argumentos>]";
    }
    if (tipo.name in variables) {
        return "&<prefijo><tipo.name>";
    }
    switch(tipo.name) {
        case "Int":
            return "int";
        case "Bool":
            return "bool";
        case "Char":
            return "str";
        case "String":
            return "str";
        default:
            return tipo.name;
    }
}
str generarDeclaracion(variableDecl(_, str nombre, TypeRef tipo, Expression valor))
    = "<generarTipo(tipo)> <nombre> = <generarExpresion(valor)>;";

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
str generarCampo(constructorParam(_, str nombre, TypeRef tipo), set[str] variables)
        = "<generarTipo(tipo, variables)> <nombre>";
str generarObjeto(
    objectDecl(str nombre, list[TypeParam] tipos, _, _), map[str, ObjectDecl] objetos, map[str, str] familias) {
    set[str] variables = {parametro.name| TypeParam parametro <- tipos};
    str tipo
        = generarTipo(
              typeRef(nombre, [typeRef(parametro.name, []) | TypeParam parametro <- tipos]), variables
          );
    list[ConstructorParam] campos = camposDeObjeto(nombre, objetos, {});
    list[str] formales = [generarCampo(campo, variables) | ConstructorParam campo <- campos];
    str constructor = "objlNuevo<nombre>(<List::intercalate(", ", formales)>)";
    if (nombre in familias) {
        return "data <familias[nombre]> = <constructor>;";
    }
    if (tipos != []) {
        // Un override puede usar parámetros genéricos que no aparecen en ningún campo.
        list[str] tiposExplicitos = [
            "type[&<tipos[indice].name>] _objlTipo<indice>" | int indice <- [0..size(tipos)]
        ];
        constructor += " | objlNuevo<nombre>(<List::intercalate(", ", formales + tiposExplicitos)>)";
    }
    return "data <tipo>(list[tuple[str, value]] _objlOverrides = []) = <constructor>;";
}

str generarExpresion(
    objectCreation(TypeRef tipo, list[Expression] argumentos, list[OverrideDecl] reemplazos)) {
    list[str] valores = [generarExpresion(arg) | Expression arg <- argumentos];
    if (reemplazos != []) {
        valores += ["#<generarTipo(arg)>" | TypeRef arg <- tipo.typeArgs];
        str entradas
            = List::intercalate(
                  ", ", [generarReemplazo(reemplazo, tipo) | OverrideDecl reemplazo <- reemplazos]
              );
        valores += ["_objlOverrides=[<entradas>]"];
    }
    str constructor = "objlNuevo<tipo.name>(<List::intercalate(", ", valores)>)";
    if (tipo.typeArgs == []) {
        return constructor;
    }
    return "({ <generarTipo(tipo)> objlInstancia = <constructor>; objlInstancia; })";
}

str generarReemplazo(OverrideDecl reemplazo, TypeRef receptor) {
    list[str] formales
        = ["<generarTipo(receptor)> este"] +
          [generarParametro(parametro, {}) | Parameter parametro <- reemplazo.parameters];
    str nombre = generarExpresion(stringLiteral(reemplazo.methodName));
    str retorno = generarTipo(reemplazo.returnType);
    str firma = List::intercalate(", ", formales);
    return "\<<nombre>, (<retorno> (<firma>) { return <generarValorReemplazo(reemplazo)>; })\>";
}

str generarValorReemplazo(overrideExp(_, _, _, Expression expresion))
    = generarExpresion(expresion);

str generarValorReemplazo(overrideBlock(_, _, _, Block bloque)) = generarBloqueValor(bloque);
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

private str raizNoGenerica(str nombre, map[str, ObjectDecl] objetos, set[str] visitados) {
    if (nombre notin objetos || nombre in visitados || objetos[nombre].typeParams != []) {
        return "";
    }
    if ([str padre] := objetos[nombre].parents) {
        return raizNoGenerica(padre, objetos, visitados + {nombre});
    }
    return nombre;
}

private bool camposCompatibles(str raiz, map[str, str] raices, map[str, ObjectDecl] objetos) {
    map[str, TypeRef] tipos = ();
    for (str nombre <- raices, raices[nombre] == raiz,
         constructorParam(_, str campo, TypeRef tipo) <- objetos[nombre].constructorParams) {
        if (campo in tipos && tipos[campo] != tipo) {
            return false;
        }
        tipos[campo] = tipo;
    }
    return true;
}

// Los constructores conservan la clase concreta aunque una familia comparta el tipo Rascal.
map[str, str] familiasVariantes(Program programa, map[str, ObjectDecl] objetos) {
    map[str, str] raices = (nombre: raizNoGenerica(nombre, objetos, {}) | str nombre <- objetos);
    set[str] usadas = {};
    visit (programa) {
        case variableDecl(_, _, typeRef(str nombre, []), ifExp(_, _, _)): {
            // El valor elegido por el condicional puede ser de una clase derivada.
            if (nombre in raices && raices[nombre] != "") {
                usadas += {raices[nombre]};
            }
        }
        case OverrideDecl reemplazo: {
            // La función de reemplazo debe compartir los tipos Rascal de la firma original.
            list[TypeRef] firma = [reemplazo.returnType] +
                [parametro.paramType | Parameter parametro <- reemplazo.parameters];
            visit (firma) {
                case typeRef(str nombre, []):
                    if (nombre in raices && raices[nombre] != "") {
                        usadas += {raices[nombre]};
                    }
            }
        }
        case typeRef(str nombre, list[TypeRef] argumentos): {
            if (nombre in objetos) {
                list[TypeParam] parametros = objetos[nombre].typeParams;
                for (int indice <- [0..size(argumentos)],
                     indice < size(parametros),
                     parametros[indice].variance != invariant(),
                     typeRef(str argumento, []) := argumentos[indice],
                     argumento in raices, raices[argumento] != "") {
                    usadas += {raices[argumento]};
                }
            }
        }
    }
    set[str] compatibles = {raiz | str raiz <- usadas, camposCompatibles(raiz, raices, objetos)};
    return (nombre: raices[nombre] | str nombre <- raices, raices[nombre] in compatibles);
}

str generarParametro(parameter(str nombre, TypeRef tipo), set[str] variables) =
    "<generarTipo(tipo, variables, "_objl_")> <nombre>";
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

str generarParametro(parameter(str nombre, TypeRef tipo)) = "<generarTipo(tipo)> <nombre>";

str generarMetodoEnObjeto(
    MethodAssign metodo, ObjectDecl objeto, map[str, ObjectDecl] objetos, map[str, str] familias) {
    set[str] variables = {parametro.name| TypeParam parametro <- objeto.typeParams};
    TypeRef receptor = typeRef(objeto.name, [typeRef(parametro.name, []) | TypeParam parametro <- objeto.typeParams]);
    str tipoReceptor = generarTipo(receptor, variables, "_objl_");
    str retorno = generarTipo(metodo.returnType, variables, "_objl_");
    str formalReceptor = "<tipoReceptor> este";
    if (objeto.name in familias) {
        str campos = List::intercalate(", ", ["_" | _ <- camposDeObjeto(objeto.name, objetos, {})]);
        formalReceptor += ":objlNuevo<objeto.name>(<campos>)";
    }
    list[str] formales
        = [formalReceptor] +
          [generarParametro(parametro, variables) | Parameter parametro <- metodo.parameters];
    list[str] tipos
        = [tipoReceptor] + [generarTipo(p.paramType, variables, "_objl_") | Parameter p <- metodo.parameters];
    list[str] argumentos = ["este"] + [p.name | Parameter p <- metodo.parameters];

    str nombre = generarExpresion(stringLiteral(metodo.methodName));
    str firma = List::intercalate(", ", formales);
    str firmaReemplazo = List::intercalate(", ", tipos);
    str llamada = List::intercalate(", ", argumentos);
    return
        "<retorno> objlMetodo<metodo.methodName>(<firma>) {
        '    for (\<<nombre>, <retorno>(<firmaReemplazo>) _objlReemplazo\> \<- este._objlOverrides) {
        '        return _objlReemplazo(<llamada>);
        '    }
        '    <generarRetornoMetodo(metodo)>
        '}";
}

str generarRetornoMetodo(methodAssign(_, _, _, _, Block cuerpo))
    = "return <generarBloqueValor(cuerpo)>;";

str generarRetornoMetodo(methodAssignSinCuerpo(_, str nombre, _, _))
    = "throw <generarExpresion(stringLiteral("El método <nombre> requiere un override."))>;";

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
str generarExpresion(callExp(identifier("print"), [Expression argumento])) =
    "print(<generarExpresion(argumento)>)";

str generarExpresion(callExp(identifier("println"), [Expression argumento])) =
    "println(<generarExpresion(argumento)>)";

str generarExpresion(callExp(identifier("range"), [Expression inicio, Expression fin])) =
    "[<generarExpresion(inicio)>..<generarExpresion(fin)>]";

str generarExpresion(forInExp(Pattern patron, Expression coleccion, Expression cuerpo)) =
    generarFor(patron, coleccion, cuerpo);

str generarExpresion(forArrowExp(Pattern patron, Expression coleccion, Expression cuerpo)) =
    generarFor(patron, coleccion, cuerpo);

str generarFor(Pattern patron, Expression coleccion, Expression cuerpo) =
    "(void () { for (<generarPatron(patron)> \<- <generarExpresion(coleccion)>) { <generarExpresion(cuerpo)>; } })()";

str generarPatron(patternId(str nombre)) = nombre;

str generarPatron(patternTuple(list[str] nombres)) =
    "\<<List::intercalate(", ", nombres)>\>";

str generarExpresion(
    memberAccess(memberAccess(Expression receptor, "getClass"), "getSimpleName"))
    = "String::substring(Node::getName(<generarExpresion(receptor)>), String::size(\"objlNuevo\"))";

str generarExpresion(memberAccess(Expression receptor, str miembro))
    = "(<generarExpresion(receptor)>).<miembro>";

str generarArgumentos(list[Expression] argumentos) =
    List::intercalate(", ", [generarExpresion(arg) | Expression arg <- argumentos]);

str generarExpresion(callExp(identifier("print"), list[Expression] argumentos)) =
    "print(<generarArgumentos(argumentos)>)";

str generarExpresion(callExp(identifier("println"), list[Expression] argumentos)) =
    "println(<generarArgumentos(argumentos)>)";
