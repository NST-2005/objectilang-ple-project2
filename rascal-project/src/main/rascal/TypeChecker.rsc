module TypeChecker

import AST;
import List;

private TypeRef tipo(str nombre) = typeRef(nombre, []);
private TypeRef unit() = tipo("Unit");
private TypeRef rango() = tipo("$Range");
private bool primitivo(str nombre) = nombre in {"Int", "Bool", "Char", "String", "Unit"};

private void exigir(bool condicion, str mensaje) {
    if (!condicion) {
        throw "Error de tipos: " + mensaje;
    }
}

private bool iguales(TypeRef izquierdo, TypeRef derecho) {
    if (typeRef(str nombreIzquierdo, list[TypeRef] argsIzquierdos) := izquierdo
        && typeRef(str nombreDerecho, list[TypeRef] argsDerechos) := derecho) {
        if (nombreIzquierdo != nombreDerecho
            || size(argsIzquierdos) != size(argsDerechos)) {
            return false;
        }
        for (int indice <- [0..size(argsIzquierdos)]) {
            if (!iguales(argsIzquierdos[indice], argsDerechos[indice])) {
                return false;
            }
        }
        return true;
    }
    return false;
}

private ObjectDecl objeto(TypeRef referencia, map[str, ObjectDecl] objetos) {
    if (typeRef(str nombre, _) := referencia && nombre in objetos) {
        return objetos[nombre];
    }
    throw "se esperaba un tipo de objeto.";
}

private bool hereda(
    str sub,
    str sup,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    if (sub == sup) {
        return true;
    }
    if (sub notin objetos || sub in visitados) {
        return false;
    }
    for (str padre <- objetos[sub].parents) {
        if (hereda(padre, sup, objetos, visitados + {sub})) {
            return true;
        }
    }
    return false;
}

public bool esSubtipo(
    TypeRef sub,
    TypeRef sup,
    map[str, ObjectDecl] objetos
) {
    if (iguales(sub, sup)) {
        return true;
    }
    if (typeRef(str nombreSub, list[TypeRef] argsSub) := sub
        && typeRef(str nombreSup, list[TypeRef] argsSup) := sup) {
        if (nombreSub == nombreSup && nombreSub in objetos) {
            ObjectDecl declarado = objetos[nombreSub];
            if (size(argsSub) != size(argsSup)
                || size(argsSub) != size(declarado.typeParams)) {
                return false;
            }
            for (int indice <- [0..size(argsSub)]) {
                Variance varianza = declarado.typeParams[indice].variance;
                if (covariant() := varianza
                    && !esSubtipo(argsSub[indice], argsSup[indice], objetos)) {
                    return false;
                }
                if (contravariant() := varianza
                    && !esSubtipo(argsSup[indice], argsSub[indice], objetos)) {
                    return false;
                }
                if (invariant() := varianza
                    && !iguales(argsSub[indice], argsSup[indice])) {
                    return false;
                }
            }
            return true;
        }
        return hereda(nombreSub, nombreSup, objetos, {});
    }
    return false;
}

private void validarTipo(
    TypeRef referencia,
    map[str, ObjectDecl] objetos,
    set[str] genericos,
    bool permitirInferencia
) {
    if (typeRef(str nombre, list[TypeRef] argumentos) := referencia) {
        exigir(primitivo(nombre) || nombre in objetos || nombre in genericos,
            "tipo desconocido.");
        if (nombre in objetos) {
            int requeridos = size(objetos[nombre].typeParams);
            exigir(permitirInferencia || size(argumentos) == requeridos,
                "cantidad incorrecta de argumentos genéricos.");
            exigir(size(argumentos) == 0 || size(argumentos) == requeridos,
                "cantidad incorrecta de argumentos genéricos.");
        }
        else {
            exigir(size(argumentos) == 0,
                "un parámetro de tipo o tipo primitivo no acepta argumentos.");
        }
        for (TypeRef argumento <- argumentos) {
            validarTipo(argumento, objetos, genericos, false);
        }
    }
}

private TypeRef sustituir(TypeRef referencia, map[str, TypeRef] sustituciones) {
    if (typeRef(str nombre, list[TypeRef] argumentos) := referencia) {
        if (nombre in sustituciones && size(argumentos) == 0) {
            return sustituciones[nombre];
        }
        return typeRef(nombre, [sustituir(argumento, sustituciones)
            | TypeRef argumento <- argumentos]);
    }
    return referencia;
}

private map[str, TypeRef] argumentosDeTipo(TypeRef referencia, ObjectDecl declarado) =
    (declarado.typeParams[indice].name: referencia.typeArgs[indice]
        | int indice <- [0..size(referencia.typeArgs)]);

private map[str, TypeRef] argumentosDelMetodo(
    TypeRef receptor, MethodAssign declarado, map[str, ObjectDecl] objetos
) = receptor.name == declarado.objectName ? argumentosDeTipo(receptor, objetos[receptor.name]) : ();

private map[str, TypeRef] unificar(
    TypeRef formal,
    TypeRef actual,
    set[str] genericos
) {
    map[str, TypeRef] resultado = ();
    if (typeRef(str nombre, list[TypeRef] argumentos) := formal) {
        if (nombre in genericos && size(argumentos) == 0) {
            resultado[nombre] = actual;
            return resultado;
        }
        if (typeRef(str nombreActual, list[TypeRef] argsActual) := actual
            && nombre == nombreActual
            && size(argumentos) == size(argsActual)) {
            for (int indice <- [0..size(argumentos)]) {
                map[str, TypeRef] encontrados = unificar(argumentos[indice], argsActual[indice],
                    genericos);
                for (str nombreEncontrado <- encontrados) {
                    resultado[nombreEncontrado] = encontrados[nombreEncontrado];
                }
            }
        }
    }
    return resultado;
}

private list[ConstructorParam] parametrosConstructor(
    str nombre,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    exigir(nombre in objetos, "objeto referenciado inexistente.");
    exigir(nombre notin visitados, "ciclo de herencia.");
    list[ConstructorParam] resultado = [];
    for (str padre <- objetos[nombre].parents) {
        resultado += parametrosConstructor(padre, objetos, visitados + {nombre});
    }
    return resultado + objetos[nombre].constructorParams;
}

private void validarHerencia(
    str nombre,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    exigir(nombre in objetos, "el objeto referenciado no existe.");
    exigir(nombre notin visitados, "la herencia de objetos contiene un ciclo.");
    for (str padre <- objetos[nombre].parents) {
        validarHerencia(padre, objetos, visitados + {nombre});
    }
}

private TypeRef campo(
    TypeRef receptor,
    str nombre,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    ObjectDecl declarado = objeto(receptor, objetos);
    map[str, TypeRef] sustituciones = argumentosDeTipo(receptor, declarado);
    for (ConstructorParam parametro <- declarado.constructorParams) {
        if (parametro.name == nombre) {
            return sustituir(parametro.declaredType, sustituciones);
        }
    }
    for (str padre <- declarado.parents) {
        if (padre notin visitados) {
            return campo(typeRef(padre, []), nombre, objetos, visitados + {declarado.name});
        }
    }
    throw "el objeto no tiene ese campo.";
}

private MethodAssign metodo(
    TypeRef receptor,
    str nombre,
    list[MethodAssign] metodos,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    ObjectDecl declarado = objeto(receptor, objetos);
    for (MethodAssign candidato <- metodos) {
        if (candidato.objectName == declarado.name && candidato.methodName == nombre) {
            return candidato;
        }
    }
    for (str padre <- declarado.parents) {
        if (padre notin visitados) {
            return metodo(typeRef(padre, []), nombre, metodos, objetos,
                visitados + {declarado.name});
        }
    }
    throw "el objeto no tiene ese método.";
}

private TypeRef comprobarFor(
    Pattern patron,
    Expression fuente,
    Expression cuerpo,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] genericos
) {
    TypeRef tipoFuente = comprobarExpresion(fuente, entorno, objetos, metodos, genericos);
    exigir(tipoFuente == rango(), "la fuente del ciclo debe ser un rango.");
    if (patternId(str nombre) := patron) {
        map[str, TypeRef] cuerpoEntorno = entorno;
        cuerpoEntorno[nombre] = tipo("Int");
        comprobarExpresion(cuerpo, cuerpoEntorno, objetos, metodos, genericos);
        return unit();
    }
    throw "Error de tipos: un rango produce enteros, no tuplas.";
}

private void comprobarInterpolado(
    InterpolatedString interpolado,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] genericos
) {
    for (StringPart parte <- interpolado.parts) {
        if (stringInterpolation(Expression expresion) := parte) {
            comprobarExpresion(expresion, entorno, objetos, metodos, genericos);
        }
    }
}

private TypeRef comprobarLlamada(
    Expression funcion,
    list[Expression] argumentos,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] genericos
) {
    if (identifier("range") := funcion) {
        exigir(size(argumentos) == 2, "range requiere exactamente dos argumentos.");
        for (Expression argumento <- argumentos) {
            exigir(comprobarExpresion(argumento, entorno, objetos, metodos, genericos) == tipo("Int"),
                "los argumentos de range deben ser Int.");
        }
        return rango();
    }
    if (identifier("print") := funcion) {
        for (Expression argumento <- argumentos) {
            comprobarExpresion(argumento, entorno, objetos, metodos, genericos);
        }
        return unit();
    }
    if (identifier("println") := funcion) {
        for (Expression argumento <- argumentos) {
            comprobarExpresion(argumento, entorno, objetos, metodos, genericos);
        }
        return unit();
    }
    if (memberAccess(Expression receptor, str nombre) := funcion) {
        TypeRef tipoReceptor = comprobarExpresion(receptor, entorno, objetos, metodos, genericos);
        MethodAssign declarado = metodo(tipoReceptor, nombre, metodos, objetos, {});
        list[Parameter] parametros = declarado.parameters;
        exigir(size(argumentos) == size(parametros), "cantidad incorrecta de argumentos.");
        map[str, TypeRef] sustituciones = argumentosDelMetodo(tipoReceptor, declarado, objetos);
        for (int indice <- [0..size(argumentos)]) {
            TypeRef esperado = sustituir(parametros[indice].paramType, sustituciones);
            TypeRef actual = comprobarExpresion(argumentos[indice], entorno, objetos, metodos,
                genericos);
            exigir(esSubtipo(actual, esperado, objetos), "argumento incompatible.");
        }
        return sustituir(declarado.returnType, sustituciones);
    }
    throw "solo se pueden invocar métodos de objetos o print/println.";
}

private TypeRef comprobarExpresion(
    Expression expresion,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] genericos
) {
    if (integerLiteral(_) := expresion) {
        return tipo("Int");
    }
    if (booleanLiteral(_) := expresion) {
        return tipo("Bool");
    }
    if (charLiteral(_) := expresion) {
        return tipo("Char");
    }
    if (stringLiteral(_) := expresion) {
        return tipo("String");
    }
    if (interpolatedStringExp(InterpolatedString texto) := expresion) {
        comprobarInterpolado(texto, entorno, objetos, metodos, genericos);
        return tipo("String");
    }
    if (identifier(str nombre) := expresion) {
        exigir(nombre in entorno, "variable no declarada.");
        return entorno[nombre];
    }
    if (objectCreation(TypeRef referencia, list[Expression] argumentos,
        list[OverrideDecl] overrides) := expresion) {
        validarTipo(referencia, objetos, genericos, true);
        ObjectDecl declarado = objeto(referencia, objetos);
        list[ConstructorParam] parametros = parametrosConstructor(declarado.name, objetos, {});
        exigir(size(argumentos) == size(parametros), "cantidad incorrecta de argumentos.");
        list[TypeRef] actuales = [comprobarExpresion(argumento, entorno, objetos, metodos, genericos)
            | Expression argumento <- argumentos];
        if (referencia.typeArgs == [] && declarado.typeParams != []) {
            set[str] porInferir = {parametro.name | TypeParam parametro <- declarado.typeParams};
            map[str, TypeRef] inferidos = ();
            for (int indice <- [0..size(argumentos)]) {
                inferidos += unificar(parametros[indice].declaredType, actuales[indice], porInferir);
            }
            for (str parametro <- porInferir) {
                exigir(parametro in inferidos, "no se pudo inferir el argumento genérico <parametro>.");
            }
            referencia = typeRef(referencia.name,
                [inferidos[parametro.name] | TypeParam parametro <- declarado.typeParams]);
        }
        map[str, TypeRef] sustituciones = argumentosDeTipo(referencia, declarado);
        for (int indice <- [0..size(argumentos)]) {
            TypeRef esperado = sustituir(parametros[indice].declaredType, sustituciones);
            exigir(esSubtipo(actuales[indice], esperado, objetos), "argumento incompatible en constructor.");
        }
        comprobarOverrides(referencia, overrides, objetos, metodos, genericos);
        return referencia;
    }
    if (memberAccess(memberAccess(Expression receptor, "getClass"), "getSimpleName") := expresion) {
        TypeRef tipoReceptor = comprobarExpresion(receptor, entorno, objetos, metodos, genericos);
        exigir(tipoReceptor.name in objetos, "getClass requiere un objeto.");
        return tipo("String");
    }
    if (memberAccess(Expression receptor, str nombre) := expresion) {
        TypeRef tipoReceptor = comprobarExpresion(receptor, entorno, objetos, metodos, genericos);
        return campo(tipoReceptor, nombre, objetos, {});
    }
    if (callExp(Expression funcion, list[Expression] argumentos) := expresion) {
        return comprobarLlamada(funcion, argumentos, entorno, objetos, metodos, genericos);
    }
    if (addExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Int");
    }
    if (subExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Int");
    }
    if (mulExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Int");
    }
    if (divExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Int");
    }
    if (modExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Int");
    }
    if (powExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Int");
    }
    if (andExp(Expression a, Expression b) := expresion) {
        exigir(iguales(comprobarExpresion(a, entorno, objetos, metodos, genericos), tipo("Bool"))
            && iguales(comprobarExpresion(b, entorno, objetos, metodos, genericos), tipo("Bool")),
            "los operadores lógicos requieren Bool.");
        return tipo("Bool");
    }
    if (orExp(Expression a, Expression b) := expresion) {
        exigir(iguales(comprobarExpresion(a, entorno, objetos, metodos, genericos), tipo("Bool"))
            && iguales(comprobarExpresion(b, entorno, objetos, metodos, genericos), tipo("Bool")),
            "los operadores lógicos requieren Bool.");
        return tipo("Bool");
    }
    if (ltExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Bool");
    }
    if (gtExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Bool");
    }
    if (leExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Bool");
    }
    if (geExp(Expression a, Expression b) := expresion) {
        comprobarAritmetica(a, b, entorno, objetos, metodos, genericos);
        return tipo("Bool");
    }
    if (eqExp(Expression a, Expression b) := expresion) {
        TypeRef izquierdo = comprobarExpresion(a, entorno, objetos, metodos, genericos);
        TypeRef derecho = comprobarExpresion(b, entorno, objetos, metodos, genericos);
        exigir(esSubtipo(izquierdo, derecho, objetos) || esSubtipo(derecho, izquierdo, objetos),
            "los operandos no son comparables.");
        return tipo("Bool");
    }
    if (neExp(Expression a, Expression b) := expresion) {
        TypeRef izquierdo = comprobarExpresion(a, entorno, objetos, metodos, genericos);
        TypeRef derecho = comprobarExpresion(b, entorno, objetos, metodos, genericos);
        exigir(esSubtipo(izquierdo, derecho, objetos) || esSubtipo(derecho, izquierdo, objetos),
            "los operandos no son comparables.");
        return tipo("Bool");
    }
    if (negExp(Expression a) := expresion) {
        exigir(iguales(comprobarExpresion(a, entorno, objetos, metodos, genericos), tipo("Bool")),
            "neg requiere Bool.");
        return tipo("Bool");
    }
    if (minusExp(Expression a) := expresion) {
        comprobarAritmetica(a, integerLiteral(0), entorno, objetos, metodos, genericos);
        return tipo("Int");
    }
    if (ifExp(Expression condicion, Expression verdadero, Expression falso) := expresion) {
        exigir(iguales(comprobarExpresion(condicion, entorno, objetos, metodos, genericos),
            tipo("Bool")), "la condición requiere Bool.");
        TypeRef izquierda = comprobarExpresion(verdadero, entorno, objetos, metodos, genericos);
        TypeRef derecha = comprobarExpresion(falso, entorno, objetos, metodos, genericos);
        if (esSubtipo(izquierda, derecha, objetos)) {
            return derecha;
        }
        exigir(esSubtipo(derecha, izquierda, objetos), "las ramas del condicional no son compatibles.");
        return izquierda;
    }
    if (forInExp(Pattern patron, Expression fuente, Expression cuerpo) := expresion) {
        return comprobarFor(patron, fuente, cuerpo, entorno, objetos, metodos, genericos);
    }
    if (forArrowExp(Pattern patron, Expression fuente, Expression cuerpo) := expresion) {
        return comprobarFor(patron, fuente, cuerpo, entorno, objetos, metodos, genericos);
    }
    throw "expresión no soportada.";
}

private void comprobarAritmetica(
    Expression izquierdo,
    Expression derecho,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] genericos
) {
    exigir(iguales(comprobarExpresion(izquierdo, entorno, objetos, metodos, genericos), tipo("Int"))
        && iguales(comprobarExpresion(derecho, entorno, objetos, metodos, genericos), tipo("Int")),
        "los operadores aritméticos requieren Int.");
}

private void comprobarOverrides(
    TypeRef receptor,
    list[OverrideDecl] overrides,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] genericos
) {
    for (OverrideDecl override <- overrides) {
        str nombre = "";
        list[Parameter] parametros = [];
        TypeRef retorno = unit();
        Expression expresion = integerLiteral(0);
        Block bloque = block([]);
        bool esBloque = false;
        if (overrideExp(str n, list[Parameter] ps, TypeRef r, Expression e) := override) {
            nombre = n; parametros = ps; retorno = r; expresion = e;
        }
        if (overrideBlock(str n, list[Parameter] ps, TypeRef r, Block b) := override) {
            nombre = n; parametros = ps; retorno = r; bloque = b; esBloque = true;
        }
        MethodAssign existente = metodo(receptor, nombre, metodos, objetos, {});
        list[Parameter] formales = existente.parameters;
        exigir(size(formales) == size(parametros), "override con cantidad incorrecta de parámetros.");
        map[str, TypeRef] sustituciones = argumentosDelMetodo(receptor, existente, objetos);
        validarTipo(retorno, objetos, genericos, false);
        exigir(esSubtipo(retorno, sustituir(existente.returnType, sustituciones), objetos),
            "el retorno del override debe ser compatible con el método original.");
        for (int indice <- [0..size(parametros)]) {
            TypeRef parametro = parametros[indice].paramType;
            validarTipo(parametro, objetos, genericos, false);
            TypeRef formal = sustituir(formales[indice].paramType, sustituciones);
            exigir(esSubtipo(formal, parametro, objetos),
                "el parámetro del override debe aceptar los argumentos del método original.");
        }
        map[str, TypeRef] local = (parametro.name: parametro.paramType | Parameter parametro <- parametros);
        local["este"] = receptor;
        if (esBloque) {
            TypeRef actual = comprobarBloque(bloque, local, objetos, metodos, genericos);
            exigir(esSubtipo(actual, retorno, objetos), "retorno incompatible en override.");
        }
        else {
            TypeRef actual = comprobarExpresion(expresion, local, objetos, metodos, genericos);
            exigir(esSubtipo(actual, retorno, objetos), "retorno incompatible en override.");
        }
    }
}

private TypeRef comprobarBloque(
    Block bloque,
    map[str, TypeRef] entornoInicial,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] genericos
) {
    map[str, TypeRef] entorno = entornoInicial;
    TypeRef resultado = unit();
    for (Statement instruccion <- bloque.statements) {
        if (stmtExp(Expression expresion) := instruccion) {
            resultado = comprobarExpresion(expresion, entorno, objetos, metodos, genericos);
        }
        if (variableDecl(_, str nombre, TypeRef declarado, Expression inicial) := instruccion) {
            validarTipo(declarado, objetos, genericos, false);
            TypeRef actual = comprobarExpresion(inicial, entorno, objetos, metodos, genericos);
            exigir(esSubtipo(actual, declarado, objetos), "tipo incompatible en variable.");
            entorno[nombre] = declarado;
            resultado = unit();
        }
        if (variableDeclSinTipo(_, str nombre, Expression inicial) := instruccion) {
            entorno[nombre] = comprobarExpresion(inicial, entorno, objetos, metodos, genericos);
            resultado = unit();
        }
        if (variableDeclBloque(_, str nombre, Block inicial) := instruccion) {
            entorno[nombre] = comprobarBloque(inicial, entorno, objetos, metodos, genericos);
            resultado = unit();
        }
        if (variableDeclBloqueConTipo(_, str nombre, TypeRef declarado, Block inicial) := instruccion) {
            validarTipo(declarado, objetos, genericos, false);
            TypeRef actual = comprobarBloque(inicial, entorno, objetos, metodos, genericos);
            exigir(esSubtipo(actual, declarado, objetos), "tipo incompatible en variable.");
            entorno[nombre] = declarado;
            resultado = unit();
        }
    }
    return resultado;
}

public void comprobarPrograma(Program programa) {
    if (program(list[Declaration] antes, mainMethod(Block principal), list[Declaration] despues)
            := programa) {
        list[Declaration] declaraciones = antes + despues;
        map[str, ObjectDecl] objetos = (objeto.name: objeto |
            Declaration declaracion <- declaraciones, declObject(ObjectDecl objeto) := declaracion);
        list[MethodAssign] metodos = [metodo |
            Declaration declaracion <- declaraciones, declMethod(MethodAssign metodo) := declaracion];
        for (str nombre <- objetos) {
            validarHerencia(nombre, objetos, {});
            set[str] genericos = {parametro.name | TypeParam parametro <- objetos[nombre].typeParams};
            for (ConstructorParam parametro <- objetos[nombre].constructorParams) {
                validarTipo(parametro.declaredType, objetos, genericos, false);
            }
        }
        for (MethodAssign declarado <- metodos) {
            exigir(declarado.objectName in objetos, "objeto dueño de método inexistente.");
            list[TypeParam] tipos = objetos[declarado.objectName].typeParams;
            set[str] genericos = {parametro.name | TypeParam parametro <- tipos};
            validarTipo(declarado.returnType, objetos, genericos, false);
            for (Parameter parametro <- declarado.parameters) {
                validarTipo(parametro.paramType, objetos, genericos, false);
            }
            if (methodAssign(_, _, list[Parameter] parametros, TypeRef retorno, Block cuerpo)
                    := declarado) {
                map[str, TypeRef] entorno = (parametro.name: parametro.paramType |
                    Parameter parametro <- parametros);
                entorno["este"] = typeRef(declarado.objectName,
                    [tipo(parametro.name) | TypeParam parametro <- tipos]);
                TypeRef actual = comprobarBloque(cuerpo, entorno, objetos, metodos, genericos);
                exigir(esSubtipo(actual, retorno, objetos), "retorno incompatible en método.");
            }
        }
        comprobarBloque(principal, (), objetos, metodos, {});
    }
}
