module TypeChecker

import AST;
import List;

private TypeRef tipo(str nombre) = typeRef(nombre, []);
private TypeRef tipoUnit() = tipo("Unit");

private bool mismosTipos(TypeRef izquierdo, TypeRef derecho) {
    if (typeRef(str nombreIzquierdo, list[TypeRef] argumentosIzquierdos) := izquierdo
        && typeRef(str nombreDerecho, list[TypeRef] argumentosDerechos) := derecho) {
        if (nombreIzquierdo != nombreDerecho
            || size(argumentosIzquierdos) != size(argumentosDerechos)) {
            return false;
        }
        for (int indice <- [0..size(argumentosIzquierdos)]) {
            if (!mismosTipos(argumentosIzquierdos[indice], argumentosDerechos[indice])) {
                return false;
            }
        }
        return true;
    }
    return false;
}

private void exigir(bool condicion, str mensaje) {
    if (!condicion) {
        throw "Error de tipos: " + mensaje;
    }
}

private void validarTipo(
    TypeRef referencia,
    map[str, ObjectDecl] objetos,
    set[str] parametrosGenericos
) {
    if (typeRef(str nombre, list[TypeRef] argumentos) := referencia) {
        bool esPrimitivo = nombre in {"Int", "Bool", "String", "Unit"};
        exigir(esPrimitivo || nombre in objetos || nombre in parametrosGenericos,
            "tipo desconocido.");

        if (nombre in objetos) {
            ObjectDecl objeto = objetos[nombre];
            int cantidadParametros = size(objeto.typeParams);
            exigir(size(argumentos) == cantidadParametros,
                "cantidad incorrecta de argumentos genéricos.");
        }
        else {
            exigir(size(argumentos) == 0,
                "el tipo no acepta argumentos genéricos.");
        }
        for (TypeRef argumento <- argumentos) {
            validarTipo(argumento, objetos, parametrosGenericos);
        }
    }
}

private void validarHerencia(
    str nombre,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    exigir(nombre in objetos, "el objeto referenciado no existe.");
    exigir(nombre notin visitados, "la herencia de objetos contiene un ciclo.");

    ObjectDecl objeto = objetos[nombre];
    for (str padre <- objeto.parents) {
        exigir(padre in objetos, "el objeto padre referenciado no existe.");
        validarHerencia(padre, objetos, visitados + {nombre});
    }
}

private void validarDefinicionesObjetos(map[str, ObjectDecl] objetos) {
    for (str nombre <- objetos) {
        ObjectDecl objeto = objetos[nombre];
        set[str] parametrosGenericos = {
            parametro.name | TypeParam parametro <- objeto.typeParams
        };

        validarHerencia(nombre, objetos, {});
        for (ConstructorParam parametro <- objeto.constructorParams) {
            validarTipo(parametro.declaredType, objetos, parametrosGenericos);
        }
    }
}

private ObjectDecl obtenerObjeto(
    TypeRef referencia,
    map[str, ObjectDecl] objetos
) {
    if (typeRef(str nombre, _) := referencia && nombre in objetos) {
        return objetos[nombre];
    }
    throw "tipo de objeto esperado.";
}

private TypeRef tipoCampo(
    TypeRef referencia,
    str campo,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    ObjectDecl objeto = obtenerObjeto(referencia, objetos);
    if (objeto.name in visitados) {
        throw "herencia circular al buscar un campo.";
    }
    for (ConstructorParam parametro <- objeto.constructorParams) {
        if (parametro.name == campo) {
            return parametro.declaredType;
        }
    }
    for (str padre <- objeto.parents) {
        if (padre in objetos) {
            TypeRef resultado = tipoCampo(
                typeRef(padre, []), campo, objetos, visitados + {objeto.name});
            return resultado;
        }
    }
    throw "el objeto no tiene ese campo.";
}

private list[ConstructorParam] parametrosConstructor(
    str nombre,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    if (nombre in visitados) {
        throw "herencia circular al buscar parámetros del constructor.";
    }
    ObjectDecl objeto = objetos[nombre];
    list[ConstructorParam] heredados = [];
    for (str padre <- objeto.parents) {
        if (padre in objetos) {
            heredados += parametrosConstructor(padre, objetos, visitados + {nombre});
        }
    }
    return heredados + objeto.constructorParams;
}

private MethodAssign obtenerMetodo(
    TypeRef referencia,
    str nombre,
    list[MethodAssign] metodos,
    map[str, ObjectDecl] objetos,
    set[str] visitados
) {
    ObjectDecl objeto = obtenerObjeto(referencia, objetos);
    if (objeto.name in visitados) {
        throw "herencia circular al buscar un método.";
    }
    for (MethodAssign metodo <- metodos) {
        if (metodo.objectName == objeto.name && metodo.methodName == nombre) {
            return metodo;
        }
    }
    for (str padre <- objeto.parents) {
        if (padre in objetos) {
            return obtenerMetodo(typeRef(padre, []), nombre, metodos, objetos,
                visitados + {objeto.name});
        }
    }
    throw "el objeto no tiene ese método.";
}

private TypeRef comprobarAritmetica(
    Expression izquierdo,
    Expression derecho,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] parametrosGenericos
) {
    exigir(mismosTipos(comprobarExpresion(izquierdo, entorno, objetos, metodos,
        parametrosGenericos), tipo("Int"))
        && mismosTipos(comprobarExpresion(derecho, entorno, objetos, metodos,
        parametrosGenericos), tipo("Int")), "los operadores aritméticos requieren Int.");
    return tipo("Int");
}

private TypeRef comprobarLogica(
    Expression izquierdo,
    Expression derecho,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] parametrosGenericos
) {
    exigir(mismosTipos(comprobarExpresion(izquierdo, entorno, objetos, metodos,
        parametrosGenericos), tipo("Bool"))
        && mismosTipos(comprobarExpresion(derecho, entorno, objetos, metodos,
        parametrosGenericos), tipo("Bool")), "los operadores lógicos requieren Bool.");
    return tipo("Bool");
}

private TypeRef comprobarExpresion(
    Expression expresion,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] parametrosGenericos
) {
    if (integerLiteral(_) := expresion) {
        return tipo("Int");
    }
    if (booleanLiteral(_) := expresion) {
        return tipo("Bool");
    }
    if (stringLiteral(_) := expresion || interpolatedStringExp(_) := expresion) {
        return tipo("String");
    }
    if (identifier(str nombre) := expresion) {
        exigir(nombre in entorno, "variable no declarada.");
        return entorno[nombre];
    }
    if (objectCreation(TypeRef referencia, list[Expression] argumentos, list[OverrideDecl] overrides)
            := expresion) {
        validarTipo(referencia, objetos, parametrosGenericos);
        ObjectDecl objeto = obtenerObjeto(referencia, objetos);
        list[ConstructorParam] parametros = parametrosConstructor(objeto.name, objetos, {});
        exigir(size(argumentos) == size(parametros),
            "cantidad incorrecta de argumentos del constructor.");
        for (int indice <- [0..size(argumentos)]) {
            TypeRef actual = comprobarExpresion(argumentos[indice], entorno, objetos,
                metodos, parametrosGenericos);
            exigir(mismosTipos(actual, parametros[indice].declaredType),
                "argumento incompatible en el constructor.");
        }
        for (OverrideDecl override <- overrides) {
            if (overrideExp(str nombre, list[Parameter] parametros, TypeRef retorno, Expression cuerpo)
                    := override) {
                validarTipo(retorno, objetos, parametrosGenericos);
                map[str, TypeRef] entornoOverride = (parametro.name: parametro.paramType |
                    Parameter parametro <- parametros);
                TypeRef tipoCuerpo = comprobarExpresion(cuerpo, entornoOverride, objetos,
                    metodos, parametrosGenericos);
                exigir(mismosTipos(tipoCuerpo, retorno),
                    "el retorno del método sobrescrito no coincide.");
            }
            if (overrideBlock(str nombre, list[Parameter] parametros, TypeRef retorno, Block cuerpo)
                    := override) {
                validarTipo(retorno, objetos, parametrosGenericos);
                map[str, TypeRef] entornoOverride = (parametro.name: parametro.paramType |
                    Parameter parametro <- parametros);
                TypeRef tipoCuerpo = comprobarBloque(cuerpo, entornoOverride, objetos,
                    metodos, parametrosGenericos);
                exigir(mismosTipos(tipoCuerpo, retorno),
                    "el retorno del método sobrescrito no coincide.");
            }
        }
        return referencia;
    }
    if (memberAccess(Expression receptor, str miembro) := expresion) {
        TypeRef tipoReceptor = comprobarExpresion(receptor, entorno, objetos, metodos,
            parametrosGenericos);
        return tipoCampo(tipoReceptor, miembro, objetos, {});
    }
    if (callExp(Expression funcion, list[Expression] argumentos) := expresion) {
        if (memberAccess(Expression receptor, str nombre) := funcion) {
            TypeRef tipoReceptor = comprobarExpresion(receptor, entorno, objetos, metodos,
                parametrosGenericos);
            MethodAssign metodo = obtenerMetodo(tipoReceptor, nombre, metodos, objetos, {});
            list[Parameter] parametros = [];
            TypeRef retorno = tipoUnit();
            if (methodAssign(_, _, list[Parameter] ps, TypeRef r, _) := metodo) {
                parametros = ps;
                retorno = r;
            }
            if (methodAssignSinCuerpo(_, _, list[Parameter] ps, TypeRef r) := metodo) {
                parametros = ps;
                retorno = r;
            }
            exigir(size(argumentos) == size(parametros),
                "cantidad incorrecta de argumentos del método.");
            for (int indice <- [0..size(argumentos)]) {
                TypeRef actual = comprobarExpresion(argumentos[indice], entorno, objetos,
                    metodos, parametrosGenericos);
                exigir(mismosTipos(actual, parametros[indice].paramType),
                    "argumento incompatible en el método.");
            }
            return retorno;
        }
        throw "solo se pueden invocar métodos de objetos.";
    }
    if (addExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarAritmetica(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (subExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarAritmetica(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (mulExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarAritmetica(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (divExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarAritmetica(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (modExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarAritmetica(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (powExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarAritmetica(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (andExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarLogica(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (orExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarLogica(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (ltExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarComparacion(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (gtExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarComparacion(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (leExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarComparacion(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (geExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarComparacion(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (eqExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarIgualdad(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (neExp(Expression izquierdo, Expression derecho) := expresion) {
        return comprobarIgualdad(izquierdo, derecho, entorno, objetos, metodos,
            parametrosGenericos);
    }
    if (negExp(Expression operando) := expresion) {
        return comprobarUnaria(operando, true, entorno, objetos, metodos, parametrosGenericos);
    }
    if (minusExp(Expression operando) := expresion) {
        return comprobarUnaria(operando, false, entorno, objetos, metodos, parametrosGenericos);
    }
    if (ifExp(Expression condicion, Expression ramaVerdadera, Expression ramaFalsa) := expresion) {
        return comprobarCondicional(condicion, ramaVerdadera, ramaFalsa, entorno, objetos,
            metodos, parametrosGenericos);
    }
    if (arrowExp(_, _) := expresion) {
        throw "las expresiones flecha no tienen un tipo ejecutable.";
    }
    if (fatArrowExp(_, _) := expresion) {
        throw "las expresiones flecha no tienen un tipo ejecutable.";
    }
    throw "expresión no soportada por el comprobador de tipos.";
}

private TypeRef comprobarComparacion(
    Expression izquierdo,
    Expression derecho,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] parametrosGenericos
) {
        exigir(mismosTipos(comprobarExpresion(izquierdo, entorno, objetos, metodos,
            parametrosGenericos), tipo("Int"))
            && mismosTipos(comprobarExpresion(derecho, entorno, objetos, metodos,
            parametrosGenericos), tipo("Int")), "los operadores aritméticos requieren Int.");
        return tipo("Bool");
}

private TypeRef comprobarIgualdad(
    Expression izquierdo,
    Expression derecho,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] parametrosGenericos
) {
    exigir(mismosTipos(comprobarExpresion(izquierdo, entorno, objetos, metodos,
        parametrosGenericos),
        comprobarExpresion(derecho, entorno, objetos, metodos, parametrosGenericos)),
        "los operandos de comparación deben tener el mismo tipo.");
    return tipo("Bool");
}

private TypeRef comprobarUnaria(
    Expression operando,
    bool booleana,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] parametrosGenericos
) {
    TypeRef actual = comprobarExpresion(operando, entorno, objetos, metodos,
        parametrosGenericos);
    exigir(mismosTipos(actual, tipo(booleana ? "Bool" : "Int")),
        "tipo incorrecto para operador unario.");
    return booleana ? tipo("Bool") : tipo("Int");
}

private TypeRef comprobarCondicional(
    Expression condicion,
    Expression ramaVerdadera,
    Expression ramaFalsa,
    map[str, TypeRef] entorno,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] parametrosGenericos
) {
    exigir(mismosTipos(comprobarExpresion(condicion, entorno, objetos, metodos,
        parametrosGenericos), tipo("Bool")), "la condición requiere Bool.");
    TypeRef tipoVerdadero = comprobarExpresion(ramaVerdadera, entorno, objetos, metodos,
        parametrosGenericos);
    TypeRef tipoFalso = comprobarExpresion(ramaFalsa, entorno, objetos, metodos,
        parametrosGenericos);
    exigir(mismosTipos(tipoVerdadero, tipoFalso),
        "las ramas del condicional deben tener el mismo tipo.");
    return tipoVerdadero;
}

private TypeRef comprobarBloque(
    Block bloque,
    map[str, TypeRef] entornoInicial,
    map[str, ObjectDecl] objetos,
    list[MethodAssign] metodos,
    set[str] parametrosGenericos
) {
    if (block(list[Statement] instrucciones) := bloque) {
        map[str, TypeRef] entorno = entornoInicial;
        TypeRef resultado = tipoUnit();
        for (Statement instruccion <- instrucciones) {
            if (stmtExp(Expression expresion) := instruccion) {
                resultado = comprobarExpresion(expresion, entorno, objetos, metodos,
                    parametrosGenericos);
            }
            if (variableDecl(_, str nombre, TypeRef declarado, Expression inicial) := instruccion) {
                validarTipo(declarado, objetos, parametrosGenericos);
                TypeRef actual = comprobarExpresion(inicial, entorno, objetos, metodos,
                    parametrosGenericos);
                exigir(mismosTipos(actual, declarado),
                    "el tipo declarado de la variable no coincide con su valor.");
                entorno[nombre] = declarado;
                resultado = tipoUnit();
            }
            if (variableDeclSinTipo(_, str nombre, Expression inicial) := instruccion) {
                TypeRef actual = comprobarExpresion(inicial, entorno, objetos, metodos,
                    parametrosGenericos);
                entorno[nombre] = actual;
                resultado = tipoUnit();
            }
            if (variableDeclBloque(_, str nombre, Block inicial) := instruccion) {
                TypeRef actual = comprobarBloque(inicial, entorno, objetos, metodos,
                    parametrosGenericos);
                entorno[nombre] = actual;
                resultado = tipoUnit();
            }
            if (variableDeclBloqueConTipo(_, str nombre, TypeRef declarado, Block inicial) := instruccion) {
                validarTipo(declarado, objetos, parametrosGenericos);
                TypeRef actual = comprobarBloque(inicial, entorno, objetos, metodos,
                    parametrosGenericos);
                exigir(mismosTipos(actual, declarado),
                    "el tipo declarado de la variable no coincide con su bloque.");
                entorno[nombre] = declarado;
                resultado = tipoUnit();
            }
        }
        return resultado;
    }
    throw "bloque inválido.";
}

public void comprobarPrograma(Program programa) {
    if (program(list[Declaration] antes, MainMethod principal, list[Declaration] despues)
            := programa) {
        list[Declaration] declaraciones = antes + despues;
        map[str, ObjectDecl] objetos = (objeto.name: objeto |
            Declaration declaracion <- declaraciones, declObject(ObjectDecl objeto) := declaracion);
        list[MethodAssign] metodos = [
            metodo | Declaration declaracion <- declaraciones, declMethod(MethodAssign metodo) := declaracion
        ];
        validarDefinicionesObjetos(objetos);
        for (MethodAssign metodo <- metodos) {
            exigir(metodo.objectName in objetos,
                "el objeto dueño del método no existe.");
            validarTipo(metodo.returnType, objetos, {});
            for (Parameter parametro <- metodo.parameters) {
                validarTipo(parametro.paramType, objetos, {});
            }
            if (methodAssign(_, _, list[Parameter] parametros, _, Block cuerpo) := metodo) {
                map[str, TypeRef] entorno = (parametro.name: parametro.paramType |
                    Parameter parametro <- parametros);
                entorno["este"] = tipo(metodo.objectName);
                TypeRef actual = comprobarBloque(cuerpo, entorno, objetos, metodos, {});
                exigir(mismosTipos(actual, metodo.returnType),
                    "el tipo de retorno del método no coincide.");
            }
        }
        comprobarBloque(principal.body, (), objetos, metodos, {});
    }
}
