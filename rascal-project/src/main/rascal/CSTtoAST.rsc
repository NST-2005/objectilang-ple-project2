module CSTtoAST

import AST;
import Syntax;
import ParseTree;
import String;
import Exception;

public AST::Expression convertirEntero(str numero) {
    return integerLiteral(toInt(numero));
}

public AST::Expression convertirSuma(str izquierda, str derecha) {
    return AST::addExp(convertirEntero(izquierda), convertirEntero(derecha));
}

public AST::Expression convertirSumaCST(Tree expresion) {
    return AST::addExp(
        convertirExpresionCST(expresion.args[0]),
        convertirExpresionCST(expresion.args[4])
    );
}

public AST::Expression convertirExpresionCST(Tree expresion) {
    if (expresion.prod.def == label("addExp", sort("Expression"))) {
        return
            AST::addExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("baseExp", sort("Expression"))) {
        return convertirExpresionCST(expresion.args[0]);
    }
    if (expresion.prod.def == label("baseLit", sort("BaseExp"))) {
        return convertirExpresionCST(expresion.args[0]);
    }
    if (expresion.prod.def == label("intLit", sort("Literal"))) {
        return convertirEntero("<expresion>");
    }
    if (expresion.prod.def == label("boolLit", sort("Literal"))) {
    return AST::booleanLiteral("<expresion>" == "true");
}
    if (expresion.prod.def == label("stringLit", sort("Literal"))) {
        return convertirCadenaCST(expresion.args[0]);
    }
 if (expresion.prod.def == label("subExp", sort("Expression"))) {
        return
            AST::subExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("mulExp", sort("Expression"))) {
        return
            AST::mulExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("divExp", sort("Expression"))) {
        return
            AST::divExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("modExp", sort("Expression"))) {
        return
            AST::modExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("powExp", sort("Expression"))) {
        return
            AST::powExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("baseParen", sort("BaseExp"))) {
        return convertirExpresionCST(expresion.args[2]);
    }
    if (expresion.prod.def == label("baseId", sort("BaseExp"))) {
        return convertirExpresionCST(expresion.args[0]);
    }
    if (expresion.prod.def == lex("Identifier")) {
        return AST::identifier("<expresion>");
    }
    if (expresion.prod.def == label("ltExp", sort("Expression"))) {
        return
            AST::ltExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("gtExp", sort("Expression"))) {
        return
            AST::gtExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("leExp", sort("Expression"))) {
        return
            AST::leExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("geExp", sort("Expression"))) {
        return
            AST::geExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("eqExp", sort("Expression"))) {
        return
            AST::eqExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("neExp", sort("Expression"))) {
        return
            AST::neExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("andExp", sort("Expression"))) {
        return
            AST::andExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("orExp", sort("Expression"))) {
        return
            AST::orExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("negExp", sort("Expression"))) {
        return AST::negExp(convertirExpresionCST(expresion.args[2]));
    }
    if (expresion.prod.def == label("minusExp", sort("Expression"))) {
        return AST::minusExp(convertirExpresionCST(expresion.args[2]));
    }
    if (expresion.prod.def == label("ifExp", sort("Expression"))) {
        return
            AST::ifExp(
                convertirExpresionCST(expresion.args[2]),
                convertirExpresionCST(expresion.args[6]),
                convertirExpresionCST(expresion.args[10])
            );
    }
    if (expresion.prod.def == label("arrowExp", sort("Expression"))) {
        return
            AST::arrowExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("fatArrowExp", sort("Expression"))) {
        return
            AST::fatArrowExp(
                convertirExpresionCST(expresion.args[0]), convertirExpresionCST(expresion.args[4])
            );
    }
    if (expresion.prod.def == label("memberAccess", sort("BaseExp"))) {
        return AST::memberAccess(convertirExpresionCST(expresion.args[0]), "<expresion.args[4]>");
    }
    if (expresion.prod.def == label("callExp", sort("BaseExp"))) {
        list[AST::Expression] argumentos = [];
        Tree lista = expresion.args[4];
        
        for (Tree argumento <- lista.args) {
            if (appl(prod(label(_, sort("Expression")), _, _), _) := argumento) {
                argumentos += [convertirExpresionCST(argumento)];
            }
        }
        
        return AST::callExp(convertirExpresionCST(expresion.args[0]), argumentos);
    }
    if (expresion.prod.def == label("forInExp", sort("Expression"))) {
        return
            AST::forInExp(
                convertirPatronCST(expresion.args[4]),
                convertirExpresionCST(expresion.args[8]),
                convertirExpresionCST(expresion.args[12])
            );
    }
    if (expresion.prod.def == label("forArrowExp", sort("Expression"))) {
        return
            AST::forArrowExp(
                convertirPatronCST(expresion.args[4]),
                convertirExpresionCST(expresion.args[8]),
                convertirExpresionCST(expresion.args[12])
            );
    }
    if (expresion.prod.def == label("newObj", sort("Expression"))) {
        AST::TypeRef tipo = convertirTipoCST(expresion.args[2]);
        
        list[AST::Expression] argumentos = [];
        list[AST::OverrideDecl] overrides = [];
        
        Tree argumentosOpcionales = expresion.args[4];
        
        if (size(argumentosOpcionales.args) > 0) {
            Tree argumentosObjeto = argumentosOpcionales.args[0];
            Tree lista = argumentosObjeto.args[2];
            
            for (Tree argumento <- lista.args) {
                if (appl(prod(label(_, sort("Expression")), _, _), _) := argumento) {
                    argumentos += [convertirExpresionCST(argumento)];
                }
            }
        }
        Tree cuerpoOpcional = expresion.args[6];
        
        if (size(cuerpoOpcional.args) > 0) {
            Tree cuerpoObjeto = cuerpoOpcional.args[0];
            Tree listaOverrides = cuerpoObjeto.args[2];
            
            for (Tree elemento <- listaOverrides.args) {
                if (appl(prod(label(_, sort("OverrideDecl")), _, _), _) := elemento) {
                    overrides += [convertirOverrideCST(elemento)];
                }
            }
        }
        return AST::objectCreation(tipo, argumentos, overrides);
    }
    throw "Expresion no soportada";
}


public AST::Expression convertirCadenaCST(Tree cadena) {
    list[AST::StringPart] partes = [];
    str texto = "";
    bool tieneInterpolacion = false;
    
    Tree listaPartes = cadena.args[1];
    
    for (Tree parte <- listaPartes.args) {
        Tree contenido = parte.args[0];
        
        if (contenido.prod.def == lex("StringChar")) {
            texto += "<contenido>";
        }
        else if (contenido.prod.def == lex("Interpolation")) {
            tieneInterpolacion = true;
            
            if (texto != "") {
                partes += [AST::stringText(texto)];
                texto = "";
            }
            if (size(contenido.args) == 2) {
                str nombre = "<contenido.args[1]>";
                
                partes += [AST::stringInterpolation(AST::identifier(nombre))];
            }
            else {
                partes += [AST::stringInterpolation(convertirExpresionCST(contenido.args[1]))];
            }
        }
        else
            if (contenido.prod.def == lex("StringEscape")) {
                map[str, str] escapes = (
                    "\\n": "\n", "\\t": "\t", "\\\"": "\"",
                    "\\\\": "\\", "\\$": "$"
                );
                texto += escapes["<contenido>"];
            }
    }
    
    if (texto != "") {
        partes += [AST::stringText(texto)];
    }
    if (tieneInterpolacion) {
        return AST::interpolatedStringExp(AST::interpolatedString(partes));
    }
    return AST::stringLiteral(texto);
}



public AST::TypeRef convertirTipoCST(Tree tipo) {
    if (tipo.prod.def == label("typeRef", sort("TypeRef"))) {
        str nombre = "<tipo.args[0]>";
        list[AST::TypeRef] genericos = [];
        
        Tree opcional = tipo.args[2];
        
        if (size(opcional.args) > 0) {
            Tree argumentos = opcional.args[0];
            Tree lista = argumentos.args[2];
            
            for (Tree argumento <- lista.args) {
                if (appl(prod(label("typeRef", sort("TypeRef")), _, _), _) := argumento) {
                    genericos += [convertirTipoCST(argumento)];
                }
            }
        }
        return AST::typeRef(nombre, genericos);
    }
    throw "Tipo no soportado";
}
public AST::VarModifier convertirModificadorCST(Tree modificador) {
    if (modificador.prod.def == label("valMod", sort("Modifier"))) {
        return AST::valMod();
    }
    if (modificador.prod.def == label("varMod", sort("Modifier"))) {
        return AST::varMod();
    }
    throw "Modificador no soportado";
}
public AST::Statement convertirVariableCST(Tree declaracion) {
    if (declaracion.prod.def == label("varDeclExp", sort("VarDecl"))) {
        AST::VarModifier modificador = convertirModificadorCST(declaracion.args[0]);
        str nombre = "<declaracion.args[2]>";
        AST::Expression valor = convertirExpresionCST(declaracion.args[8]);
        
        Tree tipoOpcional = declaracion.args[4];
        
        if (size(tipoOpcional.args) == 0) {
            return AST::variableDeclSinTipo(modificador, nombre, valor);
        }
        return
            AST::variableDecl(
                modificador, nombre, convertirTipoCST(tipoOpcional.args[0].args[2]), valor
            );
    }
    if (declaracion.prod.def == label("varDeclBlock", sort("VarDecl"))) {
        AST::VarModifier modificador = convertirModificadorCST(declaracion.args[0]);
        str nombre = "<declaracion.args[2]>";
        
        Tree tipoOpcional = declaracion.args[4];
        AST::Block bloque = convertirBlockCST(declaracion.args[8]);
        
        if (size(tipoOpcional.args) > 0) {
            Tree anotacion = tipoOpcional.args[0];
            AST::TypeRef tipo = convertirTipoCST(anotacion.args[2]);
            
            return AST::variableDeclBloqueConTipo(modificador, nombre, tipo, bloque);
        }
        return AST::variableDeclBloque(modificador, nombre, bloque);
    }
    throw "Declaracion de variable no soportada";
}

public AST::Statement convertirStatementCST(Tree instruccion) {
    if (instruccion.prod.def == label("stmtExp", sort("Statement"))) {
        return AST::stmtExp(convertirExpresionCST(instruccion.args[0]));
    }
    if (instruccion.prod.def == label("stmtVar", sort("Statement"))) {
        return convertirVariableCST(instruccion.args[0]);
    }
    throw "Instruccion no soportada";
}

public AST::Block convertirBlockCST(Tree bloque) {
    list[AST::Statement] instrucciones = [];
    
    Tree lista = bloque.args[2];
    
    for (Tree instruccion <- lista.args) {
        if (instruccion.prod.def == label("stmtExp", sort("Statement"))
            || instruccion.prod.def == label("stmtVar", sort("Statement"))) {
            instrucciones += [convertirStatementCST(instruccion)];
        }
    }
    
    return AST::block(instrucciones);
}
public AST::MainMethod convertirMainCST(Tree metodo) {
    return AST::mainMethod(convertirBlockCST(metodo.args[4]));
}
public AST::Program convertirProgramaCST(Tree programa) {
    Tree raiz = programa has top ? programa.top : programa;
    
    list[AST::Declaration] antes = [];
    list[AST::Declaration] despues = [];
    
    for (Tree declaracion <- raiz.args[0].args) {
        if (appl(prod(label(_, sort("Declaration")), _, _), _) := declaracion) {
            antes += [convertirDeclaracionCST(declaracion)];
        }
    }
    
    for (Tree declaracion <- raiz.args[4].args) {
        if (appl(prod(label(_, sort("Declaration")), _, _), _) := declaracion) {
            despues += [convertirDeclaracionCST(declaracion)];
        }
    }
    
    return AST::program(antes, convertirMainCST(raiz.args[2]), despues);
}
public AST::Pattern convertirPatronCST(Tree patron) {
    if (patron.prod.def == label("patternId", sort("Pattern"))) {
        return AST::patternId("<patron.args[0]>");
    }
    if (patron.prod.def == label("patternTuple", sort("Pattern"))) {
        list[str] nombres = [];
        
        for (Tree elemento <- patron.args[2].args) {
            if (appl(prod(lex("Identifier"), _, _), _) := elemento) {
                nombres += ["<elemento>"];
            }
        }
        
        return AST::patternTuple(nombres);
    }
    throw "Patron no soportado";
}

public AST::Declaration convertirDeclaracionCST(Tree declaracion) {
    if (declaracion.prod.def == label("declObject", sort("Declaration"))) {
        Tree objeto = declaracion.args[0];
        
        if (objeto.prod.def == label("objectDecl", sort("ObjectDecl"))) {
            str nombre = "<objeto.args[2]>";
            
            list[AST::TypeParam] genericos = [];
            Tree genericosOpcionales = objeto.args[4];
            
            if (size(genericosOpcionales.args) > 0) {
                Tree parametros = genericosOpcionales.args[0];
                Tree lista = parametros.args[2];
                
                for (Tree parametro <- lista.args) {
                    if (appl(prod(label("typeParam", sort("TypeParam")), _, _), _) := parametro) {
                        genericos += [convertirParametroGenericoCST(parametro)];
                    }
                }
            }
            list[str] padres = [];
            
            Tree herenciaOpcional = objeto.args[6];
            
            if (size(herenciaOpcional.args) > 0) {
                Tree herencia = herenciaOpcional.args[0];
                str nombrePadre = "<herencia.args[2]>";
                padres += [nombrePadre];
            }
            list[AST::ConstructorParam] constructores = [];
            Tree constructorOpcional = objeto.args[8];
            
            if (size(constructorOpcional.args) > 0) {
                Tree constructor = constructorOpcional.args[0];
                Tree lista = constructor.args[2];
                
                for (Tree parametro <- lista.args) {
                    if (appl(prod(label("constructorParam", sort("ConstructorParam")), _, _), _) := parametro) {
                        constructores += [convertirConstructorParamCST(parametro)];
                    }
                }
            }
            return AST::declObject(AST::objectDecl(nombre, genericos, padres, constructores));
        }
    }
    if (declaracion.prod.def == label("declMethod", sort("Declaration"))) {
        Tree metodo = declaracion.args[0];
        
        if (metodo.prod.def == label("methodAssign", sort("MethodAssign"))) {
            str objeto = "<metodo.args[0]>";
            str nombre = "<metodo.args[4]>";
            
            list[AST::Parameter] parametros = [];
            Tree listaParametros = metodo.args[12];
            
            for (Tree parametro <- listaParametros.args) {
                if (appl(prod(label("parameter", sort("Parameter")), _, _), _) := parametro) {
                    parametros += [convertirParametroCST(parametro)];
                }
            }
            
            AST::TypeRef retorno = convertirTipoCST(metodo.args[18]);
            
            Tree cuerpoOpcional = metodo.args[20];
            
            if (size(cuerpoOpcional.args) > 0) {
                AST::Block cuerpo = convertirBlockCST(cuerpoOpcional.args[0]);
                
                return AST::declMethod(AST::methodAssign(objeto, nombre, parametros, retorno, cuerpo));
            }
            return AST::declMethod(AST::methodAssignSinCuerpo(objeto, nombre, parametros, retorno));
        }
    }
    throw "Declaracion no soportada";
}
public AST::Variance convertirVarianzaCST(Tree varianza) {
    if (varianza.prod.def == label("covariant", sort("Variance"))) {
        return AST::covariant();
    }
    if (varianza.prod.def == label("contravariant", sort("Variance"))) {
        return AST::contravariant();
    }
    throw "Varianza no soportada";
}

public AST::TypeParam convertirParametroGenericoCST(Tree parametro) {
    str nombre = "<parametro.args[2]>";
    Tree opcional = parametro.args[0];
    
    AST::Variance varianza = AST::invariant();
    
    if (size(opcional.args) > 0) {
        varianza = convertirVarianzaCST(opcional.args[0]);
    }
    return AST::typeParam(nombre, varianza);
}
public AST::ConstructorParam convertirConstructorParamCST(Tree parametro) {
    AST::VarModifier modificador = convertirModificadorCST(parametro.args[0]);
    str nombre = "<parametro.args[2]>";
    AST::TypeRef tipo = convertirTipoCST(parametro.args[6]);
    
    return AST::constructorParam(modificador, nombre, tipo);
}
public AST::Parameter convertirParametroCST(Tree parametro) {
    str nombre = "<parametro.args[0]>";
    AST::TypeRef tipo = convertirTipoCST(parametro.args[4]);
    
    return AST::parameter(nombre, tipo);
}

public AST::OverrideDecl convertirOverrideCST(Tree declaracion) {
    if (declaracion.prod.def == label("overrideExp", sort("OverrideDecl"))) {
        str nombre = "<declaracion.args[2]>";
        
        list[AST::Parameter] parametros = [];
        Tree listaParametros = declaracion.args[6];
        
        for (Tree parametro <- listaParametros.args) {
            if (appl(prod(label("parameter", sort("Parameter")), _, _), _) := parametro) {
                parametros += [convertirParametroCST(parametro)];
            }
        }
        
        AST::TypeRef retorno = convertirTipoCST(declaracion.args[12]);
        AST::Expression expresion = convertirExpresionCST(declaracion.args[16]);
        
        return AST::overrideExp(nombre, parametros, retorno, expresion);
    }
    if (declaracion.prod.def == label("overrideBlock", sort("OverrideDecl"))) {
        str nombre = "<declaracion.args[2]>";
        
        list[AST::Parameter] parametros = [];
        Tree listaParametros = declaracion.args[6];
        
        for (Tree parametro <- listaParametros.args) {
            if (appl(prod(label("parameter", sort("Parameter")), _, _), _) := parametro) {
                parametros += [convertirParametroCST(parametro)];
            }
        }
        
        AST::TypeRef retorno = convertirTipoCST(declaracion.args[12]);
        AST::Block cuerpo = convertirBlockCST(declaracion.args[16]);
        
        return AST::overrideBlock(nombre, parametros, retorno, cuerpo);
    }
    throw "Override no soportado";
}
