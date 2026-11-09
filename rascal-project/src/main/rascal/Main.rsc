module Main

import AST;
import Parser;
import CSTtoAST;
import Generador;
import TypeChecker;
import IO;
import util::Eval;
import List;
void main(loc archivo) {
    Program programa = convertirProgramaCST(parseObjectilang(archivo));
    comprobarPrograma(programa);

    if (program(list[Declaration] antes, mainMethod(block(list[Statement] instrucciones)), list[Declaration] despues) := programa) {
        RascalRuntime entorno = createRascalRuntime();
        list[Declaration] declaraciones = antes + despues;
        map[str, ObjectDecl] objetos = (objeto.name: objeto |
            Declaration declaracion <- declaraciones, declObject(ObjectDecl objeto) := declaracion);

        for (Declaration declaracion <- declaraciones) {
            if (declObject(ObjectDecl objeto) := declaracion) {
                entorno.eval(#value, generarObjeto(objeto, objetos));
            }
        }
        list[MethodAssign] metodos = [
            metodo | Declaration declaracion <- declaraciones, declMethod(MethodAssign metodo) := declaracion
        ];
        for (MethodAssign metodo <- metodos, metodo.objectName notin objetos) {
            throw "Objeto desconocido para el método <metodo.methodName>: <metodo.objectName>.";
        }
        for (str nombre <- objetos, MethodAssign metodo <- metodosDeObjeto(nombre, objetos, metodos)) {
            entorno.eval(#value, generarMetodoEnObjeto(metodo, nombre));
        }
        entorno.eval(
        #value,
        "int objlPotencia(int base, int exponente) {
        '    if (exponente \< 0) throw \"La potencia entera requiere un exponente no negativo.\";
        '    if (exponente == 0) return 1;
        '    int mitad = objlPotencia(base, exponente / 2);
        '    int cuadrado = mitad * mitad;
        '    return exponente % 2 == 0 ? cuadrado : base * cuadrado;
        '}"
    );
        int cantidad = size(instrucciones);

        for (int indice <- [0..cantidad]) {
            Statement instruccion = instrucciones[indice];

            if (stmtExp(Expression expresion) := instruccion) {
                value resultado = entorno.eval(#value, generarExpresion(expresion) + ";").val;
                if (indice == cantidad - 1) { println(resultado); }
            }
            else {
                entorno.eval(#value, generarDeclaracion(instruccion));
            }
        }
    }
    else {
        throw "La ejecución de objetos y métodos todavía está pendiente.";
    }
}
