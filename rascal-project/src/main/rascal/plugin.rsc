module objectilang::Plugin


import util::LanguageServer;
import util::Reflective;
import ParseTree;
import objectilang::Syntax;

private Tree (str _input, loc _origin) objectilangParser() {
    return ParseTree::parser(#start[Program]);
}

set[LanguageService] objectilangServices() = {
    parsing(objectilangParser())
};

void main() {
    pcfg = getProjectPathConfig(|project://rascal-proyect|, mode=interpreter_external());
    unregisterLanguage("Objectilang", {"objl"});
    registerLanguage(
        language(
            pcfg,
            "Objectilang",
            {"objl"},
            "objectilang::Plugin",
            "objectilangServices"
        )
    );
}
