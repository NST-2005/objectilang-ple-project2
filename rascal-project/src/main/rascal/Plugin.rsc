module Plugin
import util::LanguageServer;
import util::Reflective;
import ParseTree;
import Syntax;

private Tree  objectilangParser(str input,loc origin) {
    return parse(#start[Program], input, origin);
}

set[LanguageService] objectilangServices() = {
    parsing(objectilangParser)
};

void main() {
    pcfg = getProjectPathConfig(|project://rascal-project|, mode=interpreter_external());
    unregisterLanguage("Objectilang", {"objl"});
    registerLanguage(
        language(
            pcfg,
            "Objectilang",
            {"objl"},
            "Plugin",
            "objectilangServices"
        )
    );
}
