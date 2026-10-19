module Parser

import Syntax;
import ParseTree;

public start[Program] parseObjectilang(str src, loc origin)
    = parse(#start[Program], src, origin);

public start[Program] parseObjectilang(loc origin)
    = parse(#start[Program], origin);