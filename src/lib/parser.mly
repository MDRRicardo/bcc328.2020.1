// parser.mly

%token                 EOF
%token <int>           LITINT
%token <Symbol.symbol> ID
%token                 PLUS
%token                 LT
%token                 EQ
%token                 COMMA
%token                 LPAREN
%token                 RPAREN
%token                 INT
%token                 BOOL
%token                 IF
%token                 THEN
%token                 ELSE
%token                 LET
%token                 IN

%start <Absyn.funs> program

%nonassoc IN ELSE
%nonassoc LT
%left PLUS

%%
program:
| x=funs EOF {x}

funs:
|x=nonempty_list(fundec) { x }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc , (x, p, b) }

typeid:
| INT x=ID   { (Absyn.Int, x) }
| BOOL x= ID { (Absyn.Bool, x) }

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }

exp:
| x=LITINT                          { $loc , Absyn.IntExp x }
| x=ID                              { $loc,  Absyn.IdExp x}
| y=ID LPAREN x=exps RPAREN         { $loc,  Absyn.IdFunctionExp (y,x)}
| IF x=exp THEN x1=exp ELSE x2=exp  {$loc,   Absyn.IfExp (x,x1,x2)}
| LET x=ID EQ y=exp IN z=exp        { $loc,  Absyn.LetExp (x,y,z)}
| x=exp op=operator y=exp           { $loc,  Absyn.OpExp (op, x, y) }

exps:
| x=separated_nonempty_list(COMMA, exp) { x }

%inline operator:
| PLUS { Absyn.Plus }
| LT   { Absyn.LT }
