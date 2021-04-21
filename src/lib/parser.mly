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

%start <Absyn.program> program

%nonassoc ELSE IN
%nonassoc LT
%left PLUS

%%

program:
| x=nonempty_list(fundec) EOF { x }

exp:
| x=LITINT                { $loc , Absyn.IntExp x }
| x=ID                    { $loc , Absyn.VarExp x }
| x=exp op=operator y=exp { $loc , Absyn.OpExp (op, x, y) }
| IF t=exp THEN x=exp ELSE y=exp { $loc , Absyn.IfExp (t, x, y) }
| f=ID LPAREN a=exps RPAREN { $loc , Absyn.CallExp (f, a) }
| LET x=ID EQ i=exp IN b=exp { $loc , Absyn.LetExp (x, i, b) }

%inline operator:
| PLUS { Absyn.Plus }
| LT   { Absyn.LT }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc , (x, p, b) }

typeid:
| INT x=ID   { (Absyn.Int, x) }
| BOOL x= ID { (Absyn.Bool, x) }

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }

exps:
| x=separated_nonempty_list(COMMA, exp) { x }
