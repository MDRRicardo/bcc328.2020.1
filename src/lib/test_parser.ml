(* Test syntax analyser *)

module L = Lexing

let check str =
  let lexbuf = L.from_string str in
  try
    let ast = Parser.program Lexer.token lexbuf in
    let tree = Absyntree.flat_nodes (Absyntree.tree_of_funs ast) in
    let box = Tree.box_of_tree tree in
    Format.printf "%s\n\n%!" (Box.string_of_box box);
  with
  | Parser.Error ->
     Format.printf "%a error: syntax\n%!" Location.pp_position lexbuf.L.lex_curr_p
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s%!" Location.pp_location loc msg

let%expect_test _ =
  (* function declaration and constant expression *)
  check "int f(int x) = 100";
  [%expect{|
                ╭───────╮
                │program│
                ╰────┬──╯
                   ╭───╮
                   │Fun│
                   ╰──┬╯
         ╭───────────┬┴───────────╮
    ╭────┴────╮  ╭───┴───╮  ╭─────┴────╮
    │    f    │  │Formals│  │IntExp 100│
    │Absyn.Int│  ╰───┬───╯  ╰──────────╯
    ╰─────────╯ ╭────┴────╮
                │    x    │
                │Absyn.Int│
                ╰─────────╯ |}];

  check "int f(int x, int y, bool z) = 100";
  [%expect{|
                              ╭───────╮
                              │program│
                              ╰───┬───╯
                                ╭─┴─╮
                                │Fun│
                                ╰─┬─╯
         ╭────────────────────────┴────────────────────────╮
    ╭────┴────╮              ╭────┴──╮               ╭─────┴────╮
    │    f    │              │Formals│               │IntExp 100│
    │Absyn.Int│              ╰────┬──╯               ╰──────────╯
    ╰─────────╯      ╭───────────┬┴───────────╮
                ╭────┴────╮ ╭────┴────╮ ╭─────┴────╮
                │    x    │ │    y    │ │    z     │
                │Absyn.Int│ │Absyn.Int│ │Absyn.Bool│
                ╰─────────╯ ╰─────────╯ ╰──────────╯ |}];

  check "int f() = 100";
  [%expect{| :1.7 error: syntax |}];

  check "foo f(int x) = 100";
  [%expect{| :1.3 error: syntax |}];

  (* binary operators *)
  check "bool f(int x) = 2 + 3 + 4 < 5 + 6";
  [%expect{|
                                       ╭───────╮
                                       │program│
                                       ╰───┬───╯
                                         ╭─┴─╮
                                         │Fun│
                                         ╰─┬─╯
          ╭───────────┬────────────────────┴────────────╮
    ╭─────┴────╮  ╭───┴───╮                        ╭────┴──╮
    │    f     │  │Formals│                        │OpExp <│
    │Absyn.Bool│  ╰───┬───╯                        ╰────┬──╯
    ╰──────────╯ ╭────┴────╮                 ╭──────────┴───────────────╮
                 │    x    │            ╭────┴──╮                   ╭───┴───╮
                 │Absyn.Int│            │OpExp +│                   │OpExp +│
                 ╰─────────╯            ╰────┬──╯                   ╰───┬───╯
                                       ╭─────┴──────────╮          ╭────┴─────╮
                                   ╭───┴───╮       ╭────┴───╮ ╭────┴───╮ ╭────┴───╮
                                   │OpExp +│       │IntExp 4│ │IntExp 5│ │IntExp 6│
                                   ╰───┬───╯       ╰────────╯ ╰────────╯ ╰────────╯
                                  ╭────┴─────╮
                             ╭────┴───╮ ╭────┴───╮
                             │IntExp 2│ │IntExp 3│
                             ╰────────╯ ╰────────╯ |}];

  check "bool f(int x) = 2 < 3 < 4";
  [%expect{| :1.23 error: syntax |}];

   (* Exp -> Exp + Exp *)
   check "int f(int x, int y) = x + y";
   [%expect{|
                           ╭───────╮
                           │program│
                           ╰───┬───╯
                             ╭─┴─╮
                             │Fun│
                             ╰─┬─╯
         ╭─────────────────┬───┴─────────────────╮
    ╭────┴────╮        ╭───┴───╮             ╭───┴───╮
    │    f    │        │Formals│             │OpExp +│
    │Absyn.Int│        ╰───┬───╯             ╰───┬───╯
    ╰─────────╯      ╭─────┴─────╮          ╭────┴────╮
                ╭────┴────╮ ╭────┴────╮ ╭───┴───╮ ╭───┴───╮
                │    x    │ │    y    │ │IdExp x│ │IdExp y│
                │Absyn.Int│ │Absyn.Int│ ╰───────╯ ╰───────╯
                ╰─────────╯ ╰─────────╯
   |}];

   (* Exp -> Exp < Exp *)
   check "bool f(int x) = 50 < 100";
   [%expect{|
                            ╭───────╮
                            │program│
                            ╰───┬───╯
                              ╭─┴─╮
                              │Fun│
                              ╰─┬─╯
              ╭───────────┬─────┴────────────╮
        ╭─────┴────╮  ╭───┴───╮         ╭────┴──╮
        │    f     │  │Formals│         │OpExp <│
        │Absyn.Bool│  ╰───┬───╯         ╰────┬──╯
        ╰──────────╯ ╭────┴────╮      ╭──────┴─────╮
                     │    x    │ ╭────┴────╮ ╭─────┴────╮
                     │Absyn.Int│ │IntExp 50│ │IntExp 100│
                     ╰─────────╯ ╰─────────╯ ╰──────────╯

   |}];

   check "bool f(int x) = 50 < ";
   [%expect{|:2.0 error: syntax |}];

   (* Exp -> if Exp then Exp else Exp *)
    check "int f(int x, int y) = if x then y else c";
      [%expect{|
                               ╭───────╮
                               │program│
                               ╰───┬───╯
                                 ╭─┴─╮
                                 │Fun│
                                 ╰─┬─╯
        ╭─────────────────┬────────┴─────────────────╮
   ╭────┴────╮        ╭───┴───╮                 ╭────┴───╮
   │    f    │        │Formals│                 │IfExp if│
   │Absyn.Int│        ╰───┬───╯                 ╰────┬───╯
   ╰─────────╯      ╭─────┴─────╮          ╭─────────┴─────────╮
               ╭────┴────╮ ╭────┴────╮ ╭───┴───╮ ╭───┴───╮ ╭───┴───╮
               │    x    │ │    y    │ │IdExp x│ │IdExp y│ │IdExp c│
               │Absyn.Int│ │Absyn.Int│ ╰───────╯ ╰───────╯ ╰───────╯
               ╰─────────╯ ╰─────────╯
      |}];

      (* Exp -> id (Exps) *)
      check "int f(int x) = x";
      [%expect{|
                      ╭───────╮
                      │Program│
                      ╰───┬───╯
                        ╭─┴─╮
                        │Fun│
                        ╰─┬─╯
               ╭──────────┴┬──────────╮
          ╭────┴────╮  ╭───┴───╮  ╭───┴───╮
          │    f    │  │Formals│  │IdExp x│
          │Absyn.Int│  ╰───┬───╯  ╰───────╯
          ╰─────────╯ ╭────┴────╮
                      │    x    │
                      │Absyn.Int│
                      ╰─────────╯
      |}];

