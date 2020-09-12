(*
   lisp-interpreter.ml
   kagan019
   for James Moen's CSCI 2041
*)

(* THING. A Lisp object. *)

type
  thing =
    Closure of thing * thing * environment ref |
    Cons of thing * thing |
    Nil |
    Number of int |
    Symbol of string
and
  environment = (string * thing) list ;;



(*

          SCAN

*)


(* TOKEN. A token. *)

type token =
  CloseParenToken |
  EndToken |
  NumberToken of int |
  OpenParenToken |
  SymbolToken of string ;;


(* MAKE SCANNER. Return a function NEXT TOKEN that reads TOKENs from a file
   with pathname PATH. OCaml RAISEs an exception if there's no such file. *)

let makeScanner path =

(* INPUT. Read chars from this channel. *)

  let input = (open_in path)
  in

(* CH. The char most recently read from INPUT. *)

  let ch = ref ' '
  in

(* NEXT CHAR. Advance CH to the next char from INPUT, or to '\000' if we're at
   the end of INPUT. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file -> ch := '\000'
  in


  let nextEndToken () =
    EndToken
  in
  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in
  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in
  let rec nextNumberToken prefix =
    match !ch with
    | '0' -> 
      nextChar () ; nextNumberToken (prefix ^ "0")
    | '1' -> 
      nextChar () ; nextNumberToken (prefix ^ "1")
    | '2' -> 
      nextChar () ; nextNumberToken (prefix ^ "2")
    | '3' -> 
      nextChar () ; nextNumberToken (prefix ^ "3")
    | '4' -> 
      nextChar () ; nextNumberToken (prefix ^ "4")
    | '5' -> 
      nextChar () ; nextNumberToken (prefix ^ "5")
    | '6' -> 
      nextChar () ; nextNumberToken (prefix ^ "6")
    | '7' -> 
      nextChar () ; nextNumberToken (prefix ^ "7")
    | '8' -> 
      nextChar () ; nextNumberToken (prefix ^ "8")
    | '9' -> 
      nextChar () ; nextNumberToken (prefix ^ "9")
    | _ ->
      NumberToken (int_of_string prefix)
  in
  let rec nextSymbolToken prefix =
    let symtok =
      ( if prefix = "end"
      then (nextEndToken ())
      else SymbolToken prefix )
    in match !ch with
    | ' ' -> symtok
    | '\n' -> symtok
    | '\r' -> symtok
    | '\000' -> symtok
    | '(' -> symtok
    | ')' -> symtok
    | a -> 
      nextChar () ; nextSymbolToken (prefix ^ (Char.escaped a))
  in
  let nextNumberOrSymbolToken prefix =
    match !ch with
    | '-' ->
      ( nextChar () ;
      match !ch with
      | '0' -> nextNumberToken "-"
      | '1' -> nextNumberToken "-"
      | '2' -> nextNumberToken "-"
      | '3' -> nextNumberToken "-"
      | '4' -> nextNumberToken "-"
      | '5' -> nextNumberToken "-"
      | '6' -> nextNumberToken "-"
      | '7' -> nextNumberToken "-"
      | '8' -> nextNumberToken "-"
      | '9' -> nextNumberToken "-" 
      | _ -> nextSymbolToken "-" )
    | '0' -> nextNumberToken ""
    | '1' -> nextNumberToken ""
    | '2' -> nextNumberToken ""
    | '3' -> nextNumberToken ""
    | '4' -> nextNumberToken ""
    | '5' -> nextNumberToken ""
    | '6' -> nextNumberToken ""
    | '7' -> nextNumberToken ""
    | '8' -> nextNumberToken ""
    | '9' -> nextNumberToken ""
    | _ -> nextSymbolToken ""
  in
  let rec nextToken () =
    match !ch with
    | ' ' -> nextChar () ; nextToken ()
    | '\n' -> nextChar () ; nextToken ()
    | '\r' -> nextChar () ; nextToken ()
    | '\000' -> nextEndToken ()
    | '(' -> nextOpenParenToken ()
    | ')' -> nextCloseParenToken ()
    | _ -> nextNumberOrSymbolToken ""
    
(* Finally initialize CH, and return NEXT TOKEN as promised. *)

  in nextChar () ;
     nextToken ;;



(*

   PARSE. An OCaml function that acts as a parser for Pure Lisp.

*)


(* CAN'T PARSE. Raised if the parser fails. *)

exception Can'tParse of string ;;

(* MAKE PARSER. Return a parser that reads THINGs from the file whose pathname
   is PATH. OCaml raises an exception if there's no such file. *)

let makeParser path =
  let nextToken = makeScanner path

(* NEXT THING. Read the THING whose first token is TOKEN, and return it. *)

  in let rec nextThing token =

(* NEXT THINGS. Read a series of THINGs as a Pure Lisp list, and return that
   list. *)

       let rec nextThings token =
         match token
         with CloseParenToken ->
                Nil |

              EndToken ->
                raise (Can'tParse "Unexpected end of file.") |

              _ ->
                let first = nextThing token
                in let rest = nextThings (nextToken ())
                   in Cons (first, rest)

(* This is NEXT THINGS's body. *)

       in match token
          with CloseParenToken ->
                 raise (Can'tParse "Unexpected close parenthesis.") |

               EndToken ->
                 raise (Can'tParse "Unexpected end of file.") |

               NumberToken integer ->
                 Number integer |

               OpenParenToken ->
                 nextThings (nextToken ()) |

               SymbolToken string ->
                 Symbol string

(* This is NEXT THING's body. *)

     in (fun () -> nextThing (nextToken ())) ;;


(*

        EVAL

*)

(* CAN'T EVALUATE. Raise this if EVALUATE gets something bad. The string tells
   where the bad thing was detected. *)

exception Can'tEvaluate of string;;

(* TEE. NIL means FALSE. Anything else means TRUE, so TEE means TRUE too. *)

let tee = Symbol "t" ;;

(* GLOBAL. The global environment. It's a variable so we can define functions
   in arbitrary order. *)

let global =
  ref
    [("nil", Nil) ;
     ("t",   tee)] ;;

(* EVALUATE PRODUCT. Return LEFT times RIGHT. *)

let evaluateProduct left right =
  match (left, right)
  with (Number left, Number right) -> Number (left * right) |
       _                           -> raise (Can'tEvaluate "*") ;;

(* EVALUATE SUM. Return LEFT plus RIGHT. *)

let evaluateSum left right =
  match (left, right)
  with (Number left, Number right) -> Number (left + right) |
       _                           -> raise (Can'tEvaluate "+") ;;

(* EVALUATE DIFFERENCE. Return LEFT minus RIGHT. *)

let evaluateDifference left right =
  match (left, right)
  with (Number left, Number right) -> Number (left - right) |
       _                           -> raise (Can'tEvaluate "-") ;;

(* EVALUATE QUOTIENT. Return LEFT divided by RIGHT. We can't divide by 0. *)

let evaluateQuotient left right =
  match (left, right)
  with (Number _, Number 0)        -> raise (Can'tEvaluate "/") |
       (Number left, Number right) -> Number (left / right) |
       _                           -> raise (Can'tEvaluate "/") ;;

(* EVALUATE LESS. Test if LEFT is less than RIGHT. *)

let evaluateLess left right =
  match (left, right)
  with (Number left, Number right) -> if left < right then tee else Nil |
       _                           -> raise (Can'tEvaluate "<") ;;

(* EVALUATE EQUAL. Test if an atom LEFT equals an atom RIGHT. *)

let evaluateEqual left right =
  match (left, right)
  with (Nil,         Nil         ) -> tee |
       (Number left, Number right) -> if left = right then tee else Nil |
       (Symbol left, Symbol right) -> if left = right then tee else Nil |
       _                           -> Nil ;;

(* EVALUATE GREATER. Test if LEFT is greater than RIGHT. *)

let evaluateGreater left right =
  match (left, right)
  with (Number left, Number right) -> if left > right then tee else Nil |
       _                           -> raise (Can'tEvaluate ">") ;;

(* EVALUATE ATOM. Test if RIGHT is NIL, a NUMBER, or a SYMBOL. *)

let evaluateAtom right =
  match right
  with Nil      -> tee |
       Number _ -> tee |
       Symbol _ -> tee |
       _        -> Nil ;;

(* EVALUATE CAR. Return the first element of the list RIGHT. *)

let evaluateCar right =
  match right
  with Cons (left, _) -> left |
       _              -> raise (Can'tEvaluate "car") ;;

(* EVALUATE CDR. Return all but the first element of the list RIGHT. *)

let evaluateCdr right =
  match right
  with Cons (_, right) -> right |
       _               -> raise (Can'tEvaluate "cdr") ;;

(* EVALUATE CONS. Return a list whose first element is LEFT, and whose other
   elements are in the list RIGHT. *)

let evaluateCons left right =
  match right
  with Cons (_, _) -> Cons (left, right) |
       Nil         -> Cons (left, Nil) |
       _           -> raise (Can'tEvaluate "cons") ;;

(* EVALUATE DEFINE. Bind symbol LEFT to RIGHT in the GLOBAL environment. *)

let evaluateDefine left right =
  match left
  with Symbol name -> global := (name, right) :: ! global ; left |
       _           -> raise (Can'tEvaluate "define") ;;

(* EVALUATE LAMBDA. Return a closure for a function with PARAMETERS, BODY, and
   ENVIRONMENT. *)

let evaluateLambda parameters body environment =
  if environment == ! global
  then Closure (parameters, body, global)
  else Closure (parameters, body, ref environment) ;;

(* EVALUATE SYMBOL. Return the binding of string NAME in ENVIRONMENT. NAME is
   from a SYMBOL. *)

let evaluateSymbol name environment =

  let rec evaluatingSymbol environment =
    match environment
    with [] ->
           raise (Can'tEvaluate name) |

         (otherName, otherThing) :: otherEnvironment ->
           if name = otherName
           then otherThing
           else evaluatingSymbol otherEnvironment

  in evaluatingSymbol environment ;;

(* EVALUATE. Evaluate EXPRESSION in ENVIRONMENT. *)

let rec evaluate expression environment =

(* EVALUATING. Evaluate EXPRESSION. We dispatch to code that handles all these
   expressions:

   (âˆ— Î± Î²)              Return Î± times Î².
   (+ Î± Î²)              Return Î± plus Î².
   (âˆ’ Î± Î²)              Return Î± minus Î².
   (/ Î± Î²)              Return Î± divided by Î².
   (< Î± Î²)              Test if Î± is less than Î².
   (= Î± Î²)              Test if the atom Î± equals the atom Î².
   (> Î± Î²)              Test if Î± is greater than Î².
   (ATOM Î±)             Test if Î± is an atom.
   (DEFINE Î± Î²)         Define Î± to be Î² in the global environment.
   (CAR Î±)              Return the first element of the list Î±.
   (CDR Î±)              Return all but the first element of the list Î±.
   (CONS Î± Î²)           Return a list whose CAR is Î± and whose CDR is Î².
   (IF Î± Î² Î³)           If Î± = NIL then evaluate Î³, otherwise evaluate Î².
   (LAMBDA Î± Î²)         Return a function closure with parameters Î± and body Î².
   (LIST Î±â‚ Î±â‚‚ ... Î±â±¼)  Return a list whose elements are Î±â‚, Î±â‚‚ ..., Î±â±¼.
   (Î» Ïƒ Î²)              A synonym for LAMBDA Î± Î².
   (QUOTE Î±)            Return Î± without evaluating it.
   (Î± Î²â‚ Î²â‚‚ ... Î²â±¼)     Apply closure Î± to arguments Î²â‚, Î²â‚‚ ..., Î²â±¼.

   We also handle NIL's, NUMBER's and SYMBOL's here.
*)

  let rec evaluating expression =
    match expression
    with Cons (Symbol "*", Cons (left, Cons (right, Nil))) ->
           evaluateProduct
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "+", Cons (left, Cons (right, Nil))) ->
           evaluateSum
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "-", Cons (left, Cons (right, Nil))) ->
           evaluateDifference
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "/", Cons (left, Cons (right, Nil))) ->
           evaluateQuotient
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "<", Cons (left, Cons (right, Nil))) ->
           evaluateLess
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "=", Cons (left, Cons(right, Nil))) ->
           evaluateEqual
             (evaluating left)
             (evaluating right) |

         Cons (Symbol ">", Cons (left, Cons(right, Nil))) ->
           evaluateGreater
             (evaluating left)
             (evaluating right) |

         Cons (Symbol "atom", Cons (right, Nil)) ->
           evaluateAtom (evaluating right) |

         Cons (Symbol "car", Cons (right, Nil)) ->
           evaluateCar
             (evaluating right) |

         Cons (Symbol "cdr", Cons (right, Nil)) ->
           evaluateCdr
             (evaluating right) |

         Cons (Symbol "cons", Cons (left, Cons (right, Nil))) ->
           evaluateCons
             (evaluating left)
             (evaluating right) |

         Cons(Symbol "define", Cons (left, Cons (right, Nil))) ->
           evaluateDefine
             left
             (evaluate right ! global) |

         Cons (Symbol "if", Cons (test, Cons (left, Cons (right, Nil)))) ->
           if evaluating test = Nil
           then evaluating right
           else evaluating left |

         Cons (Symbol "lambda", Cons (parameters, Cons (body, Nil))) ->
           evaluateLambda
             parameters
             body
             environment |

         Cons (Symbol "Î»", Cons (parameters, Cons (body, Nil))) ->
           evaluateLambda
             parameters
             body
             environment |

         Cons (Symbol "list", rights) ->
           let rec evaluateList rights =
             match rights
             with Nil ->
                    Nil |
                  Cons (first, rest) ->
                    Cons (evaluating first, evaluateList rest) |
                  _ ->
                    raise (Can'tEvaluate "list")
           in evaluateList rights |           

         Cons (Symbol "quote", Cons (thing, Nil)) ->
           thing |

         Cons (procedure, arguments) ->
           apply
             (evaluating procedure)
             arguments |

         Nil ->
           Nil |

         Number _ ->
           expression |

         Symbol string ->
           evaluateSymbol string environment |

         _ ->
           raise (Can'tEvaluate "evaluate")

(* APPLY. Apply CLOSURE to its ARGUMENTS. *)

  and apply closure arguments =
    match closure
    with Closure (parameters, body, environment) ->
           let rec applying environment parameters arguments =
             match (parameters, arguments)
             with (Nil, Nil) ->
                    evaluate body environment |

                  (Nil, Cons(_, _)) ->
                    raise (Can'tEvaluate "apply") |

                  (Cons(_, _), Nil) ->
                    raise (Can'tEvaluate "apply") |

                  (Cons (Symbol parameter, otherParameters),
                   Cons (argument, otherArguments)) ->
                     applying
                       ((parameter, evaluating argument) :: environment)
                       otherParameters
                       otherArguments |

                  _ ->
                    raise (Can'tEvaluate "apply")

           in applying ! environment parameters arguments |

         _ ->
           raise (Can'tEvaluate "apply")

(* This is EVALUATE's body. *)

  in evaluating expression ;;

(* EVAL. Evaluate EXPRESSION in the GLOBAL environment. *)

let eval expression =
  evaluate expression ! global ;;



(*

          PRINT

*)


(* Define the function PRINTF, among other things. *)

open Printf ;;


let rec printingThing thing =
  match thing with
  | Closure (x,y,e) -> 
    printf "%s" "[closure]"; ()
  | Cons (x,y) ->
    printf "(";
    printingThings thing; 
    printf ")";
    ()
  | Nil ->
    printf "%s" "nil"; ()
  | Number x ->
    printf "%i" x; ()
  | Symbol x ->
    printf "%s" x; ()
and printingThings things = 
  match things with
  | Cons (x, y) ->
    printingThing x;
    ( match y with
    | Cons (x2,y2) ->
      printf " ";
      printingThings y; ()
    | Nil ->
      ()
    | _ ->
      printingThing y; () )
  | _ -> 
    ()
;;

let printThing thing =
  printingThing thing;
  printf "\n";
  () 
;;



(*

          REPL

*)


let lisp pathname = 
  let nextThing = makeParser pathname
  
  in let rec lisping thing = 
    printThing thing;
    lisping ( eval ( nextThing () ) )
    
  in try
    lisping (eval (nextThing ()))
  with
    | Can'tParse s -> ()
    | Can'tEvaluate a ->
      printf "%s" ("can't evaluate `" ^ a ^ "`")
  ;;

lisp "tests123.txt"


(* OUTPUT `ocaml lisp-interpreter.ml`
solve
solving
solving-add
solving-subtract
solving-multiply
solving-divide
is-inside
operator
left
right
t
nil
t
nil
nil
t
(= x (- c a))
(= x (- c b))
(= x (- a c))
(= x (+ b c))
(= x (/ c a))
(= x (/ c b))
(= x (/ a c))
(= x (* b c))
(= y (+ (* m x) b))
(= x (/ (- y b) m))
(= a (* e (+ f (/ (+ b c) d))))
*)
