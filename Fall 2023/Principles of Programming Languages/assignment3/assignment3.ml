open List
open Ast
open ExpressionLibrary

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l

(**********************************)
(* Problem 1: Tree In-order Fold  *)
(**********************************)

let rec fold_inorder f acc t =
  match t with
    | Leaf -> acc
    | Node(l, y, r) ->
      fold_inorder f (f (fold_inorder f acc l) y) r
         
      


(**********************************)
(* Problem 2: BST Remove *)
(**********************************)

let rec remove x t =
  match t with
    | Leaf -> Leaf
    | Node(l, y, r) ->
      if y = x then
        match l, r with
          | Leaf, Leaf -> Leaf
          | _, Leaf -> l
          | Leaf, _ -> r
          | _, _ ->
            let rec inorder_successor t =
              match t with
                | Node(Leaf, y, _) -> y
                | Node(l, _, _) -> inorder_successor l
                | Leaf -> -1
            in 
            let min = inorder_successor r in
            Node(l, min, remove min r)
      else if (y > x) then Node(remove x l, y, r) else Node(l, y, remove x r)


(* ------ Type definitions for the abstract syntax tree defined in ast.ml ------- *)

(**********************************
    type binop = Add | Sub | Mul

    type expression =
      | Num of float
      | Var
      | Binop of binop * expression * expression
***********************************)



(**********************************
    There are some functions from expressionLibrary that you can use to debug your code.

    `parse: string -> expression` :
        translates a string in infix form (such as `x*x - 3.0*x + 2.5`) into an expression
        (treating `x` as the variable). The parse function parses according to the standard
        order of operations - so `5+x*8` will be read as `5+(x*8)`.
    `to_string: expression -> string` :
        prints expressions in a readable form, using infix notation. This function adds
        parentheses around every binary operation so that the output is completely unambiguous.
    `to_string_wo_paren: expression -> string` :
        prints expressions in a readable form, using infix notation. This function does not
        add any parentheses so it can only be used for expressions in standard forms.
    `make_exp: int -> expression` :
        takes in a length `l` and returns a randomly generated expression of length at most `2l`.
    `rand_exp_str: int -> string` :
        takes in a length `l` and returns a string representation of length at most `2l`.

    For example,

    let _ =
      (* The following code make an expression from a string
         "5*x*x*x + 4*x*x + 3*x + 2 + 1", and then convert the
         expression back to a string, finally it prints out the
         converted string
         *)
      let e = parse ("5*x*x*x + 4*x*x + 3*x + 2 + 1") in
      let s = to_string e in
      print_string (s^"\n")

    let _ =
      (* The following code make a random expression from a string
         and then convert the expression back to a string
         finally it prints out the converted string
         *)
      let e = make_exp 10 in
      let s = to_string e in
      print_string (s^"\n")
***********************************)




(**********************************)
(* Problem 3: Evaluation  *)
(**********************************)

(* evaluate : evaluates an expression for a particular value of x.
*  Example : evaluate (parse "x*x + 3") 2.0 = 7.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
    | Num(n) -> n
    | Var -> x
    | Binop(binop, e1, e2) ->
      match binop with
        | Add -> evaluate e1 x +. evaluate e2 x
        | Sub -> evaluate e1 x -. evaluate e2 x
        | Mul -> evaluate e1 x *. evaluate e2 x



(**********************************)
(* Problem 4: Derivatives  *)
(**********************************)

let rec derivative (e:expression) : expression =
  match e with
    | Num(n) -> Num(0.)
    | Var -> Num(1.)
    | Binop(binop, e1, e2) ->
      match binop with
      | Add -> Binop(Add, derivative e1, derivative e2)
      | Sub -> Binop(Sub, derivative e1, derivative e2)
      | Mul -> Binop(Add, Binop(Mul, derivative e1, e2), Binop(Mul, e1, derivative e2))


(**********************************)
(* Problem 5: Find Zero  *)
(**********************************)

let find_zero (e:expression) (xn:float) (epsilon:float) (lim:int)
  : float option =
  let d = derivative e in
  let rec helper xn epsilon lim =
    if lim = 0 then None else
    let guess = evaluate e xn in
    if abs_float (guess) < epsilon then Some xn else
    let new_xn = xn -. (guess /. evaluate d xn) in
    helper new_xn epsilon (lim - 1)
  in
  helper xn epsilon (lim + 1) 

(**********************************)
(* Problem 6: Simplification  *)
(**********************************)

let simplify (e:expression) : expression =
  let insert (coefficient, degree) l =
    let new_list, found = List.fold_left (fun (acc, found) (c, d) ->
      if d = degree then ((c +. coefficient, d) :: acc, true) else ((c, d) :: acc, found))
      ([], false) l in
    if found then new_list else (coefficient, degree) :: new_list
  in

  let rec flatten e acc is_pos =
    match e with
    | Num(n) -> if is_pos then insert (n, 0) acc else insert(-.n, 0) acc
    | Var -> if is_pos then insert(1., 1) acc else insert(-.1., 1) acc
    | Binop(binop, e1, e2) ->
        match binop with
        | Add -> flatten e1 (flatten e2 acc is_pos) is_pos
        | Sub -> flatten e1 (flatten e2 acc (not is_pos)) is_pos
        | Mul ->
            let l1 = flatten e1 [] true in 
            let l2 = flatten e2 [] is_pos in
            List.fold_left (fun acc1 (c1, d1) ->
              List.fold_left (fun acc2 (c2, d2) -> insert(c1 *. c2, d1 + d2) acc2) acc1 l2) acc l1
  in

  let sorted_list = List.sort (fun (c1, d1) (c2, d2) -> (Stdlib.compare d2 d1)) (flatten e [] true) in

  let rec expand_degree degree =
    if (degree = 1) then Var else Binop(Mul, Var, expand_degree (degree - 1))
  in
    
  let tuple_to_expression (coefficient, degree: float * int) =
    match (coefficient, degree) with
    | (0., _) -> None
    | (_, 0) -> Some (Num(coefficient))
    | (_, _) -> Some (Binop(Mul, Num(coefficient), expand_degree degree))
  in
  
let final_expr = 
    match sorted_list with
    | [] -> Num(0.)
    | (x, y) :: xs ->
        let initial_expr = 
          match tuple_to_expression(x, y) with
          | None -> Num(0.)
          | Some expr -> expr
        in
        List.fold_left (fun acc (x, y) -> 
          match tuple_to_expression(x, y) with
          | None -> acc
          | Some expr -> 
            if acc = Num(0.) then expr
            else Binop(Add, acc, expr)
        ) initial_expr xs

        in
  (*print_endline (to_string_wo_paren final_expr);*)
  final_expr
  





(*****************************************)
(* Problem 7: Automatic Differentiation *)
(*****************************************)

(*

"Forward mode automatic differentiation", has become an
important algorithm (since 2017 or so) in deep learning.
You can read about it in section 3.1 of this paper:
http://jmlr.org/papers/volume18/17-468/17-468.pdf
"Automatic Differentiation in Machine Learning: A Survey"
(and pay particular attention to Table 2 for a worked example).

So, the challenge (which is actually not very difficult) is,
write this function

 let evaluate2 (e: expression) (x: float) : float * float = ...

that computes both e(x) and the first derivative e'(x),
without ever calculating (derivative e).  Like evaluate,
do it by case analysis on the syntax-tree of e.

*)

let rec evaluate2 (e: expression) (x: float) : float * float =
  match e with
    | Var -> (x, 1.)
    | Num(n) -> (n, 0.) 
    | Binop(bitop, e1, e2) ->
        let (n1, d1) = evaluate2 e1 x in
        let (n2, d2) = evaluate2 e2 x in
        match bitop with
          | Add -> (n1 +. n2, d1 +. d2)
          | Sub -> (n1 -. n2, d1 -. d2)
          | Mul -> (n1 *. n2, d1 *. n2 +. n1 *. d2)



(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in
  let bonus_count = ref 1 in

 (* Testcases for fold_inorder *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6);
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Node (Leaf,1,Leaf), 2, Leaf), 3, Leaf)) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Node (Leaf,1,Leaf), 2, Leaf), 3, Leaf)) = 6);
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Leaf, 1, Node (Leaf, 2, Node (Leaf,3,Leaf)))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Leaf, 1, Node (Leaf, 2, Node (Leaf,3,Leaf)))) = 6);
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Node (Leaf,2,Leaf)), 3, Node (Leaf,4,Leaf))) = [1;2;3;4]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Node (Leaf,2,Leaf)), 3, Node (Leaf,4,Leaf))) = 10);
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Leaf,1,Leaf)) = [1]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Leaf,1,Leaf)) = 1);
      assert (fold_inorder (fun acc x -> acc @ [x]) [] Leaf = []);
      assert (fold_inorder (fun acc x -> acc + x) 0 Leaf = 0);
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Node (Leaf,2,Leaf)), 3, Node (Node (Leaf,4,Leaf),5,Leaf))) = [1;2;3;4;5]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Node (Leaf,2,Leaf)), 3, Node (Node (Leaf,4,Leaf),5,Leaf))) = 15)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for remove *)
  let _ =
    try
      assert (remove 20 (Node (Node (Node (Leaf, 20, Leaf), 30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
      assert (remove 30 (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
      assert (remove 50 (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf,                  40, Leaf                 ), 60, Node (Leaf,                  70, Node (Leaf, 80, Leaf)))));
      assert (remove 70 (Node (Node (Leaf, 40, Leaf), 60, Node (Leaf, 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf, 40, Leaf), 60, Node (Leaf, 80, Leaf))));
      assert (remove 60 (Node (Node (Leaf, 40, Leaf), 60, Node (Leaf, 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf, 40, Leaf), 70, Node (Leaf, 80, Leaf))));
      assert (remove 40 (Node (Node (Leaf, 20, Leaf), 40, Node (Leaf, 50, Node (Leaf, 60, Leaf))))
              = (Node (Node (Leaf, 20, Leaf), 50, Node (Leaf, 60, Leaf))));
      assert (remove 90 (Node (Node (Leaf, 20, Leaf), 40, Node (Leaf, 50, Node (Leaf, 60, Leaf))))
              = (Node (Node (Leaf, 20, Leaf), 40, Node (Leaf, 50, Node (Leaf, 60, Leaf)))));
      assert (remove 40 (Node (Node (Leaf, 20, Leaf), 40, Leaf))
              = (Node (Leaf, 20, Leaf)));
      assert (remove 40 (Node (Leaf, 40, Node (Leaf, 50, Leaf)))
              = (Node (Leaf, 50, Leaf)));
      assert (remove 40 (Node (Node (Leaf, 20, Leaf), 40, Node (Leaf, 50, Leaf)))
              = (Node (Node (Leaf, 20, Leaf), 50, Leaf)));
      assert (remove 80 (Node (Node (Leaf, 40, Leaf), 60, Node (Leaf, 70, Node (Leaf, 80, Leaf))))
              = (Node (Node (Leaf, 40, Leaf), 60, Node (Leaf, 70, Leaf))))
      
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for evaluate *)
  let _ =
    try
      assert (evaluate (parse "x*x + 3.0") 2.0 = 7.0);
      assert (evaluate (parse "x + 3.0") 2.0 = 5.0);
      assert (evaluate (parse "x - 3.0") 2.0 = -1.0);
      assert (evaluate (parse "x * 3.0") 2.0 = 6.0);
      assert (evaluate (parse "x*x*x + x*x + x + 1.0") 2.0 = 15.0);
      assert (evaluate (parse "3.0*x*x - 2.0*x + 4.0") 3.0 = 25.0);
      assert (evaluate (parse "3.0 + 2.0") 2.0 = 5.0);
      assert (evaluate (parse "3.0 - 2.0") 2.0 = 1.0);
      assert (evaluate (parse "3.0 * 2.0") 2.0 = 6.0);
      assert (evaluate (parse "3.0 * 0.5") 2.0 = 1.5);
      assert (evaluate (parse "x + x + x") 2.0 = 6.0);
      assert (evaluate (parse "x*x + x*x") 2.0 = 8.0);
      assert (evaluate (parse "(x + 2.0) * (x - 3.0)") 2.0 = -4.0);
      assert (evaluate (parse "(x*x + 2.0*x + 1.0) * (x - 1.0)") 2.0 = 9.0);
      assert (evaluate (parse "x * 0.0") 2.0 = 0.0);
      assert (evaluate (parse "0.0 * x") 2.0 = 0.0);
      assert (evaluate (parse "x + 0.0") 2.0 = 2.0);
      assert (evaluate (parse "0.0 + x") 2.0 = 2.0);
      assert (evaluate (parse "x*(0. - 1.)") 2.0 = -2.0);
      assert (evaluate (parse "x*x*(0. - 1.)") 2.0 = -4.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for derivative *)
  let _ =
    try
      assert (evaluate (derivative (parse "x*x + 3.0")) 2.0 = 4.0);
      assert (evaluate (derivative (parse "x")) 2.0 = 1.0);
      assert (evaluate (derivative (parse "3.0")) 2.0 = 0.0);
      assert (evaluate (derivative (parse "x*x")) 2.0 = 4.0);
      assert (evaluate (derivative (parse "x*x*x")) 2.0 = 12.0);
      assert (evaluate (derivative (parse "3.0*x*x - 2.0*x + 4.0")) 3.0 = 16.0);
      assert (evaluate (derivative (parse "3.0 + 2.0")) 2.0 = 0.0);
      assert (evaluate (derivative (parse "x + x + x")) 2.0 = 3.0);
      assert (evaluate (derivative (parse "x*x + x*x")) 2.0 = 8.0);
      assert (evaluate (derivative (parse "(x + 2.0) * (x - 3.0)")) 2.0 = 3.0);
      assert (evaluate (derivative (parse "(x*x + 2.0*x + 1.0) * (x - 1.0)")) 2.0 = 15.0);
      assert (evaluate (derivative (parse "x * 0.0")) 2.0 = 0.0);
      assert (evaluate (derivative (parse "0.0 * x")) 2.0 = 0.0);
      assert (evaluate (derivative (parse "x * (0. - 1.0)")) 2.0 = -1.0);
      assert (evaluate (derivative (parse "x*(0. - 1.0)")) 2.0 = -1.0);
      assert (evaluate (derivative (parse "x*x*(0. - 1.0)")) 2.0 = -4.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for zero finding *)
  let _ =
    try
      let e = (parse "2*x*x - x*x*x - 2") in
      let g, epsilon, lim = 3.0, 1e-3, 50 in
      let x = find_zero e g epsilon lim in
      match x with
      | None -> assert false
      | Some x ->
          let eval_result = evaluate e x in
          assert (0. -. epsilon < eval_result && eval_result < epsilon)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for simplify *)
  let _ =
    try
      (*print_string (to_string_wo_paren (simplify (parse "3*x*x + 8*x + 2*x*x - 5 - 5*x")));
       print_string (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)")));
       print_string (to_string_wo_paren (simplify (parse "x - x")));
      print_string (to_string_wo_paren (simplify (parse "x + x + 0")));
      print_string (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)*(4*x*x*x-11+66*x)")));*)
      assert (to_string_wo_paren (simplify (parse "x - x - x")) = "-1.*x");
      assert (to_string_wo_paren (simplify (parse "x")) = "1.*x");
      assert (to_string_wo_paren (simplify (parse "0.5*x + 0.5*x")) = "1.*x");
      assert (to_string_wo_paren (simplify (parse "x*0")) = "0.");
      assert (to_string_wo_paren (simplify (parse "x*0 + 5")) = "5.");
      assert (to_string_wo_paren (simplify (parse "0*x*x + 0*x - 0 + 0*x*x - 0*x")) = "0.");
      assert (to_string_wo_paren (simplify (parse "0*x*x - 2*x - 0 + 4*x*x - 0*x")) = "4.*x*x+-2.*x");
      assert (to_string_wo_paren (simplify (parse "(x + x) * (x + x)")) = "4.*x*x");
      assert (to_string_wo_paren (simplify (parse "x*x*x + x*x*x + x*x - x*x")) = "2.*x*x*x");
      assert (to_string_wo_paren (simplify (parse "3*x*x + 2*x - 5 + 4*x*x - 7*x")) = "7.*x*x+-5.*x+-5.");
      assert (to_string_wo_paren (simplify (parse "3*x*x + 8*x + 2*x*x - 5 - 13*x")) = "5.*x*x+-5.*x+-5.");
      assert (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)")) = "1.*x*x*x+-6.*x*x+5.*x");
      assert (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)*(4*x*x*x-11+66*x)")) = "4.*x*x*x*x*x*x+-24.*x*x*x*x*x+86.*x*x*x*x+-407.*x*x*x+396.*x*x+-55.*x");
      assert (to_string_wo_paren (simplify (parse "x - x")) = "0.");
      assert (to_string_wo_paren (simplify (parse "x + x + 0")) = "2.*x"); (* ORIGINALLY 2.*x+0.*)
      assert (to_string_wo_paren (simplify (parse "0")) = "0.");
      assert (to_string_wo_paren (simplify (parse "0+0+0*0+0+0+0")) = "0.");
      assert (to_string_wo_paren (simplify (parse "0*x*x+3+2+5+5*x")) = "5.*x+10.");
      assert (to_string_wo_paren (simplify (parse "x*x - x*x + x - x")) = "0.");
      assert (to_string_wo_paren (simplify (parse "1000*x + 1000*x")) = "2000.*x");
      assert (to_string_wo_paren (simplify (parse "0.1*x + 0.2*x")) = "0.3*x");
      assert (to_string_wo_paren (simplify (parse "(x + (x + (x + (x + x))))")) = "5.*x");
      assert (to_string_wo_paren (simplify (parse "x*x + x - x*x + 2*x - x")) = "2.*x");
      assert (to_string_wo_paren (simplify (parse "x + x + x + x + x - x - x - x")) = "2.*x");
      assert (to_string_wo_paren (simplify (parse "0*(x + x + x + x + x)")) = "0.");
      assert (to_string_wo_paren (simplify (parse "(x + x)*(x + x)*(x + x)")) = "8.*x*x*x");
      assert (to_string_wo_paren (simplify (parse "x*x*x - x*x")) = "1.*x*x*x+-1.*x*x");
      assert (to_string_wo_paren (simplify (parse "x*(x*(x*(x*(x*x))))")) = "1.*x*x*x*x*x*x")
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

 (* Testcases for evaluate2 *)
  let _ =
    try
      assert (evaluate2 (parse "x*x + 3") 2.0 = (7.0, 4.0))
    with e -> (bonus_count := !bonus_count - 1; print_string ((Printexc.to_string e)^"\n")) in

  if !error_count = 0 then Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 6 programming questions are incorrect.\n") (!error_count);

  if !bonus_count = 0 then Printf.printf ("The bonus problem is not solved.\n")
  else Printf.printf ("The bonus problem is solved.\n")

let _ = main()
