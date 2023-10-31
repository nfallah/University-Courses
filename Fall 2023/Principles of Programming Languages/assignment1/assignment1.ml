(******************************)
(*** For debugging purposes ***)
(******************************)

(* print out an integer list *)
let rec print_int_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_int x; print_newline ()
  | x :: xs -> print_int x; print_string "; "; print_int_list xs

(* print out a string list *)
let rec print_string_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_string x; print_newline ()
  | x :: xs -> print_string x; print_string "; "; print_string_list xs


(********************)
(* Problem 1: pow *)
(********************)

let rec pow x p =
	if p = 0 then 1 else x * pow x (p-1)

(********************)
(* Problem 2: range *)
(********************)

let rec range num1 num2 =
	if num2 < num1 then [] else num1 :: range (num1+1) num2

(**********************)
(* Problem 3: flatten *)
(**********************)

let rec flatten l =
	match l with
		| [] -> []
		| hd :: tl -> hd @ flatten tl

(*****************************)
(* Problem 4: remove_stutter *)
(*****************************)

let rec remove_stutter l =
	match l with
		| val1 :: (val2 :: val3 as tl) ->
			if val1 = val2 then remove_stutter tl else val1 :: remove_stutter tl
		| _ -> l

(*********************)
(* Problem 5: rotate *)
(*********************)

let rotate l n =
  let rec helper l frontList backList n targetN =
	match l with
		| [] -> List.rev(frontList) @ List.rev(backList)
		| hd :: tl when n >= targetN -> helper tl (hd :: frontList) backList (n+1) targetN
		| hd :: tl -> helper tl frontList (hd :: backList) (n+1) targetN
	in
	helper l [] [] 0 (List.length l - n)

(*******************)
(* Problem 6: jump *)
(*******************)

let jump lst1 lst2 =
	let rec helper isEven lst1 lst2 =
		match lst1, lst2 with
			| [], _ | _, [] -> []
			| hd1 :: tl1, hd2 :: tl2 when isEven -> hd2 :: helper false tl1 tl2
			| hd1 :: tl1, hd2 :: tl2 -> hd1 :: helper true tl1 tl2
	in
	helper true lst1 lst2

(******************)
(* Problem 7: nth *)
(******************)

let nth l n =
	let rec helper l currN n =
		match l with
			| [] -> []
			| hd :: tl when currN-1 = 0 -> hd :: helper tl n n
			| hd :: tl -> helper tl (currN-1) n
	in
	helper l n n

(*****************************************************)
(* Problem 8: Digital Roots and Additive Persistence *)
(*****************************************************)

(* digits : int -> int list
 * we assume n >= 0
 * (digits n) is the list of digits of n in the order in which they appear in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *)

let rec digitsOfInt n =
	if n < 0 then [] else if n = 0 then [0] else
	let rec helper l n =
		if n = 0 then l
		else helper (n mod 10 :: l) (n / 10)
	in
	helper [] n


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits,
 * then adding the digits of the number derived from it, etc.,
 * until the remaining number has only one digit.
 * The number of additions required to obtain a single digit from a number n
 * is called the additive persistence of n, and the digit obtained is called
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)

let additivePersistence n =
	if n < 10 then 0 else
	let rec sum l =
		match l with
			| [] -> 0
			| hd :: tl -> hd + sum tl
	in
	let rec helper n =
		let total = sum(digitsOfInt(n)) in
		if total < 10 then 1 else 1 + helper(total)
	in
	helper n

let digitalRoot n =
	let rec sum l =
		match l with
			| [] -> 0
			| hd :: tl -> hd + sum tl
	in
	let rec helper n =
		let total = sum(digitsOfInt(n)) in
		if total < 10 then total else helper(total)
	in helper n

(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for pow *)
  let _ =
    try
      assert (pow 3 1 = 3);
      assert (pow 3 2 = 9);
      assert (pow (-3) 3 = -27);
      assert (pow 0 0 = 1);
      assert (pow (-10) 0 = 1);
      assert (pow 1 1000 = 1)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for range *)
  let _ =
    try
      assert (range 2 5 = [2;3;4;5]);
      assert (range 0 0 = [0]);
      assert (range (-1) 0 = [-1;0]);
      assert (range (-10) 10 = [-10;-9;-8;-7;-6;-5;-4;-3;-2;-1;0;1;2;3;4;5;6;7;8;9;10]);
      assert (range 500 (-500) = [])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for flatten *)
  let _ =
    try
      assert (flatten [[];[];[]] = []);
      assert (flatten [[];[];[];[];[];[];[1];[];[];[];[]] = [1]);
      assert (flatten [[];[];[1;2]] = [1;2]);
      assert (flatten [[1;2];[3;4]] = [1;2;3;4]);
      assert (flatten [[1;2];[];[3;4];[]] = [1;2;3;4])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for remove_stutter *)
  let _ =
    try
      assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]);
      assert (remove_stutter [] = []);
      assert (remove_stutter [1;1;1;1;1] = [1]);
      assert (remove_stutter [1;1;1;1;1;2] = [1;2]);
      assert (remove_stutter [1;2;3;4;5;6;7;8;9] = [1;2;3;4;5;6;7;8;9]);
      assert (remove_stutter [1;1;2;2;3;3;4;4;5;5;6;6;7;7;8;8;9;9] = [1;2;3;4;5;6;7;8;9])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for rotate *)
  let _ =
    try
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 2 = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]);
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 0 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]);
      assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 7 = ["b"; "c"; "d"; "e"; "f"; "g"; "h"; "a"]);
      assert (rotate ["a"; "b"] 0 = ["a"; "b"]);
      assert (rotate ["a"; "b"] 1 = ["b"; "a"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for jump *)
  let _ =
    try
      assert (jump ["first"; "second"; "third"; "fourth"] ["fifth"; "sixth"; "seventh"; "eighth"] = ["fifth"; "second"; "seventh"; "fourth"]);
      assert (jump [1; 3; 5; 7] [0; 2; 4; 6; 8] = [0; 3; 4; 7]);
      assert (jump ["a"; "b"] ["c"] = ["c"]);
      assert (jump [1;3;5;7;9;11;13;15;17;19] [2;4;6;8;10;12] = [2;3;6;7;10;11]);
      assert (jump [] [] = []);
      assert (jump [1;2;3;4;5;6;7;8;9] [] = []);
      assert (jump [] [1;2;3;4;5;6;7;8;9] = []);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for nth *)
  let _ =
    try
      (*print_int_list (nth [1; 2; 3; 4; 5; 6; 7] 1);*)
      assert (nth [1; 2; 3; 4; 5; 6; 7] 1 = [1; 2; 3; 4; 5; 6; 7]);
      assert (nth [1; 2; 3; 4; 5; 6; 7] 2 = [2; 4; 6]);
      assert (nth [1; 2; 3; 4; 5; 6; 7] 3 = [3; 6]);
      assert (nth [1;2;3] 3 = [3]);
      assert (nth [1] 1 = [1])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitsOfInt *)
  let _ =
    try
      assert (digitsOfInt 3124 = [3;1;2;4]);
      assert (digitsOfInt 352663 = [3;5;2;6;6;3]);
      assert (digitsOfInt 31243 = [3;1;2;4;3]);
      assert (digitsOfInt 23422 = [2;3;4;2;2]);
      assert (digitsOfInt (-1234) = []);
      assert (digitsOfInt 12340 = [1;2;3;4;0]);
      assert (digitsOfInt 0 = [0;]);
      assert (digitsOfInt 10 = [1;0]);
      assert (digitsOfInt 7 = [7]);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for additivePersistence *)
  let _ =
    try
      assert (additivePersistence 9 = 0);
      assert (additivePersistence 9876 = 2);
      assert (additivePersistence 0 = 0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for digitalRoot *)
  let _ =
    try
      assert (digitalRoot 9876 = 3);
      assert (digitalRoot 7 = 7);
      assert (digitalRoot 0 = 0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 10 programming questions are correct.\n") (10 - !error_count)

let _ = main()
