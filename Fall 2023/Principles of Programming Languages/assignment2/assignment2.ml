open List

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

(* print out a list of integer lists *)
let print_int_list_list lst =
  List.iter print_int_list lst

(* print out a list of string lists *)
let print_string_list_list lst =
  List.iter print_string_list lst

(***********************)
(* Problem 1: cond_dup *)
(***********************)

let rec cond_dup l f =
  match l with
    | hd :: tl -> if (f hd) then (hd :: hd :: cond_dup tl f) else (hd :: cond_dup tl f)
    | [] -> []

(**********************)
(* Problem 2: n_times *)
(**********************)

let rec n_times (f, n, v) =
  if n <= 0 then v
  else n_times (f, n - 1, f v)

(**********************)
(* Problem 3: zipwith *)
(**********************)

let rec zipwith f l1 l2 =
  match l1, l2 with
    | [], _ | _, [] -> []
    | hd1 :: tl1, hd2 :: tl2 -> (f hd1 hd2) :: zipwith f tl1 tl2


(**********************)
(* Problem 4: buckets *)
(**********************)

let buckets p l =
  let rec update element acc =
    match acc with
      | (hd :: _ ) as bucket :: tl ->
        if p element hd then (element :: bucket) :: tl
        else bucket :: update element tl
      | _ -> [[element]]
  in
  List.rev (List.fold_right (fun x acc -> update x acc) l [])


(**************************)
(* Problem 5: fib_tailrec *)
(**************************)

let fib_tailrec n =
  let rec helper previous current n =
    if n = 0 then current else helper current (current + previous) (n - 1)
  in
  if n = 0 then 0 else helper 0 1 (n - 1)

(***********************)
(* Problem 6: sum_rows *)
(***********************)

let sum_rows (rows:int list list) : int list =
  List.map (fun x -> List.fold_left (+) 0 x) rows
    
(*****************)
(* Problem 7: ap *)
(*****************)

let ap fs args =
  List.fold_left (fun acc x -> acc @ List.map x args) [] fs

(***********************)
(* Problem 8: prefixes *)
(***********************)

let prefixes l =
  snd (List.fold_left (fun (x, acc) y -> x @ [y], acc @ [x @ [y]]) ([], []) l)

(***********************)
(* Problem 9: powerset *)
(***********************)

let powerset l =
  List.fold_left (fun acc element -> acc @ List.map (fun x -> element :: x) acc) [[]] l

(**************************)
(* Problem 10: assoc_list *)
(**************************)

let assoc_list lst =
  let update element list =
    List.fold_left (fun (found, acc) (e, count) -> if element = e then (true, (element, count + 1) :: acc) else (found, (e, count) :: acc)) (false, []) list
  in
  List.fold_left (fun acc x -> let (found, curr) = update x acc in if found then curr else (x, 1) :: acc) [] lst
   
(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in

  (* Testcases for cond_dup *)
  let _ =
    try
      assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5]);
      assert (cond_dup [] (fun x -> x mod 2 = 1) = []);
      assert (cond_dup [1;2;3;4;5] (fun x -> x mod 2 = 0) = [1;2;2;3;4;4;5]);
      assert (cond_dup [3] (fun x -> x mod 2 = 1) = [3;3]);
      assert (cond_dup [2] (fun x -> x mod 2 = 1) = [2]);
      assert (cond_dup [3;5;7] (fun x -> x mod 2 = 1) = [3;3;5;5;7;7]);
      assert (cond_dup [2;4;6] (fun x -> x mod 2 = 1) = [2;4;6]);
      assert (cond_dup [1;2;3;4;5;6] (fun x -> x mod 2 = 0) = [1;2;2;3;4;4;5;6;6]);
      assert (cond_dup [2;2;2;3;3;3] (fun x -> x mod 2 = 0) = [2;2;2;2;2;2;3;3;3]);
      assert (cond_dup [1;2;3;4;5] (fun x -> x mod 2 = 0) = [1;2;2;3;4;4;5]);
      assert (cond_dup ["a";"b";"c"] (fun x -> x = "b") = ["a";"b";"b";"c"]);
      assert (cond_dup [0;-1;-2;3] (fun x -> x <= 0) = [0;0;-1;-1;-2;-2;3])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times ((fun x -> x + 1), 50, 0) = 50);
      assert (n_times ((fun x -> x + 1), 0, 1) = 1);
      assert (n_times ((fun x -> x + 2), 50, 0) = 100);
      assert (n_times ((fun x -> x * x), 2, 2) = 16);
      assert (n_times ((fun x -> x * x), 1, 2) = 4);
      assert (n_times ((fun x -> x * x), 3, 2) = 256);
      assert (n_times ((fun x -> x * x), 4, 1) = 1);
      assert (n_times ((fun x -> x * x), 0, 2) = 2);
      assert (n_times ((fun x -> x / 2), 4, 16) = 1);
      assert (n_times ((fun x -> x / 2), 4, 18) = 1);
      assert (n_times ((fun x -> x * x), 3, (-2)) = 256);
      assert (n_times ((fun x -> x + x), 5, 1) = 32)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for zipwith *)
  let _ =
    try
      assert (zipwith (+) [1;2;3] [4;5] = [5;7]);
      assert (zipwith (fun x y -> (x,y)) [1;2;3;4] [5;6;7] = [(1,5); (2,6); (3,7)]);
      assert (zipwith (fun x y -> x * y) [1;2;3] [4;5;6] = [4;10;18]);
      assert (zipwith (-) [5;6;7] [1;2;3] = [4;4;4]);
      assert (zipwith (/) [10;20;30;40] [2;5;6;8] = [5;4;5;5]);
      assert (zipwith (fun x y -> x ^ y) ["a";"b";"c"] ["1";"2";"3"] = ["a1";"b2";"c3"]);
      assert (zipwith (fun x y -> x > y) [1;3;5;7] [2;3;5;7] = [false;false;false;false]);
      assert (zipwith (fun x y -> x > y) [1;3;5;7] [0;2;4;6] = [true;true;true;true]);
      assert (zipwith (fun x y -> x = y) [1;2;3;4] [1;2;3;4] = [true;true;true;true]);
      assert (zipwith (fun x y -> x * y) [] [4;5;6] = []);
      assert (zipwith (fun x y -> x * y) [1;2;3] [] = []);
      assert (zipwith (fun x y -> x * y) [] [] = [])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> x mod 3 = y mod 3) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]]);
      assert (buckets (=) [1;3;3;2;2;4;5;5;5;6;7] = [[1];[3;3];[2;2];[4];[5;5;5];[6];[7]]);
      assert (buckets (=) [5;5;5;4;4;4;3;3;2;2;1] = [[5;5;5];[4;4;4];[3;3];[2;2];[1]]);
      assert (buckets (fun x y -> x mod 2 = y mod 2) [1;3;5;7;2;4;6;8] = [[1;3;5;7];[2;4;6;8]]);
      assert (buckets (=) [1;2;3;1;2;3;1;2;3] = [[1;1;1];[2;2;2];[3;3;3]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 0 = 0);
      assert (fib_tailrec 1 = 1);
      assert (fib_tailrec 2 = 1);
      assert (fib_tailrec 5 = 5);
      assert (fib_tailrec 10 = 55);
      assert (fib_tailrec 15 = 610);
      assert (fib_tailrec 20 = 6765);
      assert (fib_tailrec 30 = 832040);
      assert (fib_tailrec 40 = 102334155);
      assert (fib_tailrec 50 = 12586269025);
      assert (fib_tailrec 60 = 1548008755920);
      assert (fib_tailrec 70 = 190392490709135);
      assert (fib_tailrec 80 = 23416728348467685);
      assert (fib_tailrec 90 = 2880067194370816120)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for sum_rows *)
  let _ =
    try
      assert (sum_rows [[]] = [0]);
      assert (sum_rows [[1]; []; [3; 4]; []] = [1; 0; 7; 0]);
      assert (sum_rows [] = []);
      assert (sum_rows [[-1;-2;-3]; [3;4]] = [-6; 7]);

    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let c = [] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
      assert  ([] = ap fs1 c);
      assert (["hello?"; "world?"; "hello!"; "world!"] = ap [(fun x -> x^"?"); (fun x -> x^"!")] ["hello"; "world"])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for prefixes *)
  let _ =
    try
      assert (prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]]);
      assert (prefixes [] = []);
      assert (prefixes [5] = [[5]]);
      assert (prefixes [1;2;3] = [[1]; [1;2]; [1;2;3]]);
      assert (prefixes ["a"; "b"; "c"] = [["a"]; ["a"; "b"]; ["a"; "b"; "c"]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (*sort a list of lists *)
  let sort ls =
    List.sort cmp (List.map (List.sort cmp) ls) in

  (* Testcases for powerset *)
  let _ =
    try
      (* Either including or excluding [] in the powerset is marked correct by the tester *)
      assert (sort (powerset [1;2;3]) = sort [[1]; [1; 2]; [1; 2; 3]; [1; 3]; [2]; [2; 3]; [3]] || sort (powerset [1;2;3]) = sort [[];[1]; [1; 2]; [1; 2; 3]; [1; 3]; [2]; [2; 3]; [3]]);
      assert ([] = powerset [] || [[]] = powerset []);
      assert (sort (powerset [4; 5; 6]) = sort [[4]; [4; 5]; [4; 5; 6]; [4; 6]; [5]; [5; 6]; [6]] || sort (powerset [4; 5; 6]) = sort [[]; [4]; [4; 5]; [4; 5; 6]; [4; 6]; [5]; [5; 6]; [6]]);
      assert (sort (powerset ["a"; "b"; "c"]) = sort [["a"]; ["a"; "b"]; ["a"; "b"; "c"]; ["a"; "c"]; ["b"]; ["b"; "c"]; ["c"]] || sort (powerset ["a"; "b"; "c"]) = sort [[]; ["a"]; ["a"; "b"]; ["a"; "b"; "c"]; ["a"; "c"]; ["b"]; ["b"; "c"]; ["c"]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let a = [true;false;false;true;false;false;false] in
    let b = [] in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
      assert ([(false,5);(true,2)] = List.sort cmp (assoc_list a));
      assert ([] = assoc_list b);
      assert ([(1, 1)] = List.sort cmp (assoc_list [1]));
      assert ([(1, 2); (2, 1)] = List.sort cmp (assoc_list [1; 2; 1]))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in


  Printf.printf ("%d out of 10 programming questions passed.\n") (10 - !error_count)

let _ = main()
