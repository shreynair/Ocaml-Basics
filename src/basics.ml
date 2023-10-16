open Funs
(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
  match tup with
    | (a,b,c) -> (c,b,a)

let is_even x = 
  let remainder = abs (x mod 2) in
  match remainder with
    | 0 -> true
    | 1 -> false

let volume x y = 
  match x,y with
  | (a,b,c),(x,y,z) -> (abs (x-a)) * (abs (y-b)) * (abs (z-c))

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (n-1) + fibonacci (n-2)

let rec log x y =
  match x,y with
    | x,y when y < x -> 0
    | _,_ -> 1 + log x (y/x)

let rec gcf x y = 
  match x,y with
    | x,0 -> x
    | x,y when x == y -> x
    | x,y -> gcf (max (x-y) y) (min (x-y) y) 

let rec maxFuncChain init funcs = 
  match funcs with
    | [] -> init
    | head :: tail -> max init (max (maxFuncChain (head init) tail) (maxFuncChain init tail))

(*****************)
(* Part 3: Lists *)
(*****************)

let rec reverse lst = 
  match lst with
    | [] -> []
    | head :: tail -> (reverse tail) @ [head]

let rec zip lst1 lst2 = 
  match lst1, lst2 with
  | [], [] -> []
  | _,[] -> []
  | [],_ -> []
  | (a, b) :: tail1, (c, d) :: tail2 -> [(a, b, c, d)] @ zip tail1 tail2

let rec is_palindrome lst = 
  let rec is_palindrome_helper lst rev_list =
    match lst, (reverse rev_list) with
    | [], [] -> true
    | head1 :: tail1, head2 :: tail2 when head1 == head2 -> 
      (is_palindrome_helper tail1 (reverse tail2))
    | _ -> false
  in

  is_palindrome_helper lst lst

let rec square_primes lst = 
  let is_prime n =
    if n <= 1 then
      false
    else
      let sqrt_n = (int_of_float (sqrt (float_of_int n))) in
      let rec is_prime_helper n k =
        if k <= 1 then
          true
        else if n mod k != 0 then
          true && is_prime_helper n (k - 1)
        else
          false 
      in

      is_prime_helper n sqrt_n
  in
  match lst with
    | [] -> []
    | head :: tail when (is_prime head) == true -> 
        [(head, head * head)] @ (square_primes tail)
    | _ :: tail -> (square_primes tail)


let rec partition p lst = 
  let rec part_helper p lst lst1 lst2 =
    match lst with 
    | []-> (lst1, lst2)
    | head :: tail when p head -> part_helper p tail (lst1 @ [head]) lst2
    | head :: tail when not (p head) -> part_helper p tail lst1 (lst2 @ [head])
  in

  part_helper p lst [] []
(*****************)
(* Part 4: HOF *)
(*****************)

let is_present lst x = map (fun y -> if y == x then 1 else 0) lst


let count_occ lst target = fold (fun a x -> if x == target then a + 1 else a) 0 lst
  
let jumping_tuples lst1 lst2 = 
  let combined = zip lst1 lst2 in

  let tuple_index target tup_lst =
    fold (fun index x -> if x == target then index + 1 else index) 0 tup_lst
  in
  
  let reverse_tuple tup_lst =
    map (fun x -> match x with | (a,b,c,d) when (tuple_index x tup_lst) mod 2 == 0 -> (a,b,c,d)
        | (a,b,c,d) when (tuple_index x tup_lst) mod 2 == 1 -> (d,c,b,a)) tup_lst 
  in

  let jumping_tuples_helper com_lst switch =
    fold (fun x y -> match y with | (a,b,c,d) when switch mod 2 == 0 -> x @ d 
          | (a,b,c,d) when switch mod 2 == 1 -> x @ a) [] com_lst
  in

  (jumping_tuples_helper combined 0) @ (jumping_tuples_helper combined 1)

let addgenerator x = (fun y -> x + y)

let uniq lst = 
  let member target lst = 
    fold (fun a x -> if x == target then true else a) false lst
  in

  fold (fun a x -> if (member x a) == false then x :: a else a) [] lst


let ap fns args = fold (fun a x -> a @ (map x args)) [] fns
