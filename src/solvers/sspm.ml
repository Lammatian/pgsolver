open Paritygame;;

module BString = struct
  type t = BString of bool array | Empty

  let create = function
    | [] -> Empty
    | list -> BString (Array.of_list list)

  let createLen = function
    | 0 -> Empty
    | l -> BString (Array.make l false)

  let show = function
    | Empty -> "ε"
    | BString arr ->
      let list = Array.to_list arr in
      let rec loop = function
        | [] -> ""
        | hd :: tl -> (if hd then "1" else "0") ^ loop tl
      in
      loop list

  let print bstr = print_string (show bstr)

  let length = function
    | Empty -> 0
    | BString list -> Array.length list

  let append bstr n =
    if length bstr < n then
      match bstr with
      | Empty ->
        let arr = Array.make n false in
        arr.(0) <- true;
        BString arr
      | BString bstr as bstring ->
        let arr = Array.make (n - (length bstring)) false in
        arr.(0) <- true;
        BString (Array.append bstr arr)
    else
      bstr

  let cut = function
    | Empty -> Empty
    | BString arr as bstring->
      let last_one = ref ((length bstring) - 1) in
      while arr.(!last_one) do
        last_one := !last_one - 1
      done;
      BString (Array.sub arr 0 !last_one)

  let compare bstr1 bstr2 =
    match bstr1, bstr2 with
    | Empty, Empty       -> 0
    | Empty, BString arr -> if arr.(0) then -1 else 1
    | BString arr, Empty -> if arr.(0) then 1 else -1
    | (BString arr1 as b1), (BString arr2 as b2) ->
      let i = ref 0 in
      let j = ref 0 in
      let ret = ref None in
      while Option.is_none !ret && !i < length b1 && !j < length b2 do 
        if arr1.(!i) && not arr2.(!j) then ret := Some 1
        else if not arr1.(!i) && arr2.(!j) then ret := Some (-1)
        else
          i := !i + 1;
          j := !j + 1;
      done;
      match !ret with
      | None -> 0
      | Some x -> x
end

let solve (game : Paritygame.paritygame) = (sol_make 0, str_make 0)

let register () =
  Solverregistry.register_solver
    solve
    "succsmallpm"
    "sspm"
    "Quasi-polynomial time algorithm by Jurdzinski and Lazic (2017)"