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
      let ret = ref 0 in
      while !ret == 0 && !i < length b1 && !i < length b2 do 
        if arr1.(!i) && not arr2.(!i) then ret := 1
        else if not arr1.(!i) && arr2.(!i) then ret := -1
        else
          i := !i + 1;
      done;
      !ret
end

module AdaptiveCounter = struct
  type t = ACounter of BString.t array | Top

  let create list = ACounter (Array.of_list list)

  let createTop () = Top

  let length = function
    | Top -> failwith "Length called on Top Adaptive Counter"
    | ACounter arr -> Array.length arr

  let trim ac n =
    match ac with
    | Top -> Top
    | ACounter arr -> ACounter (Array.sub arr 0 (Array.length arr - n))

  let compare ac1 ac2 =
    match ac1, ac2 with
    | Top, Top -> 1 (** TODO: Check this **)
    | Top, ACounter arr -> 1
    | ACounter arr, Top -> -1
    | (ACounter arr1 as ac1), (ACounter arr2 as ac2) ->
    let i = ref 0 in
    let ret = ref 0 in
    while !ret == 0 && !i < length ac1 && !i < length ac2 do
      ret := BString.compare arr1.(!i) arr2.(!i);
      i := !i + 1
    done;
    !ret

  let append ac bstr = 
    match ac with
    | Top -> failwith "Append called on Top Adaptive Counter"
    | ACounter arr -> ACounter (Array.append arr [|bstr|])

  let getLast = function
    | Top -> failwith "GetLast called on Top Adaptive Counter"
    | ACounter arr -> arr.(Array.length arr - 1)

  let isMax = function
    | Top -> true
    | ACounter _ -> false

  let show = function
    | Top -> "T"
    | ACounter arr ->
      let list = Array.to_list arr in
      let rec loop = function
        | [] -> ""
        | [x] -> BString.show x
        | hd :: tl -> BString.show hd ^ "," ^ loop tl
      in
      "(" ^ loop list ^ ")"

  let print ac = show ac |> print_string
end

module ProgressMeasure = struct
  type t = (Paritygame.node, AdaptiveCounter.t) Hashtbl.t

  let create nodes = Hashtbl.create (Paritygame.ns_size nodes)

  let lift pm node = ()

  let getAC pm node = Hashtbl.find pm node

  let getWinningSet pm = Paritygame.sol_make 0

  let getWinningStrategy pm = Paritygame.str_make 0
end


let solve (game : Paritygame.paritygame) = (sol_make 0, str_make 0)

let register () =
  Solverregistry.register_solver
    solve
    "succsmallpm"
    "sspm"
    "Quasi-polynomial time algorithm by Jurdzinski and Lazic (2017)"