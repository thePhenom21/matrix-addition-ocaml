open Yojson.Basic.Util
let matrix_add str = 
  let parsed = Yojson.Basic.from_string str
  in 
  let matrix1 =  member "matrix1" parsed in
  let matrix2 =  member "matrix2" parsed in
  let m1r1 = filter_string @@ to_list @@ (member "row1" matrix1) in
  let m1r2 = filter_string @@ to_list @@ (member "row2" matrix1)in
  let m1r3 = filter_string @@ to_list @@ (member "row3" matrix1) in
  let m2r1 = filter_string @@ to_list @@ (member "row1" matrix2) in
  let m2r2 = filter_string @@ to_list @@ (member "row2" matrix2)in
  let m2r3 = filter_string @@ to_list @@ (member "row3" matrix2)in
  let rec calc a b c d e f = match a, b, c, d, e, f with 
  | x::xy,z::zy,t::ty,k::kt,l::lt,o::ot -> [string_of_int(int_of_string x + int_of_string k),string_of_int(int_of_string z + int_of_string l),string_of_int(int_of_string t + int_of_string o)] @ (calc xy zy ty kt lt ot)
  | [],[],[],[],[],[] -> []  
  | _,_,_,_,_,_ -> []  
  in
  let a = calc m1r1 m1r2 m1r3 m2r1 m2r2 m2r3 in
  let  f h = match h with
  | (a,b,c) -> a ^ "n" ^ b ^ "n" ^ c ^ "n"
  in
  let rec change k = match k with
  | t::xt -> f t ^ (change xt)
  | [] -> "\n"
  in
  change a
  



  

  


let () = 
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.post "/calculate" (fun request ->
      let%lwt body = Dream.body request in
      Dream.respond
        ~headers:["Content-Type", "application/octet-stream"]
        (matrix_add body)); 
  ]