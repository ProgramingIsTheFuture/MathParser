module type Parser = sig 
  val parse : string -> int;;
end

module Compiler: Parser = struct
  type operations =
    | Sum 
    | Sub
    | Divid
    | Multiply
    | None
    | Error of (string)

  type tokens =
    | Digit of int
    (* | Letter of string *)
    | Operation of operations
    (* | Error of (string) *)
    | None

  let err_not_operator: operations = Error ("Not a valid operator")

  (* Remove the first char from the string *)
  let remove_fs_str (s: string): string = String.sub s 1 (String.length s - 1);;

  (* check if the char is an operator *)
  let is_operator (c: char)=
    let code = Char.code c in
    (* ( *  +  -  / ) *)
    code = 42 || code = 43 || code = 45 || code = 47;;

  let get_operator_from_char (c: char) =
    match (Char.code c) with
    | 42 -> Multiply
    | 43 -> Sum
    | 45 -> Sub
    | 47 -> Divid
    | _ -> err_not_operator;;

  let get_fun_from_operator = function
    | Sum -> (+)
    | Sub -> (-)
    | Divid -> (/)
    | Multiply -> fun i j -> i * j
    | Error s -> failwith (Printf.sprintf "Errors: %s\n" s)
    | _ -> failwith "Cannot convert None to Operator"

  (* if the ASCII code is between 48 and 57 it means it is a number between 0-9 *)
  let is_space (c: char) = Char.code c = 32;;

  let is_digit (c: char) =
    Char.code c >= 48 && Char.code c <= 57;;

  let get_digit_from_char (c: char) =
    Char.code c - 48;;

  (* Converts some part of the string to digits *)
  let get_digits (data: string): string * tokens =
    (* Helper function with terminal recursion *)
    let rec helper (s: string) (token: tokens): string * tokens = 
        (* not a valid string length anymore - just return *)
        if String.length s = 0 then (s, token)
        else
          (* Gets the next character *)
          let c = String.get s 0 in 

          (* if the first char is space and we didn't fine any number yet we will just move on *)
          if is_space c && token = None then helper (remove_fs_str s) token
          else if is_space c && token <> None then (remove_fs_str s, token)
          else if is_digit c then
            (* Creates a new Token with the digit *)
            let token = match token with
            | Digit n -> Digit(
              (if n > 0 then (n*10) else n)
                + (get_digit_from_char c)
              )
            | _ -> Digit(get_digit_from_char c) in
            helper (remove_fs_str s) token
          else
            (s, token)
      in

    helper data None;;

  let get_operator (data: string): string * tokens =
    let rec helper (s: string) (token: tokens): string * tokens = 
      if String.length s = 0 then (s, token)
      else
        (* Gets the next character *)
        let c = String.get s 0 in 

        (* if the first char is space and we didn't fine any number yet we will just move on *)
        if is_space c && token = None then helper (remove_fs_str s) token
        else if is_space c && token <> None then (remove_fs_str s, token)
        else 
          if is_operator c then
            (* Creates a new Token with the digit *)
            let token = Operation (get_operator_from_char c) in
            (remove_fs_str s, token)
        else (s, token)
    in

    helper data None;;

  (* This function will just take the input s *)
  (* Convert it to tokens and calculate its value *)
  (* This returns an int, that its the main type used *)
  let try_me (s: string) =
    let tokens = ref [] in
    let s = ref s in

    let rec helper (tokens: tokens list) (prev: int) (op: operations) =
      match tokens with
      | [] -> prev
      | x :: tl ->
        match (prev, op, x) with
        | (0, None, Digit n) -> helper tl n op
        | (_, None, Operation(n)) -> helper tl prev n
        | (y, n, Digit(z)) -> helper tl ((get_fun_from_operator n) y z) None
        | _ -> failwith "You are trying to do something without operators or numbers!\nPlease check your expression!"
    in


    let (ss, token) = get_digits !s in
    let () = s := ss in
    let () = tokens := !tokens @ [token] in

    let prev = ref 0 in

    while !s <> "" do
      let (ss, token) = get_operator !s in
      let () = s := ss in
      let () = tokens := !tokens @ [token] in

      let (ss, token) = get_digits !s in
      let () = s := ss in
      let () = tokens := !tokens @ [token] in

      let () = prev := helper !tokens !prev None in
      tokens := [];
    done;
    (* Now we have some 23 digit, +-/* operator and another digit *)
    
    !prev;;

  let parse (s: string) =
    let i = try_me s in
    i;;
end

let () =
  let s = read_line () in
  let result = Compiler.parse s in
  Printf.printf "Result: %d\n" result;;

