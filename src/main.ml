module type LexerSig = sig
  (* Line and Char number *)
  type line = int * int

  (* Algebric operators *)
  type operations =
    | Sum
    | Sub
    | Divid
    | Multiply

  type digit = 
    | Int of int

  type token =
    | Digit of digit * line
    | String of string * line
    | QUOTE
    (* | Letter of string *)
    | Operation of operations * line
    (* | Error of (string) *)
    | Error of line

   val lexit: string -> token list
end

module Lexer: LexerSig = struct
  (* Line and Char number *)
  type line = int * int

  (* Algebric operators *)
  type operations =
    | Sum
    | Sub
    | Divid
    | Multiply

  type digit = 
    | Int of int

  type token =
    | Digit of digit * line
    | String of string * line
    | QUOTE
    (* | Letter of string *)
    | Operation of operations * line
    (* | Error of (string) *)
    | Error of line
   
  let syntax_error (t: token) = 
    match t with
    | Error l -> failwith (Printf.sprintf "Syntax error on line: %d at position: %d" (fst l) (snd l)) |> ignore
    | _ -> ();;

  (* Remove the first char from the string *)
  let remove_fs_str (s: string): string = String.sub s 1 (String.length s - 1);;

  (* check if the char is an operator *)
  let is_operator (c: char)=
    let code = Char.code c in
    (* ( *  +  -  / ) *)
    code = 42 || code = 43 || code = 45 || code = 47;;

  (* verify is the char is a space *)
  let is_space (c: char) = Char.code c = Char.code ' ';;

  (* verify is the char is a quote *)
  let is_quote (c: char) = Char.code c = Char.code '"';;

  (* verify is the char is the end of line *)
  let is_endofline (c: char) = Char.code c = Char.code '\n';;

  (* Verify if the char is a number *)
  (* if the ASCII code is between 48 and 57 it means it is a number between 0-9 *)
  let is_digit (c: char) =
    Char.code c >= 48 && Char.code c <= 57;;

  (* converts a char code to a valid operator *)
  let get_operator_from_char (c: char) =
    match (Char.code c) with
    | 42 -> Some Multiply
    | 43 -> Some Sum
    | 45 -> Some Sub
    | 47 -> Some Divid
    | _ -> None

  let get_fs_char (str: string): char = String.get str 0

  (* converts the operator to the function to be aplyed *)
  let get_fun_from_operator = function
    | Sum -> (+)
    | Sub -> (-)
    | Divid -> (/)
    | Multiply -> fun i j -> i * j

      (* converts the char to an int *)
  let get_digit_from_char (c: char) =
    Char.code c - 48;;

  let get_digit (line: int) (str: string ref): token =
    let t = ref (Digit(Int(get_digit_from_char (get_fs_char !str)), (line, line))) in
    let () = str := remove_fs_str !str in
    if !str = "" then !t
    else begin
      while !str <> "" && is_digit (get_fs_char !str) do
        let s = (get_fs_char !str) in 
        let () = t := match !t with
          | Digit(Int(x), l) -> Digit(Int(x*10 + (get_digit_from_char s)), l)
          | _ -> failwith "" in
        str := remove_fs_str !str;
      done;
      !t
    end

  (* Take char by char *)
  (* Transform each one of them into tokens *)
  let lex_line (string_to_lex: string): token list =
    let str = ref string_to_lex in
    let tokens: token list ref = ref [] in
    let line = ref 1 in
    while !str <> "" do
      let tt = match get_fs_char !str with
      | '\n' -> 
        str := remove_fs_str !str;
        incr line; 
        None
      | ' ' -> 
        str := remove_fs_str !str; 
        None
      | x when is_operator x ->
        let op = get_operator_from_char x in
        begin
        match op with
        | Some v -> 
          let ope = Some (Operation (v, (1, 2))) in
          str := remove_fs_str !str;
          Printf.printf "Operation";
          ope
        | None -> failwith "Operator is invalid!"
        end
      | x when is_digit x -> 
          Some (get_digit !line str)
      | _ -> failwith (Printf.sprintf "Not valid expression. Line: %d" !line) in
      
      match tt with
      | Some v ->
        tokens := !tokens @ [v];
      | None -> ()
    done;
    !tokens;;


  let lexit (file: string): token list =
    let tokens: token list ref = ref [] in
    tokens := ( file |> lex_line );
    !tokens;;
end


module type ParserSig = sig 
  val parse : string -> int;;
end

module Parser: ParserSig = struct
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

  (* converts a char code to a valid operator *)
  let get_operator_from_char (c: char) =
    match (Char.code c) with
    | 42 -> Multiply
    | 43 -> Sum
    | 45 -> Sub
    | 47 -> Divid
    | _ -> err_not_operator;;

  (* converts the operator to the function to be aplyed *)
  let get_fun_from_operator = function
    | Sum -> (+)
    | Sub -> (-)
    | Divid -> (/)
    | Multiply -> fun i j -> i * j
    | Error s -> failwith (Printf.sprintf "Errors: %s\n" s)
    | _ -> failwith "Cannot convert None to Operator"

  (* verify is the char is a space *)
  let is_space (c: char) = Char.code c = Char.code ' ';;

  (* (* verify is the char is the end of line *) *)
  (* let is_endofline (c: char) = Char.code c = Char.code '\n';; *)

  (* Verify if the char is a number *)
  (* if the ASCII code is between 48 and 57 it means it is a number between 0-9 *)
  let is_digit (c: char) =
    Char.code c >= 48 && Char.code c <= 57;;

  (* converts the char to an int *)
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
            (* Creates a new Token with the operator *)
            let token = Operation (get_operator_from_char c) in
            (remove_fs_str s, token)
        else (s, token)
    in

    helper data None;;

  (* This function will just take the input s *)
  (* Convert it to tokens and calculate its value *)
  (* This returns an int, that its the main type used *)
  let evaluate (s: string) =
    let tokens = ref [] in
    let s = ref s in

    let rec helper (tokens: tokens list) (prev: int) (op: operations) =
      match tokens with
      | [] -> prev
      | [Digit (n)] when op = None -> n
      | [Operation (_)] -> failwith "Operator with number to operate"
      | x :: tl ->
        match (prev, op, x) with
        | (_, None, Digit n) -> helper tl n op
        | (_, None, Operation(n)) -> helper tl prev n
        | (y, n, Digit(z)) -> helper tl ((get_fun_from_operator n) y z) None
        | _ -> failwith "You are trying to do something without operators or numbers! Check your expression!"
    in


    let (ss, token) = get_digits !s in
    let () = s := ss in
    let () = tokens := !tokens @ [token] in

    let prev = ref (match token with 
      | Digit (n) -> n 
      | _ -> 0
    ) in

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
    let i = evaluate s in
    i;;
end

module type ReaderSig = sig
  val parse_file: string -> unit
end

module Reader (P: ParserSig): ReaderSig = struct
  let valid_extension (name: string) =
    let file_split = (String.split_on_char '.' name) in
    let rec helper = function
      | [] -> false 
      | [x] -> x = "math"
      | _ :: tl -> helper tl
    in

    helper file_split;;

  let get_file (name: string) =
    if not (valid_extension name) then None 
    else 
      Some(open_in name);;

  let parse_file (name_file: string) =
    let file = match get_file name_file with
    | None -> failwith "File not valid"
    | Some v -> v in

    let line_count = ref 1 in

    try 
      while true do
        let line = input_line file in
        P.parse line |> Printf.printf "%d Result: %d\n" !line_count;
        incr line_count;
      done;
    with e ->
      close_in file;
      if e <> End_of_file then
        raise e;;

end

module type CliSig = sig
  val cli: unit -> unit
end

module Cli (R: ReaderSig): CliSig = struct
  let file = ref "";;

  let speclist: (string * Arg.spec * string) list = [];;

  let usage = "math [filename]"

  let cli () =
    Arg.parse speclist (fun f -> file := f) usage;
    if !file != "" then
      R.parse_file !file
    else 
      let () = Printf.printf "Use (Cntr + C) to exit\n" in
      while true do
        let () = Printf.printf "|> " in
        let s = read_line () in
        let result = Parser.parse s in
        Printf.printf "Result: %d\n" result 
      done;;
end


module type RunSig = sig
  val start: unit -> unit
end

module Run: RunSig = struct
  module P = Parser;;
  module R = Reader(P);;
  module C = Cli(R);;

  let start () = C.cli ();;
end

let () =
  Run.start ();;
