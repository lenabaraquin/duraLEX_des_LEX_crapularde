type token =
  | Comma
  | Period
  | NewLine
  | Punct
  | Word of string

let string_to_list (s : string) : char list =
  List.init (String.length s) (fun i -> s.[i])

let list_to_string (c_list : char list) : string = 
  String.init (List.length c_list) (fun i -> List.nth c_list i)

let print_char (c: char) : unit =
  Printf.printf "%c" c

let print_token (t: token) : unit =
  (match t with | Comma -> Printf.printf "Comma" | Period -> Printf.printf "Period" | NewLine -> Printf.printf "NewLine" | Punct -> Printf.printf "Punct" | Word s -> Printf.printf "Word : %s" s)

let rec print_list (print_a : 'a -> unit) (liste : 'a list) : unit =
  match liste with
  | [] -> ()
  | a :: [] -> print_a a; Printf.printf "\n"
  | a :: reste -> print_a a; Printf.printf ", "; print_list print_a reste

let rec put_channel_in_char_list (t : in_channel) : char list =
  let c_option = In_channel.input_char t in
  match c_option with 
  | None -> []
  | Some c -> c :: put_channel_in_char_list t

let is_end_of_word (c: char) : bool =
  match c with
  | ' ' -> true
  | '.' -> true
  | ',' -> true
  | '\n' -> true
  | '(' -> true
  | ')' -> true
  | _ -> false

let rec word_lex (current_word : char list) (c_list: char list) : token list = 
  match c_list with
  | [] -> lexer []
  | in_word_char :: rest when is_end_of_word in_word_char -> Word (list_to_string current_word) :: lexer (in_word_char :: rest)
  | in_word_char :: rest -> word_lex (List.append current_word [in_word_char]) rest

and lexer (c_list : char list) : token list =
  match c_list with
  | [] -> []
  | ' ' :: rest -> lexer rest
  | '.' :: rest ->  Period :: lexer rest
  | ',' :: rest -> Comma :: lexer rest
  | '\n' :: rest ->  NewLine :: lexer rest
  | c :: rest when is_end_of_word c -> Punct :: lexer rest
  | c :: rest -> word_lex [] c_list

let () =
  let s = "Je suis une patate , j'aime les frites, je suis donc canibale (des paatates)." in
  let toktok = lexer (string_to_list s) in
  print_list print_token toktok


(*
destruct 
| cas when condition -> whateutodo

    Word (list_to_string [' '])
let path = "./test"

let channel = In_channel.open_text path
*)


