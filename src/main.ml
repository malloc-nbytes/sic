(*
"team name","ta_email"
"lname1, fname1",teststudent1@gmail.com,10001
"lname2, fname2",teststudent2@gmail.com,10002
"lname3, fname3",teststudent3@gmail.com,10003

#limit wildcard 10

r3(
  ?(50)(
    "\""*"\",""\""*"\""\n
    "\""*", "*"\","*"@gmail.com,"r5([1-9])
  )
)
;
*)

type token_type =
  | Comment
  | LParen
  | RParen
  | Wildcard
  | FuncCall
  | IntegerLiteral
  | Macro
  | StringLiteral
  | EOF

type token =
  { value : string
  ; ttype : token_type
  }

let isalpha c =
  let c = int_of_char c in
  (c >= 65 && c <= 90) || (c >= 97 && c <= 122)

let isnum c =
  let c = int_of_char c in
  let c = c - int_of_char '0' in
  (c >= 0) && (c <= 9)

let isalnum c =
  isalpha c || isnum c

let print_ttype t =
  match t with
  | Comment -> "COMMENT"
  | FuncCall -> "FuncCall"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Wildcard -> "WILDCARD"
  | IntegerLiteral -> "INTEGERLITERAL"
  | Macro -> "MACRO"
  | StringLiteral -> "STRINGLITERAL"
  | EOF -> "EOF"

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let char_lst_to_str lst = String.concat "" (List.map (Char.escaped) lst)

let is_ignorable c = c = '\t' || c = '\n' || c = ' '

let rec consume_until lst until : char list * char list=
  let rec aux lst acc : char list * char list =
    match lst with
    | [] -> [], []
    | hd :: tl when hd = '\\' -> aux (List.tl tl) (acc @ [List.hd tl])
    | hd :: tl when until hd -> acc, tl
    | hd :: tl -> aux tl (acc @ [hd]) in
  aux lst []

let rec peek tl ahead =
  let _ = if ahead < 1 then
            let _ = Printf.printf "tried to peak () when `ahead` < 1" in
            failwith "lexer error" in
  match tl with
  | [] -> None
  | _ :: tl when ahead <> 1 -> peek tl @@ ahead-1
  | hd :: _ -> Some hd

let rec lex_file (cur : char list) : token list =
  match cur with
  | [] -> [{value="EOF"; ttype=EOF}]
  | hd :: tl when is_ignorable hd -> lex_file tl
  | hd :: tl when hd = '/' ->
     (match peek tl 1 with
      | Some c when c = '/' ->
         let value, tl = consume_until (List.tl tl) (fun c -> c = '\n') in
         let tok = {value=char_lst_to_str value; ttype=Comment} in
         tok :: lex_file tl
      | _ -> failwith "unsupported char /")
  | hd :: tl when hd = '"' ->
     let value, tl = consume_until tl (fun c -> c = '"') in
     let tok = {value=char_lst_to_str value; ttype=StringLiteral} in
     tok :: lex_file tl
  | hd :: tl when isnum hd ->
     failwith "integer literals unimplemented"
     (* let intlit, tl = consume_until tl (fun c -> not @@ isnum c) in *)
     (* let _ = Printf.printf "len: %d\n" (List.length intlit) in *)
     (* let tok = {value=char_lst_to_str intlit; ttype=IntegerLiteral} in *)
     (* tok :: lex_file tl *)
  | hd :: tl when hd = '*' -> {value="*"; ttype=Wildcard} :: lex_file tl
  | hd :: tl when hd = '(' -> {value="("; ttype=LParen} :: lex_file tl
  | hd :: tl when hd = ')' -> {value=")"; ttype=RParen} :: lex_file tl
  | hd :: tl ->
     let value, tl = consume_until (hd :: tl) (fun c -> not @@ isalpha c) in
     let _ = Printf.printf "tl: %s\n" @@ char_lst_to_str tl in
     let tok = {value=char_lst_to_str value; ttype=FuncCall} in
     tok :: lex_file tl

let filepath = "./input.txt"

let () =
  let src = read_whole_file filepath in
  let lst : token list = lex_file (src |> String.to_seq |> List.of_seq) in
  List.iter (fun t ->
      Printf.printf "ttype = %s, value = %s\n"
        (print_ttype t.ttype) t.value
    ) lst
