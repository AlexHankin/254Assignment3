type ast_sl = ast_s list
and ast_s =
| AST_error
| AST_assign of (string * ast_e)
| AST_read of string
| AST_write of ast_e
| AST_if of (ast_e * ast_sl)
| AST_do of ast_sl
| AST_check of ast_e
and ast_e =
| AST_binop of (string * ast_e * ast_e)
| AST_id of string
| AST_num of string;;


let listofVariables = []

let string_of_chars chars = 
	let buf = Buffer.create 16 in
	List.iter (Buffer.add_char buf) listofVariables;
	Buffer.contents buf

let rec translate (ast:ast_sl)
    :  string *  string
    (* warnings  output_program *) =
    let cProgram = (String.concat " " ["#include <stdio.h>\n #include <stdlib.h>\nint getint() {\nint myint;\nif (scanf(\"%d\", &myint) == 1){\nreturn myint;}\nelse {\nprintf(\"The input value is not of type int\n\");\nprintf(\"Terminating program...\n\");\nexit(0);}}\nvoid putint(int n) {printf(\"%d\n\", n)}\nint main () {"; translate_sl(ast);"}"]) in
    let ErrorList = string_of_chars listofVariables
    return(string_of_chars, cProgram)

and translate_sl (ast:ast_sl) : string = function
	| [] -> return("")
	| x :: l -> return(String.concat " " [translate_s(x)]; translate_sl(l))

and translate_s (ast:ast_s)
    :  string =
	match ast with
		| AST_error -> return ("ERROR")
		| AST_assign (str, ex) -> return(translate_assign str ex)
		| AST_read (str) -> return(translate_read str)
		| AST_write (ex) -> return(translate_write ex)
		| AST_if (ex, statl) -> return(translate_if ex statl)
		| AST_do (statl) -> return(translate_do statl)
		| AST_check (ex) ->	return(translate_check ex)

and translate_assign (id:string) (ex:ast_e) : string =
	if (List.mem id listofVariables) then 
		return((String.concat " " [id; "="; tranlate_expr(ex); ";"]))
	else
		listofVariables := !listofVariables@[id]
		return((String.concat " " ["int"; id; "="; tranlate_expr(ex); ";"]))

and translate_read (id:string) : string =
	listofVariables := !listofVariables@[id]
	return(String.concat " " ["int"; id; ";"])
		
and translate_if (ex:ast_e) (stat:ast_sl): string =
	return (String.concat " " ["if(";translate_expr(ex);") {";translate_s(stat);"}"])

and translate_do (stat: ast_sl): string =
	return (String.concat " " ["while(";translate_sl(stat);"}"])

and translate_check (ex: ast_e) : string=
	return (String.concat " " [translate_expr(ex);") {"])

and translate_write (id:ast_e) : string =
	return (String.concat " " ["printf(%d\n,";translate_expr(ex);");"])

and translate_expr (ex:ast_e) : string = match ex with
	| AST_binop (op, left_ex, right_ex) -> return (String.concat " " [translate_expr(left_ex);op;translate_expr(right_ex)])
	| AST_id -> if List.mem ex listofVariables then 
				listofVariables = List.filter (fun s -> s != ex) !listofVariables
				return ex
	| AST_num -> return ex