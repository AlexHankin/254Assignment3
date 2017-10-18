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

let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2;;

let rec string_of_chars strs = 
	match strs with
	| [] -> ""
	| x :: l -> String.concat " " [x;string_of_chars l]
;;

let rec rem_item strs a=
	match strs with
	| [] -> []
	| x :: l when x == a -> rem_item l a
	| x :: l -> [x]@(rem_item l a)
;;

let rec translate (ast:ast_sl)
    :  string *  string
    (* warnings  output_program *) =
    let cProgram = "#include <stdio.h>\n #include <stdlib.h>\nint getint() {\nint myint;\nif (scanf(\"%d\", &myint) == 1){\nreturn myint;}\nelse {\nprintf(\"The input value is not of type int\");\nprintf(\"Terminating program...\");\nexit(0);}}\nvoid putint(int n) {\nprintf(\"%d\", n);\nprintf(\"\\n\");\n}\nint main () {" in
    let errorMsg = "Warning: the following variables are read or assigned but never used, " in
    let varList = ["List:"] in
    let cProgram2 = 
	    match ast with
	    | [] -> ("", [])
	    | x :: l -> let sReturn = translate_s x varList in
	    			let slReturn = translate_sl l varList in
	    			(String.concat " " [(fst sReturn); (fst slReturn)], (snd sReturn)@(snd slReturn)) in
	let cProgram3 = String.concat " " [cProgram;(fst cProgram2);"}"] in
	let errorList = string_of_chars (snd cProgram2) in
	let errorResult = String.concat " " [errorMsg;errorList] in
    (errorResult, cProgram3)

and translate_sl (ast:ast_sl) (vlst) = match ast with
	| [] -> ("", [])
	| x :: l -> let sReturn = translate_s x vlst in
	    		let slReturn = translate_sl l vlst in
	    		(String.concat " " [(fst sReturn); (fst slReturn)], (snd sReturn)@(snd slReturn))

and translate_s (ast:ast_s) vlst =
	match ast with
		| AST_error -> ("ERROR", [])
		| AST_assign (str, ex) -> translate_assign str ex vlst
		| AST_read (str) -> translate_read str vlst
		| AST_write (ex) -> translate_write ex vlst
		| AST_if (ex, statl) -> translate_if ex statl vlst
		| AST_do (statl) -> translate_do statl vlst
		| AST_check (ex) ->	translate_check ex vlst

and translate_assign (id:string) (ex:ast_e) vlst  =
	if List.mem id vlst then
		let exReturn = translate_expr ex vlst in
		(String.concat " " [id; "="; (fst exReturn); ";"], (snd exReturn))
	else
		let vlst2 = vlst@[id] in
		let exReturn = translate_expr ex vlst2 in
		(String.concat " " ["int"; id; "="; (fst exReturn); ";"], (snd exReturn))

and translate_read (id:string) vlst =
	if List.mem id vlst then 
		(String.concat " " ["int"; id; ";"], vlst)
	else
		let vlst2 = vlst@[id] in
		(String.concat " " ["int"; id; ";"], vlst2)
	
and translate_if (ex:ast_e) (stat:ast_sl) vlst =
	let exReturn = translate_expr ex vlst in
	let slReturn = translate_sl stat (snd exReturn) in 
	(String.concat " " ["if(";(fst exReturn);") {";(fst slReturn); "}"], (snd slReturn))

and translate_do (stat: ast_sl) vlst =
	let slReturn = translate_sl stat vlst in 
	(String.concat " " ["while(";(fst slReturn);"}"], (snd slReturn))

and translate_check (ex: ast_e) vlst =
	let exReturn = translate_expr ex vlst in
	(String.concat " " [(fst exReturn);") {"], (snd exReturn))

and translate_write (id:ast_e) vlst =
	let exReturn = translate_expr id vlst in
	(String.concat " " ["printf(%d\n,";(fst exReturn);");"], (snd exReturn))
	
and translate_expr (ex:ast_e) vlst = match ex with
	| AST_binop (op, left_ex, right_ex) -> let exLeft = translate_expr left_ex vlst in
											let exRight = translate_expr right_ex (snd exLeft) in
											(String.concat " " [(fst exLeft) ;op;(fst exRight)], (snd exRight))
	| AST_id (str) -> if List.mem str vlst then 
						let vlst2 = rem_item vlst str in
						(str, vlst2)
						else
						(str, vlst)
	| AST_num (str) -> (str, vlst)
;;

let cstuf = (translate []);;
let cstuf2 = snd cstuf;;
print_string cstuf2;;
(*	
	*)