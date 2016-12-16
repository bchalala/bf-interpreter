
(* 	Brainfuck intepreter by Brett Chalabian, 2016 *)

(* 	Feel free to use, distribute, or generally do whatever you want
	with this piece of code. For the love of god, don't, but if you do,
	then whatever I guess. This language sucks, and I'm sure a much
	better interpretter could be written, but for now this is what I have.

	If you have any changes, please submit a pull request and my team
	will evaluate your changes to ensure they meet both our coding style guide
	as well as our personal style guide. Chino shorts are never acceptable.

	Have a great day, esoteric programming languages are esoteric.	*)

open Scanf

exception LoopSyntaxError

(* Brainfuck terms as follows:
	IncPtr >
	DecPtr <
	IncByte +
	DecByte -
	Loop []
	OutByte . 
	GetByte ,
*)

type term = IncPtr | DecPtr | IncByte  | DecByte | OutByte | Loop of term list | GetByte

(* Explodes strings into character list to make pattern matching easier *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* Takes care of parsing loops. *)
let rec parseLoop charlist = match charlist with
	| ']' :: charl -> ([], charl)
	| '[' :: charl -> 
		let (tl1, ncl1) = parseLoop charl in
		let (tl2, ncl2) = parseLoop ncl1 in (Loop(tl1) :: tl2, ncl2)
	| ',' :: charl -> let (tl, ncl) = parseLoop charl in (GetByte :: tl, ncl)
	| '>' :: charl -> let (tl, ncl) = parseLoop charl in (IncPtr :: tl, ncl)
	| '<' :: charl -> let (tl, ncl) = parseLoop charl in (DecPtr :: tl, ncl)
	| '+' :: charl -> let (tl, ncl) = parseLoop charl in (IncByte :: tl, ncl)
	| '-' :: charl -> let (tl, ncl) = parseLoop charl in (DecByte :: tl, ncl)
	| '.' :: charl -> let (tl, ncl) = parseLoop charl in (OutByte :: tl, ncl)
	| _ :: charl -> parseLoop charl 
	| [] -> raise LoopSyntaxError

(* Parses brainfuck characters into usable terms *)
let rec parse charlist = match charlist with 
	| ',' :: cl -> GetByte :: (parse cl)
	| '>' :: cl -> IncPtr :: (parse cl)
	| '<' :: cl -> DecPtr :: (parse cl)
	| '+' :: cl -> IncByte :: (parse cl)
	| '-' :: cl -> DecByte :: (parse cl)
	| '.' :: cl -> OutByte :: (parse cl)
	| ']' :: _ -> raise LoopSyntaxError
	| '[' :: cl -> let (tl, ncl) = parseLoop cl in Loop(tl) :: parse ncl
	| _ :: cl-> parse cl 
	| [] -> []
	

(*  A map of values to values represents the "tape" that brainfuck uses to
	store data. This map gets passed through evaluation. *)

module Tape = Map.Make(struct type t = int let compare = compare end)


(* 	Evaluate function evaluates the current term, then attempts to evaluate the
	next term. My term typing necessitates this "EndTerm" sort of semantic which
	is kind of unnecessary had I implemented the program as a list of terms. *)

let rec eval tlist cur tape = match tlist with
	| IncPtr :: tl -> eval tl (cur + 1) tape
	| DecPtr :: tl -> eval tl (if cur == 0 then 0 else (cur - 1)) tape
	| IncByte :: tl -> 
		let ntape = (match Tape.mem cur tape with
			  false -> Tape.add cur 1 tape
			| true -> Tape.add cur ((Tape.find cur tape) + 1) tape ) in
		eval tl cur ntape
	| DecByte :: tl ->
		let ntape = ( match Tape.mem cur tape with
			  false -> Tape.add cur (-1) tape
			| true -> Tape.add cur ((Tape.find cur tape) - 1) tape) in
		eval tl cur ntape
	| OutByte :: tl -> 
		(if Tape.mem cur tape 
			then print_char (Char.chr ((Tape.find cur tape) mod 256))
			else print_char (Char.chr 0)); eval tl cur tape
	| Loop(loopt) :: tl -> if (Tape.mem cur tape) = false then (eval tl cur tape) else
		(let v = (Tape.find cur tape mod 256) in 
			if v == 0 then eval tl cur tape
			else let (ncur, ntape) = eval loopt cur tape in eval tlist ncur ntape)
	| GetByte :: tl -> 
		let inchar = input_char stdin in 
		let ntape = Tape.add cur (int_of_char inchar) tape in eval tl cur ntape
	| _ -> (cur, tape);;



(* Boilerplate stuff associated with reading lines from files, reading command line arguments, etc. *)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let () = if (Array.length Sys.argv) - 1 <> 1 then print_string "You must supply a brainfuck program file as input.\n"
	else let prgm = Sys.argv.(1) in 
		 let parsedProgram = parse (explode (String.concat "" (read_lines prgm))) in (eval parsedProgram 0 Tape.empty); ();;
