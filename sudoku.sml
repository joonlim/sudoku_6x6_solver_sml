(* Joon Lim 109558002 *)
(* hw4.sml *)
(* 6x6 Sudoku Solver *)

(* The function hw4() takes the input as a list of lists of int where the empty cells will be -1. *)

(* negative numbers use ~ rather than - *)


(* Convert list of lists into list. We can call convertToPuzzle on the result of this *)
(* list list -> list *)
fun flatten([]) = []
| flatten(X::XS) = X@flatten(XS);


(* Get the value in the kth index, assuming the starting index is 0 *)
(* int list * int -> int *)
fun kth([], k) = 0
| kth(X::XS, 0) = X
| kth(X::XS, k) = kth(XS, k-1);


(* Set the value in the kth index of a list to V *)
(* int list * int * int -> list *)
fun setKth(X::XS, 0, V) = V::XS
| setKth(X::XS, k, V) = X::setKth(XS, k-1, V);


(* Get the element using row, col in a square array *)
(* int list * int * int * int -> int *)
fun getValue(L, row, col, dimension) = 
	let
		val i = dimension * row + col
	in
		kth(L, i)
	end;


(* Set X at row, col in a square array *)
(* int list * int * int * int * int -> list *)
fun setValue(L, row, col, dimension, X) =
	let
		val i = dimension * row + col
	in
		setKth(L, i, X)
	end;

(* Check if value is valid in row *)
(* Call this for X = 1-6 *)
(* Pass in L, row, 0, X *)
fun checkRow(L, row, col, X) = if col = 5 then
	let
		val value = getValue(L, row, 5, 6)
	in
		if value = X then false
		else true
	end
else
	let
		val value = getValue(L, row, col, 6)
	in
		if value = X then false
		else checkRow(L, row, col + 1, X)
	end;

(* Check if value is valid in column *)
(* Call this for X = 1-6 *)
(* Pass in L, 0, col, X *)
fun checkCol(L, row, col, X) = if row = 5 then
	let
		val value = getValue(L, 5, col, 6)
	in
		if value = X then false
		else true
	end
else
	let
		val value = getValue(L, row, col, 6)
	in
		if value = X then false
		else checkCol(L, row + 1, col, X)
	end;


(* Check if value is valid in box *)
(* Pass in L, row, col, 0, 0, X *)
fun checkBox(L, row, col, i, j, X) =
	let
		val boxRow = row - row mod 2
		val boxCol = col - col mod 3
		val value = getValue(L, boxRow + i, boxCol +j, 6)
	in

		if value = X
			then false
		else
			if i = 1 andalso j = 2 then
				true
			else if j <> 2 then
				checkBox(L, row, col, i, j + 1, X)
			else
				checkBox(L, row, col, i + 1, 0, X)
	end;


(* Check if value is valid in row, col, and box *)
(* int list * int * int * int -> bool *)
fun checkValue(L, row, col, X) =
	checkCol(L, 0, col, X) andalso checkRow(L, row, 0, X) andalso checkBox(L, row, col, 0, 0, X);


(* Check to see if the list contains any ~1s *)
fun solved([]) = true
| solved(X::XS) = if X = ~1 then false
else solved(XS);


(* Call solve(puzzle, 0) *)
fun solve(L, row, col) = if solved(L) = true then L
else
	let
		val value = getValue(L, row, col, 6)
	in
		if value = ~1 then
			let
				fun listOfPossibilities(L, i) = 
					if checkValue(L, row, col, i) = true then
						if i = 6 then
							[6]
						else
							i::listOfPossibilities(L, i+1)
					else
						if i = 6 then
							[]
						else
							listOfPossibilities(L, i+1)

			in
				let
					val listOfPos = listOfPossibilities(L, 1)
				in
					if length(listOfPos) = 1 then
						let
							val newL = setValue(L, row, col, 6, hd(listOfPos))
						in
							if row = 5 andalso col = 5 then solve(newL, 0, 0)
							else if col = 5 then solve(newL, row+1, 0)
							else solve(newL, row, col+1)
						end
					else
						if row = 5 andalso col = 5 then solve(L, 0, 0)
						else if col = 5 then solve(L, row+1, 0)
						else solve(L, row, col+1)
				end
			end
		else
			if row = 5 andalso col = 5 then solve(L, 0, 0)
			else if col = 5 then solve(L, row+1, 0)
			else solve(L, row, col+1)
	end;


(* print puzzle *)
(* print(Int.toString(n)) *)
(* Ugly print function *)
fun printPuzzle(L, n) = 
let
	val value = kth(L, n)
in
	if n = 36 then
		print("\n")
	else if n = 0 then
		let 
			val temp = print("\n" ^ Int.toString(value) ^ " ")
		in
			printPuzzle(L, n+1)
		end
	else if n mod 6 = 5 then
		let
			val temp = print(Int.toString(value) ^ "\n")
		in
			printPuzzle(L, n+1)
		end
	else
		let
			val temp = print(Int.toString(value) ^ " ")
		in
			printPuzzle(L, n+1)
		end
end;



(* Call to function *)
fun hw4(X:int list list) = 
	let
		val L = flatten(X)
		val solution = solve(L, 0, 0)
	in
		printPuzzle(solution, 0)
	end;



