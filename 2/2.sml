use "../input.sml";

datatype Dict = Leaf
	      | Node of (char * int * Dict * Dict)

fun insert (k, v, Leaf) = Node (k, v, Leaf, Leaf)
  | insert (k, v, Node (kk, vv, left, right)) =
    if k = kk
    then Node (k, v, left, right)
    else
      if k < kk
      then Node (kk, vv, insert (k, v, left), right)
      else Node (kk, vv, left, insert (k, v, right))

fun lookup (key, Leaf) = NONE
  | lookup (key, Node (k, v, left, right)) =
    if key = k
    then SOME v
    else
      if (key < k)
      then lookup (key, left)
      else lookup (key, right)

fun count_chars cs =
    let
      fun iter acc rest =
	  case rest of
	      [] => acc
	    | head :: tail =>
	      case lookup (head, acc) of
		  NONE => iter (insert (head, 1, acc)) tail
		| SOME n => iter (insert (head, n + 1, acc)) tail
    in
      iter Leaf cs
    end

fun dict_to_list Leaf = []
  | dict_to_list (Node (k, v, left, right)) =
    (k, v) :: ((dict_to_list left) @ (dict_to_list right))

fun exactly_n n = List.filter (fn (_, m) => m = n)

fun has_n_repeats n lst = length (exactly_n n lst) > 0

val answer =
    let
      val filename = "input.txt"
      val charLists = ((map String.explode) o Input.readFile) filename
      val counted = map (dict_to_list o count_chars) charLists
      val hasTwoRepeats =
	  List.filter (fn cs => has_n_repeats 2 cs) counted
      val hasThreeRepeats =
	  List.filter (fn cs => has_n_repeats 3 cs) counted
    in
      length hasTwoRepeats * length hasThreeRepeats
    end
