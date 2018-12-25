use "../input.sml";

fun same_part (str1, str2) =
    List.filter (fn (a, b) => a = b)
		(ListPair.zip (String.explode str1,
			       String.explode str2))

fun count_diff str1 str2 =
    size str1 - length (same_part (str1, str2))

val answer =
    let
      val filename = "input.txt"
      val input = Input.readFile filename
				 
      fun pairs [] = []
	| pairs (head :: tail) =
	  (map (fn x => (head, x)) tail) @ (pairs tail)

      val p::_ = List.filter (fn (a, b) => count_diff a b = 1)
      			     (pairs input)
    in
      String.implode (map (fn (a, _) => a) (same_part p))
    end
