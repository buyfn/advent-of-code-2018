use "../input.sml";

val sum = foldl op+ 0

fun contains (lst: int list, x: int) =
    case lst of
	[] => false
      | head::tail => x = head orelse contains (tail, x)

fun firstSumRep xs =
    let
      fun aux (sums, curSum, xs') =
	  case xs' of
	      [] => aux (sums, curSum, xs)
	    | head::tail =>
	      let
		val s = curSum + head
	      in
		if contains (sums, s)
		then s
		else aux (s::sums, s, tail)
	      end
    in
      aux ([], 0, xs)
    end

val (ans1, ans2) =
    let
      val filename = "input.txt"
      val strings = Input.readFile filename
      val intList = map Input.readInt strings
    in
      (sum intList, firstSumRep intList)
    end
