exception InvalidNumber

fun readFile filename =
    let
      val file = TextIO.openIn filename
      val content = TextIO.inputAll file
      val _ = TextIO.closeIn file
    in
      String.tokens (fn c => c = #"\n") content
    end

fun readInt s =
    let
      val sign = String.sub(s, 0)
      val v = Int.fromString(String.extract (s, 1, NONE))
    in
      case v of
	  NONE => raise InvalidNumber
	| SOME n => if sign = #"-"
		    then ~n
		    else n
    end

fun sum xs = foldl (fn (x, y) => x + y) 0 xs

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
      val strings = readFile filename
      val intList = map readInt strings
    in
      (foldl (fn (x, y) => x + y) 0 intList,
       firstSumRep intList)
    end
