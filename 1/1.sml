exception invalidNumber

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
	  NONE => raise invalidNumber
	| SOME n => if sign = #"-"
		    then ~n
		    else n
    end

val ans =
    let
      val filename = "input.txt"
      val strings = readFile filename
      val intList = map readInt strings
    in
      foldl (fn (x, y) => x + y) 0 intList
    end
