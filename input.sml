signature INPUT =
sig
  val readFile : string -> string list
  val readInt  : string -> int
end

structure Input :> INPUT =
struct
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
end
