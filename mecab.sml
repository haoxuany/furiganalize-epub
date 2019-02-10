
signature MECAB = sig
  type mecab
  type result = (string * string) list

  exception ProcessSpawnFailure of string list
  val new : unit -> mecab (* process killed on system exit *)

  val query : mecab -> string -> result
  val print : result -> unit
end

functor MeCab(
  structure Configure : sig val mecab_exec : string ref end
) :> MECAB = struct
  open Unix
  open TextIO

  type mecab = (TextIO.instream, TextIO.outstream) proc
  type result = (string * string) list

  fun trim s =
    let
      val s = Substring.full s
      val s = Substring.dropl Char.isSpace s
      val s = Substring.dropr Char.isSpace s
    in Substring.string s end

  fun read mecab =
    let
      val bigread = 100
      val stream = textInstreamOf mecab

      fun polling_read stream =
        case canInput (stream, bigread) of
          NONE => nil
        | SOME 0 => nil
        | SOME k =>
            (case inputLine stream of
               NONE => nil
             | SOME line =>
                 let
                   val line = trim line
                 in
                   if String.size line = 0 then
                     polling_read stream
                   else
                     line :: (polling_read stream)
                 end)
    in
      case polling_read stream of
        nil => NONE
      | some => SOME some
    end

  fun write mecab s =
    let val stream = textOutstreamOf mecab
    in output (stream, s); output (stream, "\n"); flushOut stream end

  exception ProcessSpawnFailure of string list
  fun new () : mecab =
    let
      val mecab = execute
        (Process.find_in_path (!Configure.mecab_exec), nil)
      val () =
        case read mecab of
          SOME msg =>
            (* Something pretty horrific happened here *)
            raise ProcessSpawnFailure msg
        | NONE => ()
      val () = OS.Process.atExit
        (fn () => kill (mecab, Posix.Signal.term))
    in mecab end

  fun parse_reading s =
    let
      val result = List.filter (fn "EOS" => false | _ => true) s
      val result = List.map (fn s =>
        let
          val lr = String.fields (fn c => c = #"\t") s
          val (field, explanation) = (List.hd lr, List.last lr)
          val reading = String.tokens (fn c => c = #",") explanation
          val reading = List.nth (reading, (List.length reading) - 2)
        in (field, reading) end
      ) result
    in result end

  fun query mecab s =
    let
      val () = write mecab s;
      fun retry times =
        if times < 0 then (print "Warning: retry failure, MeCab slow\n"; nil)
        else
          case read mecab of
            NONE =>
              (OS.Process.sleep (Time.fromMilliseconds 5);
              retry (times - 1))
          | SOME result => result
      val result = retry 1000
    in parse_reading result end

  val print = fn s => print (String.concat (List.map
    (fn (word, reading) => String.concat [word, " => ", reading, "\n"]) s))
end
