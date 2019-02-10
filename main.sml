
structure Main = struct

  fun run_on_file mecab file =
    let
      open TextIO
      val instream = openIn file
      val s = inputAll instream
      val xml = XML.parse s
      val () = TextIO.closeIn instream

      val _ = Analysis.traverse_all mecab xml
      (* val () = print (XML.to_string xml) *)
    in () end

  (* dummy run to try things out *)
  fun drun () =
    let
      val mecab = MeCab.new ()
      val file = "../../sandbox/test.xhtml"
      val () = run_on_file mecab file
    in () end
end
