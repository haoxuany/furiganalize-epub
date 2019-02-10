
functor Main(
  structure Configure : sig
    val unzip_to : ( { file : string, dirout : string } -> unit ) ref
    val zip_to : ( { dir : string, file : string } -> unit ) ref
  end
) = struct

  fun run_on_file mecab file =
    let
      open TextIO
      val instream = openIn file
      val s = inputAll instream
      val xml = XML.parse s
      val () = TextIO.closeIn instream

      val xml = Analysis.traverse_all mecab xml
      val xml = XML.to_string xml

      val outstream = openOut file
      val () = TextIO.output (outstream, xml)
      val () = TextIO.closeOut outstream
    in () end

  fun run_on_epub mecab file =
    let
      (* Backup first *)
      val {dir = dir, file = f} = OS.Path.splitDirFile file
      val {base, ext} = OS.Path.splitBaseExt f
      val f = OS.Path.joinBaseExt
      { base = String.concatWith "_" [base, "bkp",
          Date.toString (Date.fromTimeLocal (Time.now ()))]
      , ext = ext
      }
      val bkp = OS.Path.joinDirFile {dir = dir, file = f}

      val instream = BinIO.openIn file
      val outstream = BinIO.openOut bkp

      val content = BinIO.inputAll instream
      val () = BinIO.closeIn instream

      val () = BinIO.output (outstream, content)
      val () = BinIO.closeOut outstream

      (* Extract *)
      val dirout = OS.Path.concat (dir,
        LargeInt.toString (Time.toSeconds (Time.now ())))
      val () = (!Configure.unzip_to) { file = file, dirout = dirout }

      (* Traverse *)
      open OS.FileSys

      fun traverse f dirstream path =
        case readDir dirstream of
          NONE => closeDir dirstream
        | SOME s =>
            let
              val fpath = OS.Path.concat (path, s)

              val () =
                if isDir fpath then traverse f (openDir fpath) fpath
                else ()

              val () = f fpath

            in traverse f dirstream path end

      val traverse = fn f => fn path => traverse f (openDir path) path

      val () = traverse
        (fn f =>
          case OS.Path.ext f of
            SOME "xhtml" => run_on_file mecab f
          | _ => ())
        dirout

      (* Zip *)
      val () = (!Configure.zip_to) { dir = dirout, file = file }
      val () =
        traverse
          (fn f =>
            let open OS.FileSys in
              if isDir f then rmDir f
              else remove f
            end)
          dirout
      val () = OS.FileSys.rmDir dirout

    in () end

  (* dummy run to try things out *)
  fun drun () =
    let
      val mecab = MeCab.new ()
      val file = "../../sandbox/test.epub"
      val () = run_on_epub mecab file
      (* val () = run_on_file mecab file *)
    in () end
end

structure Main = Main(structure Configure = Configure)
