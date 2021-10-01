
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

  fun run_on_epub mecab backup file =
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

      val () =
        if backup then
          let
            val instream = BinIO.openIn file
            val outstream = BinIO.openOut bkp

            val content = BinIO.inputAll instream
            val () = BinIO.closeIn instream

            val () = BinIO.output (outstream, content)
            val () = BinIO.closeOut outstream
          in () end
        else ()

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
          | SOME "html" => run_on_file mecab f
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

  type arg = { files : string list, make_backup : bool }

  fun run name args =
    let
      open GetOpt

      val bkp_opt =
        { short = "d"
        , long = ["no-backup"]
        , desc = NoArg (fn () => true)
        , help = "If you don't want to make a backup file before processing"
        }
      val options = [bkp_opt];

      val header =
        String.concatWith "\n"
        [ "Utility to add furigana to content in epub."
        , "Books must be in epub format, with content in UTF8 encoded HTML or XHTML."
        , ""
        , name ^ " [-d|no-backup] FILE1 FILE2 ..."
        , ""
        ]

      val usage = usageInfo
        { header = header
        , options = options
        }

      fun fail s =
        (print s; print "\n\n"; print usage;
        OS.Process.exit OS.Process.failure)

      val (opt, arg) = getOpt
        { argOrder = Permute
        , options = options
        , errFn = fail
        } args

      val arg =
        if List.null arg then
          fail "Must provide at least one epub file!"
        else
          { files = arg, make_backup = not (List.exists (fn b => b) opt) }

      val mecab = MeCab.new ()

      val _ =
        List.map (run_on_epub mecab (#make_backup arg)) (#files arg)
    in () end
end

structure Main = Main(structure Configure = Configure)
