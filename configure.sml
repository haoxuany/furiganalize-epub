
structure Configure = struct
  val mecab_exec = ref "mecab"

  (* haoxuany: if you're one of the fancy schmancy ffi
  * people, then ffi here.
  * FFI for current SML compilers tend to be a nightmare of
  * varying degrees, so I'm just gonna spawn a process.
  *)
  exception ProcessFailure of OS.Process.status
  val unzip_to = ref
    ( fn { file : string, dirout : string } =>
      let
        val unzip = Process.find_in_path "unzip"
        val cmd = String.concatWith " "
          [ unzip
          , file
          , "-d"
          , dirout
          ]
        val status = OS.Process.system cmd;
      in if OS.Process.isSuccess status then () else
        raise ProcessFailure status
      end
    )

  val zip_to = ref
    ( fn { dir : string, file : string } =>
      let
        val () =
          if OS.FileSys.access (file, [OS.FileSys.A_READ])
          then OS.FileSys.remove file
          else ()

        (* zip command shenannigans lol *)
        val current = OS.FileSys.getDir ()
        val file = OS.Path.mkAbsolute {path = file, relativeTo = current}

        val zip = Process.find_in_path "zip"
        val cmd = String.concatWith " "
          [ zip
          , "-r"
          , file
          , "."
          ]

        (* shells are stupid (so is the zip command) *)
        val cmd = String.concatWith " && "
          [ "cd " ^ dir
          , cmd
          , "cd " ^ current
          ]

        val status = OS.Process.system cmd;
      in if OS.Process.isSuccess status then () else
        raise ProcessFailure status
      end
    )
end

structure MeCab = MeCab(structure Configure = Configure)
