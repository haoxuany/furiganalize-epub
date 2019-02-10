
structure Build = struct
  fun run (name, args) = (Main.run name args; OS.Process.success);

  val bin_path = "bin"

  val () =
    if OS.FileSys.access (bin_path, [OS.FileSys.A_WRITE])
    andalso OS.FileSys.isDir bin_path then ()
    else OS.FileSys.mkDir bin_path

  val _ = OS.Process.system "cp ./njskel.sh ./bin/furiganalize"

  (* this seems broken on 110.79 wtf? *)
  val () = SMLofNJ.exportFn ("bin/furiganalize", run);
  val () = OS.Process.exit OS.Process.success;
end
