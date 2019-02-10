
structure Build = struct
  val () = Main.run (CommandLine.name ()) (CommandLine.arguments ())
end
