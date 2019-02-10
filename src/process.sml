
structure Process = struct
  fun find_in_path s =
    case OS.Process.getEnv "PATH" of
      NONE => s
    | SOME paths =>
        if OS.Path.isAbsolute s then s else
        let
          val paths = String.tokens (fn #":" => true | _ => false) paths
          val paths = List.map (fn p => OS.Path.concat (p, s)) paths
        in
          case List.find (fn s =>
            Posix.FileSys.access (s, [Posix.FileSys.A_EXEC])) paths of
            NONE => s
          | SOME s => OS.Path.mkCanonical s
        end
end
