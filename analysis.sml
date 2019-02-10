
structure Analysis = struct

  open XML

  fun analyze_text mecab elems =
    let
      fun flatten elems =
        case elems of
          nil => nil
        | head :: rest =>
            let
              val rest = flatten rest
            in
              case head of
                 PlainText s => s :: rest
               | Whatever _ => rest
               | Tag ({name = "rt", attr = nil}, _) => rest
               | Tag (_, children) =>
                   (String.concat (flatten children)) :: rest
            end
      val s = String.concat (flatten elems)
      val () = print s
      val () = print "\n"

      val query = MeCab.query mecab s
      val () = MeCab.print query
      val () = print "\n"
    in elems end

  fun traverse mecab xml =
    case xml of
      PlainText _ => xml
    | Whatever _ => xml
    | Tag (tag, elems) =>
        let val name = #name tag
        in
          (* this is what happens most of the time *)
          if name = "p" orelse name = "title" then
            Tag (tag, analyze_text mecab elems)
          else
            Tag (tag, map (traverse mecab) elems)
        end

  fun traverse_all mecab xml = map (traverse mecab) xml
end
