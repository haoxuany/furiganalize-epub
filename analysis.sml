
structure Analysis = struct

  open XML

  type ustring = UTF8Char.char list

  fun parse_string s =
    case s of
      "" => nil
    | _ =>
        let
          val (c, rest) = UTF8Char.from_string s
        in
          c :: (parse_string rest)
        end
  fun unparse_string s = String.concat (List.map UTF8Char.to_string s)

  fun dbg_ustring s = (print (unparse_string s); print "\n")

  fun tag_align (query : ustring * ustring) =
    let
      open UTF8Char
      fun h_k_split key = List.foldr (fn (c, grouping) =>
        case grouping of
          nil => [[c]]
        | h :: tail =>
            let val any = List.hd h
            in
              if (is_cjk c andalso is_cjk any) orelse
              (is_hiragana c andalso is_hiragana any)
              then
                (c :: h) :: tail
              else
                [c] :: grouping
            end)
         nil key

      val (key, value) = query
      val key = h_k_split (List.rev key)
      val value = List.rev value

      (* val () = print "key:\n" *)
      (* val _ = List.map dbg_ustring key *)

      fun split_with f l =
        case l of
          nil => (nil, nil)
        | h :: tail =>
            (case f l of
               NONE =>
                 let val (left, right) = split_with f tail
                 in (h :: left, right) end
             | SOME k => (nil, List.drop (l, k)))

      fun kana_split value splits =
        case splits of
          nil => (case value of nil => nil | _ => [value])
        | h :: rest =>
            let
              val (left, right) = split_with
                (fn l => if List.length l < List.length h then NONE
                  else
                    if ListPair.all (fn (c, c') => c = c') (l, h) then
                      SOME (List.length h)
                    else NONE)
                value
            in left :: h :: (kana_split right rest) end

      val splits = List.filter (List.exists is_hiragana) key
      (* val () = print "splits:\n" *)
      (* val _ = List.map dbg_ustring splits *)

      val value = kana_split value splits

      (* val () = print "value:\n" *)
      (* val _ = List.map dbg_ustring value *)

      val match = List.map
        (fn (a, b) => (List.rev a, List.rev b))
        (ListPair.zip (List.rev key, List.rev value))

      (* val () = print "match:\n" *)
      (* val _ = List.map (fn (a, b) => print (String.concat *)
      (*   [unparse_string a, " ", unparse_string b, "\n"])) match *)

      fun generate match =
        case match of
          nil => nil
        | (key, value) :: tail =>
            if List.exists is_hiragana key
            then (PlainText (unparse_string key)) :: generate tail
            else (Tag ({name = "ruby", attr = nil},
              [ PlainText (unparse_string key)
              , Tag ({name = "rt", attr = nil},
                [ PlainText (unparse_string value)
                ])
              ])) :: generate tail

      val tag = generate match

      (* val () = print (XML.to_string tag) *)
      (* val () = print "\n" *)

    in tag end



  fun analyze_text mecab xml =
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
      (* this is the flattened string *)
      val s = String.concat (flatten xml)
      (* val () = print s *)
      (* val () = print "\n" *)

      val query = MeCab.query mecab s

      fun sanitize query =
        let
          val query = List.foldr
            (fn ((key, value), res) =>
              let
                val s = parse_string key
              in
                if List.exists UTF8Char.is_cjk s then
                  (s, List.map UTF8Char.to_hiragana (parse_string value))
                  :: res
                else res
              end)
            nil query
        in query end

      (* this is the kanji mapping *)
      val query = sanitize query
      (* val () = print (String.concat (List.map *)
      (* (fn (key, value) => *)
      (* String.concat [ *)
      (* String.concat (List.map UTF8Char.to_string key), *)
      (* " => ", *)
      (* String.concat (List.map UTF8Char.to_string value), *)
      (* "\n"]) query)) *)
      (* val () = print "\n" *)

      fun split_first (s : ustring) (query : ustring) =
        if List.length s < List.length query then
          NONE
        else
        if ListPair.all (fn (c, c') => c = c') (s, query) then
          SOME (nil, List.drop (s, List.length query))
        else
          case s of
            nil => raise Fail "not possible due to queries nonempty"
          | h :: rest =>
              (case split_first rest query of
                 NONE => NONE
               | SOME (left, right) => SOME (h :: left, right))

      fun split_all (s : ustring) (query : (ustring * ustring) list) =
        case query of
          nil => ([PlainText (unparse_string s)], nil)
        | head :: rest =>
            (case split_first s (#1 head) of
               NONE => ([PlainText (unparse_string s)], query)
             | SOME (left, right) =>
                 let
                   val (xml, query) = split_all right rest
                   val tag = tag_align head
                 in ((PlainText (unparse_string left) :: tag) @ xml, query) end)

      fun replace elems query =
        case elems of
          nil => (nil, query)
        | head :: rest =>
            (case head of
              Whatever _ =>
                let
                  val (xml, query) = replace rest query
                in (head :: xml, query) end
            | PlainText s =>
                let
                  val s = parse_string s
                  val (xml, query) = split_all s query
                  val (rest, query) = replace rest query
                in
                  (xml @ rest, query)
                end
            (* this is the atrocius case. let's just hack it *)
            | Tag ({name = "ruby", attr = nil}, elems) =>
                (case query of
                  nil => (head :: rest, nil)
                | (key, _) :: qrest =>
                    let
                      val s = parse_string (String.concat (flatten elems))
                      (* val () = print "set:\n" *)
                      (* val () = dbg_ustring s *)
                      (* val () = dbg_ustring key *)
                    in
                      if List.hd key = List.hd s then
                        let val (xml, query) = replace rest qrest
                        in (head :: xml, query) end
                      else
                        let val (xml, query) = replace rest query
                        in (head :: xml, query) end
                    end)
            | Tag (tag, elems) =>
                let
                  val (xml, query) = replace elems query
                  val (sibling, query) = replace rest query
                in ((Tag (tag, xml)) :: sibling, query) end
            )

      val (xml, _) = replace xml query

      (* val () = print (XML.to_string xml) *)

    in xml end

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
