
(* haoxuany: Please don't use this in your own projects lmao
* I've never bothered to read the XML spec, just some example file *)
signature XML = sig
  type tag =
    { name : string, attr: (string * string) list }

  datatype xmlelem =
    PlainText of string
  | Tag of tag * xmlelem list
  | Whatever of string
  (* doctype, etc, things that aren't just simple tags or whatever *)

  type xml = xmlelem list

  val parse : string -> xml
  val to_string : xml -> string
end

structure DumbassXML : XML = struct

  type tag =
    { name : string, attr: (string * string) list }

  datatype xmlelem =
    PlainText of string
  | Tag of tag * xmlelem list
  | Whatever of string
  (* doctype, etc, things that aren't just simple tags or whatever *)

  type xml = xmlelem list

  fun dbg s =
    let
      val () = print "==dbg==\n"
      val () = print (String.implode s)
      val () = print "\n"
    in () end

  fun strip_spaces s =
    case s of
      nil => nil
    | c :: rest =>
        if Char.isSpace c
        then strip_spaces rest
        else c :: rest

  fun listuntil f l =
    let
      fun help f (nottrue, rest) =
        case rest of
          nil => (nottrue, nil)
        | c :: leftover =>
            if f c
            then (nottrue, rest)
            else help f (c :: nottrue, leftover)
      val (nottrue, rest) = help f (nil, l)
    in (List.rev nottrue, rest) end

  type clist = char list

  fun parse (s : string) : xml = let

    fun parse_begin s =
      case s of
        nil => (nil, nil)
      | #"<" :: rest =>
          (case strip_spaces rest of
             #"/" :: rest =>
             let
               (* not gonna bother to check end tags are the same *)
               val (_, rest) = parse_tag_inside rest
             in
               case strip_spaces rest of
                 #">" :: rest => (nil, rest)
               | _ => (nil, rest) (* lol *)
             end
          | c :: _ =>
              if Char.isAlpha c then
                let
                  val (tag, rest) = parse_tag_inside rest

                  fun parse_open_tag tag s =
                    let
                      val (xml, rest) = parse_begin s
                      val (sibling, rest) = parse_begin rest
                    in ((Tag (tag, xml)) :: sibling, rest) end

                  fun parse_close_tag tag s =
                    let
                      val (xml, rest) = parse_begin s
                    in ((Tag (tag, nil)) :: xml, rest) end
                in
                  case strip_spaces rest of
                    #"/" :: #">" :: rest => parse_close_tag tag rest
                  | #">" :: rest => parse_open_tag tag rest
                  | _ => parse_open_tag tag rest (* lol *)
                end
              else
                (* the whatever case *)
                let
                  val (whatev, rest) =
                    listuntil
                    (fn c => c = #">")
                    rest
                  val (xml, rest) = parse_begin (List.tl rest)
                in ((Whatever (String.implode whatev)) :: xml, rest) end
          | nil => raise Fail "Unclosed open '<'"
          (* so pernicious not dealing with *)
              )
      | _ =>
          let
            val (text, rest) =
              listuntil
              (fn c => c = #"<")
              s
            val (xml, rest) = parse_begin rest
          in ((PlainText (String.implode text)) :: xml, rest) end

    (* parses tag information (only) *)
    and parse_tag_inside s =
      let
        val (name, rest) =
          listuntil
          (fn c => Char.isSpace c orelse c = #">" orelse c = #"/")
          (strip_spaces s)

        val (attr, rest) = parse_attr rest
      in ({name = String.implode name, attr = attr}, rest) end

    (* parses attributes *)
    and parse_attr s =
      let
        fun parse_one s =
          let
            val (name, rest) =
              listuntil
              (fn c => Char.isSpace c orelse c = #"="
              orelse c = #"/" orelse c = #">")
              (strip_spaces s)
          in
            case strip_spaces rest of
              #"=" :: rest =>
                (case strip_spaces rest of
                   #"\"" :: rest =>
                   let
                     val (value, rest) =
                       listuntil (fn c => c = #"\"") rest
                    in SOME ((String.implode name, String.implode value),
                      List.tl rest) end
                 | _ => NONE)
            | _ => NONE
          end

        fun parse_all s =
          case parse_one s of
            NONE => (nil, s)
          | SOME (attr, rest) =>
              let
                val (attrs, rest) = parse_all rest
              in (attr :: attrs, rest) end

      in parse_all s end

  in #1 (parse_begin (String.explode s)) end

  fun to_string xml =
    let
      fun single xml =
        case xml of
          PlainText s => s
        | Whatever s => String.concat ["<", s, ">"]
        | Tag ({name, attr}, elems) =>
            if List.null elems
            then String.concat
              ["<", name,
                String.concat
                (List.map
                  (fn (key, value) =>
                  String.concat [" ", key, "=\"", value, "\""])
                  attr
                ),
                "/>"
              ]
            else String.concat
              ["<", name,
                String.concat
                (List.map
                  (fn (key, value) =>
                  String.concat [" ", key, "=\"", value, "\""])
                  attr
                ),
                ">",
                to_string elems,
                "</", name, ">"
              ]
    in String.concat (map single xml) end
end

structure XML = DumbassXML
