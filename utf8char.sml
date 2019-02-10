
(* haoxuany: please don't use this in your own project lmao.
* all I did is implement this: https://en.wikipedia.org/wiki/UTF-8
* I know jack shit about unicode standards.
* *)

signature UTF8_CHAR = sig
  eqtype char (* :( *)

  val from_code_point : Word32.word -> char
  val to_code_point : char -> Word32.word

  val from_string : string -> (char * string)
  val to_string : char -> string

  val is_hiragana : char -> bool
  val is_katakana : char -> bool
  val is_cjk : char -> bool

  val to_hiragana : char -> char
  val to_katakana : char -> char
end

structure UTF8Char : UTF8_CHAR = struct
  type char = Word32.word
  (* they are represented by code points *)

  fun from_code_point c = c
  fun to_code_point c = c

  fun from_string s =
    let
      open Word8
      val << = Word32.<<
      val >> = Word32.>>
      infix 4 <<
      infix 4 >>
      infix 4 andb

      fun slice n = String.extract (s, n, NONE)
      fun convert x = Word32.fromInt (toInt x)
      fun combine l = List.foldr Word32.orb 0wx0 l

      val c = Byte.stringToBytes s
      val first = Word8Vector.sub (c, 0)
    in
      if first < 0wx80 then
        (* ascii *)
        (convert first, slice 1)
      else
        let
          val second = Word8Vector.sub (c, 1)
        in
          if 0wx20 andb first = 0wx0 then
            (* 2 byte *)
            (combine
            [ (convert (first andb 0wx1F)) << 0w6
            , convert (second andb 0wx3F)
            ],
            slice 2)
          else
            let
              val third = Word8Vector.sub (c, 2)
            in
              if 0wx10 andb first = 0wx0 then
                (* 3 byte *)
                (combine
                [ (convert (first andb 0wxF)) << 0w12
                , (convert (second andb 0wx3F)) << 0w6
                , convert (third andb 0wx3F)
                ],
                slice 3)
              else
                (* 4 byte *)
                let val fourth = Word8Vector.sub (c, 3) in
                  (combine
                  [ (convert (first andb 0wx7)) << 0w18
                  , (convert (second andb 0wx3F)) << 0w12
                  , (convert (third andb 0wx3F)) << 0w6
                  , convert (fourth andb 0wx3F)
                  ],
                  slice 4)
                end
            end
        end
    end

  fun to_string c =
    let
      open Word32
      val orb = Word8.orb
      infix 5 <=
      infix 5 >>
      infix 5 orb
      infix 5 andb

      fun str c = Byte.bytesToString (Word8Vector.fromList c)
      fun convert x = Word8.fromInt (toInt x)
      fun combine l = List.foldr Word8.orb 0wx0 l
    in
      if c <= 0wx7F then
        (* ascii *)
        str [convert c]
      else
        if c <= 0wx7FF then
          (* 2 byte *)
          str
          [ 0wxC0 orb (convert (c >> 0w6))
          , 0wx80 orb (convert (c andb 0wx3F))
          ]
        else
          if c <= 0wxFFFF then
            (* 3 byte *)
            str
            [ 0wxE0 orb (convert (c >> 0w12))
            , 0wx80 orb (convert ((c >> 0w6) andb 0wx3F))
            , 0wx80 orb (convert (c andb 0wx3F))
            ]
          else
            (* 4 byte *)
            str
            [ 0wxF0 orb (convert (c >> 0w18))
            , 0wx80 orb (convert ((c >> 0w12) andb 0wx3F))
            , 0wx80 orb (convert ((c >> 0w6) andb 0wx3F))
            , 0wx80 orb (convert (c andb 0wx3F))
            ]
    end

  local
    open Word32
    infix 5 <=
    infix 5 >=
  in
    fun is_hiragana c = c >= 0wx3040 andalso c <= 0wx309F

    fun is_katakana c = c >= 0wx30A0 andalso c <= 0wx30FF

    fun to_hiragana c = if is_katakana c then c - 0wx60 else c

    fun to_katakana c = if is_hiragana c then c + 0wx60 else c

    fun is_cjk c =
      let
        val ranges =
          [ (0wx4E00, 0wx9FEF) (* CJK unified *)
          , (0wx3400, 0wx4DB5) (* Extension A *)
          , (0wx20000, 0wx2A6D6) (* Extension B *)
          , (0wx2A700, 0wx2B734) (* Extension C *)
          , (0wx2B740, 0wx2B81D) (* Extension D *)
          , (0wx2B820, 0wx2CEA1) (* Extension E *)
          , (0wx2CEB0, 0wx2EBE0) (* Extension F *)
          ]
      in List.exists (fn (a, b) => c >= a andalso c <= b) ranges end
  end
end
