
structure Configure = struct
  val mecab_exec = ref "/usr/local/bin/mecab"
end

structure MeCab = MeCab(structure Configure = Configure)
