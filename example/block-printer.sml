use "../lib/BlockStream.sig";
use "../lib/BlockStream.sml";
structure BS = BlockStream

fun word64BytesLE x = let
  fun shiftLeft (x, n) = Word64.<< (x, Word.fromInt n)
  fun shiftRight (x, n) = Word64.>> (x, Word.fromInt n)
  fun f n = let
    val ls = (7 - n) * 8
    val rs = ls + n * 8
  in
    (Word8.fromLargeWord o Word64.toLargeWord)
      (shiftRight (shiftLeft (x, ls), rs))
  end
in
  Word8Vector.tabulate (8, f)
end

fun pad (n, block) =
    Word8Vector.concat [block, Word8Vector.tabulate (n, (fn _ => 0w0))]

fun padWithLength (totalLen, blockLen, block) =
    let
      val padN = 0x38 - blockLen
    in
      Word8Vector.concat [pad (padN, block), word64BytesLE totalLen]
    end


fun hexDigit (0w0 : Word8.word) = #"0"
  | hexDigit 0w1 = #"1"
  | hexDigit 0w2 = #"2"
  | hexDigit 0w3 = #"3"
  | hexDigit 0w4 = #"4"
  | hexDigit 0w5 = #"5"
  | hexDigit 0w6 = #"6"
  | hexDigit 0w7 = #"7"
  | hexDigit 0w8 = #"8"
  | hexDigit 0w9 = #"9"
  | hexDigit 0wxa = #"a"
  | hexDigit 0wxb = #"b"
  | hexDigit 0wxc = #"c"
  | hexDigit 0wxd = #"d"
  | hexDigit 0wxe = #"e"
  | hexDigit 0wxf = #"f"

fun word8ToHex w =
    String.implode
      [ hexDigit (Word8.div (w, 0wx10))
      , hexDigit (Word8.mod (w, 0wx10))
      ]

fun printW8Vec xs =
    Word8Vector.appi
      (fn (i, x) => ((if i mod 16 = 0 then print "\n" else print "");
                     print (word8ToHex x ^ " ")))
      xs


fun md5Finalize (totalLen, block) = let
  val block = Word8Vector.concat [ block, Word8Vector.fromList [0wx80] ]
  val blockLen = Word8Vector.length block
in
  if blockLen <= 0x38 then
    [padWithLength (totalLen, Word8Vector.length block, block)]
  else
    let
      val padN = 0x40 - blockLen
      val secondToLast = pad (padN, block)
    in
      [secondToLast, padWithLength (totalLen, 0, Word8Vector.fromList [])]
    end
end

val _ = let
  val mkStream = BS.mkBlockStream { blockSize = 0x40, finalizer = md5Finalize }
  val strm = mkStream "block-printer.sml"
in
  BS.fold (fn (xs,()) => (printW8Vec xs; print "\n")) () strm
end
