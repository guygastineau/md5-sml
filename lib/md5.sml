structure MD5 =
struct
  type vars = { a : Word32.word, b : Word32.word, c : Word32.word, d : Word32.word }

  val initialVars : vars
      = { a = 0wx67452301
        , b = 0wxefcdab89
        , c = 0wx98badcfe
        , d = 0wx10325476
        }

  fun perRoundShift n =
      if n < 0x10
      then List.nth ([0w7, 0w12, 0w17, 0w22], n mod 4)
      else if n < 0x20
      then List.nth ([0w5, 0w9, 0w14, 0w20], n mod 4)
      else if n < 0x30
      then List.nth ([0w4, 0w11, 0w16, 0w23], n mod 4)
      else List.nth ([0w6, 0w10, 0w15, 0w21], n mod 4)

  val constants : Word32.word vector =
      Vector.fromList
        [ 0wxd76aa478, 0wxe8c7b756, 0wx242070db, 0wxc1bdceee
        , 0wxf57c0faf, 0wx4787c62a, 0wxa8304613, 0wxfd469501
        , 0wx698098d8, 0wx8b44f7af, 0wxffff5bb1, 0wx895cd7be
        , 0wx6b901122, 0wxfd987193, 0wxa679438e, 0wx49b40821
        , 0wxf61e2562, 0wxc040b340, 0wx265e5a51, 0wxe9b6c7aa
        , 0wxd62f105d, 0wx02441453, 0wxd8a1e681, 0wxe7d3fbc8
        , 0wx21e1cde6, 0wxc33707d6, 0wxf4d50d87, 0wx455a14ed
        , 0wxa9e3e905, 0wxfcefa3f8, 0wx676f02d9, 0wx8d2a4c8a
        , 0wxfffa3942, 0wx8771f681, 0wx6d9d6122, 0wxfde5380c
        , 0wxa4beea44, 0wx4bdecfa9, 0wxf6bb4b60, 0wxbebfbc70
        , 0wx289b7ec6, 0wxeaa127fa, 0wxd4ef3085, 0wx04881d05
        , 0wxd9d4d039, 0wxe6db99e5, 0wx1fa27cf8, 0wxc4ac5665
        , 0wxf4292244, 0wx432aff97, 0wxab9423a7, 0wxfc93a039
        , 0wx655b59c3, 0wx8f0ccc92, 0wxffeff47d, 0wx85845dd1
        , 0wx6fa87e4f, 0wxfe2ce6e0, 0wxa3014314, 0wx4e0811a1
        , 0wxf7537e82, 0wxbd3af235, 0wx2ad7d2bb, 0wxeb86d391
        ]

  val word8To32 = Word32.fromLargeWord o Word8.toLargeWord

  val indexedWord32LE = Word32.fromLargeWord o PackWord32Little.subVec

  fun word32BytesLE x = let
    fun shiftLeft (x, n) = Word32.<< (x, Word.fromInt n)
    fun shiftRight (x, n) = Word32.>> (x, Word.fromInt n)
    fun f n = let
      val ls = (3 - n) * 8
      val rs = ls + n * 8
    in
      (Word8.fromLargeWord o Word32.toLargeWord)
        (shiftRight (shiftLeft (x, ls), rs))
    end
  in
    Word8Vector.tabulate (4, f)
  end

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

  (* block must be = 512 bits (0x40 bytes)! *)
  local
    fun combineVars ({ a = a , b = b , c = c , d = d  } : vars)
                    ({ a = a', b = b', c = c', d = d' } : vars) : vars =
        { a = a + a', b = b + b', c = c + c', d = d' }

    fun fF (b, c, d) =
        Word32.orb (Word32.andb (b, c), Word32.andb (Word32.notb b, d))

    fun fG (b, c, d) =
        Word32.orb (Word32.andb (b, d), Word32.andb (c, Word32.notb d))

    fun fH (b, c, d) = Word32.xorb (Word32.xorb (b, c), d)

    fun fI (b, c, d) = Word32.xorb (c, Word32.orb (b, Word32.notb d))

    fun idxF i = i
    fun idxG i = (5 * i + 1) mod 16
    fun idxH i = (3 * i + 5) mod 16
    fun idxI i = (7 * i) mod 16

    (* block must be 64 bytes *)
    fun process (block, vars) =
        let
          fun rounds (i, { a = a, b = b, c = c, d = d }) : vars =
              let
                val (f', k) =
                    if      i < 0x10 then (fF, idxF i)
                    else if i < 0x20 then (fG, idxG i)
                    else if i < 0x30 then (fH, idxH i)
                    else                  (fI, idxI i)
                val xk = indexedWord32LE (block, k)
                val ti = Vector.sub (constants, i)
                val a' =  b + Word32.<< (a + f' (b, c, d) + xk + ti, perRoundShift i)
              in
                { a = d, b = a', c = b, d = c }
              end
        in
          combineVars vars (foldl rounds vars (List.tabulate (0x40, (fn x => x))))
        end

    fun pad (n, block) =
        Word8Vector.concat [block, Word8Vector.tabulate (n, (fn _ => 0w0))]

    fun padWithLength (totalLen, blockLen, block) =
        let
          val padN = 0x38 - Word64.toInt blockLen
          val block' = pad (padN, block)
        in
          Word8Vector.concat [pad (padN, block), word64BytesLE (totalLen * 0w8)]
        end

    fun finalize blockLen (block, (totalLen, vars)) = let
      val blockLen' = blockLen + 0w1
      val block = Word8Vector.concat [ block, Word8Vector.fromList [0wx80] ]
    in
      if blockLen' <= 0wx38 then
        let
          val last = padWithLength (totalLen, blockLen', block)
        in
          (print "last: "; printW8Vec last; print "\n";
           (totalLen, process (last, vars)))
        end
      else
        let
          val padN = 0x40 - Word64.toInt blockLen'
          val secondToLast = pad (padN, block)
          val last = padWithLength (totalLen, 0w0, Word8Vector.fromList [])
        in
          (print "secondToLast: "; printW8Vec secondToLast; print "\n";
           print "last: "; printW8Vec last;
           (totalLen, process (last, process (secondToLast, vars))))
        end
    end

    fun runBlock (block, (totalLen, vars)) =
        let
          val blockLen = Word64.fromInt (Word8Vector.length block)
          val totalLen' = totalLen + blockLen
        in
          if blockLen < 0wx40
          then finalize blockLen (block, (totalLen', vars))
          else (totalLen', process (block, vars))
        end

    fun streamFoldBlocks blockSize f acc (strm : BinIO.instream) = let
      val block = BinIO.inputN (strm, blockSize)
      val blockLen = Word8Vector.length block
      val acc' = f (block, acc)
    in
      if blockLen < blockSize then acc'
      else streamFoldBlocks blockSize f acc' strm
    end
  in

  fun streamDigest strm = let
    val (_, { a = a, b = b, c = c, d = d }) =
        streamFoldBlocks 0x40 runBlock (0w0, initialVars) strm
  in
    Word8Vector.concat (map word32BytesLE [a, b, c, d])
  end

  fun fileDigest fname = let
    val instream = BinIO.openIn fname
    val digest = streamDigest instream
  in
    BinIO.closeIn instream; digest
  end

  end

  val digestMessage = Word8Vector.foldl (fn (x, msg) => msg ^ word8ToHex x) ""
  val streamDigestMessage = digestMessage o streamDigest
  val fileDigestMessage = digestMessage o fileDigest

end
