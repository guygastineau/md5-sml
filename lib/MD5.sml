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

  open Util
  structure BS = BlockStream

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

    fun md5Finalize (totalLen, block) = let
      val block = Word8Vector.concat [ block, Word8Vector.fromList [0wx80] ]
      val blockLen = Word8Vector.length block
    in
      if blockLen <= 0x38 then
        [padWithLength64LE (totalLen, Word8Vector.length block, block)]
      else
        let
          val padN = 0x40 - blockLen
          val secondToLast = pad (padN, block)
        in
          [secondToLast, padWithLength64LE (totalLen, 0, Word8Vector.fromList [])]
        end
    end

    val mkStream = BS.mkBlockStream { blockSize = 0x40, finalizer = md5Finalize }

  in

  fun fileDigest fname = let
    val { a = a, b = b, c = c, d = d } = BS.fold process initialVars (mkStream fname)
  in
    Word8Vector.concat (map word32BytesLE [a, b, c, d])
  end

  end

  val digestMessage = Word8Vector.foldl (fn (x, msg) => msg ^ word8ToHex x) ""
  val fileDigestMessage = digestMessage o fileDigest

end
