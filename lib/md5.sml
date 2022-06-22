structure MD5 =
struct
  type vars = { a : Word32.word, b : Word32.word, c : Word32.word, d : Word32.word }

  fun perRoundShift n =
      Word.fromInt
        (if n < 16
         then List.nth ([7, 12, 17, 22], n mod 4)
         else if n < 32
         then List.nth ([5, 9, 14, 20], n mod 4)
         else if n < 48
         then List.nth ([4, 11, 16, 23], n mod 4)
         else List.nth ([6, 10, 15, 21], n mod 4))

  val constants : Word32.word vector
      = Vector.map Word32.fromLargeInt
          (Vector.fromList
            [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
            , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
            , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
            , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
            , 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
            , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
            , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
            , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
            , 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
            , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
            , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
            , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
            , 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
            , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
            , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
            , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
            ])

  val word8To32 = Word32.fromLargeWord o Word8.toLargeWord

  (* xs MUST have size of at least 4! *)
  fun indexedWord32LE (xs, i) : Word32.word =
      let
        fun f i' = word8To32 (Word8Vector.sub (xs, i'))
        fun shiftLeft (x, n) = Word32.<< (x, Word.fromInt n)
        val w = shiftLeft (f (i + 3), 24)
        val x = shiftLeft (f (i + 2), 16)
        val y = shiftLeft (f (i + 1), 8)
        val z = f i
      in
        Word32.orb (w, Word32.orb (x, Word32.orb (y, z)))
      end

  fun word64ToWord8VecLE x = let
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
    Word8Vector.tabulate (7, f)
  end

  (* block must be >= 512 bits! *)
  local
    fun andOrNotAnd (b, c, d)
        = Word32.orb (Word32.andb (b, c), Word32.andb (Word32.notb b, d))

    fun xorXor (b, c, d) = Word32.xorb (b, Word32.xorb (c, d))

    fun xorOrNot (b, c, d) = Word32.xorb (c, Word32.orb (b, Word32.notb d))

    (* block must be 64 bytes *)
    fun process (block, (totalLen, vars)) =
        let
          fun round (i, { a = a, b = b, c = c, d = d }) =
              let
                val (f', g) =
                    if i < 16
                    then (andOrNotAnd (b, c, d), Word32.fromInt i)
                    else if i < 32
                    then (andOrNotAnd (d, b, c), Word32.fromInt (5 * i + 1 mod 16))
                    else if i < 48
                    then (xorXor (b, c, d), Word32.fromInt (3 * i + 5 mod 16))
                    else (xorOrNot (b, c, d), Word32.fromInt (7 * i mod 16))
                val mg = indexedWord32LE (block, i * 4)
                val ki = Vector.sub (constants, i)
                val f = foldl Word32.+ (Word32.fromInt 0) [f', a, ki, mg]
              in
                { a = Word32.+ (a, d)
                , b = b + Word32.<< (f, perRoundShift i)
                , c = Word32.+ (c, b)
                , d = Word32.+ (d, c)
                }
              end
        in
          (Word64.+ (totalLen, Word64.fromInt 64)
          , foldl round vars (List.tabulate (63, (fn x => x)))
          )
        end

    fun trailingBit block =
        Word8Vector.concat
          [ block
          , Word8Vector.fromList [Word8.fromInt 0x80]
          ]

    fun pad (n, block) =
        let
          val padding = Word8Vector.tabulate (n - 1, (fn _ => Word8.fromInt 0))
        in
          Word8Vector.concat [block, padding]
        end

    fun padWithLength (totalLen, blockLen, block) =
        let
          val padN = 56 - Word64.toInt blockLen
          val block' = pad (padN, block)
        in
          Word8Vector.concat [pad (padN, block), word64ToWord8VecLE totalLen ]
        end

    fun finalize blockLen (block, (totalLen, vars)) = let
      val totalLen' = Word64.+ (totalLen, blockLen)
      val blockLen' = Word64.+ (blockLen, Word64.fromInt 1)
    in
      if Word64.< (blockLen, Word64.fromInt 56) then
              process (padWithLength (totalLen', blockLen', trailingBit block)
                      , (totalLen', vars))
          else
            let
              val padN = 64 - Word64.toInt blockLen'
              val (_, vars') = process (pad (padN, trailingBit block)
                                       , (totalLen', vars))
            in
              process (padWithLength (totalLen', Word64.fromInt 0, Word8Vector.fromList [])
                      , (totalLen', vars))
            end
    end
  in
    fun runBlock (block, (totalLen, vars)) =
        let
          val blockLen = Word64.fromInt (Word8Vector.length block)
        in
          if blockLen < Word64.fromInt 64
          then finalize blockLen (block, (totalLen, vars))
          else process (block, (totalLen, vars))
        end
  end

  val initialVars
      = { a = Word32.fromLargeInt 0x67452301
        , b = Word32.fromLargeInt 0xefcdab89
        , c = Word32.fromLargeInt 0x98badcfe
        , d = Word32.fromLargeInt 0x10325476
        }

end
