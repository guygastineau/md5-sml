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

  (* xs MUST have size of at least 4! *)
  fun indexedWord32LE (xs, i) : Word32.word =
      let
        fun f i' = word8To32 (Word8Vector.sub (xs, i'))
        val w = Word32.<< (f (i + 3), 0wx18)
        val x = Word32.<< (f (i + 2), 0wx10)
        val y = Word32.<< (f (i + 1), 0wx8)
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
                    if i < 0x10
                    then (andOrNotAnd (b, c, d), Word32.fromInt i)
                    else if i < 0x20
                    then (andOrNotAnd (d, b, c), Word32.fromInt (5 * i + 1 mod 16))
                    else if i < 0x30
                    then (xorXor (b, c, d), Word32.fromInt (3 * i + 5 mod 16))
                    else (xorOrNot (b, c, d), Word32.fromInt (7 * i mod 16))
                val mg = indexedWord32LE (block, i * 4)
                val ki = Vector.sub (constants, i)
                val f = foldl Word32.+ 0wx0 [f', a, ki, mg]
              in
                { a = Word32.+ (a, d)
                , b = b + Word32.<< (f, perRoundShift i)
                , c = Word32.+ (c, b)
                , d = Word32.+ (d, c)
                }
              end
        in
          (Word64.+ (totalLen, 0wx40)
          , foldl round vars (List.tabulate (0x3f, (fn x => x)))
          )
        end

    fun trailingBit block =
        Word8Vector.concat
          [ block
          , Word8Vector.fromList [0wx80]
          ]

    fun pad (n, block) =
        let
          val padding = Word8Vector.tabulate (n - 1, (fn _ => 0w0))
        in
          Word8Vector.concat [block, padding]
        end

    fun padWithLength (totalLen, blockLen, block) =
        let
          val padN = 0x38 - Word64.toInt blockLen
          val block' = pad (padN, block)
        in
          Word8Vector.concat [pad (padN, block), word64ToWord8VecLE totalLen ]
        end

    fun finalize blockLen (block, (totalLen, vars)) = let
      val totalLen' = Word64.+ (totalLen, blockLen)
      val blockLen' = Word64.+ (blockLen, 0wx1)
    in
      if Word64.< (blockLen, 0wx38) then
              process (padWithLength (totalLen', blockLen', trailingBit block)
                      , (totalLen', vars))
          else
            let
              val padN = 0x40 - Word64.toInt blockLen'
              val (_, vars') = process (pad (padN, trailingBit block)
                                       , (totalLen', vars))
            in
              process (padWithLength (totalLen',  0w0, Word8Vector.fromList [])
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
      = { a = 0wx67452301
        , b = 0wxefcdab89
        , c = 0wx98badcfe
        , d = 0wx10325476
        }



end
