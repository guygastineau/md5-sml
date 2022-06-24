structure SHA256 =
struct
  type w = Word32.word
  type regs = { a : w, b : w, c : w, d : w, e : w, f : w, g : w, h : w }

  fun combineRegs
        { a = a1, b = b1, c = c1, d = d1, e = e1, f = f1, g = g1, h = h1 }
        { a = a2, b = b2, c = c2, d = d2, e = e2, f = f2, g = g2, h = h2 } =
      { a = a1 + a2, b = b1 + b2, c = c1 + c2, d = d1 + d2
      , e = e1 + e2, f = f1 + f2, g = g1 + g2, h = h1 + h2
      }

  val initialRegs =
      { a = 0wx6a09e667, b = 0wxbb67ae85, c = 0wx3c6ef372, d = 0wxa54ff53a
      , e = 0wx510e527f, f = 0wx9b05688c, g = 0wx1f83d9ab, h = 0wx5be0cd19
      }

  val k =
      Word32Vector.fromList
        [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5
        , 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
        , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3
        , 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
        , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc
        , 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
        , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7
        , 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
        , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13
        , 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
        , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3
        , 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
        , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5
        , 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
        , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208
        , 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
        ]

  (* We build a buffer of 64 Word32s from the buffer of 64 Word8s *)
  fun buildW xs = let
    val w = Word32Array (64, 0w0)
    fun f i =
        if i < 0x10 then Word8Array.update (w, i, indexedWord32BE (xs, i))
        else let
          val w0 = Word32Array.sub (w, i - 15)
          val s0 = Word32.xorb
                     ( Word32.xorb (Word32.>> (w0, 0w7), Word32.>> (w0, 0w18))
                     , Word32.>> (w0, 0w3) )
          val w1 = Word32Array.sub (w, i - 2)
          val s1 = Word32.xorb
                     ( Word32.xorb (Word32.>> (w1, 0w17), Word.>> (w1, 0w19))
                     , Word32.>> (w1, 0w10) )
          val wi = Word32Array.sub (w, i - 16) + s0 + Word32Array.sub (w, i - 7) + s1
        in
          Word32Array.update (w, i, wi)
        end
  in
    List.app f (List.tabulate (64, fn x => x))
  end

end
