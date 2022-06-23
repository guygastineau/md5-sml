(* Forward application *)
infixr |>
fun x |> f = f x

(* Regular application *)
infixr @@
fun f @@ x = f x

(* TESTS *)

fun const x _ = x
fun identity x = x

open Expect
open TestUtil
open MD5

local
  val w8VecShow = Word8Vector.foldl (fn (x, s) => s ^ Word8.toString x ^ " ") ""
in
fun equalToWord8Vec expected actual
    = mkEqualTo { eq = op =, show = w8VecShow } (Word8Vector.fromList expected) actual
end

fun equalToString expected actual
    = mkEqualTo { eq = op =, show = identity } expected actual

fun equalToWord32 expected actual
    = mkEqualTo { eq = op =, show = Word32.toString } expected actual

fun equalToWord expected actual
    = mkEqualTo { eq = op =, show = Word.toString }
                (Word.fromLargeInt expected) actual

val testsuite =
    describe "Test Utilities"
      [ describe "perRoundShift tests"
          [ (* First group of 16 *)
            test "0 0" (fn _ => equalToWord 7 (perRoundShift 0))
          , test "0 1 with mod"
              @@ (fn _ => equalToWord 12 (perRoundShift 5))
          , test "0 2 with mod"
              @@ (fn _ => equalToWord 17 (perRoundShift 14))
          , test "0 3 with mod"
              @@ (fn _ => equalToWord 22 (perRoundShift 11))
          (* second group of 16 *)
          , test "1 0" (fn _ => equalToWord 5 (perRoundShift 16))
          , test "1 1 with mod"
              @@ (fn _ => equalToWord 9 (perRoundShift 21))
          , test "1 2 with mod"
              @@ (fn _ => equalToWord 14 (perRoundShift 18))
          , test "1 3 with mod"
              @@ (fn _ => equalToWord 20 (perRoundShift 27))
          (* third group of 16 *)
          , test "2 0" (fn _ => equalToWord 4 (perRoundShift 32))
          , test "2 1 with mod"
              @@ (fn _ => equalToWord 11 (perRoundShift 41))
          , test "2 2 with mod"
              @@ (fn _ => equalToWord 16 (perRoundShift 38))
          , test "2 3 with mod"
              @@ (fn _ => equalToWord 23 (perRoundShift 47))
          (* fourth group of 16 *)
          , test "3 0" (fn _ => equalToWord 6 (perRoundShift 48))
          , test "3 1 with mod"
              @@ (fn _ => equalToWord 10 (perRoundShift 49))
          , test "3 2 with mod"
              @@ (fn _ => equalToWord 15 (perRoundShift 58))
          , test "3 3 with mod"
              @@ (fn _ => equalToWord 21 (perRoundShift 55))
          ]
      , describe "bytes and integers utilities test"
          [ test "word32LE RoundTrip"
              @@ (fn _ => equalToWord32 0wxaabbccdd
                            @@ indexedWord32LE (word32BytesLE 0wxaabbccdd, 0))
          , test "word64LE RoundTrip"
                 @@ (fn _ => let
                       val expected =
                           [ 0wx11, 0wx00, 0wxff, 0wxee, 0wxdd, 0wxcc, 0wxbb, 0wxaa ]
                     in
                       equalToWord8Vec expected @@ word64BytesLE 0wxaabbccddeeff0011
                     end)
          ]
      , describe "digest message tests"
          [ test "pangram digest"
              @@ (fn _ => equalToString "9e107d9d372bb6826bd81d3542a419d6"
                            @@ MD5.fileDigestMessage "test/data/pangram.txt")
          , test "empty file test"
              @@ (fn _ => equalToString "d41d8cd98f00b204e9800998ecf8427e"
                            @@ MD5.fileDigestMessage "test/data/empty.txt")
          ]
      ]

(* RUN *)

val _ =
    Test.run testsuite;
