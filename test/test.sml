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

fun equalToWord32 expected actual
    = mkEqualTo { eq = op =, show = Word32.toString }
                (Word32.fromInt expected) (Word32.fromInt actual)

val testsuite =
    describe "Test Utilities"
      [ describe "perRoundShift tests"
          [ (* First group of 16 *)
            test "0 0" (fn _ => equalToWord32 7 (perRoundShift 0))
          , test "0 1 with mod"
              @@ (fn _ => equalToWord32 12 (perRoundShift 5))
          , test "0 2 with mod"
              @@ (fn _ => equalToWord32 17 (perRoundShift 14))
          , test "0 3 with mod"
              @@ (fn _ => equalToWord32 22 (perRoundShift 11))
          (* second group of 16 *)
          , test "1 0" (fn _ => equalToWord32 5 (perRoundShift 16))
          , test "1 1 with mod"
              @@ (fn _ => equalToWord32 9 (perRoundShift 21))
          , test "1 2 with mod"
              @@ (fn _ => equalToWord32 14 (perRoundShift 18))
          , test "1 3 with mod"
              @@ (fn _ => equalToWord32 20 (perRoundShift 27))
          (* third group of 16 *)
          , test "2 0" (fn _ => equalToWord32 4 (perRoundShift 32))
          , test "2 1 with mod"
              @@ (fn _ => equalToWord32 11 (perRoundShift 41))
          , test "2 2 with mod"
              @@ (fn _ => equalToWord32 16 (perRoundShift 38))
          , test "2 3 with mod"
              @@ (fn _ => equalToWord32 23 (perRoundShift 47))
          (* fourth group of 16 *)
          , test "3 0" (fn _ => equalToWord32 6 (perRoundShift 48))
          , test "3 1 with mod"
              @@ (fn _ => equalToWord32 10 (perRoundShift 49))
          , test "3 2 with mod"
              @@ (fn _ => equalToWord32 15 (perRoundShift 58))
          , test "3 3 with mod"
              @@ (fn _ => equalToWord32 21 (perRoundShift 55))
          ]
      ]

(* RUN *)

val _ = Test.run testsuite
