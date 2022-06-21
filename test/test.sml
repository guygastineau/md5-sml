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

fun assertString pred (x : string) = mkAssert { show = identity } pred x

val testsuite =
    describe "Dummy Tests"
             [ test "dummy test"
                 (fn _ => assertString (const true) (hello ())) ]

(* RUN *)

val _ = Test.run testsuite
