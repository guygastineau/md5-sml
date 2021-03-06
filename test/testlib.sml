signature EXPECT = sig
  datatype expectation = Pass | Fail of string * string
  val truthy : bool -> expectation
  val falsy : bool -> expectation
  val mkEqualTo
    : { eq : 'a * 'a -> bool, show : 'a -> string } -> 'a -> 'a  -> expectation
  val mkAssert : { show : 'a -> string } -> ('a -> bool) -> 'a -> expectation
  val nearTo : real -> real -> real -> expectation
  val anyError : (unit -> 'a) -> expectation
  val error : exn -> (unit -> 'a) -> expectation
end

structure Expect : EXPECT =
struct
  datatype expectation = Pass | Fail of string * string

  local
    fun failEq expected actual =
      Fail ("Expected: " ^ expected, "Got: " ^ actual)

    fun failExn expected actual =
      Fail ("Expected: " ^ expected, "Raised: " ^ actual)

    fun exnName (e: exn): string = General.exnName e
  in
    fun truthy x =
      if x
      then Pass
      else failEq "true" "false"

    fun falsy x =
      if x
      then failEq "false" "true"
      else Pass

    fun mkEqualTo { eq = eq, show = show } expected actual =
      if eq (actual, expected)
      then Pass
      else failEq (show expected) (show actual)

    fun mkAssert { show = show } pred x =
      if pred x
      then Pass
      else Fail ("Assertion! ", "x = " ^show x)

    fun nearTo delta expected actual =
      if Real.abs (actual - expected) <= delta * Real.abs actual orelse
         Real.abs (actual - expected) <= delta * Real.abs expected
      then Pass
      else failEq (Real.toString expected ^ " +/- " ^ Real.toString delta)
                  (Real.toString actual)

    fun anyError f =
      (
        f ();
        failExn "an exception" "Nothing"
      ) handle _ => Pass

    fun error e f =
      (
        f ();
        failExn (exnName e) "Nothing"
      ) handle e' => if exnMessage e' = exnMessage e
                     then Pass
                     else failExn (exnMessage e) (exnMessage e')
  end
end

structure TermColor =
struct
  datatype color = Red | Green | Yellow | Normal

  fun f Red    = "\027[31m"
    | f Green  = "\027[32m"
    | f Yellow = "\027[33m"
    | f Normal = "\027[0m"

  fun colorize color s = (f color) ^ s ^ (f Normal)

  val redit = colorize Red

  val greenit = colorize Green

  val yellowit = colorize Yellow
end

structure Test =
struct
  datatype testnode = TestGroup of string * testnode list
                    | Test of string * (unit -> Expect.expectation)

  local
    datatype evaluation = Success of string
                        | Failure of string * string * string
                        | Error of string * string

    fun indent n s = (implode (List.tabulate (n, fn _ => #" "))) ^ s

    fun fmt indentlvl ev =
      let
        val check = TermColor.greenit "\226\156\148 " (* ??? *)
        val cross = TermColor.redit "\226\156\150 "   (* ??? *)
        val indentlvl = indentlvl * 2
      in
      case ev of
        Success descr => indent indentlvl (check ^ descr)
      | Failure (descr, exp, got) =>
          String.concatWith "\n" [indent indentlvl (cross ^ descr),
                                  indent (indentlvl + 2) exp,
                                  indent (indentlvl + 2) got]
      | Error (descr, reason) =>
          String.concatWith "\n" [indent indentlvl (cross ^ descr),
                                  indent (indentlvl + 2) (TermColor.redit reason)]
      end

    fun eval (TestGroup _) = raise Fail "Only a 'Test' can be evaluated"
      | eval (Test (descr, thunk)) =
          (
            case thunk () of
              Expect.Pass   => ((1, 0, 0), Success descr)
            | Expect.Fail (s, s') => ((0, 1, 0), Failure (descr, s, s'))
          )
          handle e => ((0, 0, 1), Error (descr, "Unexpected error: " ^ exnMessage e))

    fun flatten depth testnode =
      let
        fun sum (x, y, z) (a, b, c) = (x + a, y + b, z + c)

        fun aux (t, (counter, acc)) =
          let
            val (counter', texts) = flatten (depth + 1) t
          in
            (sum counter' counter, texts :: acc)
          end
      in
        case testnode of
          TestGroup (descr, ts) =>
            let
              val (counter, texts) = foldr aux ((0, 0, 0), []) ts
            in
              (counter, (indent (depth * 2) descr) :: List.concat texts)
            end
        | Test _ =>
            let
              val (counter, evaluation) = eval testnode
            in
              (counter, [fmt depth evaluation])
            end
      end

    fun println s = print (s ^ "\n")
  in
    fun run suite =
      let
        val ((succeeded, failed, errored), texts) = flatten 0 suite

        val summary = String.concatWith ", " [
          TermColor.greenit ((Int.toString succeeded) ^ " passed"),
          TermColor.redit ((Int.toString failed) ^ " failed"),
          TermColor.redit ((Int.toString errored) ^ " errored"),
          (Int.toString (succeeded + failed + errored)) ^ " total"
        ]

        val status = if failed = 0 andalso errored = 0
                     then OS.Process.success
                     else OS.Process.failure

      in
        List.app println texts;
        println "";
        println ("Tests: " ^ summary);
        OS.Process.exit status
      end
  end
end

structure TestUtil =
struct
  fun describe description tests = Test.TestGroup (description, tests)
  fun test description thunk = Test.Test (description, thunk)
end
open TestUtil
