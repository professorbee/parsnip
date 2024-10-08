open Result

(* Just in case *)
fun |> (x : 'a, f : ('a -> 'b)) : 'b = f x
infix |>

type input = 
  { text : string, pos : int }

fun makeInput (s : string): input =
  { text = s, pos = 0 }

fun inputSub (start : int, len : int, s: input) : input =
  {
    text = String.substring (#text s, start, len),
    pos = (#pos s) + start
  }

type error =
  { desc : string, pos: int }

fun showError {desc, pos} =
  desc ^ " at pos: " ^ (Int.toString pos)

type 'a parser =
  { run: input -> (input * 'a, error) result }

fun map (f : 'a -> 'b, p: 'a parser) : 'b parser =
  {
    run = fn input =>
          case (#run p) input of
            Ok (input', x) => Ok (input', f x)
          | Error error => Error error
  }

fun bind (f : 'a -> 'b parser, p : 'a parser) : 'b parser =
  {
    run = fn input =>
          case (#run p) input of 
            Ok (input', x) => (#run (f x)) input'
          | Error error => Error error 
  }

fun prefix (str : string) : string parser =
  {
    run = fn input => 
          let 
            val n = String.size str
            val m = String.size (#text input)
            val pfix = inputSub (0, n, input)
            val rest = inputSub (n, m - n, input)
          in 
            if #text pfix = str then
              Ok (rest, str)
            else
              Error { 
                pos = (#pos input),
                desc = "Expected " ^ str
              }
          end
  }

fun take (n : int) : string parser =
  {
    run = fn input =>
          if n < (String.size (#text input)) then
            let 
              val str = inputSub (0, n, input)
              val m = String.size (#text input)
              val rest = inputSub (n, m - n, input)
            in
              Ok (rest, (#text str))
            end
          else
            Error {
              pos = (#pos input),
              desc = "Input not long enough!"
            }
  } 

fun takeWhile (f : (char -> bool)) : string parser =
  {
    run = fn input =>
          let 
            fun loop (idx : int) : (input * string) =
              let 
                val m = String.size (#text input)
                val substr = String.sub (#text input, idx)
                val applied = f substr
              in
                if applied then
                  loop (idx + 1)
                else
                  let 
                    val str = inputSub (0, idx, input)
                    val rest = inputSub (idx, m - idx, input) 
                  in
                    (rest, (#text str))
                  end
              end
            end in
              Ok (loop 0) 
              handle Subscript =>
                Error {
                  pos = (#pos input),
                  desc = "Reached end of input"
                }
            end
  }

infix <*
infix *>
infix <*>
infix <|>

fun op*> (p1 : 'a parser, p2: 'b parser) : 'b parser =
  {
    run = fn input =>
          case (#run p1) input of
            Ok (input', _) => ((#run p2) input')
          | Error e => Error e
  }
fun op<* (p1 : 'a parser, p2: 'b parser) : 'a parser =
  {
    run = fn input => 
          case (#run p1) input of 
            Ok (input' , x) => 
              Result.map (fn (input, _) => (input, x) ,(#run p2) input')
          | Error error => Error error
  }

fun op<*> (p1 : 'a parser, p2: 'b parser) : ('a * 'b) parser =
  { run = fn input =>
          case (#run p1) input of
            Ok (input', x) =>
              (case (#run p2) input' of 
                Ok (inp, y) => Ok (inp, (x, y))
              | Error e => Error e)
            | Error e => Error e
  }

fun op<|> (p1 : 'a parser, p2 : 'a parser) : 'a parser =
  { run = fn input =>
          case (#run p1) input of
            Ok (input', x) => Ok (input', x)
          | Error _ => (#run p2) input 
  }

fun main () = 
  let 
    val inp = makeInput "world"
    val res = (#run (prefix "world" <|> prefix "hello")) inp
  in
    case res of 
      Ok (_, x) => print x
    | Error x => print (showError x)
  end

val () = main ()
