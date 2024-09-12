datatype ('a, 'b) result = Ok of 'a | Error of 'b

type input = 
  { text : string, pos : int }

fun make_input (s : string): input =
  { text = s, pos = 0 }

fun input_sub (start : int, len : int, s: input) : input =
  {
    text = String.substring (#text s, start, len),
    pos = (#pos s) + start
  }

type error =
  { desc : string, pos: int }

type 'a parser =
  { run: input -> (input * 'a, error) result }

fun map (f : 'a -> 'b, p: 'a parser) : 'b parser =
  {
    run = (fn input =>
      (case (#run p) input of
        Ok (input', x) => Ok (input', f x)
      | Error error => Error error))
  }

fun bind (f : 'a -> 'b parser, p : 'a parser) : 'b parser =
  {
    run = (fn input =>
      (case (#run p) input of 
        Ok (input', x) => (#run (f x)) input'
      | Error error => Error error ))
  }


fun prefix (str : string) : string parser =
  {
    run = (fn input => 
      let 
        val n = String.size str
        val m = String.size (#text input)
        val pfix = input_sub (0, n, input)      
      in 
        if #text pfix = str then
          let val rest = input_sub (n, m - n, input) in
            Ok (rest, str)
          end
        else
          Error { pos = (#pos input),
            desc = "Expected " ^ str}
      end)
  }

fun main () = 
  let 
    val inp = make_input "hello world"
    val res = (#run (prefix "hello")) inp
  in
    (case res of 
      Ok ({pos, text}, _) => print text
    | Error { pos, desc } => print desc)
  end

val () = main ()
