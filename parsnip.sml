datatype ('a, 'b) result = Ok of 'a | Error of 'b

type input = 
  { text : string, pos : int }

fun make_input (s : string): input =
  { text = s, pos = 0 }

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
