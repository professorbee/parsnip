datatype ('a, 'b) result = Ok of 'a | Error of 'b

type input = 
  { text : string, pos : int }

fun make_input (s : string): input =
  { text = s, pos = 0 }

type error =
  { desc : string, pos: int }

type 'a parser =
  { run: input -> (input * 'a, error) result }

