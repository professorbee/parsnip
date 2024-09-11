type key = string
type value = string
type pair = key * value
type section =
  { name : string, pairs: pair list }

fun in_brackets (str : string) : string =
  "[" ^ str ^ "]"

fun show_pair (k : key, v : value) : string =
  "(" ^ k ^ ", " ^ v ^ "\n" ^ ")" 

val show_pairs = in_brackets o (String.concatWith ", ") o (List.map show_pair)  

fun show_section{name : string, pairs: pair list } : string =
  "{ name = " ^ name ^ "; pairs = " ^ (show_pairs pairs) ^ " }"  

val show_sections = in_brackets o (String.concatWith ", ") o (List.map show_section)  

fun read_whole_file (file_path : string) : string = 
  let
    val stream = TextIO.openIn file_path
    val res = TextIO.inputAll stream
  in
    TextIO.closeIn stream;
    res
  end

fun main () = 
  let 
    val file_str = read_whole_file "./test.ini" 
  in
    print file_str
  end

val () = main ()
