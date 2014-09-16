{
  open Parser
}

let blank = ['\n' ' ']

rule aut = parse
          | ['0'-'9']+ as number {INT (int_of_string number)}
          | '{' {OA}
          | '}' {CA}
          | '(' {OP}
          | '.' {DOT}
          | ',' {COMMA}
          | ')' {CP}
          | '[' {OB}
          | '|' {PIPE}
          | ']' {CB}
          | "n" {NATURAL}
          | "z" {RELATIVE}
          | "N" {NATURAL}
          | "Z" {RELATIVE}
          | "->" {ARROW}
          | "*)" {aut lexbuf}
          | "(*" {comment 0 lexbuf}
          | '"' {read_string (Buffer.create 17) lexbuf}
          | blank* {aut lexbuf}
          | "final" {FINAL}
          | eof {EOF}
          | _ as c {failwith ("unknown carac "^(String.make 1 c)^" outside of a comment.")}
and comment level = 
  parse
  | "*)"	{ if level = 0 then aut lexbuf
		  else comment (level-1) lexbuf
		}
  | "(*"	{ comment (level+1) lexbuf
		}
  | _		{ comment level lexbuf }
  | eof		{EOF}

and read_string buf  = 
  parse
  | '"'	{ STRING (Buffer.contents buf) }
  | [^ '"' ]+ { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof	{ failwith "string";}

