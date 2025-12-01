{
open Parser
exception Eof
}

(* helper regex definitions *)
let digit = ['0'-'9']
let pitch = digit+
let note_name = ['a'-'g''A'-'G']['#''b']?
let note_lit = note_name digit? | pitch

rule token = parse
    [' ' '\t']          { token lexbuf }
    | ['\n' ]           { EOL }
    | "test-token"      { TEST_TOKEN }
    | eof               { raise Eof }
