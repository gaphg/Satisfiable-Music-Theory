{
    open Parser
    exception Eof
}

(* helper regex definitions *)
let alphanumeric = ['0'-'9''a'-'z''A'-'Z']+
let digit = ['0'-'9']
let number = digit+
let note_name = ['a'-'g''A'-'G']['#''b']?
let note_lit = note_name digit? | number

rule tokenize = parse
    [' ' '\t' '\n']                 { tokenize lexbuf }
    | "DECLARE VOICES:"             { VOICE_DECL }
    | "DECLARE TIME UNIT TICKS:"    { TIME_UNIT_DECL }
    | "DECLARE MEASURES:"           { MEASURE_DECL }
    | "DECLARE KEY:"                { KEY_DECL } 
    | "REQUIRE:"                    { REQUIRE }
    | "DISALLOW:"                   { DISALLOW }
    | "PREFER:"                     { PREFER }
    | "AVOID:"                      { AVOID }
    | "("                           { LPAREN }
    | ")"                           { RPAREN }
    | "pitches of"                  { PITCHES }
    | "contour of"                  { CONTOUR }
    | "diads of"                    { DIADS }
    | "interval between "           { INTERVAL }
    | "+"                           { PLUS }
    | "-"                           { MINUS }
    | "not"                         { NOT }
    | "and"                         { AND }
    | "or"                          { OR }
    | "=>"                          { IMPLIES }
    | "if"                          { IFF }
    | "="                           { EQUALS }
    | "!="                          { NOT_EQUALS }
    | "<"                           { LESS }
    | "<="                          { LEQ }
    | ">"                           { GREATER }
    | ">="                          { GEQ }
    | "at"                          { AT }
    | "contains"                    { CONTAINS }
    | "is"                          { IS }
    | "is not"                      { IS_NOT }
    | "flatten"                     { FLATTEN }
    | "Voice"                       { VOICE_TYPE }
    | "Pitch"                       { PITCH_TYPE }
    | "Interval"                    { INTERVAL_TYPE }
    | "TimeStep"                    { TIMESTEP_TYPE }
    | "Boolean"                     { BOOLEAN_TYPE }
    | "TimeSeries"                  { TIMESERIES_TYPE }
    | "List"                        { LIST_TYPE }
    | "major"                       { MAJOR }
    | "minor"                       { MINOR }
    | "true"                        { TRUE }
    | "false"                       { FALSE }
    | alphanumeric as s             { ID s }
    | number as n                   { INTLIT (int_of_string n)}
    | note_lit as n                 { NOTELIT n }
    | eof                           { EOF }
