{
    open Parser
    exception Eof
}

(* helper regex definitions *)
let alphanumeric = ['0'-'9''a'-'z''A'-'Z']+
let digit = ['0'-'9']
let number = digit+
(* let note_name = ['a'-'g''A'-'G']['#''b']?
let note_lit = note_name digit? | number *)

rule tokenize = parse
    [' ' '\t' '\n']                 { tokenize lexbuf }
    | "DECLARE TIME UNIT TICKS:"    { TIME_UNIT_DECL }
    | "DECLARE MEASURES:"           { MEASURE_DECL }
    | "DECLARE VOICES:"             { VOICE_DECL }
    | "DECLARE KEY:"                { KEY_DECL }
    | "DEFINE:"                     { DEFINE }
    | "REQUIRE:"                    { REQUIRE }
    | "DISALLOW:"                   { DISALLOW }
    | "PREFER:"                     { PREFER }
    | "AVOID:"                      { AVOID }
    | "("                           { LPAREN }
    | ")"                           { RPAREN }
    | "pitches"                     { PITCHES }
    | "contour"                     { CONTOUR }
    | "diads"                       { DIADS }
    | "interval"                    { INTERVAL }
    | "between"                     { BETWEEN }
    | "+"                           { PLUS }
    | "-"                           { MINUS }
    | "not"                         { NOT }
    | "and"                         { AND }
    | "or"                          { OR }
    | "of"                          { OF }
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
    | "is not"                      { IS_NOT }
    | "is"                          { IS }
    | "flatten"                     { FLATTEN }
    | "Voice"                       { VOICE_TYPE }
    | "Pitch"                       { PITCH_TYPE }
    | "Interval"                    { INTERVAL_TYPE }
    | "TimeStep"                    { TIMESTEP_TYPE }
    | "Integer"                     { INTEGER_TYPE }
    | "Boolean"                     { BOOLEAN_TYPE }
    | "TimeSeries"                  { TIMESERIES_TYPE }
    | "List"                        { LIST_TYPE }
    | "major"                       { MAJOR }
    | "minor"                       { MINOR }
    | "true"                        { TRUE_TOKEN }
    | "false"                       { FALSE_TOKEN }
    | ","                           { COMMA }
    | "["                           { LBRACK }
    | "]"                           { RBRACK }
    | ['p''P']                      { P }
    | ['i''I']                      { I }
    | ['t''T']                      { T }
    | ['d''D']                      { D }
    | number as n                   { INTLIT (int_of_string n)}
    | alphanumeric as s             { ID s }
    | eof                           { EOF }
