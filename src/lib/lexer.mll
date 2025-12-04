{
    open Parser
    exception Eof
}

(* helper regex definitions *)
let whitespace = [' ''\t''\n']
let alphanumeric = ['0'-'9''a'-'z''A'-'Z']+
let number = ['0'-'9']+
let comment = ";" [^'\n']* ('\n' | eof)
let filename = [^' ''\t''\n']+ '.' [^' ''\t''\n']+

rule tokenize = parse
    whitespace                      { tokenize lexbuf }
    | comment                       { tokenize lexbuf }
    | "DECLARE TIME UNIT TICKS:"    { TIME_UNIT_DECL }
    | "DECLARE LENGTH:"           { MEASURE_DECL }
    | "DECLARE VOICES:"             { VOICE_DECL }
    | "DECLARE KEY:"                { KEY_DECL }
    | "DEFINE:"                     { DEFINE }
    | "REQUIRE:"                    { REQUIRE }
    | "DISALLOW:"                   { DISALLOW }
    | "PREFER:"                     { PREFER }
    | "AVOID:"                      { AVOID }
    | "INCLUDE:"                    { INCLUDE }
    | "("                           { LPAREN }
    | ")"                           { RPAREN }
    | "pitches-of"                  { PITCHES }
    | "contour-of"                  { CONTOUR }
    | "diads-of"                    { DIADS }
    | "interval-bt"                 { INTERVAL }
    | "+"                           { PLUS }
    | "-"                           { MINUS }
    | "not"                         { NOT }
    | "and"                         { AND }
    | "or"                          { OR }
    | "of"                          { OF }
    | "=>"                          { IMPLIES }
    | "<=>"                         { IFF }
    | "="                           { EQUALS }
    | "!="                          { NOT_EQUALS }
    | "<"                           { LESS }
    | "<="                          { LEQ }
    | ">"                           { GREATER }
    | ">="                          { GEQ }
    | "at"                          { AT }
    | "contains"                    { CONTAINS }
    | "in"                          { IN }
    | "is"                          { IS }
    | "is-not"                      { IS_NOT }
    | "flatten"                     { FLATTEN }
    | "Voice"                       { VOICE_TYPE }
    | "Pitch"                       { PITCH_TYPE }
    | "Interval"                    { INTERVAL_TYPE }
    | "TimeStep"                    { TIMESTEP_TYPE }
    | "Integer"                     { INTEGER_TYPE }
    | "Boolean"                     { BOOLEAN_TYPE }
    | "TimeSeries"                  { TIMESERIES_TYPE }
    | "List"                        { LIST_TYPE }
    (* | "major"                       { MAJOR } *)
    (* | "minor"                       { MINOR } *)
    | "true"                        { TRUE_TOKEN }
    | "false"                       { FALSE_TOKEN }
    | ","                           { COMMA }
    | "["                           { LBRACK }
    | "]"                           { RBRACK }
    | "up"                          { UP }
    | "down"                        { DOWN }
    | "forall"                      { FORALL }
    | "exists"                      { EXISTS }
    | "where"                       { WHERE }
    | number as n ('p' | 'P')       { PITCHLIT (int_of_string n) }
    | number as n ('i' | 'I')       { INTERVALLIT (int_of_string n) }                                    
    | number as n ('t' | 'T')       { TIMESTEPLIT (int_of_string n) }                                    
    | number as n                   { INTLIT (int_of_string n)}
    | filename as f                 { FILENAME f }
    | alphanumeric as s             { ID s }
    | eof                           { EOF }
