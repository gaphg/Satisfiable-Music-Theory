{
    open Parser
    exception Eof
}

(* helper regex definitions *)
let alphanumeric = ['0'-'9''a'-'z''A'-'Z']+
let number = ['0'-'9']+

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
    | ['d''D']                      { D }
    | number as n ('p' | 'P')       { PITCHLIT (int_of_string n) }
    | number as n ('i' | 'I')       { INTERVALLIT (int_of_string n) }                                    
    | number as n ('t' | 'T')       { TIMESTEPLIT (int_of_string n) }                                    
    | number as n                   { INTLIT (int_of_string n)}
    | alphanumeric as s             { ID s }
    | eof                           { EOF }
