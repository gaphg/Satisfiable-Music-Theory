{
    open Parser
    exception Eof
}

(* helper regex definitions *)
let whitespace = [' ''\t']
let identifier = ['0'-'9''a'-'z''A'-'Z''#''_']+
let number = ['0'-'9']+
let comment = ";" [^'\n']* ('\n' | eof)
let filename = [^' ''\t''\n']+ '.' [^' ''\t''\n']+

rule tokenize = parse
    whitespace                      { tokenize lexbuf }
    | '\n'                          { Lexing.new_line lexbuf; tokenize lexbuf }
    | comment                       { Lexing.new_line lexbuf; tokenize lexbuf }
    | "DECLARE TIME UNIT TICKS:"    { TIME_UNIT_DECL (Lexing.lexeme_start_p lexbuf) }
    | "DECLARE LENGTH:"             { MEASURE_DECL (Lexing.lexeme_start_p lexbuf) }
    | "DECLARE VOICES:"             { VOICE_DECL (Lexing.lexeme_start_p lexbuf) }
    | "DECLARE KEY:"                { KEY_DECL (Lexing.lexeme_start_p lexbuf) }
    | "DEFINE:"                     { DEFINE (Lexing.lexeme_start_p lexbuf) }
    | "REQUIRE:"                    { REQUIRE (Lexing.lexeme_start_p lexbuf) }
    | "DISALLOW:"                   { DISALLOW (Lexing.lexeme_start_p lexbuf) }
    | "PREFER:"                     { PREFER (Lexing.lexeme_start_p lexbuf) }
    | "AVOID:"                      { AVOID (Lexing.lexeme_start_p lexbuf) }
    | "INCLUDE:"                    { INCLUDE (Lexing.lexeme_start_p lexbuf) }
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
    | "weight"                      { WEIGHT }
    | "mod"                         { MOD }
    | number as n ('p' | 'P')       { PITCHLIT (int_of_string n) }
    | number as n ('i' | 'I')       { INTERVALLIT (int_of_string n) }                                    
    | number as n ('t' | 'T')       { TIMESTEPLIT (int_of_string n) }                                    
    | number as n                   { INTLIT (int_of_string n)}
    | filename as f                 { FILENAME f }
    | identifier as s             { ID s }
    | eof                           { EOF }
