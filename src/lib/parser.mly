%{
    open Ast
    open Types
%}

%token EOF
%token VOICE_DECL
%token TIME_UNIT_DECL
%token MEASURE_DECL
%token KEY_DECL 
%token REQUIRE
%token DISALLOW
%token PREFER
%token AVOID
%token LPAREN
%token RPAREN
%token PITCHES
%token CONTOUR
%token DIADS
%token INTERVAL
%token PLUS
%token MINUS
%token NOT
%token AND
%token OR
%token IMPLIES
%token IFF
%token EQUALS
%token NOT_EQUALS
%token LESS
%token LEQ
%token GREATER
%token GEQ
%token AT
%token CONTAINS
%token IS
%token IS_NOT
%token FLATTEN
%token VOICE_TYPE
%token PITCH_TYPE
%token INTERVAL_TYPE
%token INTEGER_TYPE
%token TIMESTEP_TYPE
%token BOOLEAN_TYPE
%token TIMESERIES_TYPE
%token LIST_TYPE
%token MAJOR
%token MINOR
%token TRUE
%token FALSE
%token COMMA
%token DEFINE
%token OF
%token <string> ID
%token <int> INTLIT
%start prog
%type <Ast.program> prog
%%
prog: 
    stmtList EOF  { $1}
;
stmt:
    configurationStmt       { ConfigurationStmt $1 }
    | definitionStmt        { DefinitionStmt $1}
;

(* CONFIGURATION STATEMENTS *)
configurationStmt:
    VOICE_DECL varList          { VoiceCfgStmt $2 }
    | TIME_UNIT_DECL INTLIT     { TimeUnitTicksCfgStmt $2 }
    | MEASURE_DECL INTLIT       { SongLengthUnitsCfgStmt $2 }
    | KEY_DECL INTLIT           { KeyCfgStmt (PitchLit $2) }    (* TODO allow 'C' instead of '60' *)
;

(* DEFINITION STATEMENTS *)
definitionStmt:
    DEFINE ID EQUALS expr                               { ConstDefStmt ($2, $4) }
    | DEFINE ID LPAREN formalList RPAREN EQUALS expr    { FuncDefStmt ($2, $4, $7) }
;
formal:
    ID                              { ($1, None) }
    | ID sz_type                    { ($1, Some $2) }
;

(* SPECIFICATION STATEMENTS *)

(* EXPRESSIONS! *)
expr:
    TRUE                    { BooleanLit true }
;

(* LISTS *)
stmtList:
    stmt                    { [$1] }
  | stmt stmtList           { $1 :: $2 }
;
varList:
    ID                      { [$1] }
  | ID COMMA varList        { $1 :: $3 }
;
formalList:
  formal                      { [$1] }  
  | formal COMMA formalList   { $1 :: $3}  


(* TYPES *)
sz_type:
    VOICE_TYPE                      { VoiceType }
    | PITCH_TYPE                    { PitchType }
    | INTERVAL_TYPE                 { IntervalType }
    | TIMESTEP_TYPE                 { TimeStepType }
    | INTEGER_TYPE                  { IntegerType }
    | BOOLEAN_TYPE                  { BooleanType }
    | TIMESERIES_TYPE OF sz_type    { TimeSeriesType $3 }
    | LIST_TYPE OF sz_type          { ListType $3 }