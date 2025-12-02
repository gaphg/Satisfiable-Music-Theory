%{
    open Ast
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
%token TIMESTEP_TYPE
%token BOOLEAN_TYPE
%token TIMESERIES_TYPE
%token LIST_TYPE
%token MAJOR
%token MINOR
%token TRUE
%token FALSE
%token <string> ID
%token <int> INTLIT
%token <string> NOTELIT
%start prog
%type <Ast.program> prog
%%
prog:
    stmt EOF       { [$1] }
    | prog stmt  { $1 @ [$2] }
;
stmt:
    configurationStmt       { ConfigurationStmt $1 }
configurationStmt:
    VOICE_DECL varList    { VoiceCfgStmt $2 }

varList:
    ID                    { [$1] }
    | varList ID          { $1 @ [$2] }
expr:
    FALSE   { BooleanLit false }
;
