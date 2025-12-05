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
%token INCLUDE
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
%token IN
%token IS
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
%token TRUE_TOKEN
%token FALSE_TOKEN
%token COMMA
%token DEFINE
%token UP
%token DOWN
%token LBRACK
%token RBRACK
%token OF
%token IS_NOT
%token FORALL
%token EXISTS
%token WHERE
%token <string> ID
%token <int> PITCHLIT
%token <int> INTERVALLIT
%token <int> TIMESTEPLIT
%token <int> INTLIT
%token <string> FILENAME
%right IMPLIES IFF
%nonassoc FORALL EXISTS
%left OR
%left AND
%left EQUALS NOT_EQUALS IS IS_NOT
%left LESS LEQ GREATER GEQ
%left PLUS MINUS
%left CONTAINS AT IN
%right NOT
%nonassoc PITCHES CONTOUR DIADS INTERVAL FLATTEN
%start prog
%type <Ast.program> prog
%%
prog: 
    stmtList EOF  { $1}
;
stmt:
    configurationStmt       { ConfigurationStmt $1 }
    | definitionStmt        { DefinitionStmt $1}
    | specificationStmt     { SpecificationStmt $1}
    | INCLUDE FILENAME      { IncludeStmt $2 }
;

(* CONFIGURATION STATEMENTS *)
configurationStmt:
    VOICE_DECL varList          { VoiceCfgStmt $2 }
    | TIME_UNIT_DECL INTLIT     { TimeUnitTicksCfgStmt $2 }
    | MEASURE_DECL INTLIT       { SongLengthUnitsCfgStmt $2 }
    | KEY_DECL PITCHLIT         { KeyCfgStmt (PitchLit $2) }    (* TODO allow 'C' instead of '60' *)
;

(* DEFINITION STATEMENTS *)
definitionStmt:
    DEFINE ID EQUALS expr                               { ConstDefStmt ($2, $4) }
    | DEFINE ID LPAREN formalList RPAREN EQUALS expr    { FuncDefStmt ($2, $4, $7) }
formal:
    ID                              { ($1, ref None) }
    | ID sz_type                    { ($1, ref (Some $2)) }

(* SPECIFICATION STATEMENTS *)
specificationStmt:
    REQUIRE expr                    { RequireStmt $2 }
    | DISALLOW expr                 { DisallowStmt $2 }
    | PREFER expr                   { PreferStmt $2 }
    | AVOID expr                    { AvoidStmt $2 }

(* EXPRESSIONS! *)
expr:
    FORALL LBRACK formalList RBRACK COMMA expr     { Forall ($3, $6) }
    | EXISTS LBRACK formalList RBRACK WHERE expr   { Exists ($3, $6) }
    | logic_expr                                   { $1 } 
logic_expr:
    logic_expr IMPLIES logic_expr       { Implies ($1, $3) }
    | logic_expr IFF logic_expr         { Iff ($1, $3) }
    | or_exp                            { $1 }
or_exp:
    or_exp OR or_exp        { Or ($1, $3) }
    | and_exp               { $1 }
and_exp:
    and_exp AND and_exp     { And ($1, $3) }
    | cmpeq_expr            { $1 }
cmpeq_expr:
    cmpeq_expr EQUALS cmpeq_expr        { Equals ($1, $3) }  
    | cmpeq_expr NOT_EQUALS cmpeq_expr  { NotEquals ($1, $3) }
    | cmpeq_expr IS cmpeq_expr          { EqualsModOctave ($1, $3) }
    | cmpeq_expr IS_NOT cmpeq_expr      { NotEqualsModOctave ($1, $3) }
    | cmp_expr                          { $1 }
cmp_expr:
    cmp_expr LESS cmp_expr          { LessThan ($1, $3) }
    | cmp_expr LEQ cmp_expr         { LessThanEq ($1, $3) }
    | cmp_expr GREATER cmp_expr     { GreaterThan ($1, $3) }
    | cmp_expr GEQ cmp_expr         { GreaterThanEq ($1, $3) }
    | arith_expr                    { $1 }
arith_expr:
    arith_expr PLUS arith_expr      { Plus ($1, $3) }
    | arith_expr MINUS arith_expr   { Minus ($1, $3) }
    | misc_expr                     { $1 }
misc_expr:
    misc_expr AT misc_expr          { ElementAt ($1, $3)}
    | misc_expr CONTAINS misc_expr  { Contains ($1, $3) }
    | misc_expr IN misc_expr        { Contains ($3, $1) }
    | prefix_ops                    { $1 }
prefix_ops:
    PITCHES prefix_ops                              { Pitches $2 }
    | CONTOUR prefix_ops                              { Contour $2 }
    | DIADS LPAREN prefix_ops COMMA prefix_ops RPAREN       { Diads ($3, $5) }
    | INTERVAL LPAREN prefix_ops COMMA prefix_ops RPAREN    { IntervalBetween ($3, $5) }
    | NOT prefix_ops                            { Not $2 }
    | FLATTEN prefix_ops                        { Flatten $2 }
    | atom                                      { $1 }
atom:
    literal                                     { $1 }
    | ID                                        { Var $1 }
    | ID LPAREN exprList RPAREN                 { FuncCall ($1, $3) }
    | LBRACK exprList RBRACK                    { ListExpr $2 }
    | LPAREN expr RPAREN                        { $2 }
literal:
    PITCHLIT                { PitchLit $1 }
    | INTERVALLIT           { IntervalLit ($1, None) }
    | INTERVALLIT dir_lit   { IntervalLit ($1, Some $2) }
    | TIMESTEPLIT           { TimeStepLit $1 }
    | INTLIT                { IntegerLit $1 }
    | dir_lit               { DirectionLit $1 }
    | bool_lit              { BooleanLit $1 }
dir_lit:
    UP                      { true }
    | DOWN                  { false }
bool_lit:
    TRUE_TOKEN            { true }
    | FALSE_TOKEN         { false }

(* LISTS *)
stmtList:
    stmt                    { [$1] }
  | stmt stmtList           { $1 :: $2 }
varList:
    ID                      { [$1] }
  | ID COMMA varList        { $1 :: $3 }
formalList:
  formal                      { [$1] }  
  | formal COMMA formalList   { $1 :: $3}  
exprList:
  expr                        { [$1] }  
  | expr COMMA exprList       { $1 :: $3 }  

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