/*
 * SNU 4541.664A Program Analysis 
 *
 * K-- Parser
 */
  

%{       

exception EmptyBinding
exception ParsingError

%}

%token <int> NUM
%token READINT
%token <string> ID
%token PLUS MINUS COLONEQ SEMICOLON IF THEN ELSE COMMA
%token REPEAT UNTIL
%token LP RP
%token EOF

%left SEMICOLON
%nonassoc REPEAT
%nonassoc UNTIL
%nonassoc THEN
%nonassoc ELSE
%right COLONEQ
%left PLUS MINUS

%start program
%type <Program.cmd> program

%%

program:
       cmd EOF { $1 }
    ;

cmd: 
      LP cmd RP { $2 }
    | ID COLONEQ expr { Program.ASSIGN ($1,$3) }
    | cmd SEMICOLON cmd { Program.SEQ ($1,$3) }
    | IF expr THEN cmd ELSE cmd { Program.IF ($2, $4, $6) }
    | REPEAT cmd UNTIL expr { Program.REPEAT ($2, $4) }
	;
expr:
	| LP expr RP { $2 }
	| NUM { Program.NUM ($1) }
	| expr PLUS expr { Program.ADD ($1, $3) }
	| MINUS expr { Program.MINUS ($2) }
	| ID { Program.VAR ($1) }
	| READINT LP NUM COMMA NUM RP { Program.READINT($3, $5) }
%%
