lexer grammar FCC_Lexer;

channels { LINE_DIRECTIVE_CHANNEL }

fragment A  :('A'|'a');
fragment B  :('B'|'b');
fragment C  :('C'|'c');
fragment D  :('D'|'d');
fragment E  :('E'|'e');
fragment F  :('F'|'f');
fragment G  :('G'|'g');
fragment H  :('H'|'h');
fragment I  :('I'|'i');
fragment J  :('J'|'j');
fragment K  :('K'|'k');
fragment L  :('L'|'l');
fragment M  :('M'|'m');
fragment N  :('N'|'n');
fragment O  :('O'|'o');
fragment P  :('P'|'p');
fragment Q  :('Q'|'q');
fragment R  :('R'|'r');
fragment S  :('S'|'s');
fragment T  :('T'|'t');
fragment U  :('U'|'u');
fragment V  :('V'|'v');
fragment W  :('W'|'w');
fragment X  :('X'|'x');
fragment Y  :('Y'|'y');
fragment Z  :('Z'|'z');

fragment LOWERCASE : [a-z] ;
fragment UPPERCASE : [A-Z] ;
fragment NUMBER    : [0-9] ;

fragment WHITESPACE : [ \t\n\r] ;


LINE_DIRECTIVE : '#' L I N E [ \t]+ NUMBER+ [ \t]+ '"' (UPPERCASE | LOWERCASE | NUMBER | ':' | '\\' | '/' | '.' | '-' | '_' )+ '"' -> channel(LINE_DIRECTIVE_CHANNEL);

DOTDOT         : '..' ;
DOT         : '.' ;
ASSIGN      : ':=' ;
DROPASSIGN  : '#=' ;
SETDEFAULTVALUE : '?=' ;
SEMICOLON   : ';' ;
COLON       : ':' ;
LPAREN      : '(' ;
RPAREN      : ')' ;
LBRACKET    : '[' ;
RBRACKET    : ']' ;
TPS_COMMA   : ',' ;

QUESTIONMARK : '?' ;
POUNDSIGN : '#' ;

PLUS        : '+' ;
MINUS       : '-' ;
MULT        : '*' ;
DIV         : '/' ;

LT          : '<' ;
LE          : '<=' ;
NE          : '<>' ;
EQ          : '=' ;
GT          : '>' ;
GE          : '>=' ;

fragment FILENAME    : '"' [^"\n\r]* '"' ; //Replaced \" with "

WS          : '[ \t]+' -> skip;

OPTWS       : WHITESPACE+? -> skip;





CHAR        : '\'' (LOWERCASE | UPPERCASE) '\'' ;

// Borrowed from Pascal grammar on Antlr lab, needs to be tested/checked
STRING
    : '\'' ('\'\'' | ~ ('\''))* '\''
    | '"' ('""' | ~ ('"'))* '"'
    ;

INTEGER     : NUMBER+ ;

REAL        : NUMBER* '.' NUMBER+ ; //Replace \. with .

NEWLINE     : '\n' ;

RETURN      : '\r' ;


/* Keywords */
KW_BEGIN       : B E G I N ;
KW_END         : E N D ;

KW_TABLE       : T A B L E ;
KW_VAR         : V A R ;
KW_CONSTANT    : C O N S T A N T ;

KW_ARRAY           : A R R A Y ;
KW_BOOLEANTYPE     : B O O L E A N ;
KW_TRUE            : T R U E ;
KW_FALSE           : F A L S E ;
KW_INTEGERTYPE     : I N T E G E R ;
KW_LOOPINDEXTYPE   : I N D E X ;

KW_FORM        : F O R M ;
KW_SECTION     : S E C T I O N ;
KW_FUNCTION    : F U N C T I O N ;
KW_PROCEDURE   : P R O C E D U R E ;

KW_BLANK                : B L A N K ;
KW_OPENFORM             : O P E N F O R M ;
KW_OPENFORMWITHUUID     : O P E N F O R M W I T H U U I D ;
KW_RUNSECTIONSTHATREAD  : R U N S E C T I O N S T H A T R E A D ;
KW_NUMCOPIES            : N U M C O P I E S ;
KW_FIRSTAVAILABLEINDEX  : F I R S T A V A I L A B L E I N D E X ;
KW_LASTINDEX            : L A S T I N D E X ;
KW_NUMBEROFRECORDS      : N U M B E R O F R E C O R D S ;
KW_TESTFORMIS           : T E S T F O R M I S ;
KW_TESTSUPPORTEDFORMIS  : T E S T S U P P O R T E D F O R M I S ;
KW_GETTABLEVALUE        : G E T T A B L E V A L U E ;
KW_FORMHASUSERDATA      : F O R M H A S U S E R D A T A ;
KW_ISUSERENTERED        : I S U S E R E N T E R E D ;
KW_GETCCTYPE            : G E T C C T Y P E ;
KW_ISCCTOKENIZED        : I S C C T O K E N I Z E D ;
KW_ISCCCOMPLETE         : I S C C C O M P L E T E ;
KW_IF                   : I F ;
KW_THEN                 : T H E N ;
KW_ELSE                 : E L S E ;
KW_CASE                 : C A S E ;
KW_OF                   : O F ;
KW_OTHERWISE            : O T H E R W I S E ;
KW_WHILE                : W H I L E ;
KW_DO                   : D O ;
KW_REPEAT               : R E P E A T ;
KW_UNTIL                : U N T I L ;
KW_FOR                  : F O R ;
KW_TO                   : T O ;
KW_DOWNTO               : D O W N T O ;
KW_BREAK                : B R E A K ;
KW_CONTINUE             : C O N T I N U E ;
KW_DOESNOTDEPENDON      : D O E S N O T D E P E N D O N ;

KW_ISNONFINAL           : I S N O N F I N A L ;
KW_ISNONFINALERROR      : I S N O N F I N A L E R R O R ;

KW_AUTOARRAY            : A U T O A R R A Y ;
KW_FOREACH              : F O R E A C H ;
KW_REVERSE              : R E V E R S E ;

KW_MOD                  : M O D ;
KW_IN                   : I N ;
KW_AND                  : A N D ;
KW_OR                   : O R ;
KW_NOT                  : N O T ;

KW_BOXGROUP             : B O X G R O U P ;
KW_MUTEX                : M U T E X ;

KW_EFNV                 : '@' E F ;
KW_GENERICNV            : '@' G E N E R I C ;

KW_GETPLDESCRIPTION        : G E T P L D E S C R I P T I O N ;
KW_ISFORMPURETRANSFERRED   : I S F O R M P U R E T R A N S F E R R E D ;
KW_ISROWPURETRANSFERRED    : I S R O W P U R E T R A N S F E R R E D ;
/* End Keywords */


//NAMER       : '[A-Za-z_][A-Za-z0-9_]*?' ;
NAME        : (LOWERCASE | UPPERCASE | '_') (LOWERCASE | UPPERCASE | NUMBER | '_')* ;

BACKSLASH       : '\\' -> skip;
