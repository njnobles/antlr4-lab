parser grammar FCC_Parser;

options {
	tokenVocab = FCC_Lexer;
}

//-----------------------------------------




/****************************************************************************
    The following establishes precedence for these operators
***************************************************************************
%left    _EQ _LT _GT _NE  _LE  _GE _IN
%left    _PLUS _MINUS _OR
%left    _MULT _DIV MOD _AND
%right   _NOT
%right   _UNARY
*/
/************************************************************************
   The Entire Source File
************************************************************************/

program
   : sourceProgram EOF
   ;

sourceProgram
	:	formDecls formLevelDecls proceduralCodeBlock proceduralBlockList
	|	formDecls formLevelDecls proceduralCodeBlock
		/**************  eRROR rULES  *************/
	|	error
	|	formLevelDeclError formLevelDecls proceduralCodeBlock proceduralBlockList
	|	formLevelDeclError formLevelDecls proceduralCodeBlock
	;



formLevelDeclError
   :  formDecls error
   ;



/***********************************************************************
    Form Specification
************************************************************************/

//// NOTE: Merged formDecls with formDeclList

formDecls
	:	formDeclList
	;

// formDeclList
//    :  formDecl+
//    ;

formDeclList
   :  formDecl
   |  formDecl formDeclList
   ;



formDecl
   :  KW_FORM identifier DOT identifier requiredSemicolon
      /**************  ERROR RULES  *************/
   |  KW_FORM identifier requiredSemicolon
   |  KW_FORM error SEMICOLON
   ;



/************************************************************************
   Form Level Declarations
************************************************************************/

formLevelDecls
   :  formLevelDeclList
   |
   ;



formLevelDeclList
   :  formLevelDecl
   |  formLevelDecl formLevelDeclList
   ;



formLevelDecl
   :  globalVar globalFieldDeclList
   |  globalConstant globalConstantDeclList
   |  tableDecl
   ;



globalVar
   :  KW_VAR
   ;



globalFieldDeclList
   :  globalFieldDecl
   |  globalFieldDecl globalFieldDeclList
   ;

// globalFieldDeclList
//    :  globalFieldDecl+
//    ;


globalFieldDecl
   :  identifierList COLON (optionalArrayDecl | dDArrayDecl)? globalFieldType requiredSemicolon # globalFieldDecl_simple
   |  identifierList COLON (optionalArrayDecl)? KW_BOXGROUP LPAREN identifierList RPAREN boxGroupType requiredSemicolon # globalFieldDecl_boxGroup
   //:  identifierList COLON optionalArrayDecl globalFieldType requiredSemicolon # globalFieldDecl_simple
      /*************** Double dimensioned array **********/
   //|  identifierList COLON dDArrayDecl globalFieldType requiredSemicolon # globalFieldDecl_doubleDimensionArray
      /******** Box Group Declaration *********/
   //|  identifierList COLON optionalArrayDecl KW_BOXGROUP LPAREN identifierList RPAREN boxGroupType requiredSemicolon # globalFieldDecl_boxGroup
      /**************  ERROR RULES  *************/
   |  error COLON globalFieldType requiredSemicolon # globalFieldDecl_error1
   |  identifierList error globalFieldType requiredSemicolon # globalFieldDecl_error2
   |  identifierList COLON error requiredSemicolon # globalFieldDecl_error3
   |  error requiredSemicolon # globalFieldDecl_error4
   ;



boxGroupType
   :  KW_MUTEX
   |
   ;



optionalArrayDecl
   :  KW_ARRAY LBRACKET requiredIntegerOrConstant RBRACKET KW_OF # optionalArrayDecl_array
   |  KW_AUTOARRAY optionalEmptySquareBrackets KW_OF # optionalArrayDecl_autoArray
   //|  # optionalArrayDecl_empty
   ;



optionalEmptySquareBrackets
   :  LBRACKET RBRACKET
   |
   ;



requiredIntegerOrConstant
   :  integer
   |  identifier
      /*** Errors ***/
   |
   ;



dDArrayDecl
   :  KW_ARRAY LBRACKET requiredIntegerOrConstant TPS_COMMA requiredIntegerOrConstant RBRACKET KW_OF
   //|
   ;


globalFieldType
   :  KW_BOOLEANTYPE
   |  KW_INTEGERTYPE    /*This production is here only to handle the error*/
   |  identifier
   ;


globalConstant
   :  KW_CONSTANT
   ;


globalConstantDeclList
   :  globalConstantDecl
   |  globalConstantDecl globalConstantDeclList
   ;

// globalConstantDeclList
//    :  globalConstantDecl+
//    ;



globalConstantDecl
   :  identifier constantAssignmentOperator signedLiteral requiredSemicolon
      /**************  eRROR rULES  *************/
   |  identifier constantAssignmentOperator error requiredSemicolon
   |  error requiredSemicolon
   ;



tableDecl
   :  tableHeader tableContents requiredSemicolon
      /**************  eRROR rULES  *************/
   |  tableDeclError tableContents requiredSemicolon
   ;



tableHeader
   :  KW_TABLE LPAREN tableIndex TPS_COMMA tableType TPS_COMMA tableType RPAREN requiredSemicolon
   ;



tableDeclError
   :  KW_TABLE error
   ;



tableIndex
   :  integer
      /**************  ERROR RULES  *************/
   |  error
   ;



tableType
   :  identifier
      /**************  ERROR RULES  *************/
   |  KW_BOOLEANTYPE
   |  KW_INTEGERTYPE
   ;



tableContents
   :  begin tablePairList KW_END
   ;



begin
   :  KW_BEGIN
   |
   ;




// tablePairList
//    :  tableLine
//    |  tableLine tablePairList
//    ;

tablePairList
   :  tableLine+
   ;



tableLine
   :  tablePair requiredSemicolon
      /**************  eRROR rULES  *************/
   |  error requiredSemicolon
   ;



tablePair
   :  signedNumber TPS_COMMA signedNumber
   ;



/************************************************************************
   Procedural Blocks (Section, Procedure, Function)
************************************************************************/

proceduralBlockList
   :  proceduralCodeBlock
   |  proceduralCodeBlock proceduralBlockList
   /**************  eRROR rULES  *************/
   |  proceduralCodeBlockError proceduralBlockList
   ;



proceduralCodeBlock
   :  section
   |  function
   |  procedure
   ;



proceduralCodeBlockError
   :  error SEMICOLON
   ;



section
   :  sectionDecl optDependencyDecls localDecls codeBlock requiredSemicolon
      /**************  eRROR rULES  *************/
   |  sectionDeclError optDependencyDecls localDecls codeBlock requiredSemicolon
   ;



sectionDeclError
   :  sectionDecl error
   ;



sectionDecl
   :  KW_SECTION identifier requiredSemicolon
      /**************  ERROR RULES  *************/
   |  KW_SECTION identifier formalParameterList requiredSemicolon
   |  KW_SECTION error SEMICOLON
   ;



procedure
   :  procedureDecl localDecls codeBlock requiredSemicolon
      /**************  eRROR rULES  *************/
   |  procedureDeclError localDecls codeBlock requiredSemicolon
   ;



procedureDeclError
   :  procedureDecl error
   ;



procedureDecl
   :  KW_PROCEDURE identifier requiredSemicolon
      /**************  ERROR RULES  *************/
   |  KW_PROCEDURE identifier formalParameterList requiredSemicolon
   |  KW_PROCEDURE error requiredSemicolon
   ;



function
   :  functionDecl localDecls codeBlock requiredSemicolon
      /**************  eRROR rULES  *************/
   |  functionDeclError localDecls codeBlock requiredSemicolon
   ;



functionDeclError
   :  functionDecl error
   ;



functionDecl
   :  KW_FUNCTION identifier formalParameterList COLON functionType requiredSemicolon
      /**************  ERROR RULES  *************/
   |  KW_FUNCTION identifier formalParameterList requiredSemicolon
   |  KW_FUNCTION error formalParameterList COLON functionType requiredSemicolon
   |  KW_FUNCTION error COLON functionType requiredSemicolon
   ;



functionType
   :  KW_BOOLEANTYPE
   |  identifier
      /**************  ERROR RULES  *************/
   |  KW_LOOPINDEXTYPE
   |  KW_INTEGERTYPE   /*This production is here only to handle the error*/
   ;



formalParameterList
   :  LPAREN typedIdentifierList RPAREN
   |  LPAREN RPAREN
      /**************  ERROR RULES  *************/
   |  LPAREN error RPAREN
   |  LPAREN error SEMICOLON
   ;



typedIdentifierList
   :  typedIdentifiers
   |  typedIdentifierList SEMICOLON typedIdentifiers
   ;



typedIdentifiers
   :  identifierList COLON parameterType
   ;



parameterType
   :  KW_BOOLEANTYPE
   |  identifier
      /**************  ERROR RULES  *************/
   |  KW_LOOPINDEXTYPE
   |  KW_INTEGERTYPE
   ;



/************************************************************************
   Non Dependency Declarations
************************************************************************/

optDependencyDecls
   :  KW_DOESNOTDEPENDON dependencyList
   |
   ;



// dependencyList
//    :  dependencyDecl
//    |  dependencyDecl dependencyList
//    ;

dependencyList
   :  dependencyDecl+
   ;



dependencyDecl
   :  identifier DOT identifier requiredSemicolon
      /**************  ERROR RULES  *************/
   |  error DOT identifier requiredSemicolon
   |  identifier error identifier requiredSemicolon
   |  identifier DOT error requiredSemicolon
   |  error requiredSemicolon
   ;



/************************************************************************
   Local Declarations
************************************************************************/

localDecls
   :  localDeclList
   |
   ;



localDeclList
   :  localDecl
   |  localDecl localDeclList
   ;



localDecl
   :  KW_VAR localVariableDeclList
   |  KW_CONSTANT localConstantDeclList
   ;



localVariableDeclList
   :  localVariableDecl
   |  localVariableDecl localVariableDeclList
   ;

// localVariableDeclList
//    :  localVariableDecl+
//    ;



localVariableDecl
   :  identifierList COLON (optionalArrayDecl | dDArrayDecl)? localVariableType requiredSemicolon
   //:  identifierList COLON optionalArrayDecl localVariableType requiredSemicolon
      /*************** Double dimensioned array **********/
   //|  identifierList COLON dDArrayDecl localVariableType requiredSemicolon
      /**************  ERROR RULES  *************/
   |  error COLON localVariableType requiredSemicolon
   |  identifierList error localVariableType requiredSemicolon
   |  identifierList COLON error requiredSemicolon
   |  error requiredSemicolon
   ;



localVariableType
   :  KW_INTEGERTYPE
   |  KW_LOOPINDEXTYPE
   |  KW_BOOLEANTYPE
   |  identifier
   ;



localConstantDeclList
   :  localConstantDecl
   |  localConstantDecl localConstantDeclList
   ;

// localConstantDeclList
//    :  localConstantDecl+
//    ;



localConstantDecl
   :  identifier constantAssignmentOperator signedLiteral requiredSemicolon
      /**************  ERROR RULES  *************/
   |  identifier constantAssignmentOperator error requiredSemicolon
   |  error requiredSemicolon
   ;



constantAssignmentOperator
   :  EQ
      /**************  ERROR RULES  *************/
   |  ASSIGN
   |  DROPASSIGN
   |  SETDEFAULTVALUE
   ;

/************************************************************************
   Code Blocks (statements)
************************************************************************/

codeBlock
   :  KW_BEGIN statementList KW_END
   |  KW_BEGIN KW_END
      /**************  ERROR RULES  *************/
   |  KW_BEGIN statementList KW_ELSE
   ;



statementList
   :  statementWithSemicolon
   |  statementWithSemicolon statementList
   ;

// statementList
//    :  statementWithSemicolon+
//    ;




statementWithSemicolon
   :  statement SEMICOLON
      /**************  ERROR RULES  *************/
   |  statement error
   ;



// statement
//    :  codeBlock # statement_codeBlock
//    |  assignmentStatement # statement_assignment
//    |  ifStatement # statement_if
//    |  caseStatement # statement_case
//    |  whileStatement # statement_while
//    |  repeatStatement # statement_repeat
//    |  forStatement # statement_for
//    |  forEachStatement # statement_forEach
//    |  breakStatement # statement_break
//    |  continueStatement # statement_continue
//    |  blankStatement # statement_blank
//    |  openFormStatement # statement_openForm
//    |  openFormWithUuidStatement # statement_openFormWithUuid
//    |  procedureCallStatement # statement_procedureCall
//    |  runSectionsThatReadStatement # statement_runSectionsThatRead
//       /**************  eRROR rULES  *************/
//    |  procBlockHeader # statement_procBlockHeader
//    |  error # statement_error
//    ;

statement
   :  nonIfStatement # statement_nonIfStatement
   |  ifStatement # statement_if
   |  loopStatement # statement_loop
   ;

nonIfStatement
   :  codeBlock # statement_codeBlock
   |  assignmentStatement # statement_assignment
   |  caseStatement # statement_case
   //|  KW_WHILE whileStatement # statement_while
   |  repeatStatement # statement_repeat
   //|  forStatement # statement_for
   //|  forEachStatement # statement_forEach
   |  breakStatement # statement_break
   |  continueStatement # statement_continue
   |  blankStatement # statement_blank
   |  openFormStatement # statement_openForm
   |  openFormWithUuidStatement # statement_openFormWithUuid
   |  procedureCallStatement # statement_procedureCall
   |  runSectionsThatReadStatement # statement_runSectionsThatRead
      /**************  eRROR rULES  *************/
   |  procBlockHeader # statement_procBlockHeader
   |  error # statement_error
   ;

loopStatement
   :  KW_WHILE whileStatement # statement_while
   |  forStatement # statement_for
   |  forEachStatement # statement_forEach
   ;


procBlockHeader
   :  KW_SECTION
   |  KW_FUNCTION
   |  KW_PROCEDURE
   ;



assignmentStatement
   :  writableAddress ASSIGN expression # assignmentStatement_assign
   |  writableAddress DROPASSIGN expression # assignmentStatement_dropAssign
   |  writableAddress SETDEFAULTVALUE expression # assignmentStatement_setDefaultValue
      /**************  ERROR RULES  *************/
   |  writableAddress EQ expression # assignmentStatement_equal
   |  writableAddress QUESTIONMARK ASSIGN expression # assignmentStatement_questionMarkAssign
   |  writableAddress POUNDSIGN ASSIGN expression # assignmentStatement_poundSignAssign
   ;



// ifStatement
//    :  KW_IF condition KW_THEN ifStatementBody=statement (KW_ELSE elseStatementBody=statement)?
//    //:  KW_IF condition KW_THEN ifStatementBody=statement
//    //|  KW_IF condition KW_THEN ifStatementBody=statement KW_ELSE elseStatementBody=statement
//       /**************  ERROR RULES  *************/
//    |  ifConditionError statement
//    ;

   
ifStatement
   //:  KW_IF condition KW_THEN (nonIfStatement KW_ELSE elseStatementBody=statement | statement) // not ambiguous w/o loops (at least simple if-else)
   :  KW_IF condition KW_THEN ((nonIfStatement | loopStatement) (KW_ELSE elseStatementBody=statement)? | ifStatement) // not ambiguous w/o loops (at least simple if-else)
   //:  KW_IF condition KW_THEN ifStatementBody=statement (KW_ELSE elseStatementBody=statement)? //ambiguous regardless
   //:  KW_IF condition KW_THEN ifStatementBody=statement
   //|  KW_IF condition KW_THEN ifStatementBody=statement KW_ELSE elseStatementBody=statement
      /**************  ERROR RULES  *************/
   |  ifConditionError statement
   ;



ifConditionError
   :  KW_IF error KW_THEN
   |  KW_IF condition error
   ;



caseStatement
   :  KW_CASE fieldValueExpression KW_OF caseList KW_END
      /**************  ERROR RULES  *************/
   |  caseHeaderError caseList KW_END
   ;



caseHeaderError
   :  KW_CASE error KW_OF
   |  KW_CASE fieldValueExpression error
   ;



caseList
   :  case
   |  case caseList
   ;



case
   :  signedLiteralList COLON statement requiredSemicolon
   |  KW_OTHERWISE statement requiredSemicolon
      /**************  ERROR RULES  *************/
   |  caseError statement requiredSemicolon
   ;



caseError
   :  error COLON
   ;

whileStatement
   :  condition KW_DO statement
      /**************  ERROR RULES  *************/
   |  whileConditionError statement
   ;



whileConditionError
   :  error KW_DO
   |  condition error
   ;



// whileHeader
//    :  KW_WHILE
//    ;

// whileStatement
//    :  whileHeader condition KW_DO statement
//       /**************  ERROR RULES  *************/
//    |  whileConditionError statement
//    ;



// whileConditionError
//    :  whileHeader error KW_DO
//    |  whileHeader condition error
//    ;



// whileHeader
//    :  KW_WHILE
//    ;



repeatStatement
   :  repeatHeader statementList KW_UNTIL condition
   |  repeatHeader KW_UNTIL condition
   ;



repeatHeader
   :  KW_REPEAT
   ;



forStatement
   :  forHeader forLoopIndex forLoopAssign fieldValueExpression direction fieldValueExpression KW_DO statement
      /**************  ERROR RULES  *************/
   |  forHeaderError statement
   ;



forHeaderError
   :  forHeader error forLoopAssign fieldValueExpression direction fieldValueExpression KW_DO
   |  forHeader forLoopIndex error fieldValueExpression direction fieldValueExpression KW_DO
   |  forHeader forLoopIndex forLoopAssign error direction fieldValueExpression KW_DO
   |  forHeader forLoopIndex forLoopAssign fieldValueExpression error fieldValueExpression KW_DO
   |  forHeader forLoopIndex forLoopAssign fieldValueExpression direction error KW_DO
   |  forHeader forLoopIndex forLoopAssign fieldValueExpression direction fieldValueExpression error
   ;



forHeader
   :  KW_FOR
   ;



forLoopAssign
   :  ASSIGN
   |  EQ
   |  DROPASSIGN
   |  SETDEFAULTVALUE
   ;



forLoopIndex
   :  identifier
   ;



direction
   :  KW_DOWNTO
   |  KW_TO
   ;



forEachStatement
   :  forEachHeader forEachTarget KW_IN forEachSource KW_DO optionalReverse statement
      /**************  ERROR RULES  *************/
   |  forEachHeader error KW_IN forEachSource KW_DO optionalReverse statement
   |  forEachHeader forEachTarget error forEachSource KW_DO optionalReverse statement
   |  forEachHeader forEachTarget KW_IN error KW_DO optionalReverse statement
   |  forEachHeader forEachTarget KW_IN forEachSource error optionalReverse statement
   |  forEachHeader forEachTarget KW_IN forEachSource KW_DO optionalReverse error
   ;



forEachHeader
   :  KW_FOREACH
   ;



forEachTarget
   :  writableAddress
   ;



forEachSource
   :  readableFieldAddress
   |  formAddress DOT readableFieldAddress
   ;



optionalReverse
   :  KW_REVERSE
   |  
   ;



breakStatement
   :  KW_BREAK
   ;



continueStatement
   :  KW_CONTINUE
   ;




/*
   The Blank statement must be part of the grammar instead of using the
   call back routine objects. This is because we need WritableAddresses
   for parameters instead of ReadableAddresses.  Taking this route allows
   us to use the existing mechanisms to add the dependency tree info easily.
*/

blankStatement
   :  KW_BLANK LPAREN blankList RPAREN
      /**************  ERROR RULES  *************/
   |  KW_BLANK LPAREN error RPAREN
   |  KW_BLANK LPAREN error SEMICOLON
   ;



blankList
   :  writableAddress
   |  writableAddress TPS_COMMA blankList
   ;



/*
   The OpenForm statement must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form
   address a legal primary in the YACC grammar or all heck will break
   loose.
*/

openFormStatement
   :  KW_OPENFORM LPAREN formAddress RPAREN

      /**************  ERROR RULES  *************/
   |  KW_OPENFORM LPAREN error RPAREN
   |  KW_OPENFORM LPAREN error SEMICOLON
   ;

openFormWithUuidStatement
    :  KW_OPENFORMWITHUUID LPAREN formAddress RPAREN
    | KW_OPENFORMWITHUUID LPAREN formAddress TPS_COMMA formAddress RPAREN
   /* Incase you want to add support for FormFieldAddress uncomment this.
    | KW_OPENFORMWITHUUID LPAREN FormAddress TPS_COMMA ReadableAddress RPAREN
    /**************  ERROR RULES  *************/
    |  KW_OPENFORMWITHUUID LPAREN error RPAREN
    |  KW_OPENFORMWITHUUID LPAREN error SEMICOLON
    ;

runSectionsThatReadStatement
   :  KW_RUNSECTIONSTHATREAD LPAREN readableAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_RUNSECTIONSTHATREAD LPAREN error RPAREN
   |  KW_RUNSECTIONSTHATREAD LPAREN error SEMICOLON
   ;



procedureCallStatement
   :  identifier LPAREN parameterList RPAREN
      /**************  ERROR RULES  *************/
   |  identifier LPAREN error RPAREN
   |  identifier LPAREN error SEMICOLON
   ;



parameterList
   :  expressionList
   |
   ;



/************************************************************************
   Expressions
************************************************************************/

expressionList
   :  simpleExpression
   |  simpleExpression TPS_COMMA expressionList
   ;



simpleExpression
   :  expression
   ;


// MINUS and PLUS <assoc=right>? had %prec _UNARY
expression
   :  LPAREN expression RPAREN # expression_parenthesis
   |  (PLUS | MINUS) expression # expression_unary_plus_minus
   //|  MINUS expression # expression_unary_minus
   //|  PLUS expression # expression_unary_plus
   |  expression (MULT | DIV | KW_MOD) expression # expression_multiply_divide_modulo
   //|  expression MULT expression # expression_multiply
   //|  expression DIV expression # expression_divide
   //|  expression KW_MOD expression # expression_modulo
   |  expression (PLUS | MINUS) expression # expression_plus_minus
   //|  expression PLUS expression # expression_plus
   //|  expression MINUS expression # expression_minus
   |  expression (LT | GT | GE | LE) expression # expression_comparison
   |  expression (EQ | NE) expression # expression_equality_comparison
   //|  expression LT expression # expression_less_than
   //|  expression GT expression # expression_greater_than
   //|  expression EQ expression # expression_equal
   //|  expression NE expression # expression_not_equal
   //|  expression GE expression # expression_greater_than_or_equal
   //|  expression LE expression # expression_less_than_or_equal
   |  expression KW_IN LBRACKET valueList RBRACKET # expression_in_bracket_list
   |  KW_NOT expression # expression_not
   //|  expression (KW_AND | KW_OR) expression # expression_and_or
   |  expression KW_AND expression # expression_and
   |  expression KW_OR expression # expression_or
   |  primary # expression_primary
      /**************  ERROR RULES  *************/
   |  LPAREN error postExpressionToken # expression_parenthesis_error
   |  expression KW_IN LBRACKET error postExpressionToken # expression_in_error
   |  LPAREN error RPAREN # expression_parenthesis_error_parenthesis
   |  expression KW_NOT KW_IN LBRACKET valueList RBRACKET # expression_not_in_bracket_list_error
   |  expression KW_IN LBRACKET error RBRACKET # expression_in_bracket_error
   ;



postExpressionToken
   :  SEMICOLON
   |  KW_THEN
   |  KW_DO
   |  KW_TO
   |  KW_DOWNTO
   |  TPS_COMMA
   ;



condition
   :  expression
   ;



fieldValueExpression
   :  simpleExpression
   ;



valueList
   :  valueListItem
   |  valueListItem TPS_COMMA valueList
   ;



valueListItem
   :  valueListPrimary
   |  valueListPrimary DOTDOT valueListPrimary
   ;



valueListPrimary
   :  signedLiteral
   |  readableAddress
   |  functionCall
   ;



primary
   :  literal
   |  readableAddress
   |  functionCall
   ;


// functionCall
//    :  testFormIsCall # functionCall_testFormIs
//    |  getTableValueCall # functionCall_getTableValue
//    |  numCopiesCall # functionCall_numCopies
//    |  firstAvailIndexCall # functionCall_firstAvailIndex
//    |  lastIndexCall # functionCall_lastIndex
//    |  numRecsCall # functionCall_numRecs
//    |  formHasUserDataCall # functionCall_formHasUserData
//    |  isUserEnteredCall # functionCall_isUserEntered
//    |  getCCTypeCall # functionCall_getCCType
//    |  isCCTokenizedCall # functionCall_isCCTokenized
//    |  isCCCompleteCall # functionCall_isCCComplete
//    |  getPLDescriptionCall # functionCall_getPLDescription
//    |  isFormPureTransferredCall # functionCall_isFormPureTransferred
//    |  isRowPureTransferredCall # functionCall_isRowPureTransferred
//    |  identifier LPAREN parameterList RPAREN # functionCall_generic
//       /**************  ERROR RULES  *************/
//    |  identifier LPAREN error RPAREN # functionCall_parenthesis_error_parenthesis
//    |  identifier LPAREN error postExpressionToken # functionCall_parenthesis_error
//    ;

functionCall
   :  testFormIsCall
   |  getTableValueCall
   |  numCopiesCall
   |  firstAvailIndexCall
   |  lastIndexCall
   |  numRecsCall
   |  formHasUserDataCall
   |  isUserEnteredCall
   |  getCCTypeCall
   |  isCCTokenizedCall
   |  isCCCompleteCall
   |  getPLDescriptionCall
   |  isFormPureTransferredCall
   |  isRowPureTransferredCall
   |  identifier LPAREN parameterList RPAREN
      /**************  ERROR RULES  *************/
   |  identifier LPAREN error RPAREN
   |  identifier LPAREN error postExpressionToken
   ;



isNonFinalCall
   :  KW_ISNONFINAL LPAREN identifier RPAREN
   ;


isNonFinalErrorCall
   :  KW_ISNONFINALERROR LPAREN RPAREN
      /**************  ERROR RULES  *************/
   |  KW_ISNONFINALERROR LPAREN identifier RPAREN
   ;


/************************************************************************
   The TestFormIs function must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form
   address a legal primary in the YACC grammar or all heck will break
   loose.
************************************************************************/

testFormIsCall
   :  KW_TESTFORMIS LPAREN readableFieldAddress RPAREN
   |  KW_TESTFORMIS LPAREN readableFieldAddress TPS_COMMA  formAddress RPAREN
   |  KW_TESTSUPPORTEDFORMIS LPAREN readableFieldAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_TESTFORMIS LPAREN error RPAREN
   |  KW_TESTSUPPORTEDFORMIS LPAREN error RPAREN
   |  KW_TESTFORMIS LPAREN error postExpressionToken
   |  KW_TESTSUPPORTEDFORMIS LPAREN error postExpressionToken
   ;



/*
   The NumCopies Function must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form
   address a legal primary in the YACC grammar or all heck will break
   loose.
*/

numCopiesCall
   :  KW_NUMCOPIES LPAREN formAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_NUMCOPIES LPAREN error RPAREN
   |  KW_NUMCOPIES LPAREN error SEMICOLON
   ;



/*
   The FirstAvailableIndex Function must be part of the grammar to avoid
   nasty reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form address
   a legal primary in the YACC grammar or all heck will break loose.
*/

firstAvailIndexCall
   :  KW_FIRSTAVAILABLEINDEX LPAREN readableFieldAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_FIRSTAVAILABLEINDEX LPAREN error RPAREN
   |  KW_FIRSTAVAILABLEINDEX LPAREN error SEMICOLON
   ;



/************************************************************************
   The LastIndex Function must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form
   address a legal primary in the YACC grammar or all heck will break
   loose.
************************************************************************/

lastIndexCall
   :  KW_LASTINDEX LPAREN readableFieldAddress RPAREN
   |  KW_LASTINDEX LPAREN readableFieldAddress TPS_COMMA formAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_LASTINDEX LPAREN error RPAREN
   |  KW_LASTINDEX LPAREN error SEMICOLON
   ;



/*
   The NumberOfRecords Function must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form address
   a legal primary in the YACC grammar or all heck will break loose.
*/

numRecsCall
   :  KW_NUMBEROFRECORDS LPAREN readableFieldAddress RPAREN
   |  KW_NUMBEROFRECORDS LPAREN readableFieldAddress TPS_COMMA formAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_NUMBEROFRECORDS LPAREN error RPAREN
   |  KW_NUMBEROFRECORDS LPAREN error SEMICOLON
   ;



/*
   The FormHasUserData Function must be part of the grammar to avoid
   nasty reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form address
   a legal primary in the YACC grammar or all heck will break loose.
*/

formHasUserDataCall
   :  KW_FORMHASUSERDATA LPAREN formAddress RPAREN
   |  KW_FORMHASUSERDATA LPAREN RPAREN
      /**************  ERROR RULES  *************/
   |  KW_FORMHASUSERDATA LPAREN error RPAREN
   |  KW_FORMHASUSERDATA LPAREN error SEMICOLON
   ;



/*
   The IsUserEntered Function must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form address
   a legal primary in the YACC grammar or all heck will break loose.
*/

isUserEnteredCall
   :  KW_ISUSERENTERED LPAREN readableAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_ISUSERENTERED LPAREN error RPAREN
   |  KW_ISUSERENTERED LPAREN error SEMICOLON
   ;



/*
   The GetCCTypeCall Function must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form address
   a legal primary in the YACC grammar or all heck will break loose.
*/

getCCTypeCall
   :  KW_GETCCTYPE LPAREN readableAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_GETCCTYPE LPAREN error RPAREN
   |  KW_GETCCTYPE LPAREN error SEMICOLON
   ;



/*
   The IsCCTokenizedCall Function must be part of the grammar to avoid
   nasty reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form address
   a legal primary in the YACC grammar or all heck will break loose.
*/

isCCTokenizedCall
   :  KW_ISCCTOKENIZED LPAREN readableAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_ISCCTOKENIZED LPAREN error RPAREN
   |  KW_ISCCTOKENIZED LPAREN error SEMICOLON
   ;



/*
   The IsCCCompleteCall Function must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.  The
   usual call back objects can be used but we can't make a Form address a
   legal primary in the YACC grammar or all heck will break loose.
*/

isCCCompleteCall
   :  KW_ISCCCOMPLETE LPAREN readableAddress RPAREN
      /**************  ERROR RULES  *************/
   |  KW_ISCCCOMPLETE LPAREN error RPAREN
   |  KW_ISCCCOMPLETE LPAREN error SEMICOLON
   ;



/*
   The GetTableValue function must be part of the grammar to avoid nasty
   reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form address
   a legal primary in the YACC grammar or all heck will break loose.
*/

getTableValueCall
   :  KW_GETTABLEVALUE LPAREN getTableValueParams RPAREN
      /**************  ERROR RULES  *************/
   |  KW_GETTABLEVALUE LPAREN error RPAREN
   |  KW_GETTABLEVALUE LPAREN error postExpressionToken
   ;



getTableValueParams
   :  simpleExpression TPS_COMMA simpleExpression
   |  simpleExpression TPS_COMMA simpleExpression TPS_COMMA identifier
   ;



/*
   The Is*PureTransferredCall functions are part of the grammar to force
   the parameters to be of the correct type. Without this the parameter
   passed to the function was always of the wrong type.
*/

isRowPureTransferredCall
   :  KW_ISROWPURETRANSFERRED LPAREN readableAddress RPAREN
   |  KW_ISROWPURETRANSFERRED LPAREN error RPAREN
   ;



isFormPureTransferredCall
   :  KW_ISFORMPURETRANSFERRED LPAREN isFormPureTransferredParams RPAREN
   |  KW_ISFORMPURETRANSFERRED LPAREN error RPAREN
   ;



isFormPureTransferredParams
   :  formAddress
   |
   ;



/*
   The GetPLDescriptionCall Function must be part of the grammar to avoid
   nasty reduce/reduce conflicts with Form addresses and Field Addresses.
   The usual call back objects can be used but we can't make a Form address
   a legal primary in the YACC grammar or all heck will break loose.
*/

getPLDescriptionCall
   :  KW_GETPLDESCRIPTION LPAREN getPLDescriptionParams RPAREN
      /**************  ERROR RULES  *************/
   |  KW_GETPLDESCRIPTION LPAREN error RPAREN
   |  KW_GETPLDESCRIPTION LPAREN error SEMICOLON
   ;



getPLDescriptionParams
   :  readableAddress
   |  readableAddress TPS_COMMA string
   ;



/************************************************************************
   Addressing
************************************************************************/

readableAddress
   :  readableFieldAddress
   |  formAddress DOT readableFieldAddress
   |  namedFieldAddressType LBRACKET string RBRACKET
   ;



readableFieldAddress
   :  identifier # readableFieldAddress_identifier
   |  identifier LBRACKET simpleExpression RBRACKET # readableFieldAddress_simpleExpression
   |  identifier LBRACKET simpleExpression TPS_COMMA simpleExpression RBRACKET # readableFieldAddress_simpleExpression_simpleExpression
   |  identifier LBRACKET integer DOTDOT integer RBRACKET # readableFieldAddress_integer_to_integer
   |  identifier LBRACKET simpleExpression DOTDOT integer RBRACKET # readableFieldAddress_simpleExpression_to_integer
   |  identifier LBRACKET char DOTDOT char RBRACKET # readableFieldAddress_char_to_char
   |  identifier LBRACKET integer DOTDOT simpleExpression RBRACKET # readableFieldAddress_integer_to_simpleExpression
   |  identifier LBRACKET char DOTDOT simpleExpression RBRACKET # readableFieldAddress_char_to_simpleExpression
   |  identifier LBRACKET simpleExpression DOTDOT char RBRACKET # readableFieldAddress_simpleExpression_to_char
      /**************  ERROR RULES  *************/
   |  identifier LBRACKET error postExpressionToken # readableFieldAddress_bracket_error
   |  identifier LBRACKET error RBRACKET # readableFieldAddress_bracket_error_bracket
   ;



formAddress
   :  formSpecList
   ;


formSpecList
   :  formSpec
   |  formSpec COLON formSpecList
   ;



formSpec
   :  identifier
   |  identifier LBRACKET simpleExpression RBRACKET
      /**************  ERROR RULES  *************/
   |  identifier LBRACKET error postFormSpecToken
   |  identifier LBRACKET error RBRACKET
   ;



postFormSpecToken
   :  postExpressionToken
   |  DOT
   |  COLON
   ;



namedFieldAddressType
   :  KW_EFNV
   |  KW_GENERICNV
   ;



writableAddress
   :  identifier
   |  identifier LBRACKET simpleExpression RBRACKET
   |  identifier LBRACKET simpleExpression TPS_COMMA simpleExpression RBRACKET
      /**************  ERROR RULES  *************/
   |  identifier LBRACKET error postWritableAddressToken
   |  identifier LBRACKET error RBRACKET
   |  formAddress DOT identifier
   |  formAddress DOT identifier LBRACKET simpleExpression RBRACKET
   ;



postWritableAddressToken
   :  ASSIGN
   |  DROPASSIGN
   |  SETDEFAULTVALUE
   |  EQ
   |  TPS_COMMA
   ;



/************************************************************************
   Misc low level aspects of the language
************************************************************************/

literalList
   :  literal
   |  literal TPS_COMMA literalList
   ;



literal
   :  number
   |  KW_TRUE
   |  KW_FALSE
   |  string
   |  char
   |  isNonFinalCall
   |  isNonFinalErrorCall
   ;



signedLiteralList
   :  signedLiteral
   |  signedLiteral TPS_COMMA literalList
   ;



signedLiteral
   :  literal
   |  negativeNumber
   ;



number
   :  integer
   |  real
   ;


// Needs <assoc=right>? Had %prec _UNARY on both
negativeNumber
   :  MINUS integer
   |  MINUS real
   ;



signedNumber
   :  number
   |  negativeNumber
   ;



real
   :  REAL
   ;



integer
   :  INTEGER
   ;



char
   :  CHAR
   ;



string
   :  STRING
   ;

// identifierList
//    :  identifier (TPS_COMMA identifier)*
//    ;

identifierList
   :  identifier
   |  identifier TPS_COMMA identifierList
   ;


// NAME was IDENTIFIER
identifier
   :  NAME
   ;



requiredSemicolon
   :  SEMICOLON
      /**************  ERROR RULES  *************/
   |
   ;


error
   : SEMICOLON SEMICOLON SEMICOLON SEMICOLON SEMICOLON
   ;
