%{
#include "nodetypes.h"
#include "tree.h"
#include <stdlib.h>

// This defines the type for every $$ value in the productions.
#define YYSTYPE node_t *

#define YYDEBUG 1

// This variable is located in vslc.c
extern int outputStage;

/*
 * Variables connecting the parser to the state of the scanner - defs. will be
 * generated as part of the scanner (lexical analyzer).
 */
extern char yytext[];
extern int yylineno;

/*
 * Wrapper functions for node_init. The main purpose of calling these functions
 * instead of node_init directly is to enable the debug output, as well as a reduction
 * of typing. These functions are named CN for "create
 * node", and L, T, or E if they take an additional label, type or expression_type argument
 * in addition. When the label, type or expression_type is not supplied, node_init is called with
 * default values.
 */
 
node_t* CN(nodetype_t type, int n_children, ...){
	// outputStage = 2;
	if( outputStage == 2 ) printf( "Hit rule \"%s\" on text '%s' at line %d\n", type.text , yytext, yylineno );
	va_list child_list;
	va_start(child_list, n_children);
	// printf("\nNumber of children put in: %d\n", n_children);
	node_t* to_return = node_init(type, NULL, NO_TYPE, default_e, n_children, child_list);
	va_end(child_list);
	// printf("ENDED CN\n");
	return to_return;
}

node_t* CNL(nodetype_t type, char* label, int n_children, ...){
	if( outputStage == 2 ) printf( "Hit rule \"%s\" on text '%s' at line %d\n", type.text , yytext, yylineno );
	va_list child_list;
	va_start(child_list, n_children);
	node_t* to_return = node_init(type, label, NO_TYPE, default_e, n_children, child_list);
	va_end(child_list);
	return to_return;
}

node_t* CNT(nodetype_t type, base_data_type_t base_type, int n_children, ...){
	if( outputStage == 2 ) printf( "Hit rule \"%s\" on text '%s' at line %d\n", type.text , yytext, yylineno );
	va_list child_list;
	va_start(child_list, n_children);
	node_t* to_return = node_init(type, NULL, base_type, default_e, n_children, child_list);
	va_end(child_list);
	return to_return;
}

node_t* CNE(nodetype_t type, expression_type_t expression_type, int n_children, ...){
	// printf("inside CNE\n");
	if( outputStage == 2 ) printf( "Hit rule \"%s\" on text '%s' at line %d\n", type.text , yytext, yylineno );
	// printf("after the fact\n");
	va_list child_list;
	va_start(child_list, n_children);
	node_t* to_return = node_init(type, NULL, NO_TYPE, expression_type, n_children, child_list);
	va_end(child_list);
	return to_return;
}

expression_type_t getExpressionType(et_number index)
{
	expression_type_t expr_type;
	expr_type.index = index;
	expr_type.text = NULL;
	return expr_type;
}

nodetype_t getNodeType(nt_number index, char *text)
{
	nodetype_t nodetype;
	nodetype.index = index;
	nodetype.text = text;
	return nodetype;
}

// Helper for setting the value of an Integer node
static void SetInteger(node_t* node, char *string)
{
	node->int_const = atol ( string );
	node->data_type.base_type= INT_TYPE;
}

// Helper for setting the value of an float node
static void SetFloat(node_t* node, char *string)
{
	node->float_const = atof ( string );
	node->data_type.base_type= FLOAT_TYPE;
}


// Helper for setting the value of an string node
static void SetString(node_t* node, char *string)
{
	node->string_const = STRDUP( string );
	node->data_type.base_type= STRING_TYPE;
}

/*
 * Since the return value of yyparse is an integer (as defined by yacc/bison),
 * we need the top level production to finalize parsing by setting the root
 * node of the entire syntax tree inside its semantic rule instead. This global
 * variable will let us get a hold of the tree root after it has been
 * generated.
 */
node_t *root;


/*
 * These functions are referenced by the generated parser before their
 * definition. Prototyping them saves us a couple of warnings during build.
 */
int yyerror ( const char *error );  /* Defined below */
int yylex ( void );                 /* Defined in the generated scanner */
%}


/* Tokens for all the key words in VSL */
%token INT_CONST FLOAT_CONST TRUE_CONST FALSE_CONST STRING_CONST STRING
%token INT FLOAT BOOL VOID  IDENTIFIER
%token ASSIGN FUNC START PRINT RETURN IF THEN ELSE END WHILE DO
%token EQUAL GEQUAL LEQUAL NEQUAL AND OR  
%token  NEW
%token ARRAY
%token FOR TO
%token '(' ')' ';' ','

/*
 * Operator precedences: 
 * + and - bind to the left { a+b+c = (a+b)+c }
 * * and / bind left like + and -, but has higher precedence
 * Unary minus has only one operand (and thus no direction), but highest
 * precedence. Since we've already used '-' for the binary minus, unary minus
 * needs a ref. name and explicit setting of precedence in its grammar
 * production: " '-' expression %prec UMINUS "
 */
%nonassoc ARRAY
%nonassoc ']'

%left OR
%left AND
%left EQUAL NEQUAL
%left GEQUAL LEQUAL '<' '>'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS '!'
%left '[' '.'

/*
 * The grammar productions follow below. These are mostly a straightforward
 * statement of the language grammar, with semantic rules building a tree data
 * structure which we can traverse in subsequent phases in order to understand
 * the parsed program. (The leaf nodes at the bottom need somewhat more
 * specific rules, but these should be manageable.)
 * A lot of the work to be done later could be handled here instead (reducing
 * the number of passes over the syntax tree), but sticking to a parser which
 * only generates a tree makes it easier to rule it out as an error source in
 * later debugging.
 */ 

%%

program : 
	function_list		{ $$ = CN(getNodeType(PROGRAM, "PROGRAM"), 1, $1); root = $$; }
;

function : 
	type FUNC variable '(' parameter_list ')' START statement_list END		{ /* printf("ok"); */ $$ = CN(getNodeType(FUNCTION, "FUNCTION"), 4, $1, $3, $5, $8); } 
;

function_list :
	function_list function		{ /* printf("ok"); */ $$ = CN(getNodeType(FUNCTION_LIST, "FUNCTION_LIST"), 2, $1, $2); }
	|							{ /* printf("ok"); */ $$ = CN(getNodeType(FUNCTION_LIST, "FUNCTION_LIST"), 0); }
;

statement_list :
	statement					{ $$ = CN(getNodeType(STATEMENT_LIST, "STATEMENT_LIST"), 1, $1); }
	| statement_list statement	{ $$ = CN(getNodeType(STATEMENT_LIST, "STATEMENT_LIST"), 2, $1, $2); }
;

variable_list :
	declaration_statement		{ $$ = CN(getNodeType(VARIABLE_LIST, "VARIABLE_LIST"), 1, $1); }
	| variable_list ',' declaration_statement		{ $$ = CN(getNodeType(VARIABLE_LIST, "VARIABLE_LIST"), 2, $1, $3); }
;

expression_list :
	expression			{ $$ = CN(getNodeType(EXPRESSION_LIST, "EXPRESSION_LIST"), 1, $1); }
	| expression_list ',' expression	{ $$ = CN(getNodeType(EXPRESSION_LIST, "EXPRESSION_LIST"), 2, $1, $3); }
;

parameter_list :
	variable_list		{ $$ = CN(getNodeType(PARAMETER_LIST, "PARAMETER_LIST"), 1, $1); }
	|					{ $$ = CN(getNodeType(PARAMETER_LIST, "PARAMETER_LIST"), 0); }
;

argument_list :
	expression_list		{ $$ = CN(getNodeType(ARGUMENT_LIST, "ARGUMENT_LIST"), 1, $1); }
	|					{ $$ = CN(getNodeType(ARGUMENT_LIST, "ARGUMENT_LIST"), 0); }
;

statement :
	declaration_statement ';'		{ $$ = CN(getNodeType(STATEMENT, "STATEMENT"), 1, $1); }
	| assignment_statement ';'	{ $$ = CN(getNodeType(STATEMENT, "STATEMENT"), 1, $1); }
	| if_statement		{ $$ = CN(getNodeType(STATEMENT, "STATEMENT"), 1, $1); }
	| while_statement		{ $$ = CN(getNodeType(STATEMENT, "STATEMENT"), 1, $1); }
	| for_statement		{ $$ = CN(getNodeType(STATEMENT, "STATEMENT"), 1, $1); }
	| print_statement ';'		{ $$ = CN(getNodeType(STATEMENT, "STATEMENT"), 1, $1); }
	| return_statement ';'		{ $$ = CN(getNodeType(STATEMENT, "STATEMENT"), 1, $1); }
	| call ';'		{ $$ = CN(getNodeType(STATEMENT, "STATEMENT"), 1, $1); }
;

declaration_statement :
	type variable			{ $$ = CN(getNodeType(DECLARATION_STATEMENT, "DECLARATION_STATEMENT"), 2, $1, $2); }
;

assignment_statement :
	lvalue ASSIGN expression		{ /* printf("just before assignment statement\n"); */ $$ = CN(getNodeType(ASSIGNMENT_STATEMENT, "ASSIGNMENT_STATEMENT"), 2, $1, $3); /* printf("after assignment statement\n"); */ }
;

if_statement :
	IF expression THEN statement_list END		{ $$ = CN(getNodeType(IF_STATEMENT, "IF_STATEMENT"), 2, $2, $4); }
	| IF expression THEN statement_list ELSE statement_list END		{ $$ = CN(getNodeType(IF_STATEMENT, "IF_STATEMENT"), 3, $2, $4, $6); }
;

while_statement :
	WHILE expression DO statement_list END		{ $$ = CN(getNodeType(WHILE_STATEMENT, "WHILE_STATEMENT"), 2, $2, $4); }
;

for_statement :
	FOR assignment_statement TO expression DO statement_list END	{ $$ = CN(getNodeType(FOR_STATEMENT, "FOR_STATEMENT"), 3, $2, $4, $6); }
;
return_statement :
	RETURN expression		{ $$ = CN(getNodeType(RETURN_STATEMENT, "RETURN_STATEMENT"), 1, $2); }
;

print_statement : 
	PRINT expression_list		{ $$ = CN(getNodeType(PRINT_STATEMENT, "PRINT_STATEMENT"), 1, $2); }
;

expression :
	constant							{ /* printf("constant encounter %x\n", $1); */ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(CONSTANT_E), 1, $1); }
	| expression '+' expression		{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(ADD_E), 2, $1, $3); }
	| expression '-' expression		{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(SUB_E), 2, $1, $3); }	
	| expression '*' expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(MUL_E), 2, $1, $3); }	
	| expression '/' expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(DIV_E), 2, $1, $3); }
	| expression '>' expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(GREATER_E), 2, $1, $3); }
	| expression '<' expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(LESS_E), 2, $1, $3); }
	| expression EQUAL expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(EQUAL_E), 2, $1, $3); }
	| expression NEQUAL expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(NEQUAL_E), 2, $1, $3); }
	| expression GEQUAL expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(GEQUAL_E), 2, $1, $3); }
	| expression LEQUAL expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(LEQUAL_E), 2, $1, $3); }
	| expression AND expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(AND_E), 2, $1, $3); }
	| expression OR expression			{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(OR_E), 2, $1, $3); }
	| '-' expression %prec UMINUS
	| NEW type							{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(NEW_E), 1, $2); }
	| '(' expression ')'			{ $$= CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(DEFAULT_E), 1, $2); }
	| call								{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(FUNC_CALL_E), 1, $1); }
	| lvalue							{ $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(VARIABLE_E), 1, $1); }
;

call :
	variable '(' argument_list ')'  { $$ = CNE(getNodeType(EXPRESSION, "EXPRESSION"), getExpressionType(FUNC_CALL_E), 2, $1, $3); }
;

lvalue :
	variable			{ /* printf("lvalue, variable\n"); */ $$ = CN(getNodeType(VARIABLE, "VARIABLE"), 1, $1); }
	| expression '[' expression ']'		{ $$ = CN(getNodeType(VARIABLE, "VARIABLE"), 2, $1, $3); }
;

constant :
	TRUE_CONST			{ $$ = CN(getNodeType(CONSTANT, "CONSTANT"), 1, $1); }
	| FALSE_CONST			{ $$ = CN(getNodeType(CONSTANT, "CONSTANT"), 1, $1); }
	| INT_CONST			{ $$ = CN(getNodeType(CONSTANT, "CONSTANT"), 1, $1); }
	| FLOAT_CONST			{ $$ = CN(getNodeType(CONSTANT, "CONSTANT"), 1, $1); }
	| STRING_CONST			{ $$ = CN(getNodeType(CONSTANT, "CONSTANT"), 1, $1); }
;

type :
	INT			{ $$ = CN(getNodeType(TYPE, "TYPE"), 1, $1); }
	| FLOAT			{ $$ = CN(getNodeType(TYPE, "TYPE"), 1, $1); }
	| BOOL			{ $$ = CN(getNodeType(TYPE, "TYPE"), 1, $1); }
	| VOID			{ $$ = CN(getNodeType(TYPE, "TYPE"), 1, $1); }
	| type ARRAY index_list			{ $$ = CN(getNodeType(TYPE, "TYPE"), 2, $1, $3); }
;

index_list :
	index_list '[' index ']'			{ $$ = CN(getNodeType(INDEX_LIST, "INDEX_LIST"), 2, $1, $3); }
	| '[' index ']'			{ $$ = CN(getNodeType(INDEX_LIST, "INDEX_LIST"), 1, $2); }
;

index :
	INT_CONST		{ $$ = CN(getNodeType(INDEX, "INDEX"), 1, $1); }
;

variable :
	IDENTIFIER		{  $$ = CN(getNodeType(VARIABLE, "VARIABLE"), 1, $1); }
;

%% 

/*
 * This function is called with an error description when parsing fails.
 * Serious error diagnosis requires a lot of code (and imagination), so in the
 * interest of keeping this project on a manageable scale, we just chuck the
 * message/line number on the error stream and stop dead.
 */
int
yyerror ( const char *error )
{
    fprintf ( stderr, "\tError: %s detected at line %d with yytext: %s\n", error, yylineno, yytext );
    exit ( EXIT_FAILURE );
}
