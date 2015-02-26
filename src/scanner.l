%{
/* This %{ ... %} section is code that is glued into the generated scanner. */
#define YYSTYPE node_t *
#include "tree.h"
#include "parser.h"


extern int outputStage; // This variable is located in vslc.c
    #define RETURN(t); do {                                             \
        if ( outputStage == 1 )                                        \
            printf( "TOKEN ( %d,\t'%s' )\n", t, yytext );    \
        return t;                                                      \
    } while ( 0 )

void scanUntilLinefeed();


extern node_t* CNL(nodetype_t type, char* label, int n_children, ...);

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


// Helper for setting the value of an string node
static void SetBool(node_t* node, char *string)
{
	node->string_const = STRDUP( string );
	node->data_type.base_type= BOOL_TYPE;
}


    
%}

%option array
%option noyywrap
%option yylineno

alpha		[A-Za-z]	
alphanumeric	[A-Za-z0-9]	
identifier	({alpha}|_)({alphanumeric}|_)*
whitespace 		[\t\n ]+
denary_digit	[0-9]	
denary_integer	{denary_digit}+
denary_float	{denary_integer}"."{denary_integer}
bool_true		"TRUE"
bool_false		"FALSE"
string_literal	\"(\\.|[^\\"])*\"
comment		"//"
%%

{whitespace}		{}
{comment}			{ scanUntilLinefeed(); }

"FUNC"				{ RETURN(FUNC); }
"PRINT"				{ RETURN(PRINT); }
"RETURN"			{ RETURN(RETURN); }
"NEW"				{ RETURN(NEW); }
"IF"				{ RETURN(IF); }
"THEN"				{ RETURN(THEN); }
"ELSE"				{ RETURN(ELSE); }
"END"				{ RETURN(END); }
"WHILE"				{ RETURN(WHILE); }
"DO"				{ RETURN(DO); }
"FOR"				{ RETURN(FOR); }
"TO"				{ RETURN(TO); }
"INT"				{ RETURN(INT); }
"BOOL"				{ RETURN(BOOL); }
"FLOAT"				{ RETURN(FLOAT); }
"CLASS"				{ RETURN(CLASS_TOKEN); }
"HAS"				{ RETURN(HAS); }
"WITH"				{ RETURN(WITH); }
"START"				{ RETURN(START); }
"VOID"				{ RETURN(VOID); }
"THIS"				{ RETURN(THIS); }
"ARRAY"				{ RETURN(ARRAY); }

":="				{ RETURN(ASSIGN); }
"+"					{ RETURN(PLUS); }
"-"					{ RETURN(MINUS); }
"*"					{ RETURN(MUL); }
"/"					{ RETURN(DIV); }
">"					{ RETURN(GREATER); }
"<"					{ RETURN(LESS); }
"<="				{ RETURN(LEQUAL); }
">="				{ RETURN(GEQUAL); }
"=="				{ RETURN(EQUAL); }
"!="				{ RETURN(NEQUAL); }
"&&"				{ RETURN(AND); }
"||"				{ RETURN(OR); }
"!"					{ RETURN(NOT); }
"."					{ RETURN(DOT); }

{denary_integer}	{ yylval = (node_t *) malloc(sizeof(node_t)); SetInteger(yylval, yytext); RETURN(INT_CONST); }
{denary_float}		{ yylval = (node_t *) malloc(sizeof(node_t)); SetFloat(yylval, yytext); RETURN(FLOAT_CONST); }
{bool_true}			{ yylval = (node_t *) malloc(sizeof(node_t)); SetInteger(yylval, yytext); RETURN(TRUE_CONST); }
{bool_false}		{ yylval = (node_t *) malloc(sizeof(node_t)); SetInteger(yylval, yytext); RETURN(FALSE_CONST); }
{string_literal}	{ yylval = (node_t *) malloc(sizeof(node_t)); SetString(yylval, yytext); RETURN(STRING_CONST); }

")"					{ RETURN(RPAREN); }
"("					{ RETURN(LPAREN); }
","					{ RETURN(COMMA); }
";"					{ RETURN(SEMICOLON); }
"["					{ RETURN(LBRACK); }
"]"					{ RETURN(RBRACK); }

{identifier}		 { yylval = (node_t *) malloc(sizeof(node_t)); SetString(yylval, yytext); RETURN(IDENTIFIER); }
%%

void scanUntilLinefeed()
{
	while (input() != '\n');
}
