%{
/* This %{ ... %} section is code that is glued into the generated scanner. */
#define YYSTYPE node_t *
#include "tree.h"
#include "parser.h"


extern int outputStage; // This variable is located in vslc.c
    #define RETURN(t) do {                                             \
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

"FUNC"				{ return FUNC; }
"PRINT"				{ return PRINT; }
"RETURN"			{ return RETURN; }
"NEW"				{ return NEW; }
"IF"				{ return IF; }
"THEN"				{ return THEN; }
"ELSE"				{ return ELSE; }
"END"				{ return END; }
"WHILE"				{ return WHILE; }
"DO"				{ return DO; }
"FOR"				{ return FOR; }
"TO"				{ return TO; }
"INT"				{ return INT; }
"BOOL"				{ return BOOL; }
"FLOAT"				{ return FLOAT; }
"CLASS"				{ return CLASS_TOKEN; }
"HAS"				{ return HAS; }
"WITH"				{ return WITH; }
"START"				{ return START; }
"VOID"				{ return VOID; }
"THIS"				{ return THIS; }
"ARRAY"				{ return ARRAY; }

":="				{ return ASSIGN; }
"+"					{ return PLUS; }
"-"					{ return MINUS; }
"*"					{ return MUL; }
"/"					{ return DIV; }
">"					{ return GREATER; }
"<"					{ return LESS; }
"<="				{ return LEQUAL; }
">="				{ return GEQUAL; }
"=="				{ return EQUAL; }
"!="				{ return NEQUAL; }
"&&"				{ return AND; }
"||"				{ return OR; }
"!"					{ return NOT; }
"."					{ return DOT; }

{denary_integer}	{ yylval = (node_t *) malloc(sizeof(node_t)); SetInteger(yylval, yytext); return INT_CONST; }
{denary_float}		{ yylval = (node_t *) malloc(sizeof(node_t)); SetFloat(yylval, yytext); return FLOAT_CONST; }
{bool_true}			{ yylval = (node_t *) malloc(sizeof(node_t)); SetInteger(yylval, yytext); return TRUE_CONST; }
{bool_false}		{ yylval = (node_t *) malloc(sizeof(node_t)); SetInteger(yylval, yytext); return FALSE_CONST; }
{string_literal}	{ yylval = (node_t *) malloc(sizeof(node_t)); SetString(yylval, yytext); return STRING_CONST; }

")"					{ return RPAREN; }
"("					{ return LPAREN; }
","					{ return COMMA; }
";"					{ return SEMICOLON; }
"["					{ return LBRACK; }
"]"					{ return RBRACK; }

{identifier}		 { yylval = (node_t *) malloc(sizeof(node_t)); SetString(yylval, yytext); return IDENTIFIER; }
%%

void scanUntilLinefeed()
{
	while (input() != '\n');
}
