/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_WORK_PARSER_H_INCLUDED
# define YY_YY_WORK_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    INT_CONST = 258,
    FLOAT_CONST = 259,
    TRUE_CONST = 260,
    FALSE_CONST = 261,
    STRING_CONST = 262,
    STRING = 263,
    INT = 264,
    FLOAT = 265,
    BOOL = 266,
    VOID = 267,
    IDENTIFIER = 268,
    ASSIGN = 269,
    FUNC = 270,
    START = 271,
    PRINT = 272,
    RETURN = 273,
    IF = 274,
    THEN = 275,
    ELSE = 276,
    END = 277,
    WHILE = 278,
    DO = 279,
    EQUAL = 280,
    GEQUAL = 281,
    LEQUAL = 282,
    NEQUAL = 283,
    AND = 284,
    OR = 285,
    NEW = 286,
    ARRAY = 287,
    FOR = 288,
    TO = 289,
    CLASS_TOKEN = 290,
    HAS = 291,
    WITH = 292,
    THIS = 293,
    LPAREN = 294,
    RPAREN = 295,
    SEMICOLON = 296,
    COMMA = 297,
    RBRACK = 298,
    LESS = 299,
    GREATER = 300,
    PLUS = 301,
    MINUS = 302,
    MUL = 303,
    DIV = 304,
    UMINUS = 305,
    NOT = 306,
    LBRACK = 307,
    DOT = 308
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_WORK_PARSER_H_INCLUDED  */
