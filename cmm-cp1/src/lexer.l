%option noyywrap

%{
#include <stdio.h>
#include "header.h"
int linenumber;
symtab * lookup();
symtab * ptr;
void     insertID();
void     printSymTab();
void     printSymFreq();

// #define LOG(...)  printf(__VA_ARGS__)
#define LOG(...)

#define INT         90
#define FLOAT       91
#define STRING      92
#define OP_ASSIGN   12
#define OP_OR       13
#define OP_AND      14
#define OP_NOT      15
#define OP_EQ       16
#define OP_NE       17
#define OP_GT       18
#define OP_LT       19
#define OP_GE       20
#define OP_LE       21
#define OP_PLUS     22
#define OP_MINUS    23
#define OP_MUL      24
#define OP_DIV      25
#define MK_LPAREN   26
#define MK_RPAREN   27
#define ERROR       100

%}

%s comment

letter      [A-Za-z]
digit       [0-9]
ID          {letter}({letter}|{digit}|"_")*
WS          [ \t]+

frac        ({digit}*\.{digit}+)|({digit}+\.)
expo        ([Ee][+-]?{digit}+)

LitInt      {digit}+
LitFloat    ({frac}{expo}?|{digit}+{expo})
LitString   \"[^\"\n]*\"

OP_assign   "="
OP_or       "||"
OP_and      "&&"
OP_not      "!"
OP_eq       "=="
OP_ne       "!="
OP_gt       ">"
OP_lt       "<"
OP_ge       ">="
OP_le       "<="

OP_plus     "+"
OP_minus    "-"
OP_mul      "*"
OP_div      "/"

newline     "\n"

DL_lparen   "("
DL_rparen   ")"
DL_lbrckt   "["
DL_rbrckt   "]"
DL_lbrace   "{"
DL_rbrace   "}"
DL_comma    ","
DL_semicol  ";"
DL_dot      "."

error .

%%

<INITIAL>{
  "/*"[^*]*"*"+ { yymore(); BEGIN(comment); }
}

<comment>{
  [^/\n*][^\n*]*"*"+  { yymore(); }
  {newline}  { yymore(); linenumber++; }
  "/"        { printf("%s\n", yytext); BEGIN(INITIAL); }
  <<EOF>>    { printf("ERR: unterminated comment\n"); exit(1); }
}

"return"   {}
"typedef"  {}
"if"       {}
"else"     {}
"int"      { LOG("LITER  %s\n", yytext); }
"float"    { LOG("LITER  %s\n", yytext); }
"for"      {}
"void"     {}
"while"    {}

{WS}            {}
{LitInt}        { LOG("INT    %s\n", yytext); /*return INT;*/ }
{LitFloat}      { LOG("FLOAT  %s\n", yytext); /*return FLOAT;*/ }
{LitString}     { LOG("STRING %s\n", yytext); /*return STRING;*/ }
{ID}            {
        LOG("IDENT  %s\n", yytext);
        ptr = lookup(yytext);
        if (ptr == NULL) {
            insertID(yytext);
        } else {
            ptr->counter++;
        }
}
{}

{OP_assign}     { LOG("ASSIGN %s\n", yytext); /* return OP_ASSIGN; */ }
{OP_or}         /* return OP_OR; */
{OP_plus}       /* return OP_PLUS; */
{OP_minus}      /* return OP_MINUS; */
{OP_mul}        { LOG("MUL    %s\n", yytext); /* return OP_MUL; */ }
{OP_div}        { LOG("DIV    %s\n", yytext); /* return OP_DIV; */ }
{DL_lparen}     /* return MK_LPAREN; */
{DL_rparen}     /* return MK_RPAREN; */
{DL_lbrckt}     /* return MK_LBRCKT; */
{DL_rbrckt}     /* return MK_RBRCKT; */
{DL_lbrace}     /* return MK_LBRACE; */
{DL_rbrace}     /* return MK_RBRACE; */
{DL_comma}      {}
{DL_semicol}    {}
{DL_dot}        {}

{newline}       { linenumber++; }
{error}         {
        printf("ERR %s\n", yytext);
        return ERROR;
}

%%

int main(int argc, char **argv) {
    argc--; ++argv;
    if (argc > 0)
        yyin = fopen(argv[0], "r");
    else
        yyin = stdin;
    int status = yylex();
    printSymFreq();
}
