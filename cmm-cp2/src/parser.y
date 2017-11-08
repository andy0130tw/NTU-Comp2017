/* Parser template 2015
===== Definition Section ===== */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "header.h"

int yylex();
int yyerror(char*);

int linenumber = 1;
AST_NODE *prog;

extern int g_anyErrorOccur;

static inline AST_NODE * makeSibling(AST_NODE * a, AST_NODE * b) {
    while (a->rightSibling) {
        a = a->rightSibling;
    }
    if (b == NULL) {
        return a;
    }
    b = b->leftmostSibling;
    a->rightSibling = b;

    b->leftmostSibling = a->leftmostSibling;
    b->parent = a->parent;
    while (b->rightSibling) {
        b = b->rightSibling;
        b->leftmostSibling = a->leftmostSibling;
        b->parent = a->parent;
    }
    return b;
}

static inline AST_NODE * makeChild(AST_NODE * parent, AST_NODE * child) {
    if (child == NULL) {
        return parent;
    }
    if (parent->child) {
        makeSibling(parent->child, child);
    } else {
        child = child->leftmostSibling;
        parent->child = child;
        while (child) {
            child->parent = parent;
            child = child->rightSibling;
        }
    }
    return parent;
}

static AST_NODE * makeFamily(AST_NODE * parent, int childrenCount, ...) {
    va_list childrenList;
    va_start(childrenList, childrenCount);
    AST_NODE* child = va_arg(childrenList, AST_NODE*);
    makeChild(parent, child);
    AST_NODE* tmp = child;
    int index = 1;
    for (index = 1; index < childrenCount; ++index) {
        child = va_arg(childrenList, AST_NODE*);
        tmp = makeSibling(tmp, child);
    }
    va_end(childrenList);
    return parent;
}

static inline AST_NODE * makeIDNode(char *lexeme, IDENTIFIER_KIND idKind) {
    AST_NODE* identifier = Allocate(IDENTIFIER_NODE);
    identifier->semantic_value.identifierSemanticValue.identifierName = lexeme;
    identifier->semantic_value.identifierSemanticValue.kind = idKind;
    identifier->semantic_value.identifierSemanticValue.symbolTableEntry = NULL;
    return identifier;
}

static inline AST_NODE * makeStmtNode(STMT_KIND stmtKind) {
    AST_NODE* stmtNode = Allocate(STMT_NODE);
    stmtNode->semantic_value.stmtSemanticValue.kind = stmtKind;
    return stmtNode;
}

static inline AST_NODE * makeDeclNode(DECL_KIND declKind) {
    AST_NODE* declNode = Allocate(DECLARATION_NODE);
    declNode->semantic_value.declSemanticValue.kind = declKind;
    return declNode;
}

static inline AST_NODE * makeExprNode(EXPR_KIND exprKind, int operationEnumValue) {
    AST_NODE* exprNode = Allocate(EXPR_NODE);
    exprNode->semantic_value.exprSemanticValue.isConstEval = 0;
    exprNode->semantic_value.exprSemanticValue.kind = exprKind;
    if (exprKind == BINARY_OPERATION) {
        exprNode->semantic_value.exprSemanticValue.op.binaryOp = operationEnumValue;
    } else if (exprKind == UNARY_OPERATION) {
        exprNode->semantic_value.exprSemanticValue.op.unaryOp = operationEnumValue;
    } else {
        printf("Error in static inline AST_NODE* makeExprNode(EXPR_KIND exprKind, int operationEnumValue)\n");
    }
    return exprNode;
}

static inline AST_NODE * makeConstValueNode(CON_Type* val) {
    AST_NODE* cvNode = Allocate(CONST_VALUE_NODE);
    cvNode->semantic_value.const1 = val;
    return cvNode;
}

%}



%union{
	char *lexeme;
	CON_Type  *const1;
	AST_NODE  *node;
};

%token <lexeme>ID
%token <const1>CONST
%token VOID
%token INT
%token FLOAT
%token IF
%token ELSE
%token WHILE
%token FOR
%token TYPEDEF
%token OP_ASSIGN
%token OP_OR
%token OP_AND
%token OP_NOT
%token OP_EQ
%token OP_NE
%token OP_GT
%token OP_LT
%token OP_GE
%token OP_LE
%token OP_PLUS
%token OP_MINUS
%token OP_TIMES
%token OP_DIVIDE
%token MK_LB
%token MK_RB
%token MK_LPAREN
%token MK_RPAREN
%token MK_LBRACE
%token MK_RBRACE
%token MK_COMMA
%token MK_SEMICOLON
%token MK_DOT
%token ERROR
%token RETURN

%type <node> program global_decl_list global_decl function_decl block stmt_list decl_list decl var_decl type init_id_list init_id  stmt relop_expr relop_term relop_factor expr term factor unary_op var_ref
%type <node> param_list param dim_fn expr_null id_list dim_decl cexpr mcexpr cfactor assign_expr_list test assign_expr rel_op relop_expr_list nonempty_relop_expr_list
%type <node> add_op mul_op dim_list type_decl nonempty_assign_expr_list

/* to resolve the conflict of dangling else
   ref: https://stackoverflow.com/a/17013386/2281355 */
%nonassoc MK_RPAREN
%nonassoc ELSE

%start program

%%

/* ==== Grammar Section ==== */

/* Productions */               /* Semantic actions */
program		: global_decl_list { $$=Allocate(PROGRAM_NODE);  makeChild($$,$1); prog=$$;}
		| { $$=Allocate(PROGRAM_NODE); prog=$$;}
		;

global_decl_list: global_decl_list global_decl
                    {
                        $$ = makeSibling($1, $2);
                    }
                | global_decl
                    {
                        $$ = $1;
                    }
                ;

global_decl	: decl_list function_decl
                {
                    $$ = makeSibling(makeChild(Allocate(VARIABLE_DECL_LIST_NODE), $1), $2);
                }
            | function_decl
                {
                    $$ = $1;
                }
            ;

function_decl	: type ID MK_LPAREN param_list MK_RPAREN MK_LBRACE block MK_RBRACE
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* parameterList = Allocate(PARAM_LIST_NODE);
                        makeChild(parameterList, $4);
                        makeFamily($$, 4, $1, makeIDNode($2, NORMAL_ID), parameterList, $7);
                    }
                | VOID ID MK_LPAREN param_list MK_RPAREN MK_LBRACE block MK_RBRACE
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* parameterList = Allocate(PARAM_LIST_NODE);
                        makeChild(parameterList, $4);
                        makeFamily($$, 4, makeIDNode("void", NORMAL_ID), makeIDNode($2, NORMAL_ID), parameterList, $7);
                    }
                | type ID MK_LPAREN  MK_RPAREN MK_LBRACE block MK_RBRACE
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* emptyParameterList = Allocate(PARAM_LIST_NODE);
                        makeFamily($$, 4, $1, makeIDNode($2, NORMAL_ID), emptyParameterList, $6);
                    }
                | VOID ID MK_LPAREN  MK_RPAREN MK_LBRACE block MK_RBRACE
                    {
                        $$ = makeDeclNode(FUNCTION_DECL);
                        AST_NODE* emptyParameterList = Allocate(PARAM_LIST_NODE);
                        makeFamily($$, 4, makeIDNode("void", NORMAL_ID), makeIDNode($2, NORMAL_ID), emptyParameterList, $6);
                    }
                ;

param_list	: param_list MK_COMMA  param
                {
                    $$ = makeSibling($1, $3);
                }
            | param
                {
                    $$ = $1;
                }
            ;

param		: type ID
                {
                    $$ = makeDeclNode(FUNCTION_PARAMETER_DECL);
                    makeFamily($$, 2, $1, makeIDNode($2, NORMAL_ID));
                }
            | type ID dim_fn
                {
                    $$ = makeDeclNode(VARIABLE_DECL);
                    AST_NODE* arr_id = makeIDNode($2, ARRAY_ID);
                    makeFamily($$, 2, $1, arr_id);
                    makeChild(arr_id, $3);
                    /*TODO*/
                }
            ;
dim_fn		: MK_LB expr_null MK_RB
                {
                    $$ = $2;
                }
            | dim_fn MK_LB expr MK_RB
                {
                    $$ = makeSibling($1, $3);
                }
		;

// Maybe expr := Just expr | null
expr_null	:expr
                {
                    $$ = $1;
                }
            |
                {
                    /* null? */
                    $$ = Allocate(NUL_NODE);
                }
            ;

block           : decl_list stmt_list
                    {
                        AST_NODE* decls = Allocate(VARIABLE_DECL_LIST_NODE);
                        AST_NODE* stmts = Allocate(STMT_LIST_NODE);
                        makeChild(decls, $1);
                        makeChild(stmts, $2);
                        $$ = makeFamily(Allocate(BLOCK_NODE), 2, decls, stmts);
                    }
                | stmt_list
                    {
                        $$ = Allocate(BLOCK_NODE);
                        makeChild($$, makeChild(Allocate(STMT_LIST_NODE), $1));
                    }
                | decl_list
                    {
                        $$ = Allocate(BLOCK_NODE);
                        makeChild($$, makeChild(Allocate(VARIABLE_DECL_LIST_NODE), $1));
                    }
                |   {
                        /* empty block */
                        $$ = Allocate(BLOCK_NODE);
                    }
                ;

decl_list	: decl_list decl
                {
                    $$ = makeSibling($1, $2);
                }
            | decl
                {
                    $$ = $1;
                }
            ;

decl		: type_decl
                {
                    $$ = $1;
                }
            | var_decl
                {
                    $$ = $1;
                }
            ;

type_decl 	: TYPEDEF type id_list MK_SEMICOLON
                {
                    $$ = makeDeclNode(TYPE_DECL);
                    makeFamily($$, 2, $2, $3);
                }
            | TYPEDEF VOID id_list MK_SEMICOLON
                {
                    AST_NODE* void_type = makeIDNode("void", NORMAL_ID);
                    $$ = makeDeclNode(TYPE_DECL);
                    makeFamily($$, 2, void_type, $3);
                }
            ;

var_decl	: type init_id_list MK_SEMICOLON
                {
                    $$ = makeDeclNode(VARIABLE_DECL);
                    makeFamily($$, 2, $1, $2);
                }
            | ID id_list MK_SEMICOLON
                {
                    $$ = makeDeclNode(VARIABLE_DECL);
                    makeFamily($$, 2, makeIDNode($1, NORMAL_ID), $2);
                }
            ;

type		: INT
                {
                    $$ = makeIDNode("int", NORMAL_ID);
                }
            | FLOAT
                {
                    $$ = makeIDNode("float", NORMAL_ID);
                }
            ;

id_list		: ID
                {
                    $$ = makeIDNode($1, NORMAL_ID);
                }
            | id_list MK_COMMA ID
                {
                    $$ = makeSibling($1, makeIDNode($3, NORMAL_ID));
                }
            | id_list MK_COMMA ID dim_decl
                {
                    /*TODO*/
                    $$ = makeSibling($1, makeChild(makeIDNode($3, ARRAY_ID), $4));
                }
            | ID dim_decl
                {
                    $$ = makeChild(makeIDNode($1, ARRAY_ID), $2);
                }
		;
dim_decl	: MK_LB cexpr MK_RB
                {
                    $$ = $2;
                }
            | dim_decl MK_LB cexpr MK_RB
                {
                    $$ = makeSibling($1, $3);
                }
            ;
cexpr		: cexpr OP_PLUS mcexpr
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_ADD);
                    makeFamily($$, 2, $1, $3);
                } /* This is for array declarations */
            | cexpr OP_MINUS mcexpr
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_SUB);
                    makeFamily($$, 2, $1, $3);
                }
            | mcexpr
                {
                    $$ = $1;
                }
            ;
mcexpr		: mcexpr OP_TIMES cfactor
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_MUL);
                    makeFamily($$, 2, $1, $3);
                }
            | mcexpr OP_DIVIDE cfactor
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_DIV);
                    makeFamily($$, 2, $1, $3);
                }
            | cfactor
                {
                    $$ = $1;
                }
            ;

cfactor:	CONST
                {
                    $$ = makeConstValueNode($1);
                }
            | MK_LPAREN cexpr MK_RPAREN
                {
                    $$ = $2;
                }
            ;

init_id_list	: init_id
                    {
                        $$ = $1;
                    }
                | init_id_list MK_COMMA init_id
                    {
                        $$ = makeSibling($1, $3);
                    }
                ;

init_id		: ID
                {
                    $$ = makeIDNode($1, NORMAL_ID);
                }
            | ID dim_decl
                {
                    $$ = makeChild(makeIDNode($1, ARRAY_ID), $2);
                }
            | ID OP_ASSIGN relop_expr
                {
                    $$ = makeChild(makeIDNode($1, WITH_INIT_ID), $3);

                }
            ;

stmt_list	: stmt_list stmt
                {
                    $$ = makeSibling($1, $2);
                }
            | stmt
                {
                    $$ = $1;
                }
            ;



stmt		: MK_LBRACE block MK_RBRACE
                {
                    $$ = $2;
                }
            | WHILE MK_LPAREN test MK_RPAREN stmt
                {
                    /* while statement */
                    $$ = makeFamily(makeStmtNode(WHILE_STMT), 2, $3, $5);
                }
            | FOR MK_LPAREN assign_expr_list MK_SEMICOLON relop_expr_list MK_SEMICOLON assign_expr_list MK_RPAREN stmt
                {
                    /* for statement */
                    $$ = makeFamily(makeStmtNode(FOR_STMT), 4, $3, $5, $7, $9);
                }
            | var_ref OP_ASSIGN relop_expr MK_SEMICOLON
                {
                    $$ = makeFamily(makeStmtNode(ASSIGN_STMT), 2, $1, $3);
                }
            | IF MK_LPAREN test MK_RPAREN stmt
                {
                    /* if statement */
                    $$ = makeFamily(makeStmtNode(IF_STMT), 3, $3, $5, Allocate(NUL_NODE));
                }
            | IF MK_LPAREN test MK_RPAREN stmt ELSE stmt
                {
                    /* if-else statement */
                    $$ = makeFamily(makeStmtNode(IF_STMT), 3, $3, $5, $7);
                }
            | ID MK_LPAREN relop_expr_list MK_RPAREN MK_SEMICOLON
                {
                    /* function call */
                    AST_NODE* id = makeIDNode($1, NORMAL_ID);
                    $$ = makeFamily(makeStmtNode(FUNCTION_CALL_STMT), 2, id, $3);
                }
            | MK_SEMICOLON
                {
                    // not sure if this is correct
                    $$ = Allocate(NUL_NODE);
                }
            | RETURN MK_SEMICOLON
                {
                    // not sure if this is correct; not covered in any example
                    $$ = makeChild(makeStmtNode(RETURN_STMT), Allocate(NUL_NODE));
                }
            | RETURN relop_expr MK_SEMICOLON
                {
                    $$ = makeChild(makeStmtNode(RETURN_STMT), $2);
                }
            ;

assign_expr_list : nonempty_assign_expr_list
                     {
                        $$ = makeChild(Allocate(NONEMPTY_ASSIGN_EXPR_LIST_NODE), $1);
                     }
                 |
                     {
                         $$ = Allocate(NUL_NODE);
                     }
                 ;

nonempty_assign_expr_list        : nonempty_assign_expr_list MK_COMMA assign_expr
                                    {
                                        $$ = makeSibling($1, $3);
                                    }
                                 | assign_expr
                                    {
                                        $$ = $1;
                                    }
                                 ;

test		: assign_expr
                {
                    $$ = $1;
                }
            ;

assign_expr     : ID OP_ASSIGN relop_expr
                    {
                        // the type is wrong, though
                        AST_NODE* assignStmt = makeStmtNode(ASSIGN_STMT);
                        $$ = makeFamily(assignStmt, 2, makeIDNode($1, NORMAL_ID), $3);
                    }
                | relop_expr
                    {
                        $$ = $1;
                    }
		;

relop_expr	: relop_term
                {
                    $$ = $1;
                }
            | relop_expr OP_OR relop_term
                {
                   AST_NODE* rel = makeExprNode(BINARY_OPERATION, BINARY_OP_OR);
                   $$ = makeFamily(rel, 2, $1, $3);
                }
            ;

relop_term	: relop_factor
                {
                    $$ = $1;
                }
            | relop_term OP_AND relop_factor
                {
                    AST_NODE* rel = makeExprNode(BINARY_OPERATION, BINARY_OP_AND);
                    $$ = makeFamily(rel, 2, $1, $3);
                }
            ;

relop_factor	: expr
                    {
                        $$ = $1;
                    }
                | expr rel_op expr
                    {
                        $$ = makeFamily($2, 2, $1, $3);
                    }
                ;

rel_op		: OP_EQ
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_EQ);
                }
            | OP_GE
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_GE);
                }
            | OP_LE
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_LE);
                }
            | OP_NE
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_NE);
                }
            | OP_GT
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_GT);
                }
            | OP_LT
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_LT);
                }
            ;


relop_expr_list	: nonempty_relop_expr_list
                    {
                        $$ = makeChild(Allocate(NONEMPTY_RELOP_EXPR_LIST_NODE), $1);
                    }
                |
                    {
                        $$ = Allocate(NUL_NODE);
                    }
                ;

nonempty_relop_expr_list	: nonempty_relop_expr_list MK_COMMA relop_expr
                                {
                                    $$ = makeSibling($1, $3);
                                }
                            | relop_expr
                                {
                                    $$ = $1;
                                }
                            ;

expr		: expr add_op term
                {
                    $$ = makeFamily($2, 2, $1, $3);
                }
            | term
                {
                    $$ = $1;
                }
            ;

add_op		: OP_PLUS
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_ADD);
                }
            | OP_MINUS
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_SUB);
                }
            ;

term		: term mul_op factor
                {
                    $$ = makeFamily($2, 2, $1, $3);
                }
            | factor
                {
                    $$ = $1;
                }
            ;

mul_op		: OP_TIMES
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_MUL);
                }
            | OP_DIVIDE
                {
                    $$ = makeExprNode(BINARY_OPERATION, BINARY_OP_DIV);
                }
            ;

factor		: MK_LPAREN relop_expr MK_RPAREN
                {
                    $$ = $2;
                }
            | unary_op MK_LPAREN relop_expr MK_RPAREN
                {
                    $$ = makeChild($1, $3);
                }
            | CONST
                {
                    $$ = makeConstValueNode($1);
                }
            | unary_op CONST
                {
                    $$ = makeChild($1, makeConstValueNode($2));
                }
            | ID MK_LPAREN relop_expr_list MK_RPAREN
                {
                    AST_NODE* expr = makeStmtNode(FUNCTION_CALL_STMT);
                    $$ = makeFamily(expr, 2, makeIDNode($1, NORMAL_ID), $3);
                }
            | unary_op ID MK_LPAREN relop_expr_list MK_RPAREN
                {
                    AST_NODE* expr = makeStmtNode(FUNCTION_CALL_STMT);
                    makeFamily(expr, 2, makeIDNode($2, NORMAL_ID), $4);
                    $$ = makeChild($1, expr);
                }
            | var_ref
                {
                    $$ = $1;
                }
            | unary_op var_ref
                {
                    $$ = makeChild($1, $2);
                }
            ;

unary_op    : OP_NOT
                {
                    $$ = makeExprNode(UNARY_OPERATION, UNARY_OP_LOGICAL_NEGATION);
                }
            | OP_MINUS
                {
                    $$ = makeExprNode(UNARY_OPERATION, UNARY_OP_NEGATIVE);
                }
            ;
            /* XXX: how about OP_PLUS, e.g. `+(4)`? */

var_ref		: ID
                {
                    $$ = makeIDNode($1, NORMAL_ID);
                }
            | ID dim_list
                {
                    $$ = makeChild(makeIDNode($1, ARRAY_ID), $2);
                }
            ;


dim_list	: dim_list MK_LB expr MK_RB
                {
                    $$ = makeSibling($1, $3);
                }
            | MK_LB expr MK_RB
                {
                    $$ = $2;
                }
		;


%%

#include "lex.yy.c"

void printGV(AST_NODE*, char*);

int main(int argc, char* argv[]) {
    yyin = fopen(argv[1], "r");
    yyparse();
    printf("%s\n", "Parsing completed. No errors found.");
    printGV(prog, NULL);
}

int yyerror(char* mesg) {
    printf("%s\t%d\t%s\t%s\n", "Error found in Line ", linenumber, "next token: ", yytext);
    exit(1);
}
