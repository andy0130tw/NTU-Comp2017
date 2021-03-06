#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "header.h"

#define tsSyntaxError(ts, str, ...)  \
    printf("Line %d, col %d: Syntax error: " str , (ts)->line + 1, (ts)->col + 1, ##__VA_ARGS__);

int main(int argc, char *argv[]) {
    FILE *source, *target;
    Program program;
    SymbolTable symtab;

    if (argc == 3) {
        source = fopen(argv[1], "r");
        target = fopen(argv[2], "w");
        if (!source) {
            printf("can't open the source file\n");
            exit(2);
        } else if (!target) {
            printf("can't open the target file\n");
            exit(2);
        } else {
            TokenStream source_tok = { .stream = source };
            program = parser(&source_tok);
            fclose(source);
            symtab = build(program);
            check(&program, &symtab);
            gencode(program, target);
        }
    } else {
        printf("Usage: %s source_file target_file\n", argv[0]);
    }


    return 0;
}


/*********************************************
  Scanning
 *********************************************/
char tsGetchar(TokenStream *source) {
    char c = fgetc(source->stream);
    if (c == '\n') {
        source->line++, source->col = 0;
    } else {
        source->col++;
    }
    return c;
}

char tsUngetc(char c, TokenStream *source) {
    char ret = ungetc(c, source->stream);
    if (c == '\n') {
        // cannot recover colNo if going back to previous line
        source->line--, source->col = -1;
    } else {
        source->col--;
    }
    return ret;
}

Token getNumericToken(TokenStream *source, char c) {
    Token token;
    int i = 0;

    while (isdigit(c)) {
        token.tok[i++] = c;
        c = tsGetchar(source);
    }

    if (c != '.') {
        tsUngetc(c, source);
        token.tok[i] = '\0';
        token.type = IntValue;
        return token;
    }

    token.tok[i++] = '.';

    c = tsGetchar(source);
    if (!isdigit(c)) {
        tsUngetc(c, source);
        tsSyntaxError(source, "Expect a digit, got [%c]\n", c);
        exit(1);
    }

    while (isdigit(c)) {
        token.tok[i++] = c;
        c = tsGetchar(source);
    }

    tsUngetc(c, source);
    token.tok[i] = '\0';
    token.type = FloatValue;
    return token;
}

Token scanner(TokenStream *source) {
    char c;
    Token token;

    if (source->peek != NULL) {
        token = *source->peek;
        free(source->peek);
        source->peek = NULL;
        return token;
    }

    while (!feof(source->stream)) {
        c = tsGetchar(source);

        while (isspace(c)) c = tsGetchar(source);

        if (isdigit(c))
            return getNumericToken(source, c);

        if (islower(c)) {
            int len = 0;
            token.tok[len++] = c;
            while (c = tsGetchar(source), islower(c)) {
                token.tok[len++] = c;
                // FIXME: reject if the token is too long
            }
            token.tok[len] = '\0';
            if (c != EOF) {
                tsUngetc(c, source);
            }
            // rewind c to the first character for later use
            // a rather clumsy workaround
            c = token.tok[0];

            if (len == 1) {
                if (c == 'f')
                    token.type = FloatDeclaration;
                else if (c == 'i')
                    token.type = IntegerDeclaration;
                else if (c == 'p')
                    token.type = PrintOp;
                else
                    token.type = Alphabet;
                return token;
            } else if (len > 1) {
                token.type = Alphabet;
                return token;
            }
        }

        switch (c) {
        case '=':
            token.type = AssignmentOp;
            return token;
        case '+':
            token.type = PlusOp;
            return token;
        case '-':
            token.type = MinusOp;
            return token;
        case '*':
            token.type = MulOp;
            return token;
        case '/':
            token.type = DivOp;
            return token;
        case EOF:
            token.type = EOFsymbol;
            token.tok[0] = '\0';
            return token;
        default:
            tsSyntaxError(source, "Invalid character [%c]\n", c);
            exit(1);
        }
    }

    token.tok[0] = '\0';
    token.type = EOFsymbol;
    return token;
}

int retreat(TokenStream *source, Token token) {
    if (source->peek != NULL) {
        printf("Cannot push back token -- slot is not empty\n");
        exit(1);
        return -1;
    }
    Token* tmp = (Token*) malloc(sizeof(Token));
    memcpy(tmp, &token, sizeof(Token));
    source->peek = tmp;
    return 0;
}

/********************************************************
  Parsing
 *********************************************************/
char* copyString(char* str) {
    char* ret = (char*) malloc(strlen(str) + 1);
    strcpy(ret, str);
    return ret;
}

Declaration parseDeclaration(TokenStream *source, Token token) {
    Token token2;
    switch (token.type) {
    case FloatDeclaration:
    case IntegerDeclaration:
        token2 = scanner(source);
        if (token2.type != Alphabet) {
            tsSyntaxError(source, "Expect identifier, got [%s]\n", token2.tok);
            exit(1);
        }
        if (strcmp(token2.tok, "f") == 0 ||
                strcmp(token2.tok, "i") == 0 ||
                strcmp(token2.tok, "p") == 0) {
            tsSyntaxError(source, "[%s] cannot be used as id\n", token2.tok);
            exit(1);
        }
        return makeDeclarationNode(token, token2);
    default:
        tsSyntaxError(source, "Expect Declaration, got [%s]\n", token.tok);
        exit(1);
    }
}

Declarations *parseDeclarations(TokenStream *source) {
    Token token = scanner(source);
    Declaration decl;
    Declarations *decls;
    switch (token.type) {
    case FloatDeclaration:
    case IntegerDeclaration:
        decl = parseDeclaration(source, token);
        decls = parseDeclarations(source);
        return makeDeclarationTree(decl, decls);
    case PrintOp:
    case Alphabet:
        retreat(source, token);
        return NULL;
    case EOFsymbol:
        return NULL;
    default:
        tsSyntaxError(source, "Expect declarations, got [%s]\n", token.tok);
        exit(1);
    }
}

Expression *parseValue(TokenStream *source) {
    Token token = scanner(source);
    Expression *value = (Expression *)malloc(sizeof(Expression));
    value->leftOperand = value->rightOperand = NULL;

    switch (token.type) {
    case Alphabet:
        value->v = (Value) {
            .type = Identifier,
            .val.str = copyString(token.tok)
        };
        break;
    case IntValue:
        value->v = (Value) {
            .type = IntConst,
            .val.ivalue = atoi(token.tok)
        };
        break;
    case FloatValue:
        value->v = (Value) {
            .type = FloatConst,
            .val.fvalue = (float) atof(token.tok)
        };
        break;
    default:
        tsSyntaxError(source, "Expect Identifier or a Number, got [%s]\n", token.tok);
        exit(1);
    }

    return value;
}

/*
Syntax:

Expr : Value PlusOp  Expr
     | Value MinusOp Expr
     | ExprTail
ExprTail : Value MulOp ExprTail
         | Value DivOp ExprTail
         | Value
*/

Expression *parseExpressionTail(TokenStream *source, Expression *lvalue) {
    Token token = scanner(source);
    Expression *expr;

    switch (token.type) {
    case MulOp:
        expr = (Expression *)malloc(sizeof(Expression));
        *expr = (Expression) {
            .v = { .type = MulNode, .val.op = Mul },
            .leftOperand = lvalue,
            .rightOperand = parseValue(source)
        };
        return parseExpressionTail(source, expr);
    case DivOp:
        expr = (Expression *)malloc(sizeof(Expression));
        *expr = (Expression) {
            .v = { .type = DivNode, .val.op = Div },
            .leftOperand = lvalue,
            .rightOperand = parseValue(source)
        };
        return parseExpressionTail(source, expr);
    case PlusOp:
    case MinusOp:
        retreat(source, token);
        return lvalue;
    case Alphabet:
    case PrintOp:
        retreat(source, token);
        return lvalue;
    case EOFsymbol:
        return lvalue;
    default:
        tsSyntaxError(source, "Expect a numeric value or an identifier, got [%s]\n", token.tok);
        exit(1);
    }
}

Expression *parseExpression(TokenStream *source, Expression *lvalue) {
    Token token = scanner(source);
    Expression *expr;
    Expression *rhs;

    switch (token.type) {
    case PlusOp:
        expr = (Expression *)malloc(sizeof(Expression));
        rhs = parseValue(source);
        *expr = (Expression) {
            .v = { .type = PlusNode, .val.op = Plus },
            .leftOperand = lvalue,
            .rightOperand = parseExpressionTail(source, rhs)
        };
        return parseExpression(source, expr);
    case MinusOp:
        expr = (Expression *)malloc(sizeof(Expression));
        rhs = parseValue(source);
        *expr = (Expression) {
            .v = { .type = MinusNode, .val.op = Minus },
            .leftOperand = lvalue,
            .rightOperand = parseExpressionTail(source, rhs)
        };
        return parseExpression(source, expr);
    case MulOp:
    case DivOp:
        retreat(source, token);
        expr = parseExpressionTail(source, lvalue);
        return parseExpression(source, expr);
    case Alphabet:
    case PrintOp:
        retreat(source, token);
        return lvalue;
    case EOFsymbol:
        return lvalue;
    default:
        tsSyntaxError(source, "Expect a numeric value or an identifier, got [%s]\n", token.tok);
        exit(1);
    }
}

Statement parseStatement(TokenStream *source, Token token) {
    Token next_token;
    Expression *value, *expr;

    switch (token.type) {
    case Alphabet:
        next_token = scanner(source);
        if (next_token.type == AssignmentOp) {
            value = parseValue(source);
            expr = parseExpression(source, value);
            return makeAssignmentNode(token.tok, value, expr);
        } else {
            tsSyntaxError(source, "Expect an assignment op, got [%s]\n", next_token.tok);
            exit(1);
        }
    case PrintOp:
        next_token = scanner(source);
        if (next_token.type == Alphabet)
            return makePrintNode(next_token.tok);
        else {
            tsSyntaxError(source, "Expect an identifier, got [%s]\n", next_token.tok);
            exit(1);
        }
        break;
    default:
        tsSyntaxError(source, "Expect a statement, got [%s]\n", token.tok);
        exit(1);
    }
}

Statements *parseStatements(TokenStream *source) {

    Token token = scanner(source);
    Statement stmt;
    Statements *stmts;

    switch (token.type) {
    case Alphabet:
    case PrintOp:
        stmt = parseStatement(source, token);
        stmts = parseStatements(source);
        return makeStatementTree(stmt , stmts);
    case EOFsymbol:
        return NULL;
    default:
        tsSyntaxError(source, "Expect statements, got [%s]\n", token.tok);
        exit(1);
    }
}


/*********************************************************************
  Build AST
 **********************************************************************/
Declaration makeDeclarationNode(Token declare_type, Token identifier) {
    Declaration tree_node;

    switch (declare_type.type) {
    case FloatDeclaration:
        tree_node.var.type = Float;
        break;
    case IntegerDeclaration:
        tree_node.var.type = Int;
        break;
    default:
        break;
    }
    tree_node.var.varname = copyString(identifier.tok);

    return tree_node;
}

Declarations *makeDeclarationTree(Declaration decl, Declarations *decls) {
    Declarations *new_tree = (Declarations *)malloc(sizeof(Declarations));
    new_tree->first = decl;
    new_tree->rest = decls;

    return new_tree;
}


Statement makeAssignmentNode(char* str, Expression *v, Expression *expr_tail) {
    Statement stmt;
    AssignmentStatement assign;

    stmt.type = Assignment;
    assign.str = copyString(str);
    if (expr_tail == NULL)
        assign.expr = v;
    else
        assign.expr = expr_tail;
    stmt.stmt.assign = assign;

    return stmt;
}

Statement makePrintNode(char* str) {
    Statement stmt;
    stmt.type = Print;
    stmt.stmt.str = copyString(str);

    return stmt;
}

Statements *makeStatementTree(Statement stmt, Statements *stmts) {
    Statements *new_tree = (Statements *)malloc(sizeof(Statements));
    new_tree->first = stmt;
    new_tree->rest = stmts;

    return new_tree;
}

/* parser */
Program parser(TokenStream *source) {
    Program program;

    program.declarations = parseDeclarations(source);
    program.statements = parseStatements(source);

    return program;
}


/********************************************************
  Build symbol table
 *********************************************************/
void InitializeTable(SymbolTable *table) {
    int i;

    table->size = 0;
    for (i = 0 ; i < 26; i++)
        table->table[i] = (Variable) {
            .varname = NULL,
            .type = Notype,
        };
}

void add_table(SymbolTable *table, Variable t) {
    int reg = -1;
    for (int i = 0; i < table->size; i++) {
        if (strcmp(table->table[i].varname, t.varname) == 0) {
            printf("Error : variable %s has been declared (aliased to reg '%c')\n",
                   t.varname, i + 'a');
            reg = i;
            break;
        }
    }

    if (reg < 0) {
        reg = table->size++;
    }
    if (reg >= 26) {
        printf("Fatal: Attempt to declare a new variable when the symbol table is exhausted.\n");
        exit(1);
    }
    printf("Declare variable %s as register %c\n", t.varname, reg + 'a');
    table->table[reg] = t;
}

SymbolTable build(Program program) {
    SymbolTable table;
    Declarations *decls = program.declarations;
    Declaration current;

    InitializeTable(&table);

    while (decls != NULL) {
        current = decls->first;
        add_table(&table, current.var);
        decls = decls->rest;
    }

    return table;
}


/********************************************************************
  Type checking
 *********************************************************************/
static inline __attribute((pure)) float coercingToFloat(Value val) {
    switch (val.type) {
    case IntConst:
        return (float) val.val.ivalue;
    case FloatConst:
    default:
        return val.val.fvalue;  // to eliminate warning
    }
}

static inline __attribute((pure)) int evalInt(int a, int b, Operation op) {
    switch (op) {
    case Plus:  return a + b; break;
    case Minus: return a - b; break;
    case Mul:   return a * b; break;
    case Div:   return a / b; break;
    default:    __builtin_unreachable();  // to eliminate warning
    }
}

static inline __attribute((pure)) float evalFloat(float a, float b, Operation op) {
    switch (op) {
    case Plus:  return a + b; break;
    case Minus: return a - b; break;
    case Mul:   return a * b; break;
    case Div:   return a / b; break;
    default:    __builtin_unreachable();  // to eliminate warning
    }
}

void convertType(Expression * old, DataType type) {
    if (old->type == Float && type == Int) {
        printf("error : can't convert float to integer\n");
        return;
    }
    if (old->type == Int && type == Float) {
        Expression *tmp = (Expression *)malloc(sizeof(Expression));
        if (old->v.type == Identifier) {
            printf("convert to float %s \n", old->v.val.str);
            memcpy(tmp, old, sizeof(Expression));

            *old = (Expression) {
                .v = { .type = IntToFloatConvertNode, .val.op = IntToFloatConvert },
                .type = Int,
                .leftOperand = tmp
            };
        } else if (old->v.type == IntConst) {
            // perform constant folding when type coercion is applied on a constant
            float conv = old->v.val.ivalue;
            old->v = (Value) {
                .type = FloatConst,
                .val.fvalue = conv
            };
            printf("constant folding for i2f: %g\n", conv);
        }
    }
}

DataType generalize(Expression *left, Expression *right) {
    if (left->type == Float || right->type == Float) {
        printf("generalize : float\n");
        return Float;
    }
    printf("generalize : int\n");
    return Int;
}

char lookup_id(SymbolTable *table, char *str) {
    char id = '\0';
    for (int i = 0; i < table->size; i++) {
        if (strcmp(table->table[i].varname, str) == 0) {
            id = i + 'a';
            break;
        }
    }
    if (!id) {
        printf("Error : 'variable' %s is not declared\n", str);
    }
    return id;
}

DataType lookup_table(SymbolTable *table, char c) {
    int id = c - 'a';
    if (table->table[id].type != Int && table->table[id].type != Float)
        printf("Error : identifier %c is not declared\n", c);//error
    return table->table[id].type;
}

void substitute_var(char* str, char id) {
    str[0] = id;
    str[1] = '\0';  // for safety (?)
}

void checkexpression(Expression * expr, SymbolTable * table) {
    char id;
    if (expr->leftOperand == NULL && expr->rightOperand == NULL) {
        switch (expr->v.type) {
        case Identifier:
            id = lookup_id(table, expr->v.val.str);
            expr->type = lookup_table(table, id);
            printf("identifier : %s -> %c\n", expr->v.val.str, id);
            substitute_var(expr->v.val.str, id);
            break;
        case IntConst:
            printf("constant : int\n");
            expr->type = Int;
            break;
        case FloatConst:
            printf("constant : float\n");
            expr->type = Float;
            break;
        //case PlusNode: case MinusNode: case MulNode: case DivNode:
        default:
            break;
        }
    } else {
        Expression *left = expr->leftOperand;
        Expression *right = expr->rightOperand;

        checkexpression(left, table);
        checkexpression(right, table);

        DataType type = generalize(left, right);
        convertType(left, type);//left->type = type;//converto
        convertType(right, type);//right->type = type;//converto
        expr->type = type;

        // perform constant folding if both sides are constants
        int isFoldableL = left  && (left->v.type  == IntConst || left->v.type  == FloatConst);
        int isFoldableR = right && (right->v.type == IntConst || right->v.type == FloatConst);

        if (isFoldableL && isFoldableR) {
            //
            Value newValue;
            Operation op = expr->v.val.op;
            switch (type) {
            case Int:
                newValue = (Value) {
                    .type = IntConst,
                    .val.ivalue = evalInt(left->v.val.ivalue, right->v.val.ivalue, op)
                };
                printf("constant folding for int: %d %c %d = %d\n",
                       left->v.val.ivalue, ("+-*/")[op], right->v.val.ivalue, newValue.val.ivalue);
                break;
            case Float:
                newValue = (Value) {
                    .type = FloatConst,
                    .val.fvalue = evalFloat(coercingToFloat(left->v),
                                            coercingToFloat(right->v), op)
                };
                printf("constant folding for float: %g %c %g = %g\n",
                       coercingToFloat(left->v), ("+-*/")[op], coercingToFloat(right->v), newValue.val.fvalue);
                break;
            default:
                printf("Error : Constant folding seems feasible but fails\n");
                break;
            }

            expr->v = newValue;
            expr->leftOperand = expr->rightOperand = NULL;
        }
    }
}

void checkstmt(Statement *stmt, SymbolTable * table) {
    if (stmt->type == Assignment) {
        AssignmentStatement* assign = &stmt->stmt.assign;
        printf("assignment : %s \n", assign->str);
        checkexpression(assign->expr, table);

        char id = lookup_id(table, assign->str);
        substitute_var(assign->str, id);
        assign->type = lookup_table(table, id);
        //  RHS (expr) is float, but       LHS (var) is Int
        if (assign->expr->type == Float && assign->type == Int) {
            printf("error : can't convert float to integer\n");
        } else {
            convertType(assign->expr, assign->type);
        }
    } else if (stmt->type == Print) {
        printf("print : %s \n", stmt->stmt.str);
        char id = lookup_id(table, stmt->stmt.str);
        substitute_var(stmt->stmt.str, id);
    } else printf("error : statement error\n"); //error
}

void check(Program *program, SymbolTable * table) {
    Statements *stmts = program->statements;
    while (stmts != NULL) {
        checkstmt(&stmts->first, table);
        stmts = stmts->rest;
    }
}


/***********************************************************************
  Code generation
 ************************************************************************/
void fprint_op(FILE *target, ValueType op) {
    switch (op) {
    case PlusNode:
        fprintf(target, "+\n");
        break;
    case MinusNode:
        fprintf(target, "-\n");
        break;
    case MulNode:
        fprintf(target, "*\n");
        break;
    case DivNode:
        fprintf(target, "/\n");
        break;
    default:
        fprintf(target, "Error in fprintf_op ValueType = %d\n", op);
        break;
    }
}

void fprint_expr(FILE *target, Expression *expr) {

    if (expr->leftOperand == NULL) {
        switch ((expr->v).type) {
        case Identifier:
            fprintf(target, "l%c\n", (expr->v).val.str[0]);
            break;
        case IntConst:
            fprintf(target, "%d\n", (expr->v).val.ivalue);
            break;
        case FloatConst:
            fprintf(target, "%f\n", (expr->v).val.fvalue);
            break;
        default:
            fprintf(target, "Error In fprint_left_expr. (expr->v).type=%d\n", (expr->v).type);
            break;
        }
    } else {
        fprint_expr(target, expr->leftOperand);
        if (expr->rightOperand == NULL) {
            fprintf(target, "5k\n");
        } else {
            //  fprint_right_expr(expr->rightOperand);
            fprint_expr(target, expr->rightOperand);
            fprint_op(target, (expr->v).type);
        }
    }
}

void gencode(Program prog, FILE * target) {
    Statements *stmts = prog.statements;
    Statement stmt;

    while (stmts != NULL) {
        stmt = stmts->first;
        switch (stmt.type) {
        case Print:
            fprintf(target, "l%c\n", stmt.stmt.str[0]);
            fprintf(target, "p\n");
            break;
        case Assignment:
            // set precision for correct int/float arithmetics
            switch (stmt.stmt.assign.type) {
            case Int:
                fprintf(target,"0 k\n"); break;
            case Float:
                fprintf(target,"5 k\n"); break;
            default: break;
            }
            fprint_expr(target, stmt.stmt.assign.expr);
            fprintf(target, "s%c\n", stmt.stmt.assign.str[0]);
            break;
        }
        stmts = stmts->rest;
    }

}


/***************************************
  For our debug,
  you can omit them.
 ****************************************/
void print_expr(Expression *expr) {
    if (expr == NULL)
        return;
    else {
        print_expr(expr->leftOperand);
        switch ((expr->v).type) {
        case Identifier:
            printf("%c ", (expr->v).val.str[0]);
            break;
        case IntConst:
            printf("%d ", (expr->v).val.ivalue);
            break;
        case FloatConst:
            printf("%f ", (expr->v).val.fvalue);
            break;
        case PlusNode:
            printf("+ ");
            break;
        case MinusNode:
            printf("- ");
            break;
        case MulNode:
            printf("* ");
            break;
        case DivNode:
            printf("/ ");
            break;
        case IntToFloatConvertNode:
            printf("(float) ");
            break;
        default:
            printf("error ");
            break;
        }
        print_expr(expr->rightOperand);
    }
}

void test_parser(TokenStream *source) {
    Declarations *decls;
    Statements *stmts;
    Declaration decl;
    Statement stmt;
    Program program = parser(source);

    decls = program.declarations;

    // since the register is not stored back to the decl,
    // we use a fake counter as a workaround
    char id_fake = 'a';
    while (decls != NULL) {
        decl = decls->first;
        if (decl.var.type == Int)
            printf("i ");
        if (decl.var.type == Float)
            printf("f ");
        printf("%c ", id_fake);
        decls = decls->rest;
        id_fake++;
    }

    stmts = program.statements;

    while (stmts != NULL) {
        stmt = stmts->first;
        if (stmt.type == Print) {
            printf("p %c ", stmt.stmt.str[0]);
        }

        if (stmt.type == Assignment) {
            printf("%c = ", stmt.stmt.assign.str[0]);
            print_expr(stmt.stmt.assign.expr);
        }
        stmts = stmts->rest;
    }

}
