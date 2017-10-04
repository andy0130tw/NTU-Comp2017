#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "header.h"


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
Token getNumericToken(FILE *source, char c) {
    Token token;
    int i = 0;

    while (isdigit(c)) {
        token.tok[i++] = c;
        c = fgetc(source);
    }

    if (c != '.') {
        ungetc(c, source);
        token.tok[i] = '\0';
        token.type = IntValue;
        return token;
    }

    token.tok[i++] = '.';

    c = fgetc(source);
    if (!isdigit(c)) {
        ungetc(c, source);
        printf("Expect a digit : %c\n", c);
        exit(1);
    }

    while (isdigit(c)) {
        token.tok[i++] = c;
        c = fgetc(source);
    }

    ungetc(c, source);
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

    FILE * const src_file = source->stream;

    while (!feof(src_file)) {
        c = fgetc(src_file);

        while (isspace(c)) c = fgetc(src_file);

        if (isdigit(c))
            return getNumericToken(src_file, c);

        if (islower(c)) {
            int len = 0;
            token.tok[len++] = c;
            while (c = fgetc(src_file), islower(c)) {
                token.tok[len++] = c;
                // FIXME: reject if the token is too long
            }
            token.tok[len] = '\0';
            if (c != EOF) {
                ungetc(c, src_file);
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
            printf("Invalid character : %c\n", c);
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
        if (strcmp(token2.tok, "f") == 0 ||
                strcmp(token2.tok, "i") == 0 ||
                strcmp(token2.tok, "p") == 0) {
            printf("Syntax Error: %s cannot be used as id\n", token2.tok);
            exit(1);
        }
        return makeDeclarationNode(token, token2);
    default:
        printf("Syntax Error: Expect Declaration %s\n", token.tok);
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
        printf("Syntax Error: Expect declarations %s\n", token.tok);
        exit(1);
    }
}

Expression *parseValue(TokenStream *source) {
    Token token = scanner(source);
    Expression *value = (Expression *)malloc(sizeof(Expression));
    value->leftOperand = value->rightOperand = NULL;

    switch (token.type) {
    case Alphabet:
        (value->v).type = Identifier;
        (value->v).val.str = copyString(token.tok);
        break;
    case IntValue:
        (value->v).type = IntConst;
        (value->v).val.ivalue = atoi(token.tok);
        break;
    case FloatValue:
        (value->v).type = FloatConst;
        (value->v).val.fvalue = atof(token.tok);
        break;
    default:
        printf("Syntax Error: Expect Identifier or a Number %s\n", token.tok);
        exit(1);
    }

    return value;
}

Expression *parseExpressionTail(TokenStream *source, Expression *lvalue) {
    Token token = scanner(source);
    Expression *expr;

    switch (token.type) {
    case PlusOp:
        expr = (Expression *)malloc(sizeof(Expression));
        (expr->v).type = PlusNode;
        (expr->v).val.op = Plus;
        expr->leftOperand = lvalue;
        expr->rightOperand = parseValue(source);
        return parseExpressionTail(source, expr);
    case MinusOp:
        expr = (Expression *)malloc(sizeof(Expression));
        (expr->v).type = MinusNode;
        (expr->v).val.op = Minus;
        expr->leftOperand = lvalue;
        expr->rightOperand = parseValue(source);
        return parseExpressionTail(source, expr);
    // TODO: add MulOp and DivOp
    case Alphabet:
    case PrintOp:
        retreat(source, token);
        return lvalue;
    case EOFsymbol:
        return lvalue;
    default:
        printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
        exit(1);
    }
}

Expression *parseExpression(TokenStream *source, Expression *lvalue) {
    Token token = scanner(source);
    Expression *expr;

    switch (token.type) {
    case PlusOp:
        expr = (Expression *)malloc(sizeof(Expression));
        (expr->v).type = PlusNode;
        (expr->v).val.op = Plus;
        expr->leftOperand = lvalue;
        expr->rightOperand = parseValue(source);
        return parseExpressionTail(source, expr);
    case MinusOp:
        expr = (Expression *)malloc(sizeof(Expression));
        (expr->v).type = MinusNode;
        (expr->v).val.op = Minus;
        expr->leftOperand = lvalue;
        expr->rightOperand = parseValue(source);
        return parseExpressionTail(source, expr);
    // TODO: add MulOp and DivOp
    case Alphabet:
    case PrintOp:
        retreat(source, token);
        return NULL;
    case EOFsymbol:
        return NULL;
    default:
        printf("Syntax Error: Expect a numeric value or an identifier %s\n", token.tok);
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
            printf("Syntax Error: Expect an assignment op %s\n", next_token.tok);
            exit(1);
        }
    case PrintOp:
        next_token = scanner(source);
        if (next_token.type == Alphabet)
            return makePrintNode(next_token.tok);
        else {
            printf("Syntax Error: Expect an identifier %s\n", next_token.tok);
            exit(1);
        }
        break;
    default:
        printf("Syntax Error: Expect a statement %s\n", token.tok);
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
        printf("Syntax Error: Expect statements %s\n", token.tok);
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

    // FIXME: reject if declared variable exhausts the symbol table

    if (reg < 0) {
        reg = table->size++;
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

void convertType(Expression * old, DataType type) {
    if (old->type == Float && type == Int) {
        printf("error : can't convert float to integer\n");
        return;
    }
    if (old->type == Int && type == Float) {
        Expression *tmp = (Expression *)malloc(sizeof(Expression));
        if (old->v.type == Identifier)
            printf("convert to float %s \n", old->v.val.str);
        else
            printf("convert to float %d \n", old->v.val.ivalue);
        tmp->v = old->v;
        tmp->leftOperand = old->leftOperand;
        tmp->rightOperand = old->rightOperand;
        tmp->type = old->type;

        Value v;
        v.type = IntToFloatConvertNode;
        v.val.op = IntToFloatConvert;
        old->v = v;
        old->type = Int;
        old->leftOperand = tmp;
        old->rightOperand = NULL;
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
    }
}

void checkstmt(Statement *stmt, SymbolTable * table) {
    if (stmt->type == Assignment) {
        AssignmentStatement* assign = &stmt->stmt.assign;
        printf("assignment : %s \n", assign->str);
        checkexpression(assign->expr, table);

        char id = lookup_id(table, assign->str);
        substitute_var(assign->str, id);
        stmt->stmt.assign.type = lookup_table(table, id);
        if (assign->expr->type == Float && stmt->stmt.assign.type == Int) {
            printf("error : can't convert float to integer\n");
        } else {
            convertType(assign->expr, assign->type);
        }
    } else if (stmt->type == Print) {
        printf("print : %s \n", stmt->stmt.str);
        stmt->stmt.str[0] = lookup_id(table, stmt->stmt.str);
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
    case MinusNode:
        fprintf(target, "-\n");
        break;
    case PlusNode:
        fprintf(target, "+\n");
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
            fprint_expr(target, stmt.stmt.assign.expr);
            /*
               if(stmt.stmt.assign.type == Int){
               fprintf(target,"0 k\n");
               }
               else if(stmt.stmt.assign.type == Float){
               fprintf(target,"5 k\n");
               }*/
            fprintf(target, "s%c\n", stmt.stmt.assign.str[0]);
            fprintf(target, "0 k\n");
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

    while (decls != NULL) {
        decl = decls->first;
        if (decl.var.type == Int)
            printf("i ");
        if (decl.var.type == Float)
            printf("f ");
        printf("%s ", decl.var.varname);
        decls = decls->rest;
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
