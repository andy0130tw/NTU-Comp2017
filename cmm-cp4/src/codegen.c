#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "header.h"
#include "codegen.h"
#include "symbolTable.h"
#include "registers.h"

/***********************************************/

static FILE* g_outfile = NULL;

__attribute__((format(printf, 1, 2)))
static int writeCode(const char* fmt, ...) {
    if (!g_outfile) {
        fprintf(stderr, "outfile is not set\n");
        exit(1);
    }
    va_list args;
    va_start(args, fmt);
    int ret = vfprintf(g_outfile, fmt, args);
    va_end(args);
    return ret;
}

/////////////////////////////////////////////////////////////////////

static int getNewNumber(int isReset) {
    static int num;
    if (isReset) num = 0;
    return ++num;
}

static int cgNewConstantLabel(C_type constType, void* valRef) {
    int labelNum = getNewNumber(0);

    writeCode(".data\n");
    switch (constType) {
    case INTEGERC:
        writeCode("_CONSTANT_%d: .word %d\n", labelNum, *(int*)valRef);
        writeCode(".align 3\n");
        break;
    case FLOATC:
        writeCode("_CONSTANT_%d: .float %f\n", labelNum, *(float*)valRef);
        writeCode(".align 3\n");
        break;
    case STRINGC:
        writeCode("_CONSTANT_%d: .ascii %s\n", labelNum, (char*)valRef);
        writeCode(".align 3\n");
        break;
    }
    writeCode(".text\n");

    return labelNum;
}

const char* getRegisterName(REGISTER_TYPE type, int regIndex) {
    /* produce an extra load when spills */
    if (type == REG_INT) {
        return regIntName[regIndex];
    } else if (type == REG_GENERAL) {
        return regGPName[regIndex];
    } else if (type == REG_FLOAT) {
        return regFloatName[regIndex];
    } else return NULL;
}

void cgCode3(REGISTER_TYPE type, const char* instr, int r1, int r2, int r3) {
    const char* r1n = getRegisterName(type, r1);
    const char* r2n = getRegisterName(type, r2);
    const char* r3n = getRegisterName(type, r3);

    writeCode("%-7s %s, %s, %s\n", instr, r1n, r2n, r3n);
    // TODO: save if spill
}

void cgCode2(REGISTER_TYPE type, const char* instr, int r1, int r2) {
    const char* r1n = getRegisterName(type, r1);
    const char* r2n = getRegisterName(type, r2);

    writeCode("%-7s %s, %s\n", instr, r1n, r2n);
}

void cgCode2s(REGISTER_TYPE type, const char* instr, int r1, const char* str) {
    const char* r1n = getRegisterName(type, r1);

    writeCode("%-7s %s, %s\n", instr, r1n, str);
    // TODO: save if spill
}

/////////////////////////////////////////////////////////////////////

void codegen(AST_NODE* root) {
    const char* fname_out = "output.s";
    FILE* fp_out = fopen(fname_out, "w");
    g_outfile = fp_out;
    // g_outfile = stdout;
    cgProgramNode(root);
    fclose(fp_out);
}

void cgProgramNode(AST_NODE* programNode) {
    AST_NODE* child = programNode->child;

    // if there is no error, the type should remain none
    programNode->dataType = NONE_TYPE;

    while (child) {
        // top level; varDeclList (a list of variables)
        //            or a (function) declaration
        if (child->nodeType == VARIABLE_DECL_LIST_NODE) {
            writeCode(".data\n");
            cgGlobalDeclarations(child);
        } else if (child->nodeType == DECLARATION_NODE) {
            writeCode(".text\n");
            cgDeclarationNode(child);
        }  // else generates nothing

        child = child->rightSibling;
    }
}

void cgGlobalDeclarations(AST_NODE* varDeclListNode) {
    // static const int zeroInt = 0;
    // static const float zeroFloat = 0.f;

    AST_NODE* declPtr = varDeclListNode->child;

    while (declPtr) {
        if (SEMVAL_DECL(declPtr).kind == VARIABLE_DECL) {
            AST_NODE *typeNode = declPtr->child;
            AST_NODE *idPtr = typeNode->rightSibling;

            while (idPtr) {
                // writeCode("#; L%d\n", idPtr->linenumber);

                // the reliable way to determine whether the variable is array or scalar
                // is to check the symbol table!

                // TODO: global var initialization
                if (SEMVAL_ID(idPtr).kind == WITH_INIT_ID) {
                    fprintf(stderr, "Unimplemented: init global variable %s\n", SEMVAL_ID(idPtr).identifierName);
                }

                SymbolTableEntry* sym = getIdNodeEntry(idPtr);
                TypeDescriptor* typeDesc = sym->attribute->attr.typeDescriptor;

                if (typeDesc->kind == SCALAR_TYPE_DESCRIPTOR) {
                    if (typeNode->dataType == INT_TYPE) {
                        writeCode("_g_%s: .word 0\n", SEMVAL_ID(idPtr).identifierName);
                    } else if (typeNode->dataType == FLOAT_TYPE) {
                        writeCode("_g_%s: .float 0.0\n", SEMVAL_ID(idPtr).identifierName);
                    }
                } else if (typeDesc->kind == ARRAY_TYPE_DESCRIPTOR) {
                    writeCode("_g_%s: .space %d\n", SEMVAL_ID(idPtr).identifierName,
                              getSizeByType(typeDesc));
                }

                idPtr = idPtr->rightSibling;
            }
        }
        declPtr = declPtr->rightSibling;
    }

}

void cgGeneralNode(AST_NODE* node) {
    AST_NODE* child = node->child;
    void (*checkFunc)(AST_NODE*);

    switch (node->nodeType) {
    case VARIABLE_DECL_LIST_NODE:
        checkFunc = cgDeclarationNode; break;
    case STMT_LIST_NODE:
        checkFunc = cgStmtNode; break;
    case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
        checkFunc = cgCheckAssignOrExpr; break;
    case NONEMPTY_RELOP_EXPR_LIST_NODE:
        checkFunc = cgExprRelatedNode; break;
    case NUL_NODE:  // no need to check
        return;
    default: __builtin_unreachable();
    }

    while (child) {
        checkFunc(child);
        child = child->rightSibling;
    }
}

void cgDeclarationNode(AST_NODE* declarationNode) {
    switch (SEMVAL_DECL(declarationNode).kind) {
    case FUNCTION_DECL:
        return cgDeclareFunction(declarationNode);
    case VARIABLE_DECL:
        return cgDeclareLocalVarList(declarationNode, 0);
    case FUNCTION_PARAMETER_DECL:
        return cgDeclareLocalVarList(declarationNode, 1);
    // generates nothing
    case TYPE_DECL: return;
    }
}

void cgBlockNode(AST_NODE* blockNode) {
    // openScope();

    AST_NODE* child = blockNode->child;
    while (child) {
        cgGeneralNode(child);
        child = child->rightSibling;
    }

    // closeScope();
}

void cgStmtNode(AST_NODE* stmtNode) {
    if (stmtNode->nodeType == NUL_NODE) {
        return;
    }
    if (stmtNode->nodeType == BLOCK_NODE) {
        return cgBlockNode(stmtNode);
    }
    switch (stmtNode->semantic_value.stmtSemanticValue.kind) {
    case ASSIGN_STMT:
        return cgCheckAssignmentStmt(stmtNode);
    case WHILE_STMT:
        return cgCheckWhileStmt(stmtNode);
    case FOR_STMT:
        return cgCheckForStmt(stmtNode);
    case IF_STMT:
        return cgCheckIfStmt(stmtNode);
    case FUNCTION_CALL_STMT:
        return cgCheckFunctionCall(stmtNode);
    case RETURN_STMT:
        return cgCheckReturnStmt(stmtNode);
    default: __builtin_unreachable();
    }
}

void cgCheckAssignOrExpr(AST_NODE* assignOrExprRelatedNode) {
    if (assignOrExprRelatedNode->nodeType == STMT_NODE) {
        STMT_KIND stmtType = SEMVAL_STMT(assignOrExprRelatedNode).kind;
        if (stmtType == ASSIGN_STMT) {
            cgCheckAssignmentStmt(assignOrExprRelatedNode);
        } else if (stmtType == FUNCTION_CALL_STMT) {
            cgCheckFunctionCall(assignOrExprRelatedNode);
        }
    } else {
        cgExprRelatedNode(assignOrExprRelatedNode);
    }
}

void cgExprRelatedNode(AST_NODE* exprRelatedNode) {
    switch(exprRelatedNode->nodeType) {
    case EXPR_NODE:
        cgExprNode(exprRelatedNode);
        break;
    case CONST_VALUE_NODE:
        cgConstValueNode(exprRelatedNode);
        break;
    case IDENTIFIER_NODE:
        cgVariableRValue(exprRelatedNode);
        break;
    case STMT_NODE:
        cgCheckFunctionCall(exprRelatedNode);
        break;
    default: __builtin_unreachable();
    }
}

void cgDeclareLocalVarList(AST_NODE* declarationNode, int isFunctionParameter) {
    AST_NODE* typeNode = declarationNode->child;
    AST_NODE* idNode = typeNode->rightSibling;

    while (idNode) {
        if (SEMVAL_ID(idNode).kind == WITH_INIT_ID) {
            fprintf(stderr, "Unimplementwed: init local var %s\n", SEMVAL_ID(idNode).identifierName);
        }

        idNode = idNode->rightSibling;
    }
}

void cgDeclDimList(AST_NODE* idNode, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize) {
    AST_NODE* declDimList = idNode->child;
    AST_NODE* dimPtr = declDimList;

    int dimen = 0;

    if(ignoreFirstDimSize) {
        // writeCode("[]");
        dimen++;
        dimPtr = dimPtr->rightSibling;
    }

    while (dimPtr) {
        // writeCode("[");
        cgExprRelatedNode(dimPtr);
        // writeCode("]");
        dimen++;
        dimPtr = dimPtr->rightSibling;
    }
}

void cgSaveRegisters() {
    int offs = 0;
    for (int i = 19; i < 29; i++) {
        writeCode("str     x%d, [sp, #%d]\n", i, offs + 8);
        offs += 8;
    }
}

void cgRestoreRegisters() {
    int offs = 0;
    for (int i = 19; i < 29; i++) {
        writeCode("ldr     x%d, [sp, #%d]\n", i, offs + 8);
        offs += 8;
    }
}

void cgDeclareFunction(AST_NODE* declarationNode) {
    AST_NODE* returnTypeNode = declarationNode->child;

    AST_NODE* functionNameNode = returnTypeNode->rightSibling;
    char* functionNameId = SEMVAL_ID(functionNameNode).identifierName;

    writeCode("#; L%d\n", returnTypeNode->linenumber);

    writeCode("_start_%s:\n", functionNameId);

    writeCode("str     x30, [sp, #0]\n");   // ret addr
    writeCode("str     x29, [sp, #-8]\n");  // save old fp
    writeCode("add     x29, sp, -8\n");   // new fp
    writeCode("add     sp, sp, -16\n");    // new sp
    writeCode("ldr     x30, =_frameSize_%s\n", functionNameId);
    writeCode("ldr     w30, [x30, #0]\n");
    writeCode("sub     sp, sp, w30\n");     // push new AR

    cgSaveRegisters();

    AST_NODE *parameterListNode = functionNameNode->rightSibling;
    AST_NODE* blockNode = parameterListNode->rightSibling;
    AST_NODE* stmtNode = blockNode->child;

    while (stmtNode) {
        // print annotation
        cgGeneralNode(stmtNode);
        stmtNode = stmtNode->rightSibling;
    }

    writeCode("_end_%s:\n", functionNameId);

    cgRestoreRegisters();

    writeCode("ldr     x30, [x29, #8]\n");  // restore return address
    writeCode("add     sp, x29, #8\n");     // pop AR
    writeCode("ldr     x29, [x29,#0]\n");   // restore caller (old) fp
    writeCode("ret     x30\n");

    // (used to store registers x19~x28)
    int frameSize = 10 * 8 + -(getIdNodeEntry(functionNameNode)->attribute->offset);

    writeCode(".data\n");
    writeCode("_frameSize_%s: .word %d\n", functionNameId, frameSize);
}

void cgCheckAssignmentStmt(AST_NODE* assignmentNode) {
    AST_NODE* lhsNode = assignmentNode->child;
    AST_NODE* rhsNode = lhsNode->rightSibling;
    cgExprRelatedNode(rhsNode);

    IDENTIFIER_KIND idKind = SEMVAL_ID(lhsNode).kind;

    writeCode("#; L%d\n", lhsNode->linenumber);
    if (idKind == NORMAL_ID) {
        /* FIXME: register type */
        REGISTER_TYPE regType = REG_FLOAT;

        if (lhsNode->dataType != rhsNode->dataType) {
            fprintf(stderr, "Unimplemented: implicit type conversion when assigning\n");
        }
        if (rhsNode->dataType == INT_TYPE) {
            regType = REG_INT;
        }

        const char* dstReg = getRegisterName(regType, rhsNode->regIndex);
        if (getIdNodeEntry(lhsNode)->nestingLevel == 0) {
            // global
            int regTmp = getFreeRegister(REG_GENERAL);
            const char* globTmp = getRegisterName(REG_GENERAL, regTmp);
            writeCode("ldr     %s, =_g_%s\n", globTmp, SEMVAL_ID(lhsNode).identifierName);
            writeCode("str     %s, [%s, #0]\n", dstReg, globTmp);
            freeRegister(REG_GENERAL, regTmp);
        } else {
            // local
            writeCode("str     %s, [x29, #%d]\n", dstReg, getIdNodeEntry(lhsNode)->attribute->offset);
        }
        lhsNode->regIndex = rhsNode->regIndex;
    } else if (idKind == ARRAY_ID) {
        fprintf(stderr, "Unimplemented: assign to array element of %s\n", SEMVAL_ID(lhsNode).identifierName);
    }
}

void cgCheckWhileStmt(AST_NODE* whileNode) {
    AST_NODE* condNode  = whileNode->child;
    AST_NODE* blockNode = condNode->rightSibling;
    // writeCode("while (");
    cgCheckAssignOrExpr(condNode);
    // writeCode(") {\n");
    cgStmtNode(blockNode);
    // writeCode("}\n");
}

void cgCheckForStmt(AST_NODE* forNode) {
    AST_NODE* initStmtNode = forNode->child;
    AST_NODE* condExprNode = initStmtNode->rightSibling;
    AST_NODE* postStmtNode = condExprNode->rightSibling;
    AST_NODE* blockNode    = postStmtNode->rightSibling;
    cgGeneralNode(initStmtNode);
    cgGeneralNode(condExprNode);
    cgGeneralNode(postStmtNode);
    cgStmtNode(blockNode);
}

void cgCheckIfStmt(AST_NODE* ifNode) {
    AST_NODE* condNode      = ifNode->child;
    AST_NODE* ifBlockNode   = condNode->rightSibling;
    AST_NODE* elseBlockNode = ifBlockNode->rightSibling;
    // writeCode("if (");
    cgCheckAssignOrExpr(condNode);
    // writeCode(") {\n");
    cgStmtNode(ifBlockNode);
    // writeCode("} else {\n");
    cgStmtNode(elseBlockNode);
    // writeCode("}\n");
}

void cgCheckFunctionCall(AST_NODE* functionCallNode) {
    AST_NODE* funcNameNode = functionCallNode->child;
    char* id = funcNameNode->semantic_value.identifierSemanticValue.identifierName;

    if (strcmp(id, SYMBOL_TABLE_SYS_LIB_WRITE) == 0) {
        return cgCheckWriteFunction(functionCallNode);
    }

    // writeCode("%s(", id);

    SymbolTableEntry* symEntry = retrieveSymbol(id);
    AST_NODE* callParamListNode = funcNameNode->rightSibling;
    AST_NODE* callParamNode = callParamListNode->child;
    Parameter* declParam;

    cgGeneralNode(callParamListNode);
    declParam = symEntry->attribute->attr.functionSignature->parameterList;

    while (callParamNode && declParam) {
        // cgCheckParameterPassing(declParam, callParamNode);
        // writeCode("..., ");
        callParamNode = callParamNode->rightSibling;
        declParam = declParam->next;
    }
    // writeCode("__END__)\n");
}

void cgCheckReturnStmt(AST_NODE* returnNode) {
    // writeCode("return ");

    cgExprRelatedNode(returnNode->child);

    // writeCode(";\n");
}

void cgCheckWriteFunction(AST_NODE* functionCallNode) {
    AST_NODE* funcNameNode = functionCallNode->child;
    AST_NODE* callParamListNode = funcNameNode->rightSibling;
    AST_NODE* parameter = callParamListNode->child;

    cgExprRelatedNode(parameter);

    const char* parameterRegName;
    switch (parameter->dataType) {
    case INT_TYPE:
        parameterRegName = getRegisterName(REG_INT, parameter->regIndex);
        writeCode("mov     w0, %s\n", parameterRegName);
        writeCode("bl _write_int\n");
        break;
    case FLOAT_TYPE:
        parameterRegName = getRegisterName(REG_FLOAT, parameter->regIndex);
        writeCode("fmov    s0, %s\n", parameterRegName);
        writeCode("bl _write_float\n");
        break;
    case CONST_STRING_TYPE:
        parameterRegName = getRegisterName(REG_INT, parameter->regIndex);
        writeCode("mov     w0, %s\n", parameterRegName);
        writeCode("bl _write_str\n");
    default: __builtin_unreachable();
    }
}

void cgConstValueNode(AST_NODE* constValueNode) {
    EXPRSemanticValue* exprSemVal = &constValueNode->semantic_value.exprSemanticValue;
    int id;
    const char* regName;
    switch (constValueNode->semantic_value.const1->const_type) {
    case INTEGERC:
        id = cgNewConstantLabel(INTEGERC, &exprSemVal->constEvalValue.iValue);
        constValueNode->regIndex = getFreeRegister(REG_INT);
        regName = getRegisterName(REG_INT, constValueNode->regIndex);
        writeCode("ldr     %s, _CONSTANT_%d\n", regName, id);
        break;
    case FLOATC:
        id = cgNewConstantLabel(FLOATC, &exprSemVal->constEvalValue.fValue);
        constValueNode->regIndex = getFreeRegister(REG_FLOAT);
        regName = getRegisterName(REG_FLOAT, constValueNode->regIndex);
        writeCode("ldr     %s, _CONSTANT_%d\n", regName, id);
        break;
    case STRINGC:
        id = cgNewConstantLabel(STRINGC, constValueNode->semantic_value.const1->const_u.sc);
        constValueNode->regIndex = getFreeRegister(REG_INT);
        regName = getRegisterName(REG_INT, constValueNode->regIndex);
        writeCode("ldr     %s, =_CONSTANT_%d\n", regName, id);
        // writeCode("%s", constValueNode->semantic_value.const1->const_u.sc);
        break;
    default:
        break;
    }
}

static inline int isConstExpr(AST_NODE* exprNode) {
    return exprNode->nodeType == CONST_VALUE_NODE ||
          (exprNode->nodeType == EXPR_NODE && exprNode->semantic_value.exprSemanticValue.isConstEval);
}

void cgExprNode(AST_NODE* exprNode) {
    if (SEMVAL_EXPR(exprNode).kind == BINARY_OPERATION) {
        AST_NODE* lhsNode = exprNode->child;
        AST_NODE* rhsNode = lhsNode->rightSibling;
        cgExprRelatedNode(lhsNode);
        cgExprRelatedNode(rhsNode);

        if (lhsNode->dataType != rhsNode->dataType) {
            fprintf(stderr, "Unimplemented: implicit type conversion\n");
        }

        if (exprNode->dataType == INT_TYPE) {
            exprNode->regIndex = lhsNode->regIndex;
            switch (SEMVAL_EXPR(exprNode).op.binaryOp) {
            case BINARY_OP_ADD:
                cgCode3(REG_INT, "add", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_SUB:
                cgCode3(REG_INT, "sub", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_MUL:
                cgCode3(REG_INT, "mul", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_DIV:
                cgCode3(REG_INT, "div", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_EQ:
                cgCode3(REG_INT, "cmp", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", lhsNode->regIndex, "eq");
                break;
            case BINARY_OP_NE:
                cgCode3(REG_INT, "cmp", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", lhsNode->regIndex, "ne");
                break;
            case BINARY_OP_GT:
                cgCode3(REG_INT, "cmp", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", lhsNode->regIndex, "gt");
                break;
            case BINARY_OP_LT:
                cgCode3(REG_INT, "cmp", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", lhsNode->regIndex, "lt");
                break;
            case BINARY_OP_GE:
                cgCode3(REG_INT, "cmp", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", lhsNode->regIndex, "ge");
                break;
            case BINARY_OP_LE:
                cgCode3(REG_INT, "cmp", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", lhsNode->regIndex, "le");
                break;
            case BINARY_OP_AND: case BINARY_OP_OR:
                fprintf(stderr, "Unimplemented: binary logic operator\n");
                break;
            }
        } else if (exprNode->dataType == FLOAT_TYPE) {
            exprNode->regIndex = lhsNode->regIndex;
            switch (SEMVAL_EXPR(exprNode).op.binaryOp) {
            case BINARY_OP_ADD:
                cgCode3(REG_FLOAT, "fadd", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_SUB:
                cgCode3(REG_FLOAT, "fsub", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_MUL:
                cgCode3(REG_FLOAT, "fmul", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_DIV:
                cgCode3(REG_FLOAT, "fdiv", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_EQ:
            case BINARY_OP_NE:
            case BINARY_OP_GT:
            case BINARY_OP_LT:
            case BINARY_OP_GE:
            case BINARY_OP_LE:
                fprintf(stderr, "Unimplemented: float comparing operator\n");
                break;
            case BINARY_OP_AND: case BINARY_OP_OR:
                fprintf(stderr, "Unimplemented: binary logic operator\n");
                break;
            }
        } else {
            printf("exprNode datatype (%d) is neither int nor float\n", exprNode->dataType);
        }

        // writeCode("(");
        // cgExprRelatedNode(lhsNode);
        // writeCode(" $%d$ ", SEMVAL_EXPR(exprNode).op.binaryOp);
        // cgExprRelatedNode(rhsNode);
        // writeCode(")");
        // switch (SEMVAL_EXPR(exprNode).op.binaryOp) {
        // case BINARY_OP_ADD:
        //     // writeCode("")
        // }
    } else {
        // unary operators
        AST_NODE* operand = exprNode->child;
        cgExprRelatedNode(operand);
        exprNode->regIndex = operand->regIndex;

        if (exprNode->dataType == INT_TYPE) {
            exprNode->regIndex = operand->regIndex;
            switch (SEMVAL_EXPR(exprNode).op.unaryOp) {
            case UNARY_OP_NEGATIVE:
                cgCode2(REG_INT, "neg", exprNode->regIndex, operand->regIndex);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                cgCode2s(REG_INT, "cmp", exprNode->regIndex, 0);
                cgCode2s(REG_INT, "cset", exprNode->regIndex, "eq");
                cgCode2(REG_INT, "uxtb", exprNode->regIndex, exprNode->regIndex);
                break;
            case UNARY_OP_POSITIVE: break;
            }
        } else if (exprNode->dataType == FLOAT_TYPE) {
            switch (SEMVAL_EXPR(exprNode).op.unaryOp) {
            case UNARY_OP_NEGATIVE:
                cgCode2(REG_INT, "fneg", exprNode->regIndex, operand->regIndex);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                fprintf(stderr, "Unimplemented: logical neg on float\n");
                break;
            case UNARY_OP_POSITIVE: break;
            }
        }
    }
}

void cgVariableRValue(AST_NODE* idNode) {
    // writeCode("(");
    SymbolAttribute* attr = getIdNodeEntry(idNode)->attribute;
    switch (SEMVAL_ID(idNode).kind) {
    case NORMAL_ID:
        if (idNode->dataType == INT_TYPE) {
            idNode->regIndex = getFreeRegister(REG_INT);
            const char* regLoad;

            if (getIdNodeEntry(idNode)->nestingLevel == 0) {
                // global
                int tmpIdx = getFreeRegister(REG_INT);
                const char* regTmp = getRegisterName(REG_INT, tmpIdx);
                regLoad = getRegisterName(REG_INT, idNode->regIndex);
                writeCode("ldr     %s, _g_%s\n", regTmp, SEMVAL_ID(idNode).identifierName);
                writeCode("mov     %s, %s\n", regLoad, regTmp);
                freeRegister(REG_INT, tmpIdx);
            } else {
                // local
                regLoad = getRegisterName(REG_INT, idNode->regIndex);
                writeCode("ldr     %s, [x29, #%d]\n", regLoad, attr->offset);
            }
            // TODO: save if spills
        } else if (idNode->dataType == FLOAT_TYPE) {
            idNode->regIndex = getFreeRegister(REG_FLOAT);
            const char* regLoad;

            if (getIdNodeEntry(idNode)->nestingLevel == 0) {
                // global
                int tmpIdx = getFreeRegister(REG_FLOAT);
                const char* regTmp = getRegisterName(REG_FLOAT, tmpIdx);
                regLoad = getRegisterName(REG_FLOAT, idNode->regIndex);
                writeCode("ldr     %s, _g_%s\n", regTmp, SEMVAL_ID(idNode).identifierName);
                writeCode("fmov    %s, %s\n", regLoad, regTmp);
                freeRegister(REG_FLOAT, tmpIdx);
            } else {
                // local
                regLoad = getRegisterName(REG_FLOAT, idNode->regIndex);
                writeCode("ldr     %s, [x29, #%d]\n", regLoad, attr->offset);
            }
            // TODO: save if spills
        }
        break;
    case ARRAY_ID: {
        fprintf(stderr, "Unimplemented: array id reference\n");
        int dimension = 0;
        AST_NODE *dimNode = idNode->child;
        while (dimNode) {
            dimension++;
            // writeCode("[");
            cgExprRelatedNode(dimNode);
            // writeCode("]");
            dimNode = dimNode->rightSibling;
        }
        break;
    }
    default: __builtin_unreachable();
    }
    // writeCode(")");
}
