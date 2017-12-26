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

static int getNewSerial(int isReset) {
    static int num;
    if (isReset) return num = 0;
    return ++num;
}

static int cgNewConstantLabel(C_type constType, void* valRef) {
    int labelNum = getNewSerial(0);

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
        writeCode("_CONSTANT_%d: .ascii %s \"\\0\"\n", labelNum, (char*)valRef);
        writeCode(".align 3\n");
        break;
    }
    writeCode(".text\n");

    return labelNum;
}

/**
 * We use the work register 0 to reload
 *                          1 to store immediate result temporarily
 */

const char* spareRegister(REGISTER_TYPE type, int regIndex) {
    if (type == REG_INT) {
        if (regIndex < 0) {
            // always save the first register for later use
            return WORK_REG(REG_INT, 0);
        }
        return regIntName[regIndex];
    } else if (type == REG_GENERAL) {
        if (regIndex < 0) {
            return "x17";
        }
        return regGPName[regIndex];
    } else if (type == REG_FLOAT) {
        if (regIndex < 0) {
            return WORK_REG(REG_FLOAT, 0);
        }
        return regFloatName[regIndex];
    } else return NULL;
}

void cgCode3(REGISTER_TYPE type, const char* instr, int r1, int r2, int r3) {
    const char* r1n = spareRegister(type, r1);
    const char* r2n = spareRegister(type, r2);
    const char* r3n = spareRegister(type, r3);

    writeCode("%-7s %s, %s, %s\n", instr, r1n, r2n, r3n);
    // TODO: save if spill
}

void cgCode2(REGISTER_TYPE type, const char* instr, int r1, int r2) {
    const char* r1n = spareRegister(type, r1);
    const char* r2n = spareRegister(type, r2);

    writeCode("%-7s %s, %s\n", instr, r1n, r2n);
}

void cgCode2s(REGISTER_TYPE type, const char* instr, int r1, const char* str) {
    const char* r1n = spareRegister(type, r1);

    writeCode("%-7s %s, %s\n", instr, r1n, str);
    // TODO: save if spill
}

/////////////////////////////////////////////////////////////////////

void codegen(AST_NODE* root) {
    const char* fname_out = "output.s";
    FILE* fp_out = fopen(fname_out, "w");
    g_outfile = fp_out;
    // g_outfile = stdout;
    getNewSerial(1);
    cgProgramNode(root);
    fclose(fp_out);
}

void cgProgramNode(AST_NODE* programNode) {
    AST_NODE* child = programNode->child;

    while (child) {
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
    AST_NODE* child = blockNode->child;
    while (child) {
        cgGeneralNode(child);
        child = child->rightSibling;
    }
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
        return cgAssignmentStmt(stmtNode);
    case WHILE_STMT:
        return cgWhileStmt(stmtNode);
    case FOR_STMT:
        return cgForStmt(stmtNode);
    case IF_STMT:
        return cgIfStmt(stmtNode);
    case FUNCTION_CALL_STMT:
        return cgFunctionCall(stmtNode);
    case RETURN_STMT:
        return cgReturnStmt(stmtNode);
    default: __builtin_unreachable();
    }
}

void cgCheckAssignOrExpr(AST_NODE* assignOrExprRelatedNode) {
    if (assignOrExprRelatedNode->nodeType == STMT_NODE) {
        STMT_KIND stmtType = SEMVAL_STMT(assignOrExprRelatedNode).kind;
        if (stmtType == ASSIGN_STMT) {
            cgAssignmentStmt(assignOrExprRelatedNode);
        } else if (stmtType == FUNCTION_CALL_STMT) {
            cgFunctionCall(assignOrExprRelatedNode);
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
        cgFunctionCall(exprRelatedNode);
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
    int offs = 8;
    for (int i = 9; i <= 15; i++) {
        writeCode("str     x%d, [sp, #%d]\n", i, offs);
        offs += 8;
    }
    for (int i = 16; i <= 23; i++) {
        writeCode("str     s%d, [sp, #%d]\n", i, offs);
        offs += 4;
    }
}

void cgRestoreRegisters() {
    int offs = 8;
    for (int i = 9; i <= 15; i++) {
        writeCode("ldr     x%d, [sp, #%d]\n", i, offs);
        offs += 8;
    }
    for (int i = 16; i <= 23; i++) {
        writeCode("ldr     s%d, [sp, #%d]\n", i, offs);
        offs += 4;
    }
}

void cgDeclareFunction(AST_NODE* declarationNode) {
    AST_NODE* returnTypeNode = declarationNode->child;

    AST_NODE* functionNameNode = returnTypeNode->rightSibling;
    char* functionNameId = SEMVAL_ID(functionNameNode).identifierName;

    writeCode("#; L%d\n", returnTypeNode->linenumber);

    writeCode("_start_%s:\n", functionNameId);

    writeCode("str     x30, [sp]\n");       // ret addr
    writeCode("str     x29, [sp, #-8]\n");  // save old fp
    writeCode("add     x29, sp, -8\n");     // new fp
    writeCode("add     sp, sp, -16\n");     // new sp
    writeCode("ldr     x30, =_frameSize_%s\n", functionNameId);
    writeCode("ldr     w30, [x30]\n");
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
    writeCode("ldr     x29, [x29]\n");     // restore caller (old) fp
    writeCode("ret     x30\n");

    // (used to store registers x9~x15, s16~s23, 88 at base)
    int frameSize = 7 * 8 + 8 * 4 + -(getIdNodeEntry(functionNameNode)->attribute->offset);

    writeCode(".data\n");
    writeCode("_frameSize_%s: .word %d\n", functionNameId, frameSize);
}

void cgAssignmentStmt(AST_NODE* assignmentNode) {
    AST_NODE* lhsNode = assignmentNode->child;
    AST_NODE* rhsNode = lhsNode->rightSibling;
    cgExprRelatedNode(rhsNode);

    IDENTIFIER_KIND idKind = SEMVAL_ID(lhsNode).kind;

    writeCode("#; L%d\n", lhsNode->linenumber);

    const char* dimRegName = NULL;
    int dimRegIdx = -1;

    if (idKind == ARRAY_ID) {
        int dimension = 0;
        AST_NODE *dimNode = lhsNode->child;

        while (dimNode) {
            dimension++;
            if (dimension > 1) {
                fprintf(stderr, "Multi-dimensional array lval\n");
                break;
            }
            cgExprRelatedNode(dimNode);
            dimRegName = spareRegister(REG_INT, dimNode->regIndex);
            dimRegIdx = dimNode->regIndex;
            dimNode = dimNode->rightSibling;
        }

        writeCode("lsl     %s, %s, 2\n", dimRegName, dimRegName);
    }

    if (idKind == NORMAL_ID || idKind == ARRAY_ID) {
        REGISTER_TYPE regType = REG_FLOAT;

        if (lhsNode->dataType != rhsNode->dataType) {
            fprintf(stderr, "Unimplemented: implicit type conversion when assigning\n");
        }
        if (rhsNode->dataType == INT_TYPE) {
            regType = REG_INT;
        }

        const char* dstReg = spareRegister(regType, rhsNode->regIndex);

        if (getIdNodeEntry(lhsNode)->nestingLevel == 0) {
            // global
            int regTmp = getFreeRegister(REG_GENERAL);
            const char* globTmp = spareRegister(REG_GENERAL, regTmp);
            writeCode("ldr     %s, =_g_%s\n", globTmp, SEMVAL_ID(lhsNode).identifierName);
            if (dimRegName) {
                const char* regGen = spareRegister(REG_GENERAL, dimRegIdx);
                writeCode("sxtw    %s, %s\n", regGen, dimRegName);
                writeCode("str     %s, [%s, %s]\n", dstReg, globTmp, regGen);
            } else {
                writeCode("str     %s, [%s]\n", dstReg, globTmp);
            }
            freeRegister(REG_GENERAL, regTmp);
        } else {
            // local
            if (dimRegName) {
                const char* regGen = spareRegister(REG_GENERAL, dimRegIdx);
                writeCode("add     %s, %s, #%d\n", dimRegName, dimRegName, getIdNodeEntry(lhsNode)->attribute->offset);
                writeCode("sxtw    %s, %s\n", regGen, dimRegName);
                writeCode("str     %s, [x29, %s]\n", dstReg, regGen);
            } else {
                writeCode("str     %s, [x29, #%d]\n", dstReg, getIdNodeEntry(lhsNode)->attribute->offset);
            }
        }
        lhsNode->regIndex = rhsNode->regIndex;
        freeRegister(regType, lhsNode->regIndex);
    }

    if (dimRegName) {
        freeRegister(REG_INT, dimRegIdx);
    }
}

void cgWhileStmt(AST_NODE* whileNode) {
    AST_NODE* condNode  = whileNode->child;
    AST_NODE* blockNode = condNode->rightSibling;

    int labelNumber = getNewSerial(0);

    writeCode("_whileTestLabel_%d:\n", labelNumber);
    cgCheckAssignOrExpr(condNode);

    if (condNode->dataType == INT_TYPE) {
        cgCode2s(REG_INT, "cmp", condNode->regIndex, "0");
        writeCode("beq     _whileExitLabel_%d\n", labelNumber);
        freeRegister(REG_INT, condNode->regIndex);
    } else {
        fprintf(stderr, "Unimplemented: float type in while condition\n");
    }

    cgStmtNode(blockNode);
    writeCode("b       _whileTestLabel_%d\n", labelNumber);
    writeCode("_whileExitLabel_%d:\n", labelNumber);
}

void cgForStmt(AST_NODE* forNode) {
    AST_NODE* initStmtNode = forNode->child;
    AST_NODE* condExprNode = initStmtNode->rightSibling;
    AST_NODE* postStmtNode = condExprNode->rightSibling;
    AST_NODE* blockNode    = postStmtNode->rightSibling;
    fprintf(stderr, "Unimplemented: for loop\n");
    cgGeneralNode(initStmtNode);
    cgGeneralNode(condExprNode);
    cgGeneralNode(postStmtNode);
    cgStmtNode(blockNode);
}

void cgIfStmt(AST_NODE* ifNode) {
    AST_NODE* condNode      = ifNode->child;
    AST_NODE* ifBlockNode   = condNode->rightSibling;
    AST_NODE* elseBlockNode = ifBlockNode->rightSibling;

    int labelNumber = getNewSerial(0);

    // writeCode("if (");
    cgCheckAssignOrExpr(condNode);

    if (condNode->dataType == INT_TYPE) {
        cgCode2s(REG_INT, "cmp", condNode->regIndex, "0");
        writeCode("beq     _elseLabel_%d\n", labelNumber);
        freeRegister(REG_INT, condNode->regIndex);
    } else {
        cgCode2s(REG_FLOAT, "fcmp", condNode->regIndex, "#0.0");
        writeCode("beq     _elseLabel_%d\n", labelNumber);
        freeRegister(REG_FLOAT, condNode->regIndex);
    }

    cgStmtNode(ifBlockNode);

    writeCode("b       _ifExitLabel_%d\n", labelNumber);
    writeCode("_elseLabel_%d:\n", labelNumber);

    cgStmtNode(elseBlockNode);

    writeCode("_ifExitLabel_%d:\n", labelNumber);
}

void cgFunctionCall(AST_NODE* functionCallNode) {
    AST_NODE* funcNameNode = functionCallNode->child;
    char* id = funcNameNode->semantic_value.identifierSemanticValue.identifierName;

    if (strcmp(id, SYMBOL_TABLE_SYS_LIB_WRITE) == 0) {
        return cgWriteFunction(functionCallNode);
    }

    // writeCode("%s(", id);

    SymbolTableEntry* symEntry = retrieveSymbol(id);
    AST_NODE* callParamListNode = funcNameNode->rightSibling;
    AST_NODE* callParamNode = callParamListNode->child;
    Parameter* declParam;

    cgGeneralNode(callParamListNode);
    declParam = symEntry->attribute->attr.functionSignature->parameterList;

    while (callParamNode && declParam) {
        fprintf(stderr, "Unimplemented: general function call with arguments\n");
        break;
        // cgCheckParameterPassing(declParam, callParamNode);
        // writeCode("..., ");
        callParamNode = callParamNode->rightSibling;
        declParam = declParam->next;
    }

    if (strcmp(id, SYMBOL_TABLE_SYS_LIB_READ) == 0) {
        writeCode("bl      _read_int\n");
    } else if (strcmp(id, SYMBOL_TABLE_SYS_LIB_FREAD) == 0) {
        writeCode("bl      _read_float\n");
    } else {
        writeCode("bl      _start_%s\n", id);
    }

    DATA_TYPE retType = getIdNodeEntry(funcNameNode)->attribute->attr.functionSignature->returnType;

    if (retType == INT_TYPE) {
        functionCallNode->regIndex = getFreeRegister(REG_INT);
        const char* retRegName = spareRegister(REG_INT, functionCallNode->regIndex);
        writeCode("mov     %s, w0\n", retRegName);
        // TODO: spill
    } else if (retType == FLOAT_TYPE) {
        functionCallNode->regIndex = getFreeRegister(REG_FLOAT);
        const char* retRegName = spareRegister(REG_FLOAT, functionCallNode->regIndex);
        writeCode("fmov    %s, s0\n", retRegName);
        // TODO: spill
    }
}

void cgReturnStmt(AST_NODE* returnNode) {
    AST_NODE* retValNode = returnNode->child;
    cgExprRelatedNode(retValNode);

    /* TODO: implicit type convertion */
    const char* regName;
    if (returnNode->dataType == INT_TYPE) {
        regName = spareRegister(REG_INT, retValNode->regIndex);
        writeCode("mov     w0, %s\n", regName);
        freeRegister(REG_INT, retValNode->regIndex);
    } else if (returnNode->dataType == FLOAT_TYPE) {
        regName = spareRegister(REG_FLOAT, retValNode->regIndex);
        writeCode("fmov    s0, %s\n", regName);
        freeRegister(REG_FLOAT, retValNode->regIndex);
    }

    AST_NODE* funcPtr = returnNode;
    while (!(funcPtr && funcPtr->nodeType == DECLARATION_NODE && SEMVAL_DECL(funcPtr).kind == FUNCTION_DECL)) {
        funcPtr = funcPtr->parent;
    }
    if (!funcPtr) {
        fprintf(stderr, "FATAL: cannot get function name from return stmt QQ\n");
    }
    char* funcName = SEMVAL_ID(funcPtr->child->rightSibling).identifierName;
    writeCode("b       _end_%s\n", funcName);
}

void cgWriteFunction(AST_NODE* functionCallNode) {
    AST_NODE* funcNameNode = functionCallNode->child;
    AST_NODE* callParamListNode = funcNameNode->rightSibling;
    AST_NODE* parameter = callParamListNode->child;

    cgExprRelatedNode(parameter);

    const char* parameterRegName;
    switch (parameter->dataType) {
    case INT_TYPE:
        parameterRegName = spareRegister(REG_GENERAL, parameter->regIndex);
        writeCode("mov     x0, %s\n", parameterRegName);
        writeCode("bl      _write_int\n");
        freeRegister(REG_INT, parameter->regIndex);
        break;
    case FLOAT_TYPE:
        parameterRegName = spareRegister(REG_FLOAT, parameter->regIndex);
        writeCode("fmov    s0, %s\n", parameterRegName);
        writeCode("bl      _write_float\n");
        freeRegister(REG_FLOAT, parameter->regIndex);
        break;
    case CONST_STRING_TYPE:
        parameterRegName = spareRegister(REG_INT, parameter->regIndex);
        writeCode("mov     w0, %s\n", parameterRegName);
        writeCode("bl      _write_str\n");
        freeRegister(REG_INT, parameter->regIndex);
        break;
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
        regName = spareRegister(REG_INT, constValueNode->regIndex);
        writeCode("ldr     %s, _CONSTANT_%d\n", regName, id);
        break;
    case FLOATC:
        id = cgNewConstantLabel(FLOATC, &exprSemVal->constEvalValue.fValue);
        constValueNode->regIndex = getFreeRegister(REG_FLOAT);
        regName = spareRegister(REG_FLOAT, constValueNode->regIndex);
        writeCode("ldr     %s, _CONSTANT_%d\n", regName, id);
        break;
    case STRINGC:
        id = cgNewConstantLabel(STRINGC, constValueNode->semantic_value.const1->const_u.sc);
        constValueNode->regIndex = getFreeRegister(REG_INT);
        regName = spareRegister(REG_INT, constValueNode->regIndex);
        writeCode("ldr     %s, =_CONSTANT_%d\n", regName, id);
        break;
    default:
        break;
    }
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
                cgCode3(REG_INT, "sdiv", exprNode->regIndex, lhsNode->regIndex, rhsNode->regIndex);
                break;
            case BINARY_OP_EQ:
                cgCode2(REG_INT, "cmp", lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", exprNode->regIndex, "eq");
                break;
            case BINARY_OP_NE:
                cgCode2(REG_INT, "cmp", lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", exprNode->regIndex, "ne");
                break;
            case BINARY_OP_GT:
                cgCode2(REG_INT, "cmp", lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", exprNode->regIndex, "gt");
                break;
            case BINARY_OP_LT:
                cgCode2(REG_INT, "cmp", lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", exprNode->regIndex, "lt");
                break;
            case BINARY_OP_GE:
                cgCode2(REG_INT, "cmp", lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", exprNode->regIndex, "ge");
                break;
            case BINARY_OP_LE:
                cgCode2(REG_INT, "cmp", lhsNode->regIndex, rhsNode->regIndex);
                cgCode2s(REG_INT, "cset", exprNode->regIndex, "le");
                break;
            case BINARY_OP_AND: case BINARY_OP_OR:
                fprintf(stderr, "Unimplemented: binary logic operator\n");
                break;
            }
        } else if (exprNode->dataType == FLOAT_TYPE) {
            exprNode->regIndex = lhsNode->regIndex;
            int labelNumber;

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
                cgCode2(REG_FLOAT, "fcmp", lhsNode->regIndex, rhsNode->regIndex);
                labelNumber = getNewSerial(0);
                writeCode("bne     _bool_setZero_%d\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "1.0");
                writeCode("b       _bool_exitLabel_%d\n", labelNumber);
                writeCode("_bool_setZero_%d:\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "wzr");
                writeCode("_bool_exitLabel_%d:\n", labelNumber);
                break;
            case BINARY_OP_NE:
                cgCode2(REG_FLOAT, "fcmp", lhsNode->regIndex, rhsNode->regIndex);
                labelNumber = getNewSerial(0);
                writeCode("beq     _bool_setZero_%d\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "1.0");
                writeCode("b       _bool_exitLabel_%d\n", labelNumber);
                writeCode("_bool_setZero_%d:\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "wzr");
                writeCode("_bool_exitLabel_%d:\n", labelNumber);
                break;
            case BINARY_OP_GT:
                cgCode2(REG_FLOAT, "fcmpe", lhsNode->regIndex, rhsNode->regIndex);
                labelNumber = getNewSerial(0);
                writeCode("ble     _bool_setZero_%d\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "1.0");
                writeCode("b       _bool_exitLabel_%d\n", labelNumber);
                writeCode("_bool_setZero_%d:\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "wzr");
                writeCode("_bool_exitLabel_%d:\n", labelNumber);
                break;
            case BINARY_OP_LT:
                cgCode2(REG_FLOAT, "fcmpe", lhsNode->regIndex, rhsNode->regIndex);
                labelNumber = getNewSerial(0);
                writeCode("bpl     _bool_setZero_%d\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "1.0");
                writeCode("b       _bool_exitLabel_%d\n", labelNumber);
                writeCode("_bool_setZero_%d:\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "wzr");
                writeCode("_bool_exitLabel_%d:\n", labelNumber);
                break;
            case BINARY_OP_GE:
                cgCode2(REG_FLOAT, "fcmpe", lhsNode->regIndex, rhsNode->regIndex);
                labelNumber = getNewSerial(0);
                writeCode("blt     _bool_setZero_%d\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "1.0");
                writeCode("b       _bool_exitLabel_%d\n", labelNumber);
                writeCode("_bool_setZero_%d:\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "wzr");
                writeCode("_bool_exitLabel_%d:\n", labelNumber);
                break;
            case BINARY_OP_LE:
                cgCode2(REG_FLOAT, "fcmpe", lhsNode->regIndex, rhsNode->regIndex);
                labelNumber = getNewSerial(0);
                writeCode("bhi     _bool_setZero_%d\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "1.0");
                writeCode("b       _bool_exitLabel_%d\n", labelNumber);
                writeCode("_bool_setZero_%d:\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", lhsNode->regIndex, "wzr");
                writeCode("_bool_exitLabel_%d:\n", labelNumber);
                break;
            case BINARY_OP_AND: case BINARY_OP_OR:
                fprintf(stderr, "Unimplemented: binary logic operator\n");
                break;
            }
        } else {
            printf("exprNode datatype (%d) is neither int nor float\n", exprNode->dataType);
        }
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
            int labelNumber;
            switch (SEMVAL_EXPR(exprNode).op.unaryOp) {
            case UNARY_OP_NEGATIVE:
                cgCode2(REG_FLOAT, "fneg", exprNode->regIndex, operand->regIndex);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                cgCode2s(REG_FLOAT, "fcmp", operand->regIndex, "#0.0");
                labelNumber = getNewSerial(0);
                writeCode("bne     _bool_setZero_%d\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", exprNode->regIndex, "1.0");
                writeCode("b       _bool_exitLabel_%d\n", labelNumber);
                writeCode("_bool_setZero_%d:\n", labelNumber);
                cgCode2s(REG_FLOAT, "fmov", exprNode->regIndex, "wzr");
                writeCode("_bool_exitLabel_%d:\n", labelNumber);
                break;
            case UNARY_OP_POSITIVE: break;
            }
        }
    }
}

void cgVariableRValue(AST_NODE* idNode) {
    SymbolAttribute* attr = getIdNodeEntry(idNode)->attribute;
    const char* dimRegName = NULL;
    int dimRegIdx = -1;

    if (SEMVAL_ID(idNode).kind == ARRAY_ID) {
        int dimension = 0;
        AST_NODE *dimNode = idNode->child;

        while (dimNode) {
            dimension++;
            if (dimension > 1) {
                fprintf(stderr, "Multi-dimensional array rval\n");
                break;
            }
            cgExprRelatedNode(dimNode);
            dimRegName = spareRegister(REG_INT, dimNode->regIndex);
            dimRegIdx = dimNode->regIndex;
            dimNode = dimNode->rightSibling;
        }

        writeCode("lsl     %s, %s, 2\n", dimRegName, dimRegName);
    }

    switch (SEMVAL_ID(idNode).kind) {
    case NORMAL_ID:
    case ARRAY_ID:
        if (idNode->dataType == INT_TYPE) {
            idNode->regIndex = getFreeRegister(REG_INT);
            const char* regLoad;

            if (getIdNodeEntry(idNode)->nestingLevel == 0) {
                // global
                // const char* regTmp = WORK_REG(REG_INT, 1);
                const char* regTmp = "x17";
                regLoad = spareRegister(REG_INT, idNode->regIndex);
                writeCode("ldr     %s, =_g_%s\n", regTmp, SEMVAL_ID(idNode).identifierName);
                if (dimRegName) {
                    const char* regGen = spareRegister(REG_GENERAL, dimRegIdx);
                    writeCode("sxtw    %s, %s\n", regGen, dimRegName);
                    writeCode("add     %s, %s, %s\n", regTmp, regTmp, regGen);
                }
                writeCode("ldr     %s, [%s]\n", regLoad, regTmp);
            } else {
                // local
                regLoad = spareRegister(REG_INT, idNode->regIndex);
                if (dimRegName) {
                    const char* regGen = spareRegister(REG_GENERAL, dimRegIdx);
                    writeCode("add     %s, %s, #%d\n", dimRegName, dimRegName, attr->offset);
                    writeCode("sxtw    %s, %s\n", regGen, dimRegName);
                    writeCode("ldr     %s, [x29, %s]\n", regLoad, regGen);
                } else {
                    writeCode("ldr     %s, [x29, #%d]\n", regLoad, attr->offset);
                }
            }
            // TODO: save if spills
        } else if (idNode->dataType == FLOAT_TYPE) {
            idNode->regIndex = getFreeRegister(REG_FLOAT);
            const char* regLoad;

            if (getIdNodeEntry(idNode)->nestingLevel == 0) {
                // global
                const char* regTmp = "x17";
                regLoad = spareRegister(REG_FLOAT, idNode->regIndex);
                writeCode("ldr     %s, =_g_%s\n", regTmp, SEMVAL_ID(idNode).identifierName);
                if (dimRegName) {
                    const char* regGen = spareRegister(REG_GENERAL, dimRegIdx);
                    writeCode("sxtw    %s, %s\n", regGen, dimRegName);
                    writeCode("add     %s, %s, %s\n", regTmp, regTmp, regGen);
                }
                writeCode("ldr     %s, [%s]\n", regLoad, regTmp);
            } else {
                // local
                regLoad = spareRegister(REG_FLOAT, idNode->regIndex);
                if (dimRegName) {
                    const char* regGen = spareRegister(REG_GENERAL, dimRegIdx);
                    writeCode("add     %s, %s, #%d\n", dimRegName, dimRegName, attr->offset);
                    writeCode("sxtw    %s, %s\n", regGen, dimRegName);
                    writeCode("ldr     %s, [x29, %s]\n", regLoad, regGen);
                } else {
                    writeCode("ldr     %s, [x29, #%d]\n", regLoad, attr->offset);
                }
            }
            // TODO: save if spills
        }

        break;
    default: __builtin_unreachable();
    }

    if (dimRegName) {
        freeRegister(REG_INT, dimRegIdx);
    }
}
