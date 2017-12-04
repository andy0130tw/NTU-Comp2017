#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
// This file is for reference only, you are not required to follow the implementation. //
// You only need to check for errors stated in the hw4 assignment document. //
int g_anyErrorOccur = 0;

DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2);
void processProgramNode(AST_NODE *programNode);
void processDeclarationNode(AST_NODE* declarationNode);
void declareIdList(AST_NODE* typeNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize);
void declareFunction(AST_NODE* returnTypeNode);
void processDeclDimList(AST_NODE* variableDeclDimList, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void processTypeNode(AST_NODE* typeNode);
void processBlockNode(AST_NODE* blockNode);
void processStmtNode(AST_NODE* stmtNode);
void processGeneralNode(AST_NODE *node);
void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode);
void checkWhileStmt(AST_NODE* whileNode);
void checkForStmt(AST_NODE* forNode);
void checkAssignmentStmt(AST_NODE* assignmentNode);
void checkIfStmt(AST_NODE* ifNode);
void checkWriteFunction(AST_NODE* functionCallNode);
void checkFunctionCall(AST_NODE* functionCallNode);
void processExprRelatedNode(AST_NODE* exprRelatedNode);
void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter);
void checkReturnStmt(AST_NODE* returnNode);
void processExprNode(AST_NODE* exprNode);
void processVariableLValue(AST_NODE* idNode);
void processVariableRValue(AST_NODE* idNode);
void processConstValueNode(AST_NODE* constValueNode);
void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue);
void evaluateExprValue(AST_NODE* exprNode);


typedef enum ErrorMsgKind {
    SYMBOL_IS_NOT_TYPE,  // (1)
    SYMBOL_REDECLARE,  // 1.b
    SYMBOL_UNDECLARED,  // 1.a
    NOT_FUNCTION_NAME,
    TRY_TO_INIT_ARRAY,  // (3)
    EXCESSIVE_ARRAY_DIM_DECLARATION,  // (4)
    RETURN_ARRAY,
    VOID_VARIABLE,
    TYPEDEF_VOID_ARRAY,
    PARAMETER_TYPE_UNMATCH,
    TOO_FEW_ARGUMENTS,  // 2.a.1
    TOO_MANY_ARGUMENTS,  // 2.a.2
    RETURN_TYPE_UNMATCH,  // 2.b
    INCOMPATIBLE_ARRAY_DIMENSION,  // 3.a
    NOT_ASSIGNABLE,
    NOT_ARRAY,
    IS_TYPE_NOT_VARIABLE,
    IS_FUNCTION_NOT_VARIABLE,
    STRING_OPERATION,
    ARRAY_SIZE_NOT_INT,
    ARRAY_SIZE_NEGATIVE,
    ARRAY_SUBSCRIPT_NOT_INT,  // 3.b
    PASS_ARRAY_TO_SCALAR,  // 3.c.1
    PASS_SCALAR_TO_ARRAY   // 3.c.2
} ErrorMsgKind;

#define getIdNodeEntry(node)  ((node)->semantic_value.identifierSemanticValue.symbolTableEntry)

void printErrorMsgSpecial(AST_NODE* node1, char* name2, ErrorMsgKind errorMsgKind) {
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node1->linenumber);

    // only identifier nodes can be named
    char* name1 = node1->nodeType == IDENTIFIER_NODE ?
        node1->semantic_value.identifierSemanticValue.identifierName : (char*) "expression";

    switch(errorMsgKind)
    {
    case PASS_SCALAR_TO_ARRAY:
        printf("Scalar %s passed to array parameter '%s'.\n", name1, name2);
        break;
    case PASS_ARRAY_TO_SCALAR:
        printf("Array %s passed to scalar parameter '%s'.\n", name1, name2);
        break;
    default:
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind)\n");
        break;
    }

}


void printErrorMsg(AST_NODE* node, ErrorMsgKind errorMsgKind) {
    g_anyErrorOccur = 1;
    printf("Error found in line %d\n", node->linenumber);

    // only identifier nodes can be named
    char* id = node->nodeType == IDENTIFIER_NODE ?
        node->semantic_value.identifierSemanticValue.identifierName : (char*) "expression";

    switch(errorMsgKind)
    {
    case SYMBOL_UNDECLARED:
        printf("ID %s undeclared.\n", id);
        break;
    case SYMBOL_REDECLARE:
        printf("ID %s redeclared.\n", id);
        break;
    case TOO_FEW_ARGUMENTS:
        printf("too few arguments to function %s.\n", id);
        break;
    case TOO_MANY_ARGUMENTS:
        printf("too many arguments to function %s.\n", id);
        break;
    case RETURN_TYPE_UNMATCH:
        printf("Incompatible return type.\n");
        break;
    case INCOMPATIBLE_ARRAY_DIMENSION:
        printf("Incompatible array dimensions.\n");
        break;
    case ARRAY_SUBSCRIPT_NOT_INT:
        printf("Array subscript is not an integer.\n");
        break;
    /***** other messages not covered in HW4 spec *****/
    case SYMBOL_IS_NOT_TYPE:    printf("ID %s does not name a type.\n", id); break;
    case NOT_FUNCTION_NAME:     printf("ID %s is not a function.\n", id); break;
    case TRY_TO_INIT_ARRAY:     printf("Try to initialize variable %s as an array.\n", id); break;
    case EXCESSIVE_ARRAY_DIM_DECLARATION:
                                printf("Cannot declare array %s of excess dimensions.\n", id); break;
    case RETURN_ARRAY:          printf("Returning array from function %s\n", id); break;
    case VOID_VARIABLE:         printf("Declaring variable(s) to void type.\n"); break;
    // case TYPEDEF_VOID_ARRAY: printf(""); break;
    case PARAMETER_TYPE_UNMATCH:
                                printf("Conflicting parameter type.\n"); break;
    case NOT_ASSIGNABLE:        printf("The expression is not assignable.\n"); break;
    case NOT_ARRAY:             printf("ID %s is not an array.\n", id); break;
    case IS_TYPE_NOT_VARIABLE:  printf("ID %s is declared as a type, not a variable.\n", id); break;
    case IS_FUNCTION_NOT_VARIABLE:
                                printf("ID %s is declared as a function, not a variable.\n", id); break;
    case STRING_OPERATION:      printf("String constant is invalid as an operand.\n"); break;
    case ARRAY_SIZE_NOT_INT:    printf("Array size is not an integer.\n"); break;
    // case ARRAY_SIZE_NEGATIVE: printf(""); break;

    default:
        printf("Unhandled case in void printErrorMsg(AST_NODE* node, ERROR_MSG_KIND* errorMsgKind=%d)\n", errorMsgKind);
        break;
    }
}


void semanticAnalysis(AST_NODE *root) {
    processProgramNode(root);
}


DATA_TYPE getBiggerType(DATA_TYPE dataType1, DATA_TYPE dataType2) {
    if (dataType1 == FLOAT_TYPE || dataType2 == FLOAT_TYPE) {
        return FLOAT_TYPE;
    } else {
        return INT_TYPE;
    }
}


void processProgramNode(AST_NODE *programNode) {
    AST_NODE* child = programNode->child;

    // if there is no error, the type should remain none
    programNode->dataType = NONE_TYPE;

    while (child) {
        // top level; varDeclList (a list of variables)
        //            or a (function) declaration
        if (child->nodeType == VARIABLE_DECL_LIST_NODE) {
            processGeneralNode(child);
        } else {
            processDeclarationNode(child);
        }

        if (child->dataType == ERROR_TYPE) {
            programNode->dataType = ERROR_TYPE;
        }

        child = child->rightSibling;
    }
}

void processDeclarationNode(AST_NODE* declarationNode) {
    AST_NODE* typeNode = declarationNode->child;
    processTypeNode(typeNode);

    if (typeNode->dataType == ERROR_TYPE) {
        declarationNode->dataType = ERROR_TYPE;
        return;
    }

    switch (declarationNode->semantic_value.declSemanticValue.kind) {
    case VARIABLE_DECL:
        return declareIdList(declarationNode, VARIABLE_ATTRIBUTE, 0);
    case TYPE_DECL:
        return declareIdList(declarationNode, TYPE_ATTRIBUTE, 0);
    case FUNCTION_DECL:
        return declareFunction(declarationNode);
    case FUNCTION_PARAMETER_DECL:
        return declareIdList(declarationNode, VARIABLE_ATTRIBUTE, 1);
    }
}


void processTypeNode(AST_NODE* idNodeAsType) {
    IdentifierSemanticValue* idSemVal = &idNodeAsType->semantic_value.identifierSemanticValue;
    char* id = idSemVal->identifierName;
    SymbolTableEntry* symEntry = retrieveSymbol(id);

    if (!symEntry) {
        printErrorMsg(idNodeAsType, SYMBOL_UNDECLARED);
        idNodeAsType->dataType = ERROR_TYPE;
    } else if (symEntry->attribute->attributeKind != TYPE_ATTRIBUTE) {
        printErrorMsg(idNodeAsType, SYMBOL_IS_NOT_TYPE);
        idNodeAsType->dataType = ERROR_TYPE;
    } else {
        idSemVal->symbolTableEntry = symEntry;
        // determine variable data type
        TypeDescriptor* typeDesc = symEntry->attribute->attr.typeDescriptor;
        switch (typeDesc->kind) {
        case SCALAR_TYPE_DESCRIPTOR:
            idNodeAsType->dataType = typeDesc->properties.dataType;
            break;
        case ARRAY_TYPE_DESCRIPTOR:
            idNodeAsType->dataType = typeDesc->properties.arrayProperties.elementType;
            break;
        }
    }
}

static inline void checkInitialType(AST_NODE* idNode, AST_NODE* declarationNode) {
    AST_NODE *rhsExpr = idNode->child;
    processExprRelatedNode(rhsExpr);

    int success = 0;
    switch (rhsExpr->dataType) {
    case INT_PTR_TYPE: case FLOAT_PTR_TYPE:
        printErrorMsg(idNode, TRY_TO_INIT_ARRAY);
        break;
    case CONST_STRING_TYPE:
        printErrorMsg(idNode, NOT_ASSIGNABLE);
        break;
    case VOID_TYPE: case NONE_TYPE:
    case ERROR_TYPE:
        break;
    case INT_TYPE: case FLOAT_TYPE:
        success = 1;
        break;
    }

    if (!success) {
        declarationNode->dataType = ERROR_TYPE;
    }
}

void declareIdList(AST_NODE* declarationNode, SymbolAttributeKind isVariableOrTypeAttribute, int ignoreArrayFirstDimSize) {
    AST_NODE* typeNode = declarationNode->child;
    AST_NODE* idNode = typeNode->rightSibling;

    SymbolTableEntry* typeEntry = getIdNodeEntry(typeNode);
    TypeDescriptor *typeDesc = typeEntry->attribute->attr.typeDescriptor;

    declarationNode->dataType = NONE_TYPE;

    if (isVariableOrTypeAttribute == VARIABLE_ATTRIBUTE &&
        typeDesc->kind == SCALAR_TYPE_DESCRIPTOR && typeDesc->properties.dataType == VOID_TYPE) {
        // declare variable(s) as void
        printErrorMsg(typeNode, VOID_VARIABLE);
        typeNode->dataType = ERROR_TYPE;
        declarationNode->dataType = ERROR_TYPE;
        return;
    }

    while (idNode) {
        char* id = idNode->semantic_value.identifierSemanticValue.identifierName;
        IdentifierSemanticValue* idSemVal = &idNode->semantic_value.identifierSemanticValue;

        if (declaredLocally(id)) {
            printErrorMsg(idNode, SYMBOL_REDECLARE);
            goto child_fail;
        }

        SymbolAttribute* attr;
        TypeDescriptor tmpTypeDesc;

        switch (idSemVal->kind) {
        case NORMAL_ID:
            // copy the type descriptor from type
            attr = allocateSymbolAttribute(isVariableOrTypeAttribute, (void*) typeDesc);
            break;
        case ARRAY_ID:
            // array is complicated

            if (isVariableOrTypeAttribute == TYPE_ATTRIBUTE &&
                typeDesc->kind == SCALAR_TYPE_DESCRIPTOR && typeDesc->properties.dataType == VOID_TYPE) {
                printErrorMsg(idNode, TYPEDEF_VOID_ARRAY);
                idNode->dataType = ERROR_TYPE;
                declarationNode->dataType = ERROR_TYPE;
                return;
            }

            processDeclDimList(idNode, &tmpTypeDesc, ignoreArrayFirstDimSize);
            if (idNode->dataType == ERROR_TYPE) {
                // failed to process this decl
                goto child_fail;
            } else if (typeDesc->kind == SCALAR_TYPE_DESCRIPTOR) {
                // array of scalars
                tmpTypeDesc.properties.arrayProperties.elementType = typeDesc->properties.dataType;
            } else {  // typeDesc->kind == ARRAY_TYPE_DESCRIPTOR
                // array of arrays
                int dimFromType = typeDesc->properties.arrayProperties.dimension;
                int dimFromDecl = tmpTypeDesc.properties.arrayProperties.dimension;

                if (dimFromDecl + dimFromType > MAX_ARRAY_DIMENSION) {
                    printErrorMsg(idNode, EXCESSIVE_ARRAY_DIM_DECLARATION);
                    goto child_fail;
                }

                ArrayProperties* arrayProp = &tmpTypeDesc.properties.arrayProperties;

                arrayProp->elementType = typeDesc->properties.dataType;
                arrayProp->dimension = dimFromType + dimFromDecl;

                // copy dimension from type
                for (int idxForType = 0; idxForType < dimFromType; idxForType++) {
                    arrayProp->sizeInEachDimension[dimFromDecl + idxForType] =
                        typeDesc->properties.arrayProperties.sizeInEachDimension[idxForType];
                }
            }

            attr = allocateSymbolAttribute(isVariableOrTypeAttribute, (void*) &tmpTypeDesc);
            break;
        case WITH_INIT_ID:
            if (typeDesc->kind == ARRAY_TYPE_DESCRIPTOR) {
                printErrorMsg(idNode, TRY_TO_INIT_ARRAY);
                goto child_fail;
            }
            attr = allocateSymbolAttribute(isVariableOrTypeAttribute, (void*) typeDesc);
            checkInitialType(idNode, declarationNode);
            break;
        default: __builtin_unreachable();
        }

        idNode->semantic_value.identifierSemanticValue.symbolTableEntry =
            enterSymbol(id, attr);

        goto child_next;

    child_fail:
        typeNode->dataType = ERROR_TYPE;
        declarationNode->dataType = ERROR_TYPE;

    child_next:
        idNode = idNode->rightSibling;
    }
}

void checkAssignOrExpr(AST_NODE* assignOrExprRelatedNode) {
    if (assignOrExprRelatedNode->nodeType == STMT_NODE) {
        STMT_KIND stmtType = assignOrExprRelatedNode->semantic_value.stmtSemanticValue.kind;
        if (stmtType == ASSIGN_STMT) {
            checkAssignmentStmt(assignOrExprRelatedNode);
        } else if (stmtType == FUNCTION_CALL_STMT) {
            checkFunctionCall(assignOrExprRelatedNode);
        }
    } else {
        processExprRelatedNode(assignOrExprRelatedNode);
    }
}

void checkWhileStmt(AST_NODE* whileNode) {
    AST_NODE* condNode  = whileNode->child;
    AST_NODE* blockNode = condNode->rightSibling;
    checkAssignOrExpr(condNode);
    processStmtNode(blockNode);
}


void checkForStmt(AST_NODE* forNode) {
    AST_NODE* initStmtNode = forNode->child;
    AST_NODE* condExprNode = initStmtNode->rightSibling;
    AST_NODE* postStmtNode = condExprNode->rightSibling;
    AST_NODE* blockNode    = postStmtNode->rightSibling;
    processGeneralNode(initStmtNode);
    processGeneralNode(condExprNode);
    processGeneralNode(postStmtNode);
    processStmtNode(blockNode);
}


void checkAssignmentStmt(AST_NODE* assignmentNode) {
    AST_NODE* lhsNode = assignmentNode->child;
    AST_NODE* rhsNode = lhsNode->rightSibling;
    processVariableLValue(lhsNode);
    if (lhsNode->dataType == ERROR_TYPE) {
        goto stmt_fail;
    }
    processExprRelatedNode(rhsNode);
    if (rhsNode->dataType == ERROR_TYPE) {
        goto stmt_fail;
    }

    if (rhsNode->dataType == INT_PTR_TYPE || rhsNode->dataType == FLOAT_PTR_TYPE) {
        printErrorMsg(rhsNode, INCOMPATIBLE_ARRAY_DIMENSION);
        goto stmt_fail;
    }
    if (rhsNode->dataType == CONST_STRING_TYPE) {
        printErrorMsg(rhsNode, NOT_ASSIGNABLE);
        goto stmt_fail;
    }

    assignmentNode->dataType = lhsNode->dataType;
    return;

stmt_fail:
    assignmentNode->dataType = ERROR_TYPE;
}


void checkIfStmt(AST_NODE* ifNode) {
    AST_NODE* condNode      = ifNode->child;
    AST_NODE* ifBlockNode   = condNode->rightSibling;
    AST_NODE* elseBlockNode = ifBlockNode->rightSibling;
    checkAssignOrExpr(condNode);
    processStmtNode(ifBlockNode);
    processStmtNode(elseBlockNode);
}

void checkWriteFunction(AST_NODE* functionCallNode) {
    AST_NODE* funcNameNode = functionCallNode->child;
    AST_NODE* callParamListNode = funcNameNode->rightSibling;
    AST_NODE* callParamNode = callParamListNode->child;

    processGeneralNode(callParamListNode);

    if (!callParamNode) {
        printErrorMsg(funcNameNode, TOO_FEW_ARGUMENTS);
        return;
    }

    if (callParamNode->dataType == ERROR_TYPE) {
        functionCallNode->dataType = ERROR_TYPE;
        return;
    }

    if (callParamNode->dataType != INT_TYPE &&
        callParamNode->dataType != FLOAT_TYPE &&
        callParamNode->dataType != CONST_STRING_TYPE) {
        printErrorMsg(funcNameNode, PARAMETER_TYPE_UNMATCH);
        functionCallNode->dataType = ERROR_TYPE;
        return;
    }

    if (callParamNode->rightSibling) {
        printErrorMsg(funcNameNode, TOO_MANY_ARGUMENTS);
        functionCallNode->dataType = ERROR_TYPE;
        return;
    }

    functionCallNode->dataType = VOID_TYPE;
}

void checkFunctionCall(AST_NODE* functionCallNode) {
    AST_NODE* funcNameNode = functionCallNode->child;
    char* id = funcNameNode->semantic_value.identifierSemanticValue.identifierName;

    if (strcmp(id, SYMBOL_TABLE_SYS_LIB_WRITE) == 0) {
        return checkWriteFunction(functionCallNode);
    }

    SymbolTableEntry* symEntry = retrieveSymbol(id);
    AST_NODE* callParamListNode = funcNameNode->rightSibling;
    AST_NODE* callParamNode = callParamListNode->child;
    Parameter* declParam;

    if (!symEntry) {
        printErrorMsg(funcNameNode, SYMBOL_UNDECLARED);
        goto func_call_fail;
    } else if (symEntry->attribute->attributeKind != FUNCTION_SIGNATURE) {
        printErrorMsg(funcNameNode, NOT_FUNCTION_NAME);
        goto func_call_fail;
    }

    processGeneralNode(callParamListNode);
    declParam = symEntry->attribute->attr.functionSignature->parameterList;

    while (callParamNode && declParam) {
        if (callParamNode->dataType == ERROR_TYPE) {
            goto func_call_fail;
        }
        checkParameterPassing(declParam, callParamNode);

        callParamNode = callParamNode->rightSibling;
        declParam = declParam->next;
    }

    if (callParamNode) {
        printErrorMsg(funcNameNode, TOO_MANY_ARGUMENTS);
    } else if (declParam) {
        printErrorMsg(funcNameNode, TOO_FEW_ARGUMENTS);
    }
    return;

func_call_fail:
    functionCallNode->dataType = ERROR_TYPE;
}

void checkParameterPassing(Parameter* formalParameter, AST_NODE* actualParameter) {
    int isPassingArray = actualParameter->dataType == INT_PTR_TYPE ||
                         actualParameter->dataType == FLOAT_PTR_TYPE;

    if (actualParameter->dataType == CONST_STRING_TYPE) {
        printErrorMsg(actualParameter, NOT_ASSIGNABLE);
        return;
    }

    if (formalParameter->type->kind == SCALAR_TYPE_DESCRIPTOR && isPassingArray) {
        printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_ARRAY_TO_SCALAR);
        actualParameter->dataType = ERROR_TYPE;
    } else if (formalParameter->type->kind == ARRAY_TYPE_DESCRIPTOR && !isPassingArray) {
        printErrorMsgSpecial(actualParameter, formalParameter->parameterName, PASS_SCALAR_TO_ARRAY);
        actualParameter->dataType = ERROR_TYPE;
    }
}


void processExprRelatedNode(AST_NODE* exprRelatedNode) {
    switch(exprRelatedNode->nodeType) {
    case CONST_VALUE_NODE:
        processConstValueNode(exprRelatedNode);
        break;
    case IDENTIFIER_NODE:
        processVariableRValue(exprRelatedNode);
        break;
    case EXPR_NODE:
        processExprNode(exprRelatedNode);
        break;
    case STMT_NODE:
        checkFunctionCall(exprRelatedNode);
        break;
    default:
        fprintf(stderr, "[%s:%d] Unhandled case when node type=%d\n",
            __func__, __LINE__, exprRelatedNode->nodeType);
        exprRelatedNode->dataType = ERROR_TYPE;
        break;
    }
}

void getExprOrConstValue(AST_NODE* exprOrConstNode, int* iValue, float* fValue) {
    if (exprOrConstNode->nodeType == CONST_VALUE_NODE) {
        CON_Type* constVal = exprOrConstNode->semantic_value.const1;
        if (exprOrConstNode->dataType == INT_TYPE) {
            if (fValue) {
                *fValue = constVal->const_u.intval;
            } else {
                *iValue = constVal->const_u.intval;
            }
        } else {
            *fValue = constVal->const_u.fval;
        }
    } else {
        EXPRSemanticValue* semVal = &exprOrConstNode->semantic_value.exprSemanticValue;
        if (exprOrConstNode->dataType == INT_TYPE) {
            if (fValue) {
                *fValue = semVal->constEvalValue.iValue;
            } else {
                *iValue = semVal->constEvalValue.iValue;
            }
        } else {
            *fValue = semVal->constEvalValue.fValue;
        }
    }
}

void evaluateExprValue(AST_NODE* exprNode) {
    // not needed for now?
}

static inline int isConstExpr(AST_NODE* exprNode) {
    return exprNode->nodeType == CONST_VALUE_NODE ||
          (exprNode->nodeType == EXPR_NODE && exprNode->semantic_value.exprSemanticValue.isConstEval);
}

void processExprNode(AST_NODE* exprNode) {
    if (exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION) {
        AST_NODE* lhsNode = exprNode->child;
        AST_NODE* rhsNode = lhsNode->rightSibling;

        processExprRelatedNode(lhsNode);
        processExprRelatedNode(rhsNode);

        // operators can only be used between ints and floats

        if (lhsNode->dataType == CONST_STRING_TYPE || rhsNode->dataType == CONST_STRING_TYPE) {
            printErrorMsg(exprNode, STRING_OPERATION);
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        if (lhsNode->dataType == INT_PTR_TYPE || lhsNode->dataType == FLOAT_PTR_TYPE ||
            rhsNode->dataType == INT_PTR_TYPE || rhsNode->dataType == FLOAT_PTR_TYPE) {
            printErrorMsg(lhsNode, INCOMPATIBLE_ARRAY_DIMENSION);
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        if (lhsNode->dataType != INT_TYPE && lhsNode->dataType != FLOAT_TYPE &&
            rhsNode->dataType != INT_TYPE && rhsNode->dataType != FLOAT_TYPE) {
            // fails silently
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        switch (exprNode->semantic_value.exprSemanticValue.op.binaryOp) {
        case BINARY_OP_EQ: case BINARY_OP_NE: case BINARY_OP_GE: case BINARY_OP_LE:
        case BINARY_OP_GT: case BINARY_OP_LT: case BINARY_OP_AND: case BINARY_OP_OR:
            exprNode->dataType = INT_TYPE;  // 0/1
            break;
        default:
            exprNode->dataType = getBiggerType(lhsNode->dataType, rhsNode->dataType);
            break;
        }

        if (isConstExpr(lhsNode) && isConstExpr(rhsNode)) {
            // constant folding
            evaluateExprValue(exprNode);
            exprNode->semantic_value.exprSemanticValue.isConstEval = 1;
        }
    } else {
        // unary operators

        AST_NODE* operand = exprNode->child;
        processExprRelatedNode(operand);

        if (operand->dataType == CONST_STRING_TYPE) {
            printErrorMsg(exprNode, STRING_OPERATION);
            exprNode->dataType = ERROR_TYPE;
        }

        if (operand->dataType == INT_PTR_TYPE || operand->dataType == FLOAT_PTR_TYPE) {
            printErrorMsg(operand, INCOMPATIBLE_ARRAY_DIMENSION);
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        if (operand->dataType != INT_TYPE && operand->dataType != FLOAT_TYPE) {
            exprNode->dataType = ERROR_TYPE;
            return;
        }

        if (exprNode->semantic_value.exprSemanticValue.op.unaryOp == UNARY_OP_LOGICAL_NEGATION) {
            exprNode->dataType = INT_TYPE;
        } else {
            exprNode->dataType = operand->dataType;
        }

        if (isConstExpr(exprNode)) {
            evaluateExprValue(exprNode);
            exprNode->semantic_value.exprSemanticValue.isConstEval = 1;
        }
    }
}


void processVariableLValue(AST_NODE* idNode) {
    SymbolTableEntry *symEntry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
    idNode->semantic_value.identifierSemanticValue.symbolTableEntry = symEntry;
    if (!symEntry) {
        printErrorMsg(idNode, SYMBOL_UNDECLARED);
        idNode->dataType = ERROR_TYPE;
        return;
    }

    SymbolAttributeKind symKind = symEntry->attribute->attributeKind;

    if (symKind == TYPE_ATTRIBUTE) {
        printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    } else if (symKind == FUNCTION_SIGNATURE) {
        printErrorMsg(idNode, IS_FUNCTION_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    }

    TypeDescriptor *typeDesc = getIdNodeEntry(idNode)->attribute->attr.typeDescriptor;

    switch (idNode->semantic_value.identifierSemanticValue.kind) {
    case NORMAL_ID:
        if (typeDesc->kind == ARRAY_TYPE_DESCRIPTOR) {
            printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
            idNode->dataType = ERROR_TYPE;
        } else {
            idNode->dataType = typeDesc->properties.dataType;
        }
        break;
    case ARRAY_ID: {
        if (typeDesc->kind == SCALAR_TYPE_DESCRIPTOR) {
            printErrorMsg(idNode, NOT_ARRAY);
            idNode->dataType = ERROR_TYPE;
            break;
        }

        int dimension = 0;
        AST_NODE *dimNode = idNode->child;
        while (dimNode) {
            dimension++;
            processExprRelatedNode(dimNode);
            if (dimNode->dataType == ERROR_TYPE) {
                idNode->dataType = ERROR_TYPE;
            } else if (dimNode->dataType == FLOAT_TYPE) {
                printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
                idNode->dataType = ERROR_TYPE;
            }
            dimNode = dimNode->rightSibling;
        }

        if (dimension == typeDesc->properties.arrayProperties.dimension) {
            idNode->dataType = typeDesc->properties.arrayProperties.elementType;
        } else {
            printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
            idNode->dataType = ERROR_TYPE;
        }
        break;
    }
    default: __builtin_unreachable();
    }
}

void processVariableRValue(AST_NODE* idNode) {
    SymbolTableEntry *symEntry = retrieveSymbol(idNode->semantic_value.identifierSemanticValue.identifierName);
    idNode->semantic_value.identifierSemanticValue.symbolTableEntry = symEntry;
    if (!symEntry) {
        printErrorMsg(idNode, SYMBOL_UNDECLARED);
        idNode->dataType = ERROR_TYPE;
        return;
    }

    SymbolAttributeKind symKind = symEntry->attribute->attributeKind;

    if (symKind == TYPE_ATTRIBUTE) {
        printErrorMsg(idNode, IS_TYPE_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    } else if (symKind == FUNCTION_SIGNATURE) {
        printErrorMsg(idNode, IS_FUNCTION_NOT_VARIABLE);
        idNode->dataType = ERROR_TYPE;
        return;
    }

    TypeDescriptor *typeDesc = getIdNodeEntry(idNode)->attribute->attr.typeDescriptor;
    // ... above is copied from lvalue ...

    switch (idNode->semantic_value.identifierSemanticValue.kind) {
    case NORMAL_ID:
        if (typeDesc->kind == ARRAY_TYPE_DESCRIPTOR) {
            // array IS allowed in rvalue
            idNode->dataType = typeDesc->properties.arrayProperties.elementType == INT_TYPE ?
                               INT_PTR_TYPE : FLOAT_TYPE;
        } else {
            idNode->dataType = typeDesc->properties.dataType;
        }
        break;
    case ARRAY_ID: {
        if (typeDesc->kind == SCALAR_TYPE_DESCRIPTOR) {
            printErrorMsg(idNode, NOT_ARRAY);
            idNode->dataType = ERROR_TYPE;
            break;
        }

        int dimension = 0;
        AST_NODE *dimNode = idNode->child;
        while (dimNode) {
            dimension++;
            processExprRelatedNode(dimNode);
            if (dimNode->dataType == ERROR_TYPE) {
                idNode->dataType = ERROR_TYPE;
            } else if (dimNode->dataType == FLOAT_TYPE) {
                printErrorMsg(idNode, ARRAY_SUBSCRIPT_NOT_INT);
                idNode->dataType = ERROR_TYPE;
            }
            dimNode = dimNode->rightSibling;
        }

        ArrayProperties* arrProp = &typeDesc->properties.arrayProperties;
        int dimSpecifiedByType = arrProp->dimension;
        if (dimension == dimSpecifiedByType) {
            idNode->dataType = arrProp->elementType;
        } else if (dimension < dimSpecifiedByType) {
            // array slices IS allowed in rvalue
            idNode->dataType = arrProp->elementType == INT_TYPE ?
                               INT_PTR_TYPE : FLOAT_PTR_TYPE;
        } else {
            printErrorMsg(idNode, INCOMPATIBLE_ARRAY_DIMENSION);
            idNode->dataType = ERROR_TYPE;
        }
        break;
    }
    default: __builtin_unreachable();
    }
}


void processConstValueNode(AST_NODE* constValueNode) {
    EXPRSemanticValue* exprSemVal = &constValueNode->semantic_value.exprSemanticValue;
    switch(constValueNode->semantic_value.const1->const_type) {
    case INTEGERC:
        constValueNode->dataType = INT_TYPE;
        exprSemVal->constEvalValue.iValue = constValueNode->semantic_value.const1->const_u.intval;
        break;
    case FLOATC:
        constValueNode->dataType = FLOAT_TYPE;
        exprSemVal->constEvalValue.fValue = constValueNode->semantic_value.const1->const_u.fval;
        break;
    case STRINGC:
        constValueNode->dataType = CONST_STRING_TYPE;
        break;
    default:
        constValueNode->dataType = ERROR_TYPE;
        break;
    }
}


void checkReturnStmt(AST_NODE* returnNode) {
    AST_NODE* parentNode = returnNode->parent;
    DATA_TYPE returnType = NONE_TYPE;

    // find the innermost function scope; the return type lies there
    while (parentNode) {
        if (parentNode->nodeType == DECLARATION_NODE) {
            if (parentNode->semantic_value.declSemanticValue.kind == FUNCTION_DECL) {
                returnType = parentNode->child->dataType;
            }
            break;
        }
        parentNode = parentNode->parent;
    }

    if (returnNode->child->nodeType == NUL_NODE && returnType != VOID_TYPE) {
        // when return NUL_NODE is permitted in grammer, it is only allowed to return void
        goto ret_fail;
    }

    processExprRelatedNode(returnNode->child);
    if (returnType != returnNode->child->dataType) {
        // the only compatiable return types
        if (!((returnType == FLOAT_TYPE && returnNode->child->dataType == INT_TYPE) ||
              (returnType == INT_TYPE && returnNode->child->dataType == FLOAT_TYPE))) {
            goto ret_fail;
        }
        if (returnType != returnNode->child->dataType) {
            printf("Warning (line %d): Implicitly converting the type of return value.\n",
                returnNode->linenumber);
        }
    }
    returnNode->dataType = returnType;
    return;

ret_fail:
    // error is preferrably be reported only once
    if (returnNode->child->dataType != ERROR_TYPE) {
        printErrorMsg(returnNode, RETURN_TYPE_UNMATCH);
    }
    returnNode->dataType = ERROR_TYPE;
}


void processBlockNode(AST_NODE* blockNode) {
    openScope();

    AST_NODE* child = blockNode->child;
    while (child) {
        processGeneralNode(child);
        child = child->rightSibling;
    }

    closeScope();
}


void processStmtNode(AST_NODE* stmtNode) {
    if (stmtNode->nodeType == NUL_NODE) {
        return;
    }
    if (stmtNode->nodeType == BLOCK_NODE) {
        return processBlockNode(stmtNode);
    }

    switch (stmtNode->semantic_value.stmtSemanticValue.kind) {
    case ASSIGN_STMT:
        return checkAssignmentStmt(stmtNode);
    case WHILE_STMT:
        return checkWhileStmt(stmtNode);
    case FOR_STMT:
        return checkForStmt(stmtNode);
    case IF_STMT:
        return checkIfStmt(stmtNode);
    case FUNCTION_CALL_STMT:
        return checkFunctionCall(stmtNode);
    case RETURN_STMT:
        return checkReturnStmt(stmtNode);
    default:
        fprintf(stderr, "[%s:%d] Unhandled case of node type %d\n",
            __func__, __LINE__, stmtNode->semantic_value.stmtSemanticValue.kind);
        stmtNode->dataType = ERROR_TYPE;
    }
}


void processGeneralNode(AST_NODE *node) {
    AST_NODE* child = node->child;
    void (*checkFunc)(AST_NODE*);
    // whether the type should be inferred from children, or simply none
    int willDetermineType = 1;

    switch (node->nodeType) {
    case VARIABLE_DECL_LIST_NODE:
        checkFunc = processDeclarationNode; willDetermineType = 0; break;
    case STMT_LIST_NODE:
        checkFunc = processStmtNode; willDetermineType = 0; break;
    case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
        checkFunc = checkAssignOrExpr; break;
    case NONEMPTY_RELOP_EXPR_LIST_NODE:
        checkFunc = processExprRelatedNode; break;
    case NUL_NODE:  // no need to check
        return;
    default:
        fprintf(stderr, "[%s:%d] Unhandled node type (%d)\n",
            __func__, __LINE__, node->nodeType);
        node->dataType = ERROR_TYPE;
        return;
    }

    while (child) {
        checkFunc(child);
        if (child->dataType == ERROR_TYPE) {
            node->dataType = ERROR_TYPE;
        } else {
            node->dataType = willDetermineType ? child->dataType : NONE_TYPE;
        }
        child = child->rightSibling;
    }
}

void processDeclDimList(AST_NODE* idNode, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize) {
    AST_NODE* declDimList = idNode->child;
    AST_NODE* dimPtr = declDimList;
    typeDescriptor->kind = ARRAY_TYPE_DESCRIPTOR;
    ArrayProperties* arrProp = &typeDescriptor->properties.arrayProperties;

    int dimen = 0;

    // when ignoring, the supplied dimension must have a NUL_NODE
    if(ignoreFirstDimSize && dimPtr->nodeType == NUL_NODE) {
        arrProp->sizeInEachDimension[0] = -1;
        dimen++;
        dimPtr = dimPtr->rightSibling;
    }

    while (dimPtr) {
        if (dimen > MAX_ARRAY_DIMENSION) {
            printErrorMsg(idNode, EXCESSIVE_ARRAY_DIM_DECLARATION);
            idNode->dataType = ERROR_TYPE;
            break;
        }

        processExprRelatedNode(dimPtr);
        if (dimPtr->dataType == ERROR_TYPE) {
            goto arr_dim_fail;
        }
        if (dimPtr->dataType == FLOAT_TYPE) {
            printErrorMsg(idNode, ARRAY_SIZE_NOT_INT);
            goto arr_dim_fail;
        }
        // TODO: check if it is non-negative

        arrProp->sizeInEachDimension[dimen] =  dimPtr->semantic_value.exprSemanticValue.constEvalValue.iValue;

        dimen++;
        dimPtr = dimPtr->rightSibling;
        continue;

    arr_dim_fail:
        idNode->dataType = ERROR_TYPE;
        return;
    }

    typeDescriptor->properties.arrayProperties.dimension = dimen;
}


void declareFunction(AST_NODE* declarationNode) {
    AST_NODE* returnTypeNode = declarationNode->child;
    SymbolTableEntry* returnEntry = getIdNodeEntry(returnTypeNode);
    AST_NODE* functionNameNode = returnTypeNode->rightSibling;
    IdentifierSemanticValue* funcNameSemVal = &functionNameNode->semantic_value.identifierSemanticValue;

    if (returnEntry->attribute->attr.typeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR) {
        printErrorMsg(functionNameNode, RETURN_ARRAY);
        returnTypeNode->dataType = ERROR_TYPE;
        declarationNode->dataType = ERROR_TYPE;
        return;
    }

    char* functionNameId = funcNameSemVal->identifierName;

    if (declaredLocally(functionNameId)) {
        printErrorMsg(functionNameNode, SYMBOL_REDECLARE);
        functionNameNode->dataType = ERROR_TYPE;
        declarationNode->dataType = ERROR_TYPE;
        return;
    }

    FunctionSignature funcsigTemp = {};
    funcsigTemp.returnType = returnTypeNode->dataType;

    SymbolAttribute* attr = allocateSymbolAttribute(FUNCTION_SIGNATURE, &funcsigTemp);
    funcNameSemVal->symbolTableEntry = enterSymbol(functionNameId, attr);
    FunctionSignature* funcsig = attr->attr.functionSignature;

    openScope();

    AST_NODE *parameterListNode = functionNameNode->rightSibling;
    AST_NODE *param = parameterListNode->child;
    Parameter* paramPrevEntry = NULL;

    while (param) {
        AST_NODE* paramIdNode = param->child->rightSibling;
        processDeclarationNode(param);

        if (param->dataType == ERROR_TYPE) {
            functionNameNode->dataType = ERROR_TYPE;
            break;
        }

        Parameter* paramEntry = (Parameter*) malloc(sizeof(Parameter));

        paramEntry->next = NULL;
        if (funcsig->parametersCount == 0) {
            funcsig->parameterList = paramEntry;
        } else {
            paramPrevEntry->next = paramEntry;
        }

        IdentifierSemanticValue* paramSemVal = &paramIdNode->semantic_value.identifierSemanticValue;
        paramEntry->parameterName = paramSemVal->identifierName;
        paramEntry->type = getIdNodeEntry(paramIdNode)->attribute->attr.typeDescriptor;

        funcsig->parametersCount++;

        paramPrevEntry = paramEntry;
        param = param->rightSibling;
    }

    AST_NODE* blockNode = parameterListNode->rightSibling;
    AST_NODE* stmtNode = blockNode->child;

    while (stmtNode) {
        processGeneralNode(stmtNode);
        stmtNode = stmtNode->rightSibling;
    }

    closeScope();
}
