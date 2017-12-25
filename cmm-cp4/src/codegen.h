#ifndef __CODEGEN_H__
#define __CODEGEN_H__

#include "symbolTable.h"

void codegen(AST_NODE*);

// void codeGenProgramNode(AST_NODE *programNode);
// void codeGenGlobalVariable(AST_NODE *varaibleDeclListNode);
// void codeGenFunctionDeclaration(AST_NODE *functionDeclNode);
// void codeGenGeneralNode(AST_NODE* node);
// void codeGenStmtNode(AST_NODE* stmtNode);
// void codeGenBlockNode(AST_NODE* blockNode);
// void codeGenWhileStmt(AST_NODE* whileStmtNode);
// void codeGenForStmt(AST_NODE* forStmtNode);
// void codeGenIfStmt(AST_NODE* ifStmtNode);
// void codeGenReturnStmt(AST_NODE* returnStmtNode);
// void codeGenAssignOrExpr(AST_NODE* testNode);
// void codeGenAssignmentStmt(AST_NODE* assignmentStmtNode);
// void codeGenExprRelatedNode(AST_NODE* exprRelatedNode);
// void codeGenExprNode(AST_NODE* exprNode);
// void codeGenFunctionCall(AST_NODE* functionCallNode);
// void codeGenVariableReference(AST_NODE* idNode);
// void codeGenConstantReference(AST_NODE* constantNode);
// int codeGenCalcArrayElemenetAddress(AST_NODE* idNode);
// void codeGenStoreArgument(AST_NODE *traverseParameter, Parameter* formalParameter);
/*********************/

#define getIdNodeEntry(node)  ((node)->semantic_value.identifierSemanticValue.symbolTableEntry)

void cgProgramNode(AST_NODE*);
void cgGeneralNode(AST_NODE*);  // removed

void cgGlobalDeclarations(AST_NODE*);

void cgDeclarationNode(AST_NODE*);  // removed

void cgDeclareFunction(AST_NODE*);

void cgBlockNode(AST_NODE*);
void cgStmtNode(AST_NODE*);
void cgCheckAssignOrExpr(AST_NODE*);
void cgExprRelatedNode(AST_NODE*);
void cgDeclareLocalVarList(AST_NODE* declarationNode, int isFunctionParameter);
void cgDeclDimList(AST_NODE* idNode, TypeDescriptor* typeDescriptor, int ignoreFirstDimSize);
void cgDeclareFunction(AST_NODE*);
void cgCheckAssignmentStmt(AST_NODE* assignmentStmtNode);
void cgCheckWhileStmt(AST_NODE* whileStmtNode);
void cgCheckForStmt(AST_NODE* forStmtNode);
void cgCheckIfStmt(AST_NODE* ifStmtNode);
void cgCheckFunctionCall(AST_NODE* functionCallNode);
void cgCheckReturnStmt(AST_NODE* returnStmtNode);
void cgCheckWriteFunction(AST_NODE* functionCallNode);
void cgConstValueNode(AST_NODE* constValueNode);
void cgExprNode(AST_NODE* exprNode);
void cgVariableRValue(AST_NODE*);

void cgSaveRegisters();
void cgRestoreRegisters();

#endif  // __CODEGEN_H__
