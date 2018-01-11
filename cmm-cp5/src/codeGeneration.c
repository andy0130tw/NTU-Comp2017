#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
#include "offsetInAR.h"
#include "myRegister.h"
#include "printSourceFile.h"

FILE* g_codeGenOutputFp = NULL;
char* g_currentFunctionName = NULL;

int getLabelNumber();
int codeGenConstantLabel(C_type constantType, void* valuePtr);
void codeGenGetBoolOfFloat(int boolRegIndex, int floatRegIndex);
void codeGenPrepareRegister(ProcessorType processorType, int regIndex, int needToBeLoaded, int workRegIndexIfPseudo, char** regName);
void codeGenSaveToMemoryIfPsuedoRegister(ProcessorType processorType, int regIndex, char* regName);
void codeGenLogicalInstruction(ProcessorType processorType, char *instruction, int dstRegIndex, int srcReg1Index, int srcReg2Index);
//reg1 is dst
void codeGen2RegInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index);
//reg1 is dst
void codeGen3RegInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, int reg3Index);
void codeGen2Reg1ImmInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, void* imm);
int codeGenConvertFromIntToFloat(int intRegIndex);
int codeGenConvertFromFloatToInt(int floatRegIndex);
//*************************

void codeGenProgramNode(AST_NODE *programNode);
void codeGenGlobalVariable(AST_NODE *varaibleDeclListNode);
void codeGenFunctionDeclaration(AST_NODE *functionDeclNode);
void codeGenGeneralNode(AST_NODE* node);
void codeGenStmtNode(AST_NODE* stmtNode);
void codeGenBlockNode(AST_NODE* blockNode);
void codeGenWhileStmt(AST_NODE* whileStmtNode);
void codeGenForStmt(AST_NODE* forStmtNode);
void codeGenIfStmt(AST_NODE* ifStmtNode);
void codeGenReturnStmt(AST_NODE* returnStmtNode);
void codeGenAssignOrExpr(AST_NODE* testNode);
void codeGenAssignmentStmt(AST_NODE* assignmentStmtNode);
void codeGenExprRelatedNode(AST_NODE* exprRelatedNode);
void codeGenExprNode(AST_NODE* exprNode);
void codeGenFunctionCall(AST_NODE* functionCallNode);
void codeGenVariableReference(AST_NODE* idNode);
void codeGenConstantReference(AST_NODE* constantNode);
int codeGenCalcArrayElementAddress(AST_NODE* idNode);

int getLabelNumber() {
	static int labelNumber = 0;
	return labelNumber++;
}


int codeGenConstantLabel(C_type constantType, void* valuePtr) {
	int labelNumber = getLabelNumber();

	fprintf(g_codeGenOutputFp, ".data\n");

	if (constantType == INTEGERC) {
		int* val = (int*)valuePtr;
		fprintf(g_codeGenOutputFp, "_CONSTANT_%d: .word %d\n", labelNumber, *val);
		fprintf(g_codeGenOutputFp, ".align 3\n");
	} else if (constantType == FLOATC) {
		float* val = (float*)valuePtr;
		fprintf(g_codeGenOutputFp, "_CONSTANT_%d: .float %f\n", labelNumber, *val);
		fprintf(g_codeGenOutputFp, ".align 3\n");
	} else if (constantType == STRINGC) {
		char* val;
		val = (char *)valuePtr;
		val[strlen(valuePtr) - 1] = '\0';
		fprintf(g_codeGenOutputFp, "_CONSTANT_%d: .ascii %s", labelNumber, val);
		fprintf(g_codeGenOutputFp, "\\000\"\n");
		fprintf(g_codeGenOutputFp, ".align 3\n");
		val[strlen(valuePtr) - 1] = '"';
		val[strlen(valuePtr)] = '\0';
	}

	fprintf(g_codeGenOutputFp, ".text\n");

	return labelNumber;
}

void codeGenSetReg(ProcessorType processorType, char* instruction, int reg1Index, int value) {
	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);
	fprintf(g_codeGenOutputFp, "%s %s, #%d\n", instruction, reg1Name, value);
	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}

void codeGenSetReg_cond(ProcessorType processorType, char* instruction, int reg1Index, char* cond) {
	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);
	fprintf(g_codeGenOutputFp, "%s %s, %s\n", instruction, reg1Name, cond);
	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}


void codeGenPrepareRegister_64(ProcessorType processorType, int regIndex, int needToBeLoaded, int workRegIndexIfPseudo, char** regName) {
	int realRegisterCount = (processorType == INT_REG) ? INT_REGISTER_COUNT : FLOAT_REGISTER_COUNT;
	char** realRegisterName = (processorType == INT_REG) ? intRegisterName_64 : floatRegisterName;
	char** workRegisterName = (processorType == INT_REG) ? intWorkRegisterName_64 : floatWorkRegisterName;
	char** reloadRegisterName = (processorType == INT_REG) ? intWorkRegisterName : floatWorkRegisterName;
	char* loadInstruction = (processorType == INT_REG) ? "ldr" : "ldr";

	if (regIndex >= realRegisterCount) {
		//pseudo register
		int pseudoIndex = regIndex - realRegisterCount;
		*regName = workRegisterName[workRegIndexIfPseudo];
		if (needToBeLoaded) {
			char* reloadRegName = reloadRegisterName[workRegIndexIfPseudo];
			fprintf(g_codeGenOutputFp, "#; FIXME: will sometimes break logic\n");
			fprintf(g_codeGenOutputFp, "%s %s, [x29, #%d]\n", loadInstruction, reloadRegName, getPseudoRegisterCorrespondingOffset(pseudoIndex));
		}
	} else {
		*regName = realRegisterName[regIndex];
		g_intRegisterTable.is64[regIndex] = 1;
	}
}






void codeGenGetBoolOfFloat(int boolRegIndex, int floatRegIndex) {

	int zero = 0x0;
	int constantZeroLabelNumber = codeGenConstantLabel(INTEGERC, &zero);
	char* boolRegName = NULL;
	codeGenPrepareRegister(INT_REG, boolRegIndex, 0, 0, &boolRegName);

	char* tmpZeroRegName = intWorkRegisterName[0];
	fprintf(g_codeGenOutputFp, "ldr %s, =_CONSTANT_%d\n", intWorkRegisterName_64[1], constantZeroLabelNumber);
	fprintf(g_codeGenOutputFp, "ldr %s, [%s,#0]\n", tmpZeroRegName, intWorkRegisterName_64[1]);
	char* origFloatRegName = NULL;
	codeGenPrepareRegister(FLOAT_REG, floatRegIndex, 1, 1, &origFloatRegName);
	fprintf(g_codeGenOutputFp, "str %s, [%s,#0]\n", origFloatRegName , intWorkRegisterName_64[1]);
	fprintf(g_codeGenOutputFp, "ldr %s, [%s,#0]\n", boolRegName, intWorkRegisterName_64[1]);
	fprintf(g_codeGenOutputFp, "cmp %s, %s\n", tmpZeroRegName,  boolRegName);

	char* reg1Name = NULL;
	fprintf(g_codeGenOutputFp, "cset %s, ne\n", boolRegName);


	codeGenSaveToMemoryIfPsuedoRegister(INT_REG, boolRegIndex, boolRegName);
}


void codeGenPrepareRegister(ProcessorType processorType, int regIndex, int needToBeLoaded, int workRegIndexIfPseudo, char** regName) {
	int realRegisterCount = (processorType == INT_REG) ? INT_REGISTER_COUNT : FLOAT_REGISTER_COUNT;
	char** realRegisterName;
	char** workRegisterName;
	char* loadInstruction = (processorType == INT_REG) ? "ldr" : "ldr";


	if (regIndex >= realRegisterCount) {
		//pseudo register
		realRegisterName = (processorType == INT_REG) ? intRegisterName : floatRegisterName;
		workRegisterName = (processorType == INT_REG) ? intWorkRegisterName : floatWorkRegisterName;
		int pseudoIndex = regIndex - realRegisterCount;
		*regName = workRegisterName[workRegIndexIfPseudo];
		if (needToBeLoaded) {
			fprintf(g_codeGenOutputFp, "%s %s, [x29, #%d]\n", loadInstruction, *regName, getPseudoRegisterCorrespondingOffset(pseudoIndex));
		}
	} else {
		if (processorType == INT_REG) {
			if (g_intRegisterTable.is64[regIndex] == 1) {
				*regName = intRegisterName_64[regIndex];
			} else {
				*regName = intRegisterName[regIndex];
			}

		} else {
			realRegisterName = floatRegisterName;
			workRegisterName = floatWorkRegisterName;
			*regName = realRegisterName[regIndex];
		}
	}
}


void codeGenSaveToMemoryIfPsuedoRegister(ProcessorType processorType, int regIndex, char* regName) {
	int realRegisterCount = (processorType == INT_REG) ? INT_REGISTER_COUNT : FLOAT_REGISTER_COUNT;
	char* saveInstruction = (processorType == INT_REG) ? "str" : "str";

	if (regIndex >= realRegisterCount) {
		//pseudo register
		int pseudoIndex = regIndex - realRegisterCount;
		fprintf(g_codeGenOutputFp, "%s %s, [x29, #%d]\n", saveInstruction, regName, getPseudoRegisterCorrespondingOffset(pseudoIndex));
	}
}







void codeGen1Reg1ImmInstruction(ProcessorType processorType, char* instruction, int reg1Index, int *value) {
	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);


	if (processorType == INT_REG) {
		fprintf(g_codeGenOutputFp, "%s %s, #%d\n", instruction, reg1Name, *((int *)value));
	}

	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}


void codeGenLogicalInstruction(ProcessorType processorType, char *instruction, int dstRegIndex, int srcReg1Index, int srcReg2Index) {
	int boolReg1Index = -1;
	int boolReg2Index = -1;

	if (processorType == FLOAT_REG) {
		boolReg1Index = getRegister(INT_REG);
		boolReg2Index = getRegister(INT_REG);
		codeGenGetBoolOfFloat(boolReg1Index, srcReg1Index);
		codeGenGetBoolOfFloat(boolReg2Index, srcReg2Index);
	} else if (processorType == INT_REG) {
		int zero = 0;
		boolReg1Index = srcReg1Index;
		boolReg2Index = srcReg2Index;
		codeGen1Reg1ImmInstruction(INT_REG, "cmp", srcReg1Index, &zero);
		codeGenSetReg_cond(INT_REG, "cset", srcReg1Index, "ne");
		codeGen1Reg1ImmInstruction(INT_REG, "cmp", srcReg2Index, &zero);
		codeGenSetReg_cond(INT_REG, "cset", srcReg2Index, "ne");

	}

	codeGen3RegInstruction(INT_REG, instruction, dstRegIndex, boolReg1Index, boolReg2Index);

	if (processorType == FLOAT_REG) {
		freeRegister(INT_REG, boolReg1Index);
		freeRegister(INT_REG, boolReg2Index);
	}
}


void codeGenCmp0Instruction(ProcessorType processorType, char* instruction, int reg1Index, int imm) {


	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);
	fprintf(g_codeGenOutputFp, "%s %s, #%d\n", instruction, reg1Name, imm);
	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);

}


void codeGen2RegInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index) {
	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);

	char* reg2Name = NULL;
	codeGenPrepareRegister(processorType, reg2Index, 1, 1, &reg2Name);

	fprintf(g_codeGenOutputFp, "%s %s, %s\n", instruction, reg1Name, reg2Name);

	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}

void codeGen3RegInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, int reg3Index) {
	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);

	char* reg2Name = NULL;
	codeGenPrepareRegister(processorType, reg2Index, 1, 0, &reg2Name);

	char* reg3Name = NULL;
	codeGenPrepareRegister(processorType, reg3Index, 1, 1, &reg3Name);

	fprintf(g_codeGenOutputFp, "%s %s, %s, %s\n", instruction, reg1Name, reg2Name, reg3Name);

	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}

void codeGen2Reg1ImmInstruction_64(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, void* imm) {
	char* reg1Name = NULL;
	codeGenPrepareRegister_64(processorType, reg1Index, 0, 0, &reg1Name);

	char* reg2Name = NULL;
	codeGenPrepareRegister_64(processorType, reg2Index, 1, 0, &reg2Name);

	if (processorType == INT_REG) {
		int* val = (int*)imm;
		fprintf(g_codeGenOutputFp, "%s %s, %s, #%d\n", instruction, reg1Name, reg2Name, *val);
	} else if (processorType == FLOAT_REG) {
		float* val = (float*)imm;
		fprintf(g_codeGenOutputFp, "%s %s, %s, %f\n", instruction, reg1Name, reg2Name, *val);
	}

	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}






void codeGen2Reg1ImmInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, void* imm) {
	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);

	char* reg2Name = NULL;
	codeGenPrepareRegister(processorType, reg2Index, 1, 0, &reg2Name);

	if (processorType == INT_REG) {
		int* val = (int*)imm;
		fprintf(g_codeGenOutputFp, "%s %s, %s, #%d\n", instruction, reg1Name, reg2Name, *val);
	} else if (processorType == FLOAT_REG) {
		float* val = (float*)imm;
		fprintf(g_codeGenOutputFp, "%s %s, %s, %f\n", instruction, reg1Name, reg2Name, *val);
	}

	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}


int codeGenConvertFromIntToFloat(int intRegIndex) {
	int floatRegisterIndex = getRegister(FLOAT_REG);
	char* regFrom, * regTo;
	codeGenPrepareRegister(INT_REG, intRegIndex, 1, 0, &regFrom);
	codeGenPrepareRegister(FLOAT_REG, floatRegisterIndex, 0, 0, &regTo);
	fprintf(g_codeGenOutputFp, "scvtf %s, %s\n", regTo, regFrom);
	freeRegister(INT_REG, intRegIndex);
	return floatRegisterIndex;
}


int codeGenConvertFromFloatToInt(int floatRegIndex) {
	int intRegisterIndex = getRegister(INT_REG);
	char* regFrom, * regTo;
	codeGenPrepareRegister(FLOAT_REG, floatRegIndex, 1, 0, &regFrom);
	codeGenPrepareRegister(INT_REG, intRegisterIndex, 0, 0, &regTo);
	fprintf(g_codeGenOutputFp, "fcvtzs %s, %s\n", regTo, regFrom);
	freeRegister(FLOAT_REG, floatRegIndex);
	return intRegisterIndex;
}


void codeGenerate(AST_NODE *root) {
	const char* outputfileName = "output.s";
	g_codeGenOutputFp = fopen(outputfileName, "w");
	if (!g_codeGenOutputFp) {
		printf("Cannot open file \"%s\"", outputfileName);
		exit(EXIT_FAILURE);
	}

	codeGenProgramNode(root);
}


void codeGenProgramNode(AST_NODE *programNode) {
	AST_NODE *traverseDeclaration = programNode->child;
	while (traverseDeclaration) {
		if (traverseDeclaration->nodeType == VARIABLE_DECL_LIST_NODE) {
			fprintf(g_codeGenOutputFp, ".data\n");
			codeGenGlobalVariable(traverseDeclaration);
			fprintf(g_codeGenOutputFp, ".text\n");
		} else if (traverseDeclaration->nodeType == DECLARATION_NODE) {
			codeGenFunctionDeclaration(traverseDeclaration);
		}
		traverseDeclaration = traverseDeclaration->rightSibling;
	}
	return;
}


void codeGenGlobalVariable(AST_NODE* varaibleDeclListNode) {
	AST_NODE *traverseDeclaration = varaibleDeclListNode->child;
	while (traverseDeclaration) {
		if (traverseDeclaration->semantic_value.declSemanticValue.kind == VARIABLE_DECL) {
			AST_NODE *idNode = traverseDeclaration->child->rightSibling;

			while (idNode) {
				SymbolTableEntry* idSymbolTableEntry = SEMVAL_ID(idNode).symbolTableEntry;
				TypeDescriptor* idTypeDescriptor = idSymbolTableEntry->attribute->attr.typeDescriptor;
				if (idTypeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR) {
					if (idTypeDescriptor->properties.dataType == INT_TYPE) {
						int initVal = 0;
						if (SEMVAL_ID(idNode).kind == WITH_INIT_ID) {
							initVal = SEMVAL_EXPR(idNode->child).constEvalValue.iValue;
						}
						fprintf(g_codeGenOutputFp, "_g_%s: .word %d\n", idSymbolTableEntry->name, initVal);
					} else if (idTypeDescriptor->properties.dataType == FLOAT_TYPE) {
						float initVal = 0.0;
						if (SEMVAL_ID(idNode).kind == WITH_INIT_ID) {
							initVal = SEMVAL_EXPR(idNode->child).constEvalValue.fValue;
						}
						fprintf(g_codeGenOutputFp, "_g_%s: .float %g\n", idSymbolTableEntry->name, initVal);
					}
				} else if (idTypeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR) {
					int variableSize = getVariableSize(idTypeDescriptor);
					fprintf(g_codeGenOutputFp, "_g_%s: .space %d\n", idSymbolTableEntry->name, variableSize);
				}
				idNode = idNode->rightSibling;
			}
		}
		traverseDeclaration = traverseDeclaration->rightSibling;
	}
	return;
}


void codeGenFunctionDeclaration(AST_NODE *functionDeclNode) {
	AST_NODE* functionIdNode = functionDeclNode->child->rightSibling;

	g_currentFunctionName = SEMVAL_ID(functionIdNode).identifierName;

	fprintf(g_codeGenOutputFp, ".text\n");
	if (strcmp(SEMVAL_ID(functionIdNode).identifierName, "main") != 0) {
		fprintf(g_codeGenOutputFp, "_start_%s:\n", SEMVAL_ID(functionIdNode).identifierName);
	} else {
		fprintf(g_codeGenOutputFp, "%s:\n", SEMVAL_ID(functionIdNode).identifierName);
	}

	//prologue
	fprintf(g_codeGenOutputFp, "str x30, [sp, #0]\n");
	fprintf(g_codeGenOutputFp, "str x29, [sp, #-8]\n");
	fprintf(g_codeGenOutputFp, "add x29, sp, #-8\n");
	fprintf(g_codeGenOutputFp, "add sp, sp, #-16\n");
	fprintf(g_codeGenOutputFp, "ldr x30, =_frameSize_%s\n", SEMVAL_ID(functionIdNode).identifierName);
	fprintf(g_codeGenOutputFp, "ldr w30, [x30, #0]\n");
	fprintf(g_codeGenOutputFp, "sub sp, sp, w30\n");
	printStoreRegister(g_codeGenOutputFp);

	resetRegisterTable(SEMVAL_ID(functionIdNode).symbolTableEntry->attribute->offsetInAR);

	AST_NODE* blockNode = functionIdNode->rightSibling->rightSibling;
	AST_NODE *traverseListNode = blockNode->child;
	while (traverseListNode) {
		codeGenGeneralNode(traverseListNode);
		traverseListNode = traverseListNode->rightSibling;
	}

	//epilogue
	fprintf(g_codeGenOutputFp, "_end_%s:\n", g_currentFunctionName);
	printRestoreRegister(g_codeGenOutputFp);
	fprintf(g_codeGenOutputFp, "ldr x30, [x29, #8]\n");
	fprintf(g_codeGenOutputFp, "mov sp, x29\n");
	fprintf(g_codeGenOutputFp, "add sp, sp, #8\n");
	fprintf(g_codeGenOutputFp, "ldr x29, [x29,#0]\n");
	if (strcmp(SEMVAL_ID(functionIdNode).identifierName, "main") == 0) {
	} else {
		fprintf(g_codeGenOutputFp, "RET x30\n");
	}
	fprintf(g_codeGenOutputFp, ".data\n");
	int frameSize = abs(SEMVAL_ID(functionIdNode).symbolTableEntry->attribute->offsetInAR) +
					(INT_REGISTER_COUNT + INT_WORK_REGISTER_COUNT + INT_OTHER_REGISTER_COUNT + FLOAT_REGISTER_COUNT + FLOAT_WORK_REGISTER_COUNT) * 4 +
					g_pseudoRegisterTable.isAllocatedVector->size * 4;

	while (frameSize % 8 != 0) {
		frameSize = frameSize + 4;
	}
	frameSize = frameSize + (INT_REGISTER_COUNT + INT_WORK_REGISTER_COUNT + INT_OTHER_REGISTER_COUNT) * 4;
	while (frameSize % 8 != 0) {
		frameSize = frameSize + 4;
	}
	fprintf(g_codeGenOutputFp, "_frameSize_%s: .word %d\n", SEMVAL_ID(functionIdNode).identifierName,
			frameSize + 8);
	return;
}


void codeGenBlockNode(AST_NODE* blockNode) {
	AST_NODE *traverseListNode = blockNode->child;
	while (traverseListNode) {
		codeGenGeneralNode(traverseListNode);
		traverseListNode = traverseListNode->rightSibling;
	}
}


void codeGenExprNode(AST_NODE* exprNode) {
	if (exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION) {
		AST_NODE* leftOp = exprNode->child;
		AST_NODE* rightOp = leftOp->rightSibling;
		codeGenExprRelatedNode(leftOp);
		codeGenExprRelatedNode(rightOp);

		int labelNumber;

		if (leftOp->dataType == FLOAT_TYPE || rightOp->dataType == FLOAT_TYPE) {
			if (leftOp->dataType == INT_TYPE) {
				leftOp->registerIndex = codeGenConvertFromIntToFloat(leftOp->registerIndex);
			}
			if (rightOp->dataType == INT_TYPE) {
				rightOp->registerIndex = codeGenConvertFromIntToFloat(rightOp->registerIndex);
			}

			switch (exprNode->semantic_value.exprSemanticValue.op.binaryOp) {
			case BINARY_OP_ADD:
				exprNode->registerIndex = leftOp->registerIndex;
				codeGen3RegInstruction(FLOAT_REG, "fadd", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
				break;
			case BINARY_OP_SUB:
				exprNode->registerIndex = leftOp->registerIndex;
				codeGen3RegInstruction(FLOAT_REG, "fsub", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
				break;
			case BINARY_OP_MUL:
				exprNode->registerIndex = leftOp->registerIndex;
				codeGen3RegInstruction(FLOAT_REG, "fmul", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
				break;
			case BINARY_OP_DIV:
				exprNode->registerIndex = leftOp->registerIndex;
				codeGen3RegInstruction(FLOAT_REG, "fdiv", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
				break;
			case BINARY_OP_EQ:
				exprNode->registerIndex = getRegister(INT_REG);
				codeGen2RegInstruction(FLOAT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "eq");
				freeRegister(FLOAT_REG, leftOp->registerIndex);
				break;
			case BINARY_OP_GE:
				exprNode->registerIndex = getRegister(INT_REG);
				codeGen2RegInstruction(FLOAT_REG, "fcmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "ge");
				freeRegister(FLOAT_REG, leftOp->registerIndex);
				break;
			case BINARY_OP_LE:
				exprNode->registerIndex = getRegister(INT_REG);
				codeGen2RegInstruction(FLOAT_REG, "fcmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "le");
				freeRegister(FLOAT_REG, leftOp->registerIndex);
				break;
			case BINARY_OP_NE:
				exprNode->registerIndex = getRegister(INT_REG);
				codeGen2RegInstruction(FLOAT_REG, "fcmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "ne");
				freeRegister(FLOAT_REG, leftOp->registerIndex);
				break;
			case BINARY_OP_GT:
				exprNode->registerIndex = getRegister(INT_REG);
				codeGen2RegInstruction(FLOAT_REG, "fcmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "ge");
				freeRegister(FLOAT_REG, leftOp->registerIndex);
				break;
			case BINARY_OP_LT:
				exprNode->registerIndex = getRegister(INT_REG);
				codeGen2RegInstruction(FLOAT_REG, "fcmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "lt");
				freeRegister(FLOAT_REG, leftOp->registerIndex);
				break;
			case BINARY_OP_AND:
				labelNumber = getLabelNumber();
				exprNode->registerIndex = getRegister(INT_REG);
				codeGenSetReg_cond(FLOAT_REG, "fcmp", leftOp->registerIndex, "#0.0");
				fprintf(g_codeGenOutputFp, "beq _booleanFalse_%d\n", labelNumber);
				codeGenSetReg_cond(FLOAT_REG, "fcmp", rightOp->registerIndex, "#0.0");
				fprintf(g_codeGenOutputFp, "beq _booleanFalse_%d\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "mov", exprNode->registerIndex, "1");
				fprintf(g_codeGenOutputFp, "b _booleanExit_%d\n", labelNumber);
				fprintf(g_codeGenOutputFp, "_booleanFalse_%d:\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "mov", exprNode->registerIndex, "0");
				fprintf(g_codeGenOutputFp, "_booleanExit_%d:\n", labelNumber);

				freeRegister(FLOAT_REG, leftOp->registerIndex);
				break;
			case BINARY_OP_OR:
				labelNumber = getLabelNumber();
				exprNode->registerIndex = getRegister(INT_REG);
				codeGenSetReg_cond(FLOAT_REG, "fcmp", leftOp->registerIndex, "#0.0");
				fprintf(g_codeGenOutputFp, "bne _booleanTrue_%d\n", labelNumber);
				codeGenSetReg_cond(FLOAT_REG, "fcmp", rightOp->registerIndex, "#0.0");
				fprintf(g_codeGenOutputFp, "beq _booleanFalse_%d\n", labelNumber);
				fprintf(g_codeGenOutputFp, "_booleanTrue_%d:\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "mov", exprNode->registerIndex, "1");
				fprintf(g_codeGenOutputFp, "b _booleanExit_%d\n", labelNumber);
				fprintf(g_codeGenOutputFp, "_booleanFalse_%d:\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "mov", exprNode->registerIndex, "0");
				fprintf(g_codeGenOutputFp, "_booleanExit_%d:\n", labelNumber);

				freeRegister(FLOAT_REG, leftOp->registerIndex);
				break;
			default:
				printf("Unhandled case in void evaluateExprValue(AST_NODE* exprNode)\n");
				break;
			}

			freeRegister(FLOAT_REG, rightOp->registerIndex);
		} else if (exprNode->dataType == INT_TYPE) {
			exprNode->registerIndex = leftOp->registerIndex;
			switch (exprNode->semantic_value.exprSemanticValue.op.binaryOp) {
			case BINARY_OP_ADD:
				codeGen3RegInstruction(INT_REG, "add", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
				break;
			case BINARY_OP_SUB:
				codeGen3RegInstruction(INT_REG, "sub", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
				break;
			case BINARY_OP_MUL:
				codeGen3RegInstruction(INT_REG, "mul", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
				break;
			case BINARY_OP_DIV:
				codeGen3RegInstruction(INT_REG, "sdiv", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
				break;
			case BINARY_OP_EQ:
				codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "eq");
				break;
			case BINARY_OP_GE:
				codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "ge");
				break;
			case BINARY_OP_LE:
				codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "le");
				break;
			case BINARY_OP_NE:
				codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "ne");
				break;
			case BINARY_OP_GT:
				codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "gt");
				break;
			case BINARY_OP_LT:
				codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
				codeGenSetReg_cond(INT_REG, "cset", exprNode->registerIndex, "lt");
				break;
			case BINARY_OP_AND:
				labelNumber = getLabelNumber();
				exprNode->registerIndex = getRegister(INT_REG);
				codeGenSetReg_cond(INT_REG, "cmp", leftOp->registerIndex, "0");
				fprintf(g_codeGenOutputFp, "beq _booleanFalse_%d\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "cmp", rightOp->registerIndex, "0");
				fprintf(g_codeGenOutputFp, "beq _booleanFalse_%d\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "mov", exprNode->registerIndex, "1");
				fprintf(g_codeGenOutputFp, "b _booleanExit_%d\n", labelNumber);
				fprintf(g_codeGenOutputFp, "_booleanFalse_%d:\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "mov", exprNode->registerIndex, "0");
				fprintf(g_codeGenOutputFp, "_booleanExit_%d:\n", labelNumber);
				break;
			case BINARY_OP_OR:
				labelNumber = getLabelNumber();
				exprNode->registerIndex = getRegister(INT_REG);
				codeGenSetReg_cond(INT_REG, "cmp", leftOp->registerIndex, "0");
				fprintf(g_codeGenOutputFp, "bne _booleanTrue_%d\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "cmp", rightOp->registerIndex, "0");
				fprintf(g_codeGenOutputFp, "beq _booleanFalse_%d\n", labelNumber);
				fprintf(g_codeGenOutputFp, "_booleanTrue_%d:\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "mov", exprNode->registerIndex, "1");
				fprintf(g_codeGenOutputFp, "b _booleanExit_%d\n", labelNumber);
				fprintf(g_codeGenOutputFp, "_booleanFalse_%d:\n", labelNumber);
				codeGenSetReg_cond(INT_REG, "mov", exprNode->registerIndex, "0");
				fprintf(g_codeGenOutputFp, "_booleanExit_%d:\n", labelNumber);
				break;
			default:
				printf("Unhandled case in void evaluateExprValue(AST_NODE* exprNode)\n");
				break;
			}

			freeRegister(INT_REG, rightOp->registerIndex);
		}
	}//endif BINARY_OPERATION
	else if (exprNode->semantic_value.exprSemanticValue.kind == UNARY_OPERATION) {
		int tmpZero = 0;
		AST_NODE* operand = exprNode->child;
		codeGenExprRelatedNode(operand);
		if (operand->dataType == FLOAT_TYPE) {
			switch (exprNode->semantic_value.exprSemanticValue.op.unaryOp) {
			case UNARY_OP_POSITIVE:
				exprNode->registerIndex = operand->registerIndex;
				break;
			case UNARY_OP_NEGATIVE:
				exprNode->registerIndex = operand->registerIndex;
				codeGen2RegInstruction(FLOAT_REG, "fneg", exprNode->registerIndex, exprNode->registerIndex);
				break;
			case UNARY_OP_LOGICAL_NEGATION:
				exprNode->registerIndex = getRegister(INT_REG);
				codeGenGetBoolOfFloat(exprNode->registerIndex, operand->registerIndex);
				freeRegister(FLOAT_REG, operand->registerIndex);
				break;
			default:
				printf("Unhandled case in void evaluateExprValue(AST_NODE* exprNode)\n");
				break;
			}
		} else if (operand->dataType == INT_TYPE) {
			switch (exprNode->semantic_value.exprSemanticValue.op.unaryOp) {
			case UNARY_OP_POSITIVE:
				exprNode->registerIndex = operand->registerIndex;
				break;
			case UNARY_OP_NEGATIVE:
				exprNode->registerIndex = operand->registerIndex;
				codeGen2RegInstruction(INT_REG, "neg", exprNode->registerIndex, exprNode->registerIndex);
				break;
			case UNARY_OP_LOGICAL_NEGATION:
				exprNode->registerIndex = operand->registerIndex;
				codeGenCmp0Instruction(INT_REG, "cmp", exprNode->registerIndex, 0);
				codeGenSetReg(INT_REG, "mov", exprNode->registerIndex, 0);
				codeGenSetReg(INT_REG, "moveq", exprNode->registerIndex, 1);
				break;
			default:
				printf("Unhandled case in void evaluateExprValue(AST_NODE* exprNode)\n");
				break;
			}
		}
	}
}

void codeGenPassArguments(AST_NODE* parameterNode, FunctionSignature* funcsig) {
	int paramCnt = funcsig->parametersCount;
	AST_NODE* paramNodeList[paramCnt];
	Parameter* declParamNodeList[paramCnt];

	// pass parameters, pushing parameters in reversed order
	AST_NODE* paramPtr = parameterNode;
	Parameter* declParamPtr = funcsig->parameterList;
	for (int i = paramCnt - 1; i >= 0; i--) {
		// evaluate node if it is a scalar
		if (declParamPtr->type->properties.dataType == INT_TYPE ||
			declParamPtr->type->properties.dataType == FLOAT_TYPE) {
			fprintf(g_codeGenOutputFp, "#; parameter %s\n", declParamPtr->parameterName);
			codeGenExprRelatedNode(paramPtr);
		}

		paramNodeList[i] = paramPtr;
		declParamNodeList[i] = declParamPtr;
		paramPtr = paramPtr->rightSibling;
		declParamPtr = declParamPtr->next;
	}

	char* parameterRegName;

	// generate store command
	for (int i = 0; i < paramCnt; i++) {
		if (declParamNodeList[i]->type->properties.dataType == INT_TYPE) {
			if (paramNodeList[i]->dataType == INT_TYPE) {
				codeGenPrepareRegister(INT_REG, paramNodeList[i]->registerIndex, 1, 0, &parameterRegName);
				fprintf(g_codeGenOutputFp, "str %s, [sp, #%d]\n", parameterRegName, -i * 8);
				freeRegister(INT_REG, paramNodeList[i]->registerIndex);
			} else {
				int regIndex = codeGenConvertFromFloatToInt(paramNodeList[i]->registerIndex);
				codeGenPrepareRegister(INT_REG, regIndex, 1, 0, &parameterRegName);
				fprintf(g_codeGenOutputFp, "str %s, [sp, #%d]\n", parameterRegName, -i * 8);
				freeRegister(INT_REG, regIndex);
			}
		} else if (declParamNodeList[i]->type->properties.dataType == FLOAT_TYPE) {
			if (paramNodeList[i]->dataType == FLOAT_TYPE) {
				codeGenPrepareRegister(FLOAT_REG, paramNodeList[i]->registerIndex, 1, 0, &parameterRegName);
				fprintf(g_codeGenOutputFp, "str %s, [sp, #%d]\n", parameterRegName, -i * 8);
				freeRegister(FLOAT_REG, paramNodeList[i]->registerIndex);
			} else {
				int regIndex = codeGenConvertFromIntToFloat(paramNodeList[i]->registerIndex);
				codeGenPrepareRegister(FLOAT_REG, regIndex, 1, 0, &parameterRegName);
				fprintf(g_codeGenOutputFp, "str %s, [sp, #%d]\n", parameterRegName, -i * 8);
				freeRegister(FLOAT_REG, regIndex);
			}
		} else {  // INT_PTR / FLOAT_PTR
			printf("Unimplemented!!\n");
		}
	}
	fprintf(g_codeGenOutputFp, "sub sp, sp, %d\n", paramCnt * 8);
}

void codeGenFunctionCall(AST_NODE* functionCallNode) {
	AST_NODE* functionIdNode = functionCallNode->child;
	AST_NODE* parameterList = functionIdNode->rightSibling;
	if (strcmp(SEMVAL_ID(functionIdNode).identifierName, "write") == 0) {
		AST_NODE* firstParameter = parameterList->child;
		codeGenExprRelatedNode(firstParameter);
		char* parameterRegName = NULL;
		switch (firstParameter->dataType) {
		case INT_TYPE:
			codeGenPrepareRegister(INT_REG, firstParameter->registerIndex, 1, 0, &parameterRegName);
			fprintf(g_codeGenOutputFp, "mov w0, %s\n", parameterRegName);
			fprintf(g_codeGenOutputFp, "bl _write_int\n");
			freeRegister(INT_REG, firstParameter->registerIndex);
			break;
		case FLOAT_TYPE:
			codeGenPrepareRegister(FLOAT_REG, firstParameter->registerIndex, 1, 0, &parameterRegName);
			fprintf(g_codeGenOutputFp, "fmov s0, %s\n", parameterRegName);
			fprintf(g_codeGenOutputFp, "bl _write_float\n");
			freeRegister(FLOAT_REG, firstParameter->registerIndex);
			break;
		case CONST_STRING_TYPE:
			codeGenPrepareRegister(INT_REG, firstParameter->registerIndex, 1, 0, &parameterRegName);
			fprintf(g_codeGenOutputFp, "mov x0, %s\n", parameterRegName);
			fprintf(g_codeGenOutputFp, "bl _write_str\n");
			freeRegister(INT_REG, firstParameter->registerIndex);
			break;
		default:
			printf("Unhandled case in void codeGenFunctionCall(AST_NODE* functionCallNode)\n");
			printf("firstParameter->registerIndex was not free\n");
			break;
		}
		functionCallNode->registerIndex = -1;
		return;
	}


	if (strcmp(SEMVAL_ID(functionIdNode).identifierName, "read") == 0) {
		fprintf(g_codeGenOutputFp, "bl _read_int\n");
	} else if (strcmp(SEMVAL_ID(functionIdNode).identifierName, "fread") == 0) {
		fprintf(g_codeGenOutputFp, "bl _read_float\n");
	} else {
		AST_NODE* paramPtr = parameterList->child;
		FunctionSignature* funcsig = SEMVAL_ID(functionIdNode).symbolTableEntry->attribute->attr.functionSignature;

		if (paramPtr) {
			codeGenPassArguments(paramPtr, funcsig);
		}

		if (strcmp(SEMVAL_ID(functionIdNode).identifierName, "main") != 0) {
			fprintf(g_codeGenOutputFp, "bl _start_%s\n", SEMVAL_ID(functionIdNode).identifierName);
		} else {
			fprintf(g_codeGenOutputFp, "bl %s\n", SEMVAL_ID(functionIdNode).identifierName);
		}

		// recover stack after return
		if (paramPtr) {
			fprintf(g_codeGenOutputFp, "add sp, sp, %d\n", funcsig->parametersCount * 8);
		}
	}

	if (SEMVAL_ID(functionIdNode).symbolTableEntry) {
		// if VOID_TYPE, then no need to allocate a register for it
		functionCallNode->registerIndex = -1;

		if (SEMVAL_ID(functionIdNode).symbolTableEntry->attribute->attr.functionSignature->returnType == INT_TYPE) {
			functionCallNode->registerIndex = getRegister(INT_REG);
			char* returnIntRegName = NULL;
			codeGenPrepareRegister(INT_REG, functionCallNode->registerIndex, 0, 0, &returnIntRegName);

			fprintf(g_codeGenOutputFp, "mov %s, w0\n", returnIntRegName);

			codeGenSaveToMemoryIfPsuedoRegister(INT_REG, functionCallNode->registerIndex, returnIntRegName);
		} else if (SEMVAL_ID(functionIdNode).symbolTableEntry->attribute->attr.functionSignature->returnType == FLOAT_TYPE) {
			functionCallNode->registerIndex = getRegister(FLOAT_REG);
			char* returnfloatRegName = NULL;
			codeGenPrepareRegister(FLOAT_REG, functionCallNode->registerIndex, 0, 0, &returnfloatRegName);

			fprintf(g_codeGenOutputFp, "fmov %s, s0\n", returnfloatRegName);

			codeGenSaveToMemoryIfPsuedoRegister(INT_REG, functionCallNode->registerIndex, returnfloatRegName);
		}
	}
}


int codeGenCalcArrayElementAddress(AST_NODE* idNode) {
	AST_NODE* traverseDim = idNode->child;
	int* sizeInEachDimension = SEMVAL_ID(idNode).symbolTableEntry->attribute->attr.typeDescriptor->properties.arrayProperties.sizeInEachDimension;

	codeGenExprRelatedNode(traverseDim);
	int linearIdxRegisterIndex = traverseDim->registerIndex;
	char* linearIdxRegName;
	codeGenPrepareRegister(INT_REG, linearIdxRegisterIndex, 1, 0, &linearIdxRegName);

	traverseDim = traverseDim->rightSibling;

	int dimIndex = 1;
	int dimTmpRegIndex = getRegister(INT_REG);
	while(traverseDim) {
		char* tmpReg, * currReg;
        codeGenPrepareRegister(INT_REG, dimTmpRegIndex, 0, 1, &tmpReg);
        // load and multiply
        fprintf(g_codeGenOutputFp, "mov %s, %d\n", tmpReg, sizeInEachDimension[dimIndex]);
        fprintf(g_codeGenOutputFp, "mul %s, %s, %s\n", linearIdxRegName, linearIdxRegName, tmpReg);

        codeGenExprRelatedNode(traverseDim);
        codeGenPrepareRegister(INT_REG, traverseDim->registerIndex, 1, 1, &currReg);
        // add current offset
        fprintf(g_codeGenOutputFp, "add %s, %s, %s\n", linearIdxRegName, linearIdxRegName, currReg);
        freeRegister(INT_REG, traverseDim->registerIndex);

        dimIndex++;
		traverseDim = traverseDim->rightSibling;
  	}
  	// XXX: should use work register?
  	freeRegister(INT_REG, dimTmpRegIndex);

	int shiftLeftTwoBits = 2;
	codeGen2Reg1ImmInstruction_64(INT_REG, "lsl", linearIdxRegisterIndex, linearIdxRegisterIndex, &shiftLeftTwoBits);

	char* linearOffsetRegName = NULL;
	if (!isGlobalVariable(SEMVAL_ID(idNode).symbolTableEntry)) {
		int baseOffset = SEMVAL_ID(idNode).symbolTableEntry->attribute->offsetInAR;
		codeGen2Reg1ImmInstruction_64(INT_REG, "add", linearIdxRegisterIndex, linearIdxRegisterIndex, &baseOffset);
		codeGenPrepareRegister_64(INT_REG, linearIdxRegisterIndex, 1, 0, &linearOffsetRegName);
		fprintf(g_codeGenOutputFp, "add %s, %s, x29\n", linearOffsetRegName, linearOffsetRegName);
	} else {
		fprintf(g_codeGenOutputFp, "ldr %s,= _g_%s\n", intWorkRegisterName_64[0], SEMVAL_ID(idNode).identifierName);
		codeGenPrepareRegister_64(INT_REG, linearIdxRegisterIndex, 1, 1, &linearOffsetRegName);
		fprintf(g_codeGenOutputFp, "add %s, %s, %s\n", linearOffsetRegName, linearOffsetRegName, intWorkRegisterName_64[0]);
	}

	codeGenSaveToMemoryIfPsuedoRegister(INT_REG, linearIdxRegisterIndex, linearOffsetRegName);

	return linearIdxRegisterIndex;
}

void codeGenVariableReference(AST_NODE* idNode) {
	SymbolAttribute *idAttribute = SEMVAL_ID(idNode).symbolTableEntry->attribute;
	if (SEMVAL_ID(idNode).kind == NORMAL_ID) {
		if (idNode->dataType == INT_TYPE) {
			idNode->registerIndex = getRegister(INT_REG);
			char* loadRegName = NULL;
			if (!isGlobalVariable(SEMVAL_ID(idNode).symbolTableEntry)) {
				codeGenPrepareRegister(INT_REG, idNode->registerIndex, 0, 0, &loadRegName);
				fprintf(g_codeGenOutputFp, "ldr %s, [x29, #%d]\n", loadRegName, idAttribute->offsetInAR);
			} else {
				fprintf(g_codeGenOutputFp, "ldr %s, =_g_%s\n", intWorkRegisterName_64[0], SEMVAL_ID(idNode).identifierName);
				codeGenPrepareRegister(INT_REG, idNode->registerIndex, 0, 1, &loadRegName);
				fprintf(g_codeGenOutputFp, "ldr %s, [%s,#0]\n", loadRegName, intWorkRegisterName_64[0]);
			}
			codeGenSaveToMemoryIfPsuedoRegister(INT_REG, idNode->registerIndex, loadRegName);
		} else if (idNode->dataType == FLOAT_TYPE) {
			idNode->registerIndex = getRegister(FLOAT_REG);
			char* loadRegName = NULL;
			if (!isGlobalVariable(SEMVAL_ID(idNode).symbolTableEntry)) {
				codeGenPrepareRegister(FLOAT_REG, idNode->registerIndex, 0, 0, &loadRegName);
				fprintf(g_codeGenOutputFp, "ldr %s, [x29, #%d]\n", loadRegName, idAttribute->offsetInAR);
			} else {
				fprintf(g_codeGenOutputFp, "ldr %s, =_g_%s\n", intWorkRegisterName_64[0], SEMVAL_ID(idNode).identifierName);
				codeGenPrepareRegister(FLOAT_REG, idNode->registerIndex, 0, 0, &loadRegName);
				fprintf(g_codeGenOutputFp, "ldr %s, [%s, #0]\n", loadRegName, intWorkRegisterName_64[0]);
			}
			codeGenSaveToMemoryIfPsuedoRegister(FLOAT_REG, idNode->registerIndex, loadRegName);
		}
	} else if (SEMVAL_ID(idNode).kind == ARRAY_ID) {
		if (idNode->dataType == INT_TYPE || idNode->dataType == FLOAT_TYPE) {
			int elementAddressRegIndex = codeGenCalcArrayElementAddress(idNode);
			char* elementAddressRegName = NULL;
			codeGenPrepareRegister_64(INT_REG, elementAddressRegIndex, 1, 0, &elementAddressRegName);

			if (idNode->dataType == INT_TYPE) {
				char* dstRegName = NULL;
				idNode->registerIndex = getRegister(INT_REG);
				codeGenPrepareRegister(INT_REG, idNode->registerIndex, 0, 0, &dstRegName);
				fprintf(g_codeGenOutputFp, "ldr %s, [%s, #0]\n", dstRegName , elementAddressRegName);
				freeRegister(INT_REG, elementAddressRegIndex);

				codeGenSaveToMemoryIfPsuedoRegister(INT_REG, idNode->registerIndex, dstRegName);
			} else if (idNode->dataType == FLOAT_TYPE) {
				char* dstRegName = NULL;
				idNode->registerIndex = getRegister(FLOAT_REG);
				codeGenPrepareRegister(FLOAT_REG, idNode->registerIndex, 0, 0, &dstRegName);

				char* elementAddressRegName = NULL;
				codeGenPrepareRegister(INT_REG, elementAddressRegIndex, 1, 0, &elementAddressRegName);

				fprintf(g_codeGenOutputFp, "ldr %s, [%s, #0]\n", dstRegName, elementAddressRegName);
				codeGenSaveToMemoryIfPsuedoRegister(FLOAT_REG, idNode->registerIndex, dstRegName);

				freeRegister(INT_REG, elementAddressRegIndex);
			}
		}
	}
}

void codeGenConstantReference(AST_NODE* constantNode) {
	C_type cType = constantNode->semantic_value.const1->const_type;
	if (cType == INTEGERC) {
		int tmpInt = constantNode->semantic_value.const1->const_u.intval;
		int constantLabelNumber = codeGenConstantLabel(INTEGERC, &tmpInt);
		constantNode->registerIndex = getRegister(INT_REG);
		char* regName = NULL;
		codeGenPrepareRegister(INT_REG, constantNode->registerIndex, 0, 0, &regName);
		fprintf(g_codeGenOutputFp, "ldr %s, _CONSTANT_%d\n", regName, constantLabelNumber);
		codeGenSaveToMemoryIfPsuedoRegister(INT_REG, constantNode->registerIndex, regName);
	} else if (cType == FLOATC) {
		float tmpFloat = constantNode->semantic_value.const1->const_u.fval;
		int constantLabelNumber = codeGenConstantLabel(FLOATC, &tmpFloat);
		constantNode->registerIndex = getRegister(FLOAT_REG);
		char* regName = NULL;
		codeGenPrepareRegister(FLOAT_REG, constantNode->registerIndex, 0, 0, &regName);
		fprintf(g_codeGenOutputFp, "ldr %s, =_CONSTANT_%d\n", intWorkRegisterName_64[0], constantLabelNumber);
		fprintf(g_codeGenOutputFp, "ldr %s, [%s, #0]\n", regName, intWorkRegisterName_64[0]);
		codeGenSaveToMemoryIfPsuedoRegister(FLOAT_REG, constantNode->registerIndex, regName);
	} else if (cType == STRINGC) {
		char* tmpCharPtr = constantNode->semantic_value.const1->const_u.sc;
		int constantLabelNumber = codeGenConstantLabel(STRINGC, tmpCharPtr);
		constantNode->registerIndex = getRegister(INT_REG);
		char* regName = NULL;
		codeGenPrepareRegister_64(INT_REG, constantNode->registerIndex, 0, 0, &regName);
		fprintf(g_codeGenOutputFp, "ldr %s, =_CONSTANT_%d\n", regName, constantLabelNumber);
		codeGenSaveToMemoryIfPsuedoRegister(INT_REG, constantNode->registerIndex, regName);
	}
}


void codeGenExprRelatedNode(AST_NODE* exprRelatedNode) {
	switch (exprRelatedNode->nodeType) {
	case EXPR_NODE:
		codeGenExprNode(exprRelatedNode);
		break;
	case STMT_NODE:
		codeGenFunctionCall(exprRelatedNode);
		break;
	case IDENTIFIER_NODE:
		codeGenVariableReference(exprRelatedNode);
		break;
	case CONST_VALUE_NODE:
		codeGenConstantReference(exprRelatedNode);
		break;
	default:
		printf("Unhandle case in void processExprRelatedNode(AST_NODE* exprRelatedNode)\n");
		exprRelatedNode->dataType = ERROR_TYPE;
		break;
	}
}

void codeGenAssignmentStmt(AST_NODE* assignmentStmtNode) {
	AST_NODE* leftOp = assignmentStmtNode->child;
	AST_NODE* rightOp = leftOp->rightSibling;
	codeGenExprRelatedNode(rightOp);

	if (leftOp->dataType != rightOp->dataType) {
		if (leftOp->dataType == INT_TYPE) {
			rightOp->registerIndex = codeGenConvertFromFloatToInt(rightOp->registerIndex);
		} else {
			rightOp->registerIndex = codeGenConvertFromIntToFloat(rightOp->registerIndex);
		}
	}

	if (SEMVAL_ID(leftOp).kind == NORMAL_ID) {
		if (leftOp->dataType == INT_TYPE) {
			char* rightOpRegName = NULL;
			codeGenPrepareRegister(INT_REG, rightOp->registerIndex, 1, 0, &rightOpRegName);
			if (!isGlobalVariable(SEMVAL_ID(leftOp).symbolTableEntry)) {
				fprintf(g_codeGenOutputFp, "str %s, [x29, #%d]\n", rightOpRegName, SEMVAL_ID(leftOp).symbolTableEntry->attribute->offsetInAR);
			} else {
				int tmp_reg_index = getRegister(INT_REG);
				char *tmp_reg_name = intRegisterName_64[tmp_reg_index] ;
				fprintf(g_codeGenOutputFp, "ldr %s, =_g_%s\n", tmp_reg_name, SEMVAL_ID(leftOp).identifierName);
				fprintf(g_codeGenOutputFp, "str %s, [%s, #0]\n", rightOpRegName, tmp_reg_name);
				freeRegister(INT_REG, tmp_reg_index);
			}
			leftOp->registerIndex = rightOp->registerIndex;
		} else if (leftOp->dataType == FLOAT_TYPE) {
			char* rightOpRegName = NULL;
			codeGenPrepareRegister(FLOAT_REG, rightOp->registerIndex, 1, 0, &rightOpRegName);
			if (!isGlobalVariable(SEMVAL_ID(leftOp).symbolTableEntry)) {
				fprintf(g_codeGenOutputFp, "str %s, [x29, #%d]\n", rightOpRegName, SEMVAL_ID(leftOp).symbolTableEntry->attribute->offsetInAR);
			} else {
				int tmp_reg_index = getRegister(INT_REG);
				char *tmp_reg_name = intRegisterName_64[tmp_reg_index] ;
				fprintf(g_codeGenOutputFp, "ldr %s, =_g_%s\n", tmp_reg_name, SEMVAL_ID(leftOp).identifierName);
				fprintf(g_codeGenOutputFp, "str %s, [%s, #0]\n", rightOpRegName, tmp_reg_name);
				freeRegister(INT_REG, tmp_reg_index);
			}
			leftOp->registerIndex = rightOp->registerIndex;
		}
	} else if (SEMVAL_ID(leftOp).kind == ARRAY_ID) {
		int elementAddressRegIndex = codeGenCalcArrayElementAddress(leftOp);

		char* elementAddressRegName = NULL;
		codeGenPrepareRegister_64(INT_REG, elementAddressRegIndex, 1, 0, &elementAddressRegName);
		if (leftOp->dataType == INT_TYPE) {
			char* rightOpRegName = NULL;
			codeGenPrepareRegister(INT_REG, rightOp->registerIndex, 1, 1, &rightOpRegName);
			fprintf(g_codeGenOutputFp, "str %s, [%s, #0]\n", rightOpRegName, elementAddressRegName);

			leftOp->registerIndex = rightOp->registerIndex;
		} else if (leftOp->dataType == FLOAT_TYPE) {
			char* rightOpRegName = NULL;
			codeGenPrepareRegister(FLOAT_REG, rightOp->registerIndex, 1, 0, &rightOpRegName);
			fprintf(g_codeGenOutputFp, "str %s, [%s, #0]\n", rightOpRegName, elementAddressRegName);

			leftOp->registerIndex = rightOp->registerIndex;
		}

		freeRegister(INT_REG, elementAddressRegIndex);
	}

	assignmentStmtNode->registerIndex = leftOp->registerIndex;
}


void codeGenAssignOrExpr(AST_NODE* testNode) {
	if (testNode->nodeType == STMT_NODE) {
		if (SEMVAL_STMT(testNode).kind == ASSIGN_STMT) {
			codeGenAssignmentStmt(testNode);
		} else if (SEMVAL_STMT(testNode).kind == FUNCTION_CALL_STMT) {
			codeGenFunctionCall(testNode);
		}
	} else {
		codeGenExprRelatedNode(testNode);
	}
}


void codeGenWhileStmt(AST_NODE* whileStmtNode) {
	AST_NODE* boolExpression = whileStmtNode->child;

	int constantZeroLabelNumber = -1;
	if (boolExpression->dataType == FLOAT_TYPE) {
		float zero = 0.0f;
		constantZeroLabelNumber = codeGenConstantLabel(FLOATC, &zero);
	}

	int labelNumber = getLabelNumber();
	fprintf(g_codeGenOutputFp, "_whileTestLabel_%d:\n", labelNumber);

	codeGenAssignOrExpr(boolExpression);

	if (boolExpression->dataType == INT_TYPE) {
		char* boolRegName = NULL;
		codeGenPrepareRegister(INT_REG, boolExpression->registerIndex, 1, 0, &boolRegName);
		fprintf(g_codeGenOutputFp, "cmp %s, #0\n", boolRegName);
		fprintf(g_codeGenOutputFp, "beq _whileExitLabel_%d\n", labelNumber);
		freeRegister(INT_REG, boolExpression->registerIndex);
	} else if (boolExpression->dataType == FLOAT_TYPE) {
		fprintf(g_codeGenOutputFp, "ldr %s, _CONSTANT_%d\n", floatWorkRegisterName[0], constantZeroLabelNumber);
		char* boolRegName = NULL;
		codeGenPrepareRegister(FLOAT_REG, boolExpression->registerIndex, 1, 1, &boolRegName);
		fprintf(g_codeGenOutputFp, "fcmp %s, %s\n", boolRegName, floatWorkRegisterName[0]);
		fprintf(g_codeGenOutputFp, "beq _whileExitLabel_%d\n", labelNumber);
		freeRegister(FLOAT_REG, boolExpression->registerIndex);
	}

	AST_NODE* bodyNode = boolExpression->rightSibling;
	codeGenStmtNode(bodyNode);

	fprintf(g_codeGenOutputFp, "b _whileTestLabel_%d\n", labelNumber);
	fprintf(g_codeGenOutputFp, "_whileExitLabel_%d:\n", labelNumber);
}


void codeGenForStmt(AST_NODE* forStmtNode) {
	AST_NODE* initStmtNode = forStmtNode->child;
    AST_NODE* condExprNode = initStmtNode->rightSibling;
    AST_NODE* postStmtNode = condExprNode->rightSibling;
    AST_NODE* bodyNode     = postStmtNode->rightSibling;

    int constantZeroLabelNumber = -1;
	if (condExprNode->dataType == FLOAT_TYPE) {
		float zero = 0.0f;
		constantZeroLabelNumber = codeGenConstantLabel(FLOATC, &zero);
	}

	int labelNumber = getLabelNumber();

	// for head
	codeGenGeneralNode(initStmtNode);
	freeRegister(initStmtNode->dataType, initStmtNode->registerIndex);

	// for condition
	fprintf(g_codeGenOutputFp, "_forTestLabel_%d:\n", labelNumber);
	codeGenGeneralNode(condExprNode);

	char* boolRegName = NULL;
	if (condExprNode->dataType == INT_TYPE) {
		codeGenPrepareRegister(INT_REG, condExprNode->registerIndex, 1, 0, &boolRegName);
		fprintf(g_codeGenOutputFp, "cmp %s, #0\n", boolRegName);
		freeRegister(INT_REG, condExprNode->registerIndex);
	} else if (condExprNode->dataType == FLOAT_TYPE) {  // FLOAT_TYPE
		codeGenPrepareRegister(FLOAT_REG, condExprNode->registerIndex, 1, 1, &boolRegName);
		fprintf(g_codeGenOutputFp, "fcmp %s, #0.0\n", boolRegName);
		freeRegister(FLOAT_REG, condExprNode->registerIndex);
	}

	fprintf(g_codeGenOutputFp, "beq _forExitLabel_%d\n", labelNumber);
	fprintf(g_codeGenOutputFp, "b _forBodyLabel_%d\n", labelNumber);

	// for post statement
	fprintf(g_codeGenOutputFp, "_forIncLabel_%d:\n", labelNumber);
	codeGenGeneralNode(postStmtNode);
	freeRegister(postStmtNode->dataType, postStmtNode->registerIndex);

	fprintf(g_codeGenOutputFp, "b _forTestLabel_%d\n", labelNumber);

	// for body
	fprintf(g_codeGenOutputFp, "_forBodyLabel_%d:\n", labelNumber);
	codeGenStmtNode(bodyNode);

	fprintf(g_codeGenOutputFp, "b _forIncLabel_%d\n", labelNumber);
	fprintf(g_codeGenOutputFp, "_forExitLabel_%d:\n", labelNumber);
}


void codeGenIfStmt(AST_NODE* ifStmtNode) {
	AST_NODE* boolExpression = ifStmtNode->child;

	int constantZeroLabelNumber = -1;
	if (boolExpression->dataType == FLOAT_TYPE) {
		float zero = 0.0f;
		constantZeroLabelNumber = codeGenConstantLabel(FLOATC, &zero);
	}

	int labelNumber = getLabelNumber();

	codeGenAssignOrExpr(boolExpression);

	if (boolExpression->dataType == INT_TYPE) {
		char* boolRegName = NULL;
		codeGenPrepareRegister(INT_REG, boolExpression->registerIndex, 1, 0, &boolRegName);
		fprintf(g_codeGenOutputFp, "cmp %s, #0\n", boolRegName);
		fprintf(g_codeGenOutputFp, "beq _elseLabel_%d\n", labelNumber);
		freeRegister(INT_REG, boolExpression->registerIndex);
	} else if (boolExpression->dataType == FLOAT_TYPE) {
		fprintf(g_codeGenOutputFp, "ldr %s, _CONSTANT_%d\n", floatWorkRegisterName[0], constantZeroLabelNumber);
		char* boolRegName = NULL;
		codeGenPrepareRegister(FLOAT_REG, boolExpression->registerIndex, 1, 1, &boolRegName);
		fprintf(g_codeGenOutputFp, "vcmp.f32 %s, %s\n", boolRegName, floatWorkRegisterName[0]);
		fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
		fprintf(g_codeGenOutputFp, "beq _whileExitLabel_%d\n", labelNumber);
		freeRegister(FLOAT_REG, boolExpression->registerIndex);
		codeGenPrepareRegister(FLOAT_REG, boolExpression->registerIndex, 1, 1, &boolRegName);
	}

	AST_NODE* ifBodyNode = boolExpression->rightSibling;
	codeGenStmtNode(ifBodyNode);

	fprintf(g_codeGenOutputFp, "b _ifExitLabel_%d\n", labelNumber);
	fprintf(g_codeGenOutputFp, "_elseLabel_%d:\n", labelNumber);
	AST_NODE* elsePartNode = ifBodyNode->rightSibling;
	codeGenStmtNode(elsePartNode);
	fprintf(g_codeGenOutputFp, "_ifExitLabel_%d:\n", labelNumber);
}


void codeGenReturnStmt(AST_NODE* returnStmtNode) {
	AST_NODE* returnVal = returnStmtNode->child;
	if (returnVal->nodeType != NUL_NODE) {
		codeGenExprRelatedNode(returnVal);

		if (returnStmtNode->dataType != returnVal->dataType) {
			if (returnStmtNode->dataType == INT_TYPE) {
				returnVal->registerIndex = codeGenConvertFromFloatToInt(returnVal->registerIndex);
			} else {  // FLOAT_TYPE
				returnVal->registerIndex = codeGenConvertFromIntToFloat(returnVal->registerIndex);
			}
		}

		char* returnValRegName = NULL;
		if (returnVal->dataType == INT_TYPE) {
			codeGenPrepareRegister(INT_REG, returnVal->registerIndex, 1, 0, &returnValRegName);
			fprintf(g_codeGenOutputFp, "mov w0, %s\n", returnValRegName);
			freeRegister(INT_REG, returnVal->registerIndex);
		} else if (returnVal->dataType == FLOAT_TYPE) {
			codeGenPrepareRegister(FLOAT_REG, returnVal->registerIndex, 1, 0, &returnValRegName);
			fprintf(g_codeGenOutputFp, "fmov s0, %s\n", returnValRegName);
			freeRegister(FLOAT_REG, returnVal->registerIndex);
		}
	}
	fprintf(g_codeGenOutputFp, "b _end_%s\n", g_currentFunctionName);
}


void codeGenStmtNode(AST_NODE* stmtNode) {
	//printSourceFile(g_codeGenOutputFp, stmtNode->linenumber);

	if (stmtNode->nodeType == NUL_NODE) {
		return;
	} else if (stmtNode->nodeType == BLOCK_NODE) {
		codeGenBlockNode(stmtNode);
	} else {
		switch (stmtNode->semantic_value.stmtSemanticValue.kind) {
		case WHILE_STMT:
			codeGenWhileStmt(stmtNode);
			break;
		case FOR_STMT:
			codeGenForStmt(stmtNode);
			break;
		case ASSIGN_STMT:
			codeGenAssignmentStmt(stmtNode);
			if (stmtNode->child->dataType == INT_TYPE) {
				freeRegister(INT_REG, stmtNode->child->registerIndex);
			} else if (stmtNode->child->dataType == FLOAT_TYPE) {
				freeRegister(FLOAT_REG, stmtNode->child->registerIndex);
			}
			break;
		case IF_STMT:
			codeGenIfStmt(stmtNode);
			break;
		case FUNCTION_CALL_STMT:
			codeGenFunctionCall(stmtNode);
			if (stmtNode->registerIndex != -1) {
				if (stmtNode->dataType == INT_TYPE) {
					freeRegister(INT_REG, stmtNode->registerIndex);
				} else if (stmtNode->dataType == FLOAT_TYPE) {
					freeRegister(FLOAT_REG, stmtNode->registerIndex);
				}
			}
			break;
		case RETURN_STMT:
			codeGenReturnStmt(stmtNode);
			break;
		default:
			printf("Unhandle case in void processStmtNode(AST_NODE* stmtNode)\n");
			break;
		}
	}
}


void codeGenGeneralNode(AST_NODE* node) {

	AST_NODE *traverseChildren = node->child;
	switch (node->nodeType) {
	case VARIABLE_DECL_LIST_NODE:
		while (traverseChildren) {
			AST_NODE* typeNode = traverseChildren->child;
			AST_NODE* idNode = typeNode->rightSibling;

			while (idNode) {
				if (SEMVAL_ID(idNode).kind == WITH_INIT_ID) {

					if (typeNode->dataType == INT_TYPE) {
						// XXX: use work reg instead
						int tmpRegIndex = getRegister(INT_REG);
						char* regName;
						int constLabelNum = codeGenConstantLabel(INTEGERC, &SEMVAL_EXPR(idNode->child).constEvalValue.iValue);
						codeGenPrepareRegister(INT_REG, tmpRegIndex, 0, 0, &regName);

						if (idNode->child->dataType != INT_TYPE) {
							printf("WARNING: should use matching type (int) when initialization\n");
						} else {
							fprintf(g_codeGenOutputFp, "ldr %s, _CONSTANT_%d\n", regName, constLabelNum);
							fprintf(g_codeGenOutputFp, "str %s, [x29, #%d]\n", regName, SEMVAL_ID(idNode).symbolTableEntry->attribute->offsetInAR);
						}
						freeRegister(INT_REG, tmpRegIndex);

					} else {  // FLOAT_TYPE
						// XXX: use work reg instead
						int tmpRegIndex = getRegister(FLOAT_REG);
						char* regName;
						int constLabelNum = codeGenConstantLabel(FLOATC, &SEMVAL_EXPR(idNode->child).constEvalValue.fValue);
						codeGenPrepareRegister(FLOAT_REG, tmpRegIndex, 0, 0, &regName);

						if (idNode->child->dataType != FLOAT_TYPE) {
							printf("WARNING: should use matching type (float) when initialization\n");
						} else {
							fprintf(g_codeGenOutputFp, "ldr %s, _CONSTANT_%d\n", regName, constLabelNum);
							fprintf(g_codeGenOutputFp, "str %s, [x29, #%d]\n", regName, SEMVAL_ID(idNode).symbolTableEntry->attribute->offsetInAR);
						}
						freeRegister(FLOAT_REG, tmpRegIndex);

					}
				}
				idNode = idNode->rightSibling;
			}

			traverseChildren = traverseChildren->rightSibling;
		}
		break;
	case STMT_LIST_NODE:
		while (traverseChildren) {
			codeGenStmtNode(traverseChildren);
			traverseChildren = traverseChildren->rightSibling;
		}
		break;
	case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
		while (traverseChildren) {
			codeGenAssignOrExpr(traverseChildren);

			if (traverseChildren->rightSibling) {
				if (traverseChildren->dataType == INT_TYPE) {
					freeRegister(INT_REG, traverseChildren->registerIndex);
				} else if (traverseChildren->dataType == FLOAT_TYPE) {
					freeRegister(FLOAT_REG, traverseChildren->registerIndex);
				}
			} else {
				// only save the last result as the value
				// of the whole expression list
				node->registerIndex = traverseChildren->registerIndex;
			}
			traverseChildren = traverseChildren->rightSibling;
		}
		break;
	case NONEMPTY_RELOP_EXPR_LIST_NODE:
		while (traverseChildren) {
			codeGenExprRelatedNode(traverseChildren);
			if (traverseChildren->rightSibling) {
				if (traverseChildren->dataType == INT_TYPE) {
					freeRegister(INT_REG, traverseChildren->registerIndex);
				} else if (traverseChildren->dataType == FLOAT_TYPE) {
					freeRegister(FLOAT_REG, traverseChildren->registerIndex);
				}
			} else {
				node->registerIndex = traverseChildren->registerIndex;
			}
			traverseChildren = traverseChildren->rightSibling;
		}
		break;
	case NUL_NODE:
		break;
	default:
		printf("Unhandle case in void codeGenGeneralNode(AST_NODE *node), type=%d\n", node->nodeType);
		break;
	}
}
