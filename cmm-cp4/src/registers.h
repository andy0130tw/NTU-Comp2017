#ifndef __REGISTERS_H__
#define __REGISTERS_H__

#include "symbolTable.h"

int g_AROffset;
int g_regNumber;
int g_maxOffsetInProcedure;

#define REGISTER_INT_COUNT   (int)(sizeof(regIntName)   / sizeof(regIntName[0]))
#define REGISTER_FLOAT_COUNT (int)(sizeof(regFloatName) / sizeof(regFloatName[0]))

extern const char* regGPName[];
extern const char* regIntName[];
extern const char* regFloatName[];
extern int regIntFreeCount;
extern int regFloatFreeCount;
extern int regIntIsUsed[];
extern int regFloatIsUsed[];

typedef enum {
    REG_INT,
    REG_FLOAT,
    REG_GENERAL
} REGISTER_TYPE;

void resetOffset();
void increaseOffset(SymbolAttribute*);
int getFreeRegister(REGISTER_TYPE);
void freeRegister(REGISTER_TYPE, int idx);
void freeAllRegisters();

#endif  // __REGISTERS_H__
