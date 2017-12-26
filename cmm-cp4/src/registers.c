#include <stdio.h>
#include "codegen.h"
#include "symbolTable.h"
#include "registers.h"

const char* regGPName[] = {
    // temporatory reg.
    "x9", "x10", "x11", "x12", "x13", "x14", "x15",
    // callee-saved reg.
    "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28",
};

const char* regIntName[] = {
    // temporatory reg.
    "w9", "w10", "w11", "w12", "w13", "w14", "w15",
    // callee-saved reg.
    "w19", "w20", "w21", "w22", "w23", "w24", "w25", "w26", "w27", "w28",
};

const char* regFloatName[] = {
    // temporatory reg.
    "s16", "s17", "s18", "s19", "s20", "s21",
    // callee-sased reg.
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
    "s8", "s9", "s10", "s11", "s12", "s13", "s14", "s15",
};

// http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.den0024a/ch09s01s01.html
const char* regWorkName[] = {
    "w16", "w17", "s22", "s23"
};

const int regIntCount = REGISTER_INT_COUNT;
const int regFloatCount = REGISTER_FLOAT_COUNT;
int regIntFreeCount = 0;
int regFloatFreeCount = 0;
int regIntIsUsed[REGISTER_INT_COUNT];
int regFloatIsUsed[REGISTER_FLOAT_COUNT];

////////////////////

void resetOffset() {
    g_AROffset = 0;
    g_maxOffsetInProcedure = 0;
}

void increaseOffset(SymbolAttribute* attribute) {
    int variableSize = getSizeByType(attribute->attr.typeDescriptor);
    g_AROffset = g_AROffset - variableSize;
    attribute->offset = g_AROffset;
}

int getFreeRegister(REGISTER_TYPE type) {
    if (type == REG_INT || type == REG_GENERAL) {
        for (int i = 0; i < REGISTER_INT_COUNT; i++) {
            if (!regIntIsUsed[i]) {
                regIntIsUsed[i] = 1;
                regIntFreeCount--;
                return i;
            }
        }
    } else {  // REG_FLOAT
        for (int i = 0; i < REGISTER_FLOAT_COUNT; i++) {
            if (!regFloatIsUsed[i]) {
                regFloatIsUsed[i] = 1;
                regFloatFreeCount--;
                return i;
            }
        }
    }
    fprintf(stderr, "Warning: register used up...\n");
    return -1;
}

void freeRegister(REGISTER_TYPE type, int idx) {
    if (type == REG_INT || type == REG_GENERAL) {
        if (!regIntIsUsed[idx]) {
            fprintf(stderr, "Warning: reg #%d is not yet used\n", idx);
            return;
        }
        regIntIsUsed[idx] = 0;
        regIntFreeCount++;
    } else {
        if (!regFloatIsUsed[idx]) {
            fprintf(stderr, "Warning: reg #%d is not yet used\n", idx);
            return;
        }
        regFloatIsUsed[idx] = 0;
        regFloatFreeCount++;
    }
}

void freeTempRegisters() {
    // for (int i = 0; i < REGISTER_COUNT; i++) {
    //     regIsUsed[i] = 0;
    // }

    // regFreeCount = REGISTER_COUNT;
}
