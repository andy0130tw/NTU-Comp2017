#include "symbolTable.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
// This file is for reference only, you are not required to follow the implementation. //

int HASH(char * str) {
    int idx = 0;
    while (*str) {
        idx = idx << 1;
        idx += *str;
        str++;
    }
    return (idx & (HASH_TABLE_SIZE - 1));
}

SymbolTable symbolTable;

SymbolTableEntry* newSymbolTableEntry(int nestingLevel) {
    SymbolTableEntry* symbolTableEntry = (SymbolTableEntry*)malloc(sizeof(SymbolTableEntry));
    symbolTableEntry->nextInHashChain = NULL;
    symbolTableEntry->prevInHashChain = NULL;
    symbolTableEntry->nextInSameLevel = NULL;
    symbolTableEntry->sameNameInOuterLevel = NULL;
    symbolTableEntry->attribute = NULL;
    symbolTableEntry->name = NULL;
    symbolTableEntry->nestingLevel = nestingLevel;
    return symbolTableEntry;
}

void removeFromHashTrain(int hashIndex, SymbolTableEntry* entry) {
    // find previous entry on the chain;
    // if it is the first entry of the chain, modify its head instead
    SymbolTableEntry** pred = entry->prevInHashChain ?
        &entry->prevInHashChain->nextInHashChain :
        &symbolTable.hashTable[hashIndex];

    *pred = entry->nextInHashChain;

    if (entry->nextInHashChain) {
        entry->nextInHashChain->prevInHashChain = entry->prevInHashChain;
    }

    // detach from the hash chain, and this entry should be manually freed
    entry->nextInHashChain = NULL;
    entry->prevInHashChain = NULL;
}

void enterIntoHashTrain(int hashIndex, SymbolTableEntry* entry) {
    SymbolTableEntry** head = &symbolTable.hashTable[hashIndex];

    // insert as the first entry
    if (*head) {
        (*head)->prevInHashChain = entry;
        entry->nextInHashChain = *head;
    }
    *head = entry;
}

SymbolAttribute* allocateSymbolAttribute(SymbolAttributeKind attrType, const void* ptrToTempAttrDesc) {
    SymbolAttribute* attr = (SymbolAttribute*) malloc(sizeof(SymbolAttribute));
    void* desc;
    attr->attributeKind = attrType;
    switch (attrType) {
    case TYPE_ATTRIBUTE: case VARIABLE_ATTRIBUTE:
        desc = malloc(sizeof(TypeDescriptor));
        memcpy(desc, ptrToTempAttrDesc, sizeof(TypeDescriptor));
        attr->attr.typeDescriptor = (TypeDescriptor*) desc;
        break;
    case FUNCTION_SIGNATURE:
        desc = malloc(sizeof(FunctionSignature));
        memcpy(desc, ptrToTempAttrDesc, sizeof(FunctionSignature));
        attr->attr.functionSignature = (FunctionSignature*) desc;
        break;
    default: __builtin_unreachable();
    }
    return attr;
}

void initializeSymbolTable() {
    const int scopeDisplayElemCntInit = 4;

    symbolTable = (SymbolTable) {
        .hashTable = {},
        .currentLevel = 0,
        .scopeDisplayElementCount = scopeDisplayElemCntInit,
        .scopeDisplay = (SymbolTableEntry**) calloc(scopeDisplayElemCntInit, sizeof(SymbolTableEntry*))
    };

    // TODO: add pre-defined symbols into symbol table
    struct {
        const char* name;
        SymbolAttributeKind attrType;
        union {
            TypeDescriptor typedesc;
            FunctionSignature funcsig;
        } u;
    } predefinedSymbols[] = {
        { SYMBOL_TABLE_INT_NAME,   TYPE_ATTRIBUTE, .u.typedesc = { SCALAR_TYPE_DESCRIPTOR, .properties.dataType = INT_TYPE   } },
        { SYMBOL_TABLE_FLOAT_NAME, TYPE_ATTRIBUTE, .u.typedesc = { SCALAR_TYPE_DESCRIPTOR, .properties.dataType = FLOAT_TYPE } },
        { SYMBOL_TABLE_VOID_NAME,  TYPE_ATTRIBUTE, .u.typedesc = { SCALAR_TYPE_DESCRIPTOR, .properties.dataType = VOID_TYPE  } },
        { SYMBOL_TABLE_SYS_LIB_READ,  FUNCTION_SIGNATURE, .u.funcsig = { 0, NULL, INT_TYPE   } },
        { SYMBOL_TABLE_SYS_LIB_FREAD, FUNCTION_SIGNATURE, .u.funcsig = { 0, NULL, FLOAT_TYPE } },
        // FIXME: this function signature is dummy; check of "write" is made in a specialized function for overloading;
        // only return type is checked!
        { SYMBOL_TABLE_SYS_LIB_WRITE, FUNCTION_SIGNATURE, .u.funcsig = { 0, NULL, VOID_TYPE  } },
    };

    for (size_t i = 0; i < sizeof(predefinedSymbols) / sizeof(predefinedSymbols[0]); i++) {
        SymbolAttribute* attr = allocateSymbolAttribute(predefinedSymbols[i].attrType, &predefinedSymbols[i].u);
        enterSymbol((char*) predefinedSymbols[i].name, attr);
    }
}

void symbolTableEnd() {
}

SymbolTableEntry* retrieveSymbol(char* symbolName) {
    SymbolTableEntry* entry = symbolTable.hashTable[HASH(symbolName)];

    while (entry != NULL) {
        if (strcmp(symbolName, entry->name) == 0) {
            return entry;
        }
        entry = entry->nextInHashChain;
    }
    return NULL;
}

SymbolTableEntry* enterSymbol(char* symbolName, SymbolAttribute* attribute) {
    int idx = HASH(symbolName);
    SymbolTableEntry* entry = symbolTable.hashTable[idx];
    SymbolTableEntry* curr = newSymbolTableEntry(symbolTable.currentLevel);
    curr->name = symbolName;
    curr->attribute = attribute;

    // search for symbols of the same name
    while (entry) {
        if (strcmp(symbolName, entry->name) == 0) {
            if (entry->nestingLevel == curr->nestingLevel) {
                fprintf(stderr, "[%s:%d] Attempt to redeclare a symbol [%s] in level %d\n",
                    __func__, __LINE__, symbolName, entry->nestingLevel);
                return NULL;
            }
            // "covers" the definition of outer scope
            removeFromHashTrain(idx, entry);
            curr->sameNameInOuterLevel = entry;
            break;
        }
        entry = entry->nextInHashChain;
    }

    enterIntoHashTrain(idx, curr);

    // insert into display
    curr->nextInSameLevel = symbolTable.scopeDisplay[symbolTable.currentLevel];
    symbolTable.scopeDisplay[symbolTable.currentLevel] = curr;

    return curr;
}

//remove the symbol from the current scope
void removeSymbol(char* symbolName) {
    int idx = HASH(symbolName);
    SymbolTableEntry* entry = symbolTable.hashTable[idx];

    while (entry) {
        if (strcmp(symbolName, entry->name) == 0) {
            if (entry->nestingLevel != symbolTable.currentLevel) {
                fprintf(stderr, "[%s:%d] Attempt to remove a symbol [%s] from level %d, "
                    "but it is declared in outer scope of level %d.",
                    __func__, __LINE__, symbolName, entry->nestingLevel, symbolTable.currentLevel);
                return;
            }
            removeFromHashTrain(idx, entry);
            // restore entry of the same name from outer scope if there's one
            if (entry->sameNameInOuterLevel) {
                enterIntoHashTrain(idx, entry->sameNameInOuterLevel);
            }
            break;
        }
        entry = entry->nextInHashChain;
    }

    if (!entry) {
        fprintf(stderr, "[%s:%d] Attempt to remove an undeclared symbol [%s].",
            __func__, __LINE__, symbolName);
        return;
    }

    // remove from display
    SymbolTableEntry* scopeEntry = symbolTable.scopeDisplay[symbolTable.currentLevel];
    SymbolTableEntry* scopeEntryPrev = NULL;
    while (scopeEntry) {
        if (strcmp(symbolName, scopeEntry->name) == 0) {
            if (scopeEntryPrev) {
                scopeEntryPrev->nextInSameLevel = scopeEntry->nextInSameLevel;
            } else {
                symbolTable.scopeDisplay[symbolTable.currentLevel] = scopeEntry->nextInSameLevel;
            }
            break;
        }
        scopeEntryPrev = scopeEntry;
        scopeEntry = scopeEntry->nextInSameLevel;
    }
}

int declaredLocally(char* symbolName) {
    int idx = HASH(symbolName);
    SymbolTableEntry* entry = symbolTable.hashTable[idx];

    // search for symbols of the same name
    while (entry) {
        if (strcmp(symbolName, entry->name) == 0) {
            return (entry->nestingLevel == symbolTable.currentLevel);
        }
        entry = entry->nextInHashChain;
    }
    return 0;
}

void openScope() {
    int newSize = ++symbolTable.currentLevel;
    // enlarge it if too small
    if (newSize == symbolTable.scopeDisplayElementCount) {
        symbolTable.scopeDisplayElementCount *= 2;
        symbolTable.scopeDisplay = (SymbolTableEntry**) realloc(
            symbolTable.scopeDisplay,
            symbolTable.scopeDisplayElementCount * sizeof(SymbolTableEntry*));
    }
    symbolTable.scopeDisplay[newSize] = NULL;
}

void closeScope() {
    if (symbolTable.currentLevel < 0) {
        fprintf(stderr, "[%s:%d] Attempt to close beyond global scope\n", __func__, __LINE__);
        return;
    }

    // clear (or replace if there's one from the outer) symbols declared in current scope
    // also wipe it from the hash table
    SymbolTableEntry* scopeEntry = symbolTable.scopeDisplay[symbolTable.currentLevel];
    while (scopeEntry) {
        int idx = HASH(scopeEntry->name);
        removeFromHashTrain(idx, scopeEntry);
        if (scopeEntry->sameNameInOuterLevel) {
            enterIntoHashTrain(idx, scopeEntry->sameNameInOuterLevel);
        }
        scopeEntry = scopeEntry->nextInSameLevel;
    }

    // exit this scope
    symbolTable.currentLevel--;
}
