#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<math.h>
#include"header.h"

#define TABLE_SIZE  256

symtab * hash_table[TABLE_SIZE];
extern int linenumber;

int idcnt;

int HASH(char * str) {
	unsigned int idx = 0;
	while (*str) {
		idx = *str + (idx << 6) + (idx << 16) - idx;
		str++;
	}
	return (idx & (TABLE_SIZE - 1));
}

/*returns the symbol table entry if found else NULL*/

symtab * lookup(char *name) {
    int hash_key;
    symtab* symptr;
    if (!name)
        return NULL;
    hash_key = HASH(name);
    symptr = hash_table[hash_key];

    while (symptr) {
        if (!(strcmp(name, symptr->lexeme)))
            return symptr;
        symptr = symptr->front;
    }
    return NULL;
}


void insertID(char *name) {
    int hash_key;
    symtab* ptr;
    symtab* symptr = (symtab*)malloc(sizeof(symtab));

    hash_key = HASH(name);
    ptr = hash_table[hash_key];

    if (ptr == NULL) {
        /*first entry for this hash_key*/
        hash_table[hash_key] = symptr;
        symptr->front = NULL;
        symptr->back = symptr;
    } else {
        symptr->front = ptr;
        ptr->back = symptr;
        symptr->back = symptr;
        hash_table[hash_key] = symptr;
    }

	idcnt++;

	strcpy(symptr->lexeme, name);
	symptr->line = linenumber;
	symptr->counter = 1;
}

void printSym(symtab* ptr) {
	printf("%s\t%d\n", ptr->lexeme, ptr->counter);
}

void printSymTab() {
	int i;
	printf("----- Symbol Table ---------\n");
	for (i = 0; i < TABLE_SIZE; i++) {
		symtab* symptr;
		symptr = hash_table[i];
		int empty = 1;
		while (symptr != NULL) {
			if (empty) {
				printf("[index = %d]\n", i);
				empty = 0;
			}
			printSym(symptr);
			symptr = symptr->front;
		}
	}
}

int sortSymFn(const void* _a, const void* _b) {
	symtab* a = *(symtab**) _a;
	symtab* b = *(symtab**) _b;
	return strcmp(a->lexeme, b->lexeme);
}

void printSymFreq() {
	int cnt = 0;
	symtab** list = malloc(sizeof(*list) * idcnt);

	// collect symbols
	for (int i = 0; i < TABLE_SIZE; i++) {
		symtab* symptr = hash_table[i];
		while (symptr) {
			list[cnt++] = symptr;
			symptr = symptr->front;
		}
	}

	qsort(list, cnt, sizeof(*list), sortSymFn);

	puts("Frequency of identifiers:");
	for (int i = 0; i < cnt; i++) {
		printSym(list[i]);
	}
}
