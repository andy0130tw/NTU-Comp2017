#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"

static FILE* g_outfile = NULL;

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

void codegen(AST_NODE* root) {
    const char* fname_out = "output.s";
    FILE* fp_out = fopen(fname_out, "w");
    g_outfile = fp_out;
    g_outfile = stdout;
    // cgProgramNode(root);
    fclose(fp_out);
}
