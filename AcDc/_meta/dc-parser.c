#include <stdio.h>
#include <stdlib.h>

typedef union {
    int ival;
    float fval;
} value;

int stack_top;
value stack[1024];
value reg[32];  // 0: 'a' - 25: 'z'
char errBuf[128];

enum PrecisionType {
    ENUM_PREC_INT = 0,
    ENUM_PREC_FLOAT = 5
};

static inline int isoper(char c) {
    return c == '+' || c == '-' || c == '*' || c == '/';
}

int readReg(char* buf, enum PrecisionType pt, value* var) {
    switch (pt) {
    case ENUM_PREC_INT:
        var->ival = atoi(buf);
        return 0;
    case ENUM_PREC_FLOAT:
        var->fval = atof(buf);
        return 0;
    }
    return 1;
}

int main(void) {
    size_t lineCnt = 0;
    ssize_t len;
    char* err = NULL;
    char* line = NULL;
    size_t sz = 0;
    int precType = ENUM_PREC_INT;
    value tmp;

    stack_top = 0;
    while ((len = getline(&line, &sz, stdin)) >= 0) {
        line[len--] = '\0';
        lineCnt++;
        if (len <= 0) continue;
        if (line[0] == 's') {
            // stack top -> reg
            printf("> saving #%d to %c\n", stack_top, line[1]);
            reg[line[1] - 'a'] = stack[--stack_top];
        } else if (line[0] == 'l') {
            stack[stack_top++] = reg[line[1] - 'a'];
        } else if (line[0] == 'p') {
            if (precType == ENUM_PREC_INT) {
                printf("%d\n", stack[stack_top - 1].ival);
            } else {
                printf("%f\n", stack[stack_top - 1].fval);
            }
        } else if (isoper(line[0])) {
            if (line[0] == '+') {
                // ...
            }
        } else if (line[len - 1] == 'k' && line[len - 2] == ' ') {
            // set prec
            if (line[0] == '0') {
                precType = ENUM_PREC_INT;
            } else if (line[0] == '5') {
                precType = ENUM_PREC_FLOAT;
            } else {
                sprintf(errBuf, "Line %zu: Unrecognized precision specification '%c'", lineCnt, line[0]);
                err = errBuf;
                goto cleanup;
            }
        } else if (readReg(line, precType, &tmp) == 0) {
            // number
            stack[stack_top++] = tmp;
        } else {
            sprintf(errBuf, "Line %zu: Illegal instruction", lineCnt);
            err = errBuf;
            goto cleanup;
        }
    }

cleanup:
    free(line);
    if (err) {
        fprintf(stderr, "%s\n", err);
        return 1;
    }
}
