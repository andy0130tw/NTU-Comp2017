TARGET = parser
OBJECT = parser.tab.c parser.tab.o lex.yy.c alloc.o functions.o semanticAnalysis.o symbolTable.o
OUTPUT = parser.output parser.tab.h
CC = gcc
CFLAGS = -g -std=gnu99
LEX = flex
YACC = bison
YFLAGS = -v -d
LIBS = -lfl

parser: parser.tab.o alloc.o functions.o symbolTable.o semanticAnalysis.o
	$(CC) $(CFLAGS) -o $(TARGET) parser.tab.o alloc.o functions.o symbolTable.o semanticAnalysis.o $(LIBS)

parser.tab.o: parser.tab.c lex.yy.c alloc.o functions.c symbolTable.o semanticAnalysis.o
	$(CC) $(CFLAGS) -c parser.tab.c

semanticAnalysis.o: semanticAnalysis.c symbolTable.o
	$(CC) $(CFLAGS) -c semanticAnalysis.c

symbolTable.o: symbolTable.c
	$(CC) $(CFLAGS) -c symbolTable.c

lex.yy.c: lexer3.l
	$(LEX) lexer3.l

parser.tab.c: parser.y
	$(YACC) $(YFLAGS) parser.y

alloc.o: alloc.c
	$(CC) $(CFLAGS) -c alloc.c

functions.o: functions.c
	$(CC) $(CFLAGS) -c functions.c

clean:
	rm -f $(TARGET) $(OBJECT) $(OUTPUT)

