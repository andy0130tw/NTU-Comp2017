TARGET = parser
OBJECT = parser.tab.c parser.tab.o lex.yy.c alloc.o functions.o
CC = gcc
CFLAGS = -g
LEX = flex
YACC = bison
YFLAGS = -v -d
LIBS = -lfl

SVG_OUT = AST_Graph.svg

parser: parser.tab.o alloc.o functions.o
	$(CC) $(CFLAGS) -o $(TARGET) parser.tab.o alloc.o functions.o $(LIBS)

svg: AST_Graph.gv
	dot -Tsvg $< -Nfontname=sans-serif -Efontname=sans-serif -o $(SVG_OUT)

svg-ref: AST_Graph.gv
	node ../checker < $<  \
	  | tail -n +7  \
	  | sed -r 's/(<title>[^&]+)&#45;/\1-/g;'  \
	  | sed -r 's/<([a-z]+)([^\/]+)\/>/<\1\2><\/\1>/g;'  \
	  | sed -r 's/(EXPR_NODE )&#45;/\1-/g;'  \
	  | sed -r 's/(CONST_VALUE_NODE )&quot;([^&]*)&quot;/\1"\2"/g;'  \
      > $(SVG_OUT)

parser.tab.o: parser.tab.c lex.yy.c alloc.o functions.c
	$(CC) $(CFLAGS) -c $<

lex.yy.c: lexer3.l
	$(LEX) $<

parser.tab.c: parser.y
	$(YACC) $(YFLAGS) $<

alloc.o: alloc.c
	$(CC) $(CFLAGS) -c $<

functions.o: functions.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(TARGET) $(OBJECT)
