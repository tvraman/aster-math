#Note: when linking lex and yacc object code give -ly -ll not -ll -ly
OBJ= lispify.o
CFLAGS = -O4
CLEXFLAGS = -ll
CFLEXFLAGS =-lfl
FLEXFLAGS = -t
LEX = flex
CC = gcc
lispify: $(OBJ)
	$(CC)  $(CFLAGS) $(OBJ)  $(CFLEXFLAGS)  -o $@
lispify.c: lispify.l
	rm -f lispify.c
	$(LEX)  $(FLEXFLAGS) lispify.l >   $@
clean:
	rm -f lispify.o lispify.c lispify 
	
