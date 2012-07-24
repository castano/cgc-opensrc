
OBJS=atom.o binding.o cgcmain.o cgstruct.o check.o compile.o constfold.o cpp.o generic_hal.o hal.o ihash.o inline.o memory.o parser.o printutils.o scanner.o semantic.o stdlib.o support.o support_iter.o symbols.o tokens.o

TOKENIZE_OBJS=atom.o cgstruct.o scanner.o tokenize.o tokens.o

cgc: $(OBJS)
	$(CC) -o $@ $^

tokenize: $(TOKENIZE_OBJS)
	$(CC) -o $@ $^
	
stdlib.c: stdlib.cg tokenize
	./tokenize stdlib.cg >stdlib.c

parser.c parser.h: parser.y
	bison parser.y --defines --output=parser.c --verbose

clean:
	rm -f *.o cgc tokenize stdlib.c
