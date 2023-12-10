CC=gcc
CFLAGS=-g -Wall -D_FILE_OFFSET_BITS=64
LDFLAGS=-lfuse

OBJ=rufs.o block.o

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

rufs: $(OBJ)
	$(CC) $(OBJ) $(LDFLAGS) -o rufs

stress_tests:
	$(CC) -g -o stress_tests stress_tests.c

.PHONY: clean
clean:
	rm -f *.o rufs stress_tests
