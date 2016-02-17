OCB_FLAGS=-I src -use-ocamlfind
CC = ocamlbuild
OCB = $(CC) $(OCB_FLAGS)

all: coinche

coinche:
	$(OCB) $@.native

run: coinche
	./coinche.native

clean:
	$(CC) -clean
