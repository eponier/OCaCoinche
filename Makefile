OCB_FLAGS=-I src -use-ocamlfind -mod 'gtkInit' -lflag "lablgtkextras.cmxa"
CC = ocamlbuild
OCB = $(CC) $(OCB_FLAGS)

all: coinche

coinche:
	$(OCB) $@.native

clean:
	$(CC) -clean
