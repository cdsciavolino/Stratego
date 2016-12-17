default:
	ocamlbuild board.byte
	ocamlbuild display.byte
	ocamlbuild game.byte
	ocamlbuild main.byte
	ocamlbuild ai.byte

test:
	ocamlbuild -pkgs oUnit,str,unix test.byte && ./test.byte

play:
	ocamlbuild -pkgs oUnit,ANSITerminal main.byte && ./main.byte