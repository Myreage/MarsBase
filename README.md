# MarsBase

## Compile
```bash
ocamlc analyse.cm*
ocaml analyse.cmo < phase1.ml
```
## Possible makefile
```
all: main1 main2 main3

.cmi.mli
	@ocamlc -c $<

.cmo.ml
	@ocamlc -c $<

main1: graph.cmo toto.cmo
	@ocamlc $^ main.ml -o $@

```
