all: main.ml read_write.ml psetdict.ml helpers.ml hungarian.ml pagerank.ml
	corebuild main.native
	corebuild read_write.native
	corebuild psetdict.native
	corebuild helpers.native
	corebuild hungarian.native
	corebuild matrix.native
	corebuild pagerank.native

read_write: corebuild read_write.native

psetdict:
	corebuild psetdict.native

helpers:
	corebuild helpers.native

matrix:
	corebuild matrix.native

hungarian:
	corebuild hungarian.native

pagerank:
	corebuild pagerank.native

main:
	corebuild main.native

clean:
	rm -rf _build *.native
