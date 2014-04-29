all: read_write.ml psetdict.ml helpers.ml matrixops.ml
	corebuild read_write.native
	corebuild psetdict.native
	corebuild helpers.native
	corebuild matrixops.native

read_write: corebuild read_write.native

psetdict:
	corebuild psetdict.native

helpers:
	corebuild helpers.native

matrixtest:
	corebuild matrixops.native

clean:
	rm -rf _build *.native
