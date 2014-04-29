all: read_write.ml psetdict.ml helpers.ml
	corebuild read_write.native
	corebuild psetdict.native
	corebuild helpers.native

read_write: corebuild read_write.native

psetdict:
	corebuild psetdict.native

helpers:
	corebuild helpers.native

clean:
	rm -rf _build *.native
