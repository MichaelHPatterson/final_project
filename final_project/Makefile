all: read_write.ml psetdict.ml
	corebuild read_write.native
	corebuild psetdict.native

read_write: corebuild read_write.native

psetdict:
	corebuild psetdict.native

clean:
	rm -rf _build *.native
