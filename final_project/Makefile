all: matrix polynomial hungarian
	corebuild matrix.native
	corebuild polynomial.native
	corebuild hungarian.native

matrix:
	corebuild matrix.native

polynomial:
	corebuild polynomial.native

hungarian:
	corebuild hungarian.native

check:
	chmod u+x ../check_width
	../check_width modules.ml
	../check_width ps4.ml

clean:
	rm -rf _build *.native
