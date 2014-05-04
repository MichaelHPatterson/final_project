Instructions for compilation:

Two prerequisites are required for compilation:
(1) the OCaml corebuild compiler, and
(2) the JaneStreet standard library.

If both of these are present, along with other necessary OCaml installations,
then one must simply run the "make" command while in the code/ directory.

Instructions for running the program:

The program will be run with ./main.native. Three operations are possible with
./main.native:

(1) running the NxN matching algorithm. This is accomplished with the following
syntax:
./main.native input_file output_file

where input_file is an existing .txt file that meets the invariant of

owner_name1
elt_3 : rank
elt_4 : rank
elt_2 : rank
...
owner_name2
elt_5 : rank
elt_9 : rank
...

...

owner_nameN
...

Note that any owner can have rankings for any number or combination of elts. The
only necessary restriction is that the total number of owners must equal the
total number of elements.

output_file can be a nonexistent file (with a .txt extension) or an existing
.txt that can and will be overwritten.

(2) adding/updating an owner-elt ranking in place. The syntax is:
./main.native update input_file owner elt value

where input_file is an existing .txt file that meets the aforementioned
invariant. owner is an owner in that .txt, and elt is an element in the .txt;
value is the ranking that this (owner,element) pair should be updated to

(3) removing an owner-elt ranking in place. The syntax is:
./main.native remove input_file owner elt

where input_file is an existing .txt file that meets the aforementioned
owner is an owner in that .txt, and elt is an element in the .txt; the (owner,
elt) pair should be one that should have its ranking reset to the default
ranking.


Note on Testing

There are run_tests() functions in both matrix.ml and hungarian.ml. In order to
test these, an additional call would need to be coded at the end of these files,
and ./matrix.native and/or ./hungarian.native would need to be called.
