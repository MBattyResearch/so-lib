# SO Lib

This breaks out the framework for building SO formulae from PrideMM
for easier use in other projects.

## Build

You will need some opam packages:

```
opam install batteries ppx_deriving, sedlex opal
```

Then you can run `make` to call ocamlbuild appropriately: it should output `./example.byte`

## Running

You need the SO solver on your path, a binary is included in
`bin/`. There is a script to set your evironment variables to include
`bin/` in `$PATH`.

```
. ./setenv
```

You can then run the example, which iterates through a series of SO
formulae and gives True/False determinations.

```
./example.byte
```


## SO Language

