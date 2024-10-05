# Yocto

A custom programming language compiler built with OCaml and LLVM.

## Build & Run

```bash
# Build
ocamlbuild -use-ocamlfind -pkgs llvm -use-menhir main.byte

# Run - WIP
```

## Dependencies

- OCaml
- LLVM
- Menhir

## Simple program using custom lang

```

fn calc(int a, int b){
int n = a*b
write(n)
}

fn main(){
int res = calc(4, 6)
write(res)
return 0
}

```
