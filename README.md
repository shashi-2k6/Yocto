# Yocto

A custom programming language compiler built with OCaml.

## Build & Run

```bash
# Build
ocamlbuild -use-ocamlfind -pkgs menhir,yojson main.byte

# Run
./main.byte <path to input file>
```

## Dependencies

- OCaml
- Menhir

## Simple program using custom lang

```

fn main() {
    a = 10;
    write(12);
    return 0;
}

fn vars(){
    result = 20;
    write(result);
    return 0;
  }

```
