# learning rust

## installation

macosx

```
$ curl https://sh.rustup.rs -sSf | sh
```

```
$ source $HOME/.cargo/env
```

## bald source

```
$ rustc main.rs
```

AOT compile

## project

```
$ cargo new hello_cargo --bin
```

--bin means to genearte executable instead of library

no --bin will be the same by default

```
$ cargo build
```

or, with --release to build release

```
$ cargo run
```

in fact it runs target/debug/hello\_cargo


