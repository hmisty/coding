-module(cpu_intensive).
-compile(export_all).

fib_test() ->
    fib(40),
    fib(40),
    fib(40).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

