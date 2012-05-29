all:

cpu_intensive: cpu_intensive_native cpu_intensive_normal cpu_intensive_c cpu_intensive_co2 cpu_intensive_co3

cpu_intensive_native:
	@erlc +native cpu_intensive.erl
	@echo ""
	@echo "Fib erlang native code"
	@time erl -noshell -s cpu_intensive fib_test -s erlang halt

cpu_intensive_normal:
	@erlc cpu_intensive.erl
	@echo ""
	@echo "Fib erlang normal code"
	@time erl -noshell -s cpu_intensive fib_test -s erlang halt

cpu_intensive_c:
	@gcc -O0 -o cpu_intensive cpu_intensive.c
	@echo ""
	@echo "Fib C w/out optimization"
	@time ./cpu_intensive

cpu_intensive_co2:
	@gcc -O2 -o cpu_intensive cpu_intensive.c
	@echo ""
	@echo "Fib C w/ O2 optimization"
	@time ./cpu_intensive

cpu_intensive_co3:
	@gcc -O3 -o cpu_intensive cpu_intensive.c
	@echo ""
	@echo "Fib C w/ O3 optimization"
	@time ./cpu_intensive
