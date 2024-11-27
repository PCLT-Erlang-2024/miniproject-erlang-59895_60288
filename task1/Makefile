all: compile_task1 run_task1

compile_task1:
	erlc product_dist_task1.erl

run_task1:
	erl -noshell -eval "product_dist_task1:start(3, 2, 50)." -s init stop

clean:
	rm -f *.beam *.dump
