all: compile_task2 run_task2

compile_task2:
	erlc product_dist_task2.erl

run_task2:
	erl -noshell -eval "product_dist_task2:start(3, 2, 50)." -s init stop

clean:
	rm -f *.beam *.dump
