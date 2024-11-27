all: compile_task3 run_task3

compile_task3:
	erlc product_dist_task3.erl

run_task3:
	erl -noshell -eval "product_dist_task3:start(3, 2, 50)." -s init stop

clean:
	rm -f *.beam *.dump
