all: compile_task1 run_task1 compile_task2 run_task2

compile_task1:
	erlc product_dist_task1.erl

compile_task2:
	erlc product_dist_task2.erl

run_task1:
	erl -noshell -eval "product_dist_task1:start(3, 2, 50)." -s init stop

run_task2:
	erl -noshell -eval "product_dist_task2:start(3, 2, 50)." -s init stop

task1: compile_task1 run_task1

task2: compile_task2 run_task2

clean:
	rm -f *.beam