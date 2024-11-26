all: compile run

compile:
	erlc product_dist_task1.erl

run:
	erl -noshell -eval "product_dist_task1:start(3, 2, 50)." -s init stop

clean:
	rm -f *.beam
