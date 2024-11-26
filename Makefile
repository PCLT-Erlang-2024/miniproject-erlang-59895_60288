all: compile run

compile:
	erlc product_dist_system.erl

run:
	erl -noshell -eval "product_dist_system:start(3, 2, 50)." -s init stop

clean:
	rm -f product_dist_system.beam
