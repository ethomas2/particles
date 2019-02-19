.PHONY: watch, lint, run, test

watch:
	stack build --file-watch

lint:
	hlint src

run:
	stack build && stack exec particles

test:
	stack test
