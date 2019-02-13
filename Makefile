.PHONY: watch, lint, run

watch:
	stack build --file-watch

lint:
	hlint src

run:
	stack build && stack exec particles
