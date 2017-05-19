.PHONY: stylish-haskell

devel:
	stack test --file-watch

test:
	stack test

stylish-haskell:
	find ./test -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i && \
		find ./src -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i
