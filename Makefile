.PHONY: devel hlint stylish-haskell test

devel:
	stack test feature-flipper --file-watch --exec="$(MAKE) hlint"

test:
	stack build examples/environment-config && \
		stack test feature-flipper --exec="$(MAKE) hlint"

hlint:
	hlint src

stylish-haskell:
	find ./test -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i && \
		find ./src -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i
		find ./examples -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i
