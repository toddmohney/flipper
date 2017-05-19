.PHONY: devel hlint stylish-haskell test

devel:
	stack test --file-watch --exec="$(MAKE) hlint"

test:
	stack test --exec="$(MAKE) hlint"

hlint:
	hlint src

stylish-haskell:
	find ./test -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i && \
		find ./src -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i
