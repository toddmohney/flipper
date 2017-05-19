.PHONY: stylish-haskell

stylish-haskell:
	find ./test -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i && \
		find ./src -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i && \
