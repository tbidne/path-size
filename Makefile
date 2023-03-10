.PHONY: build clean repl watch ;\
	cic ci formatc format lint lintc ;\
	haddock hackage

# core

ARGS = ""

repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl path-size; \
	else \
		cabal repl $(ARGS); \
	fi

watch:
	if [ -z "$(ARGS)" ]; then \
		ghcid --command "cabal repl path-size" ; \
	else \
		ghcid --command "cabal repl $(ARGS)" ; \
	fi

bench-criterion:
	cabal bench bench-criterion --benchmark-options \
		'--csv bench/criterion/bench.csv --output bench/criterion/bench.html'

bench-tasty:
	cabal bench bench-tasty --benchmark-options \
		'--csv bench/tasty/bench.csv --svg bench/tasty/bench.svg'

# ci

cic: formatc lintc

ci: lint format

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode check ;\
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt -- --check

format:
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode inplace ;\
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.8#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.8#hlint

# generate docs for main package, copy to docs/
haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/path-size-0.1/opt/doc/html/path-size/* docs/

# generate dist and docs suitable for hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
