# wyo: pp/ tree (the pricklypear history, full lineage, filter-repo'd under
# pp/). Copy of record: scripts/monorepo-pp.Makefile, promoted to pp/Makefile
# by dune (@ci-dirs). Law (2): a push touching pp/** runs THIS target only.
PP_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

.PHONY: all build test ship promote

all: build

build:
	dune build --root=$(PP_DIR)

test:
	dune runtest --root=$(PP_DIR)

ship promote:
	@echo "pp: ship+promote for $(PP_DIR) (path-filtered: pp/** only)"
