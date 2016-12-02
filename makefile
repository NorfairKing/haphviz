SOURCES = $(shell find $(SRC_DIR) -type f -name '*.hs')

doc: $(SOURCES)
	  stack haddock --haddock-options="--no-warnings --no-print-missing-docs --pretty-html"

