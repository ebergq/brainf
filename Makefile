BIN_DIR = bin
MKDIR_P = mkdir -p

.PHONY: directories cleanall

all: test

directories: ${BIN_DIR}

${BIN_DIR}:
	@${MKDIR_P} ${BIN_DIR}

clean:
	@rm -rf $(BIN_DIR)

# Haskell
$(BIN_DIR)/haskell.bf.out: src/haskell/Main.hs directories
	@ghc -o $(BIN_DIR)/haskell.bf.out src/haskell/Main.hs -outputdir $(BIN_DIR)

haskell: $(BIN_DIR)/haskell.bf.out

test: haskell
	@$(BIN_DIR)/haskell.bf.out < examples/helloworld.bf
