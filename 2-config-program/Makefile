BIN_DIR := ./out/bin
CONFIG_PROGRAM_BIN := $(BIN_DIR)/config-program

$(CONFIG_PROGRAM_BIN):
	stack build --copy-bins --local-bin-path $(BIN_DIR)

build: $(CONFIG_PROGRAM_BIN) ## build application
.PHONY: build

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
