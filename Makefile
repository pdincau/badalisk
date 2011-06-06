EBIN_DIR := ebin
SRC_DIR := src
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -o $(EBIN_DIR)
LOG_DIR := logs

all: 
	@ mkdir -p $(EBIN_DIR)
	@ mkdir -p $(LOG_DIR)
	@ $(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/*.erl
	@ cp $(SRC_DIR)/badalisk.app $(EBIN_DIR)
clean: 
	@ rm -rf $(EBIN_DIR)
	@ rm -rf $(LOG_DIR)
	@ rm -rf erl_crash.dump
