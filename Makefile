EBIN_DIR := ebin
SRC_DIR := src
INCLUDE_DIR := include
ERLC := erlc
ERL := erl
ERLC_FLAGS := -o $(EBIN_DIR)
ERL_FLAGS := -detached -pa $(EBIN_DIR)
LOG_DIR := logs
DUMP := erl_crash.dump

all: 
	@ mkdir -p $(EBIN_DIR)
	@ mkdir -p $(LOG_DIR)
	@ $(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/*.erl
	@ cp $(SRC_DIR)/badalisk.app $(EBIN_DIR)
	@ $(ERL) $(ERL_FLAGS) -s systools make_script 'badalisk_rel-1' local -s init stop
clean: 
	@ rm -rf $(EBIN_DIR)
	@ rm -rf $(LOG_DIR)
	@ rm -rf badalisk_rel-1.script badalisk_rel-1.boot
	@ rm -rf $(DUMP)
