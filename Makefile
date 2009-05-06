EBIN_DIR = ./ebin
INCLUDE_DIR = ./include
SRC_DIR = ./src

ERLC_FLAGS = -I $(INCLUDE_DIR) -o $(EBIN_DIR)
ERLC = erlc $(ERLC_FLAGS)

ERL_FLAGS = -pa $(EBIN_DIR)
ERL = erl $(ERL_FLAGS)

build: clean
	$(ERLC) $(SRC_DIR)/*.erl

run: build
	$(ERL)

clean:
	rm -fv $(EBIN_DIR)/*.beam
