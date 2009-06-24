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

install: build
	mkdir -p /usr/local/lib/erlang/lib/erlang-ruby-marshal/ebin
	mkdir -p /usr/local/lib/erlang/lib/erlang-ruby-marshal/include
	cp -R $(EBIN_DIR)/*.beam /usr/local/lib/erlang/lib/erlang-ruby-marshal/ebin
	cp -R $(INCLUDE_DIR)/*.hrl /usr/local/lib/erlang/lib/erlang-ruby-marshal/include