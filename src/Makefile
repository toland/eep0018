# Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
# Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
#
# This file is part of EEP0018, which is released under the MIT
# license.

# Get the erlang version, root directory, etc.
ERTS_VSN=$(shell erl -noshell -eval "io:format(\"~s\n\", [erlang:system_info(version)]), halt(0).")
ROOT_DIR=$(shell erl -noshell -eval "io:format(\"~s\n\", [code:root_dir()]), halt(0).")
ERTS_DIR=$(ROOT_DIR)/erts-$(ERTS_VSN)
EI_DIR=$(shell erl -noshell -eval "io:format(\"~s\n\", [code:lib_dir(erl_interface)]), halt(0).")

INCLUDES = -I$(ERTS_DIR)/include -I$(EI_DIR)/include
LIBS = -L$(EI_DIR)/lib -lerl_interface -lei

include Makefile.$(shell uname -s)

CFLAGS += $(GCCFLAGS) $(INCLUDES)
LDFLAGS += $(GCCFLAGS) $(LIBS)

OBJECTS = \
	eep0018.o \
	json_to_term.o \
	term_buf.o \
	term_to_json.o \
	yajl.o \
	yajl_encode.o \
	yajl_lex.o \
	yajl_buf.o \
	yajl_parser.o

EBIN_DIR=../ebin
PRIV_DIR=../priv

DRIVER = $(PRIV_DIR)/eep0018_drv.so
BEAM = $(EBIN_DIR)/eep0018.beam

# -- rules --------------------------------------------------------------------

all: $(DRIVER) $(BEAM)

clean: 
	rm -f *.o $(BEAM) $(DRIVER)

$(DRIVER): $(OBJECTS)
	gcc -o $@ $^ $(LDFLAGS)

$(BEAM): eep0018.erl
	erlc +debug_info -o $(EBIN_DIR) $^

check:
	@true
