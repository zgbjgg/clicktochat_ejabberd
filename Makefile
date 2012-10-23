## -------------------------------------------------------------------
## Makefile : Makefile for clicktochat
## Copyright (c) 2012 All Rights Reserved.
## Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
## -------------------------------------------------------------------
#
# path to ejabberd ebin, is necessary for implements and compile
# all .erl in src directory
# 
PATH_TO_EJABBERD_EBIN = /usr/lib/ejabberd/ebin/

#
# path to include files from ejabberd, is necessary for include and
# compile all .erl in src directory
#
PATH_TO_EJABBERD_INCLUDE = /usr/lib/ejabberd/include/

#
# path to config directory for ejabberd, where the .conf files
# will be placed
#
PATH_TO_EJABBERD_ETC = /etc/ejabberd/

.PHONY: compile

all: make_dir compile

make_dir:
	mkdir -p ebin
	cp -r include/clicktochat.hrl $(PATH_TO_EJABBERD_INCLUDE)

compile:
	erlc -I $(PATH_TO_EJABBERD_INCLUDE) -pz $(PATH_TO_EJABBERD_EBIN) -o ebin/ src/*.erl

clean:
	rm -rf ebin/*

install:
	cp -r include/*.hrl $(PATH_TO_EJABBERD_INCLUDE)
	cp -r ebin/*.beam $(PATH_TO_EJABBERD_EBIN)
	cp -r clicktochat.conf $(PATH_TO_EJABBERD_ETC)
