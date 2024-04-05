# Makefile for class project
#
# COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
# All rights reserved.
#
# CMSC 22600 --- Compilers for Computer Languages
# Spring 2024
# University of Chicago
#
# targets:
#	make ovc	-- Build Ovid compiler and copy heap file to bin
#			   directory
#

SHELL =         /bin/sh
OS =            $(shell uname -s)

# we assume that sml (and thus ml-build is in the path)
#
ML_BUILD =	ml-build

HEAP_SUFFIX =	$(shell sml @SMLsuffix)

BINDIR =	../bin
LIBDIR =	../lib

CM_FILES =	$(wildcard */sources.cm)

COMMON_SRCS =	bind-checker/binding.sml \
		bind-checker/context.sml \
		bind-tree/bind-basis.sml \
		bind-tree/bind-tree.sml \
		bind-tree/dump-bind-tree.sml \
		bind-tree/identifier-fn.sml \
		common/dump-util.sml \
		common/error.sml \
		common/options.sml \
		common/stamp.sml \
		parse-tree/dump-parse-tree.sml \
		parse-tree/op-names.sml \
		parse-tree/parse-tree.sml \
		parser/parser.sml \
		parser/ovid.grm \
		parser/ovid.lex

OVC_SRCS =	$(COMMON_SRCS)

.PHONY:		ovc
ovc:		ovc.$(HEAP_SUFFIX)
	mv ovc.$(HEAP_SUFFIX) $(BINDIR)

# build rule for compiler
ovc.$(HEAP_SUFFIX):	$(CM_FILES) $(OVC_SRCS)
	$(ML_BUILD) driver/sources.cm Main.main ovc

.PHONY:		clean
clean:
		rm -rf bin/*.$(HEAP_SUFFIX)
		rm -rf .cm */.cm
