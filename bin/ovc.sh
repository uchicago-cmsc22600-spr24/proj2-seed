#!/bin/sh
#
# Shell wrapper for Ovid compiler (ovc.sh)
#
# CMSC 22600 --- Compilers for Computer Languages
# Spring 2024
# University of Chicago
#

OVC=$0
BINDIR=${OVC%ovc.sh}

HEAP_SUFFIX=$(sml @SMLsuffix)

HEAP="$BINDIR/ovc.$HEAP_SUFFIX"

if test ! -r "$HEAP" ; then
  echo "$OVC: no heap image; run make in src directrory to build compiler"
  exit 1
fi

exec sml @SMLcmdname=ovc @SMLload="$HEAP" "$@"
