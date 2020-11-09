#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
# Copyright (c) 2020 Chua Hou
#
# Wrapper around quickdep.

set -e

print_usage () {
	cat <<EOF
Usage: $0 <package-name>.meta
	Creates a metapackage named <package-name> from <package-name>.meta, which
	is a newline separated list of dependencies with lines starting with # being
	ignored.
Usage: $0 <package-name> <dependencies>
	Creates a metapackage named <package-name> with dependencies as
	space-separated arguments in <dependencies>.
EOF
}

if [ $# -lt 1 ]; then
	print_usage
	exit 1
fi

PKG_NAME=$(basename $1 | sed -n 's/^\(.*\).meta$/\1/p')
if [ ! -z $PKG_NAME ]; then # <package-name>.meta usage
	if [ $# -gt 1 ]; then # too many arguments
		print_usage
		exit 1
	fi
	grep -v '^\s*#' $1 | sed '/^\s*$/d' | xargs quickdep-internal $PKG_NAME
else
	if [ $# -le 1 ]; then # too little arguments
		print_usage
		exit 1
	fi
	quickdep-internal $@
fi
