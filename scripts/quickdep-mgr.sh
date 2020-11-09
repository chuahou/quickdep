#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
# Copyright (c) 2020 Chua Hou
#
# Runs quickdep and stores the package in the data directory, allowing for
# listing/management of quickdep packages.

set -e

DATA_DIR=$HOME/.local/share/quickdep
[ ! -d $DATA_DIR ] && mkdir -p $DATA_DIR

print_usage  () {
	cat <<EOF
Usage: $0 create <package-name>.meta
       $0 create <package-name> <dependencies>
	Calls quickdep with these arguments, storing the created metapackage in
	$DATA_DIR and installing it with apt-get.
Usage: $0 remove <package-name>
	Purges <package-name> with apt-get with --auto-remove, removing the
	corresponding package in $DATA_DIR.
Usage: $0 list
	Lists packages previously created and not yet uninstalled.
EOF
}

if [ $# -lt 1 ]; then
	print_usage
	exit 1
fi

if [ $# -ge 2 ]; then
	PKG_NAME=$(sed 's/\.meta$//' <<< $2)
	DEB_NAME=${PKG_NAME}_1.0_all.deb
fi

case $1 in
	create)
		(
			if [ $# -lt 2 ]; then
				print_usage
				exit 1
			fi
			cd $DATA_DIR
			[ -f ./$DEB_NAME ] && rm ./$DEB_NAME
			shift 1
			quickdep $@
			sudo apt-get install ./$DEB_NAME
		)
		;;
	remove)
		(
			if [ $# -lt 2 ]; then
				print_usage
				exit 1
			fi
			cd $DATA_DIR
			if [ ! -f ./$DEB_NAME ]; then
				echo "No such package installed by quickdep-mgr"
				exit 1
			fi
			sudo apt-get purge --auto-remove $PKG_NAME
			rm ./$DEB_NAME
		)
		;;
	list)
		ls $DATA_DIR | sed 's/_1\.0_all\.deb$//'
		;;
	*) print_usage; exit 1 ;;
esac
