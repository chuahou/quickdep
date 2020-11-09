########
Quickdep
########

|License: MIT|

*A quick tool for Debian packaging systems to temporarily install a list
of dependencies*

Quickdep creates a metapackage depending on a specified list of
dependencies, allowing for installation of that metapackage to cleanly
install—and later remove—these dependencies, preserving manual / auto
installation marking. It should be useful for temporarily installing
dependencies for compiling something from source, for example.

Installation
============

**USE WITH CAUTION ON YOUR OWN SYSTEMS.** I use this for my own systems
but I have no confidence it will not break with other usecases. The
creation of the package should not be damaging, but installing the
generated package should be done with caution—check the contents first!

To install, simply navigate to the project directory and run::

	make
	sudo make install

Alternatively, download a binary release.

Usage
=====

Creation
--------

To create a metapackage named *package* with dependencies *foo, bar,
hoo*, run::

	quickdep package foo bar hoo

which should create a package ``package_1.0_all.deb``. To install it,
run::

	sudo apt-get install ./package_1.0_all.deb

and to remove the package with the temporary dependencies, run::

	sudo apt-get purge --auto-remove ./package_1.0_all.deb

By installing a metapackage instead of directly installing dependencies,
you can ensure that they are all removed after as they will not be
marked as manually installed.

Using a meta file
-----------------

*Since 0.3.0.0*

If you create a newline-separated list of dependencies in a file named
<package-name>.meta, you can simply run::

	quickdep <package-name>.meta
	
to create the corresponding metapackage named <package-name>_1.0_all.deb.

Using quickdep-mgr
------------------

*Since 0.3.0.0*

By replacing ``quickdep`` with ``quickdep-mgr create``, the package will
be created in ~/.local/share/quickdep, and can be removed with
``quickdep-mgr remove <package-name>``. The list of packages installed
using ``quickdep-mgr`` can be shown using ``quickdep-mgr list``.

Note that if you create multiple packages with the same name, only the
latest one will persist!

Dependencies
============

* equivs (``apt-get install equivs``)
* cabal-install
* ghc-8.8.4
* make

.. |License: MIT| image:: https://img.shields.io/badge/License-MIT-yellow.svg
	:target: https://opensource.org/licenses/MIT
