# openerpdist - Packaging and distributing OpenERP

`openerpdist` builds the OpenERP framework and addons as nice tarballs, ready
to be uploaded to pypi with minimal changes from upstream sources.

This project is unsupported by OpenERP s.a.

Please note that even if `openerpdist` itself uses a 3-clause BSD license, the
resulting packages remain under the AGPL license.

## Install

The development version can be installed by cloning the Git repository and using cabal:

    > git clone git://github.com/noteed/openerpdist.git
    > cd openerpdist && cabal install

## Usage

`openerpdist` is simple to use, either withing the server source directory, or
within a specific addons.

The command

    > openerpdist patch

when run inside a serve source tree, will replace the existing `setup.py` and
`MANIFEST.in` files, and when run inside a specific addons directory, will
generate a `setup.py` file. Note that it will first check with `bzr diff` if
the directory (assumed to be a `.bzr` repository) is clean.

Once done, you can simple execute

    > python2 setup.py sdist

or alternatively

    > openerpdist sdist

to generate a nice tarball inside the newly created `dist` directory.

## Limitations

- Assume the directories where `openerpdist` is run are `bzr` repositories.
