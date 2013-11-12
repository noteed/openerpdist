# openerpdist - Packaging and distributing OpenERP

`openerpdist` builds the OpenERP framework and addons as nice tarballs, ready
to be uploaded to pypi with minimal changes from upstream sources.

This project is unsupported by OpenERP s.a.

Please note that even if `openerpdist` itself uses a 3-clause BSD license, the
resulting packages remain under the AGPL license.

## Details

`openerpdist` builds packages ready to be consumed by the Python
infrastructure. In particular, it uses the regular `setup.py` files (using
`setuptools`), and the resulting tarballs are installable using e.g. `pip`.

One possible usage is to generate the desired packages and make them available
through a web server with an HTML listing (e.g. Nginx with `autoindex on`).
Packages can then be installed for instance in a new virtual env:

    > virtualenv2 oe-env
    > oe-env/bin/pip install openerp-sale \
        --no-index \
        --find-links http://localhost/tarballs
        
or alternatively you can edit [`$HOME/.pip/pip.conf`](http://www.pip-installer.org/en/latest/configuration.html#configuration) file and append:

    [global]
    find-links =
        http://localhost/tarballs

and then just use:

    > virtualenv2 oe-env
    > oe-env/bin/pip install openerp-sale

Package dependencies are extracted from the `__openerp__.py` descriptor files
and listed in the generated `setup.py` files. This means that installing, say,
`openerp-sale` will install all its dependencies, including `openerp-core` (the
package for the server/framework).

## Install

The development version can be installed by cloning the Git repository and using cabal:

    > git clone git://github.com/noteed/openerpdist.git
    > cd openerpdist && cabal install

## Usage

`openerpdist` is simple to use, either within the server source directory, or
within a specific addons, or from a directory containing multiple addons.

The command

    > openerpdist patch

when run inside a serve source tree, will replace the existing `setup.py` and
`MANIFEST.in` files; for addons it will generate a `setup.py` file. Note that
it will first check with `bzr diff` if the directory (assumed to be a `.bzr`
repository) is clean.

Once done, you can simply execute

    > openerpdist sdist

to generate some nice tarballs inside the newly created `dist` directories.

## Other dependencies

`openerpdist` lists the correct dependencies in the `setup.py` file. This is
enough for Python dependencies. But in addition, some of those dependencies,
and thus indirectly the packaged OpenERP libraries, need libraries and
programs to be installed with your operating system facilities.

On Ubuntu 12.04 those additional dependencies are:

(The list is incomplete for now.)

- libjpeg-dev
- zlib1g-dev

libjpeg-dev and zlib1g-dev are for JPEG and ZIP support in Pillow (`openerpdist`
sets Pillow instead of PIL).

## Limitations / TODO

- Assume the directories where `openerpdist` is run are `bzr` repositories.
- Does not generate a README file from the `__openerp__.py` descriptor.
- The version used for each package is hard-coded to 7.0.1.
- The listed dependencies for `openerp-core` are not correct (too many things,
  required by some addons) while the ones for the addons don't list the
  non-openerp dependencies.

