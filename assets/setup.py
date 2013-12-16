#! /usr/bin/env python2
"""
Replacement (unofficial) `setup.py` for the core `openerp` library, to create
an `openerp-core` package. The name `openerp` is not use since it might be used
by an official distribution.
"""

import setuptools

# We don't package openerp/tests/addons.
# We support only Linux.
setuptools.setup(
    name             = 'openerp-core',
    # Keep the official version.
    # TODO This is wrong for trunk.
    version          = '7.0.1',
    description      = 'Core OpenERP package',
    # TODO
    long_description = """Core OpenERP Package""",
    url              = 'http://www.openerp.com',
    # Should these be changed ?
    author           = 'OpenERP s.a.',
    author_email     = 'info@openerp.com',
    classifiers      = [
      'Development Status :: 5 - Production/Stable',
      'Environment :: No Input/Output (Daemon)',
      'Environment :: Web Environment',
# TODO'Framework :: OpenERP',
      'Intended Audience :: Developers',
      'License :: OSI Approved :: GNU Affero General Public License v3',
      'Natural Language :: English',
      'Operating System :: POSIX :: Linux',
      'Programming Language :: Python :: 2.7',
      'Programming Language :: Python :: Implementation :: CPython',
      'Topic :: Software Development :: Libraries :: Application Frameworks',
    ],
    license          = 'AGPL-3',
    scripts          = ['openerp-server'], # TODO Package separately oe.
    packages         = setuptools.find_packages(),
    package_data     = {
        # csv, xml, js, css, po, pot, sql, sxw, xsl, rng
        'openerp': ['*.rng'],
        'openerp.addons.base' : [
            '*.csv', '*.sql', '*.xml',
            'data/*.csv', 'data/*.png',
            'i18n/*.po*',
            'report/*.css', 'report/*.xml', 'report/*.xsl',
            'rng/*.rng',
            'security/*.csv', 'security/*.xml',
            'static/src/css/*.css', 'static/src/js/*.js', 'static/src/img/*.png',
            'test/*.xml', 'test/*.yml',
        ],
        'openerp.addons.base.ir' : [
            '*.xml',
            'report/*.sxw', 'report/*.xml',
        ],
        'openerp.addons.base.ir.wizard' : ['*.xml'],
        'openerp.addons.base.module' : ['*.xml', 'report/*.sxw'],
        'openerp.addons.base.module.wizard' : ['*.xml'],
        'openerp.addons.base.res' : [
            '*.png', '*.csv', '*.xml', '*.yml',
            'config_pixmaps/*.png',
            'report/*.xml', 'report/*.xsl'
        ],
        'openerp.addons.base.workflow' : ['*.xml'], # For trunk.
        'openerp.addons.base.ir.workflow' : ['*.xml'], # For 7.0.
    },
    dependency_links = ['http://download.gna.org/pychart/'],
    # TODO Keep only the openerp-core dependencies here.
    install_requires = [
        'babel',
        'coverage', # TODO for oe initialize --coverage, make it optional.
        'docutils',
        #'feedparser',
        #'gdata',
        #'Jinja2',
        'lxml',
        'mako',
        #'mock',
        'pillow',
        'psutil',
        'psycopg2',
        # pychart is not on pypi, use:
        #   > pip install http://download.gna.org/pychart/PyChart-1.39.tar.gz
        # or the dependency_links above.
        'pychart',
        'pydot',
        # pyparsing is a pydot dependency but pydot does not require pyparsing
        # < 2 although pyparsing >= 2 is Python 3 only.
        'pyparsing < 2',
        'python-dateutil < 2',
        #'python-openid',
        'pytz',
        #'pywebdav',
        'pyyaml',
        'reportlab',
        'simplejson',
        'unittest2',
        #'vatnumber',
        #'vobject',
        'werkzeug',
        #'xlwt',
    ],
    extras_require = {
        'SSL': ['pyopenssl'],
    },
    tests_require = ['unittest2']
)
