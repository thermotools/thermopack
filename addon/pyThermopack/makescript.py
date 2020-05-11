#!/usr/bin/env python
"""This script uses f2py to create a python layer for Thermopack.

The python layer is coupled with utility functions and packaged in the pytp
package, which may be installed on ones system. See README.txt for more info.
"""

from __future__ import print_function

import os
import sys
import glob
import shutil
import subprocess
import textwrap


class dotdict(dict):
    """dot.notation access to dictionary attributes"""
    __getattr__ = dict.get
    __setattr__ = dict.__setitem__
    __delattr__ = dict.__delitem__

def build():
    """Combine and run the build command."""
    opts = get_opts()

    cmd = (get_f2py()
           + ' --verbose -c -m {modname} --fcompiler={fc}'
           + ' --compiler={cc} --f90flags="{fflags}"'
           + ' --opt="{opt}" {include} {link} {src} {libs}').format(**opts)

    cmd_print = textwrap.wrap(cmd, width=72)
    print('Command :: ' + cmd_print[0])
    for line in cmd_print[1:]:
        print('    ' + line)
    print('')

    print('Building ... ', end='')
    sys.stdout.flush()
    with open('makescript.log', 'w') as f:
        result = subprocess.call(cmd, shell=True, stdout=f, stderr=f)

    if result == 0:
        print('completed.')
        files = glob.glob('%s*%s' % (opts.modname, opts.outext))
        if not files or len(files) > 1:
            print('Could not find the appropriate output object file.')
            sys.exit()
        else:
            outfile = files[0]
        shutil.move(outfile, os.path.join('pytp', outfile))
    else:
        print('failed!')
        print('Please check makescript.log for errors!')
        sys.exit(1)

def get_f2py():
    """Get the appropriate f2py executable"""
    candidates = [os.path.basename(x)
                  for p in os.environ['PATH'].split(':')
                  for x in glob.glob(os.path.join(p, 'f2py*'))]
    if not candidates:
        print('ERROR: f2py command not found!')
        print('       Make sure f2py is installed and in PATH.')
        sys.exit()
    elif len(candidates) == 1:
        return candidates[0]
    else:
        for wanted in ['f2py%d' % sys.version_info[0], 'f2py']:
            if wanted in candidates:
                return wanted

        print('ERROR: Ambiguous f2py commands!')
        print("       Can't decide which f2py command is appropriate.")
        sys.exit()

def get_opts():
    """Set build options."""
    opts = dotdict({
        'modname' : 'tp',
        'fc' : 'gfortran',
        'tp_libname' : '-lthermopack',
        'fflags' : '-fPIC -fdefault-real-8 -fdefault-double-8',
        'opt' : '-O0',
        'srcs' : [''],
        'links' : [''],
        'includes' : [''],
        'tp_root' : os.path.join('..', '..'),
    })

    if os.name == 'nt':
        set_opts_win(opts)
    elif os.name == 'posix':
        set_opts_linux(opts)
    else:
        print('Unrecognized operating system')
        sys.exit()

    opts.srcs.append(os.path.join('src', 'pyThermopack_wrappers.f90'))
    opts.srcs.append(os.path.join('src', 'pyThermopack_wrappers_tv.f90'))
    opts.srcs.append(os.path.join('src', 'pyThermopack_wrappers_SAFT.f90'))
    opts.links.append(os.path.join(opts.tp_root, opts.libdir))
    opts.includes.append(os.path.join(opts.tp_root, opts.objdir))
    opts.includes.append(os.path.join(opts.dep_root, 'addon', 'trend_interface', 'include'))

    opts.include = ' -I'.join(opts.includes)
    opts.link = ' -L'.join(opts.links)
    opts.libs = '%s %s' % (opts.tp_libname, opts.extra_libs)
    opts.src = ' '.join(opts.srcs)

    return opts

def set_opts_win(opts):
    """Sets build options for Windows systems."""
    opts.cc = 'mingw32'
    opts.objdir = 'mingw32'
    opts.libdir = 'mingw32'
    opts.outext = '.pyd'
    opts.dep_root = os.path.join(opts.tp_root, '..')
    opts.extra_libs = ('-lstdc++ -lmsvcr90 -lnum'
                       '-ltrend_normal_gfortran_MINGW32_NT-6.1'
                       '-llapack -lrefblas')
    opts.links = [
        os.path.join(opts.dep_root, 'libnum', opts.libdir),
        os.path.join(opts.dep_root, 'trend', 'bin'),
        os.path.join(opts.dep_root, 'lapack'),
        ".",
    ]

def set_opts_linux(opts):
    """Sets build options for Linux systems."""
    try:
        mode = sys.argv[1]
        target = '%s_%s_Linux' % (mode, opts.fc)
    except IndexError:
        print('Usage: python makescript.py debug/optim/etc...')
        sys.exit()

    opts.tp_libname = '%s_%s' % (opts.tp_libname, target)
    opts.cc = 'unix'
    opts.objdir = os.path.join('objects', target)
    opts.libdir = 'bin'
    opts.outext = '.so'
    opts.dep_root = opts.tp_root
    opts.extra_libs = '-llapack -lstdc++'

    if mode == 'optim':
        opts.opt = '-O3'


build()
