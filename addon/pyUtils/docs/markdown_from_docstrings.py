import inspect
from datetime import datetime
from thermopack.thermo import thermo
from tools import remove_illegal_link_chars, THERMOPACK_ROOT, MARKDOWN_DIR


def get_autogen_header(classname):
    header = '<!--- \n'
    header += 'Generated at: ' + datetime.today().isoformat() + '\n'
    header += 'This is an auto-generated file, generated using the script at ' \
              'thermopack/addon/pyUtils/docs/markdown_from_docstrings.py\n'
    header += 'The file is created by parsing the docstrings of the methods in the \n'
    header += classname + ' class. For instructions on how to use the parser routines, see the\n' \
                          'file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py'
    header += '--->\n\n'
    return header


def to_markdown(methods):
    md_text = ''
    for name, meth in methods:
        docparts = meth.__doc__.split('\n\n')
        header_lines = [line.strip() for line in docparts[0].split('\n')]
        header = ' '.join(header_lines[1:]) # Cutting out the section identifier
        header.replace('\n', ' ')

        if len(docparts) > 1:
            content = '\n\n'.join(docparts[1:])
        else:
            content = ''

        content_lines = [line.strip() for line in content.split('\n')]

        md_text += '### `' + name + str(inspect.signature(meth)) + '`\n'
        md_text += header + '\n\n'

        pad = '&nbsp;' * 4 + ' '
        endl = '\n\n'
        for line in content_lines:
            if ('args:' in line.lower()) or ('returns:' in line.lower()):
                md_text += '#### ' + line + endl

            elif ':' in line:
                line = line.split(':')
                md_text += pad + '**' + line[0] + ':** ' + endl
                md_text += 2 * pad + ':'.join(line[1:]) + endl
            else:
                md_text += 2 * pad + line + endl

    return md_text

def split_methods_by_section(sections, methods):
    method_dict = {}
    for name, meth in methods:
        for sec in sections:
            if sec.lower() in meth.__doc__.split('\n')[0].lower():
                if sec in method_dict.keys():
                    method_dict[sec].append((name, meth))
                    break
                else:
                    method_dict[sec] = [(name, meth)]
                    break
        else:
            print('Method :', name, "did not contain a section on the first line of its docstring! Adding to 'Other'")
            print('\t', meth.__doc__.split('\n')[0].lower())

            if 'Other' in method_dict.keys():
                method_dict['Other'].append((name, meth))
            else:
                method_dict['Other'] = [(name, meth)]

    return method_dict

def get_toc(sections, section_headers, method_dict):
    toc_text = '## Table of contents\n'
    for sec in sections:
        if sec not in method_dict.keys():
            continue

        sec_name = section_headers[sec]
        sec_id = remove_illegal_link_chars(sec_name)
        toc_text += '  * [' + sec_name + '](#' + sec_id + ')\n'

        for meth in method_dict[sec]:
            method_name = meth[0].replace('__', '\_\_')
            method_id = meth[0] + str(inspect.signature(meth[1]))
            method_id = remove_illegal_link_chars(method_id)
            toc_text += '    * [' + method_name + '](#' + method_id + ')\n'

    return toc_text + '\n'

def thermo_to_markdown():
    thermo_methods = inspect.getmembers(thermo, predicate=inspect.isfunction)

    sections = ['TV-property',
                'Tp-property',
                'Flash interface',
                'Saturation interface',
                'Isoline',
                'Stability interface',
                'Virial interface',
                'Joule-Thompson interface',
                'Utility',
                'Internal']

    section_headers = {'Internal' : 'Internal methods',
                       'Utility' : 'Utility methods',
                       'TV-property' : 'TV-property interfaces',
                       'Tp-property' : 'Tp-property interfaces',
                       'Flash interface' : 'Flash interfaces',
                       'Saturation interface' : 'Saturation interfaces',
                       'Isoline' : 'Isolines',
                       'Stability interface' : 'Stability interfaces',
                       'Virial interface' : 'Virial interfaces',
                       'Joule-Thompson interface' : 'Joule-Thompson interface',
                       'Other' : 'Other methods'}

    section_intro = {'Internal': 'Methods for handling communication with the Fortran library.',
                       'Utility': 'Methods for setting ... and getting ...',
                       'TV-property': 'Computing properties as a function of temperature and volume. Derivatives '
                                      'returned by methods in this section are computed as functions of (T, V, n).',
                       'Tp-property': 'Computing properties as a function of temperature and pressure. Derivatives '
                                      'returned by methods in this section are computed as functions of (T, p, n).',
                       'Flash interface': 'Methods for flash calculations.',
                       'Saturation interface': 'Bubble- and dew point calculations and phase envelopes.',
                       'Isoline': 'Computing isolines.',
                       'Stability interface': 'Critical point solver.',
                       'Virial interface': 'Retrieve various virial coefficients.',
                       'Joule-Thompson interface': 'Joule-Thompson inversion curves.',
                       'Other': 'Methods that do not have a section identifier in their docstring.'}

    method_dict = split_methods_by_section(sections, thermo_methods)

    ofile_text = get_autogen_header('thermo')
    ofile_text += '# Methods in the thermo class (`thermo.py`)\n\n'
    ofile_text += 'The `thermo` class, found in `addon/pycThermopack/thermo.py`, is the core of the ThermoPack Python interface. ' \
                  'All equation of state classes inherit from `thermo`. This is the class that contains the interface to all ' \
                  'practical calculations that can be done from the python-side of ThermoPack. Derived classes only implement ' \
                  'specific functions for parameter handling etc.\n\n'
    ofile_text += get_toc(sections, section_headers, method_dict)

    for sec in sections:
        if sec not in method_dict.keys():
            continue
        ofile_text += '## ' + section_headers[sec] + '\n\n'
        ofile_text += section_intro[sec] + '\n\n'
        ofile_text += to_markdown(method_dict[sec])

    if 'Other' in method_dict.keys():
        ofile_text += section_headers['Other'] + '\n\n'
        ofile_text += section_intro['Other'] + '\n\n'
        ofile_text += to_markdown(method_dict['Other'])

    with open(MARKDOWN_DIR + 'thermo_methods.md', 'w') as ofile:
        ofile.write(ofile_text)

thermo_to_markdown()