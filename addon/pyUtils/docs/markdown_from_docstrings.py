'''
Functions to auto-generate markdown documentation by parsing docstrings

Intent : As long as we keep docstrings consistently formatted, this ensures that we can quickly generate up-to-date
        documentation that is easily accessible. It also ensures that documentation only needs to be updated one place,
        and that place is in the codebase itself.

Usage : Current functionality is designed to parse the docstrings of a given class. It assumes that the docstrings of
        the methods in the class are formatted as

        def myfunc(self, p1, p2, p3, p4=None, p5=<something>, ...):
            """Section Name
            Description of what this function does (a kind of header). We can write lots of stuff here
            NOTE the double lineshift here

            Args:
                 p1 (int) : The lineshift before 'Args' is necessary.
                 p2 (float) : The colons here are also necessary.
                 p3 (bool) : SomethingSomething
                 p4 (Optional, list) : etc.
            Returns:
                (float) : The colon here is also necessary.
            """

    1 :  Use the function get_autogen_header(<classname>) to generate a comment with a timestamp and a description of
         how the markdown file was generated (using this module)

    2 : Define a list of sections that the methods in the class should be divided into, see : thermo_to_markdown() for
        an example. The sections should equate to the 'Section Name' in the above formatting example. The sections in
        the markdown file will be ordered according to the order of this list.

    3 : Define a dict of section name => section header mappings. This maps the section names in the docstrings to
        the section headers in the markdown file.

    4 : Define a dict of section name => section intro mappings. This maps the section names to an intro text that is
        displayed at the start of the section.

    5 : Use
            my_methods = inspect.getmembers(<MyClass>, predicate=inspect.isfunction)
        to extract the list of class methods.

    6 : Use
            method_dict = split_methods_by_section(sections, my_methods)
        to split the methods into a single list for each method, placed in a dict

    7 : Use
            toc_markdown_text = get_toc(sections, section_headers, method_dict)
        to generate a string, consisting of the markdown table of contents (with links)

    8 : Use
            contents = get_markdown_contents(sections, section_headers, section_intro, method_dict)
        to generate the contents of the file

    9 : Concatenate the strings you have generated and write to file.
'''
import copy
import inspect
from warnings import warn
from datetime import datetime
from thermopack.thermo import thermo
from thermopack.saft import saft
from thermopack.saftvrmie import saftvrmie
from thermopack.pcsaft import pcsaft
from thermopack.saftvrqmie import saftvrqmie
from tools import remove_illegal_link_chars, check_is_changed, write_file, THERMOPACK_ROOT, MARKDOWN_DIR


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
    """
    Generate markdown text from the docstring of the methods in the list methods

    Args:
        methods list<tuple<str, function>> : A list of tuples, with the first element being the name of the method,
                                            and the second element being the method itself.
    Returns:
        (str) : A markdown - formatted string with the contents of the docstring.
    """
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
            if ('args:' in line.lower()) or ('returns:' in line.lower()) or ('raises:' in line.lower()):
                md_text += '#### ' + line + endl

            elif ':' in line:
                line = line.split(':')
                md_text += pad + '**' + line[0] + ':** ' + endl
                md_text += 2 * pad + ':'.join(line[1:]) + endl
            else:
                md_text += 2 * pad + line + endl

    return md_text

def split_methods_by_section(sections, methods):
    """
    Organise the various methods of a class into sections, determined by the id 'Section Name' as in the example at
    the top of this file. Warns if there are methods for which no matching section was found.

    Args:
        sections (list<str>) : The name of each section, corresponding to the first line in each docstring.
        methods (list<tuple<str, function>>) : List of tuples, with the first element corresponding to the method
                                                names, and the second to the methods themself.
    Returns:
        dict : With keys corresponding to the sections, and value being a list of the (name, method) tuples with
                the first line of the docstring matching the section (key).
    """
    method_dict = {}
    for name, meth in methods:
        method_sections = [m.strip() for m in meth.__doc__.split('\n')[0].lower().split('&')]
        # A method can be added to several sections, by giving it the header
        # def myfunc():
        #   """Section1 & Section2 & Section5 ... """
        method_has_section = False
        for sec in sections:
            if sec.lower() in method_sections:
                method_has_section = True
                if sec in method_dict.keys():
                    method_dict[sec].append((name, meth))
                else:
                    method_dict[sec] = [(name, meth)]
        if method_has_section is False: # Generate a section called "Other" and add the method to that section. Warn that this is done.
            warn('Method : ' + name + " did not contain a section on the first line of its docstring! Adding to 'Other'",
                 SyntaxWarning, stacklevel=3)
            warn('Section name in docstring is : '+ meth.__doc__.split('\n')[0].lower(), SyntaxWarning, stacklevel=3)

            if 'Other' in method_dict.keys():
                method_dict['Other'].append((name, meth))
            else:
                method_dict['Other'] = [(name, meth)]

        # print('other is : ', method_dict['Other'] if 'Other' in method_dict.keys() else None)
    return method_dict

def get_toc(sections, section_headers, method_dict, include_other=True):
    """
    Generate a table of contents with links to the sections, using the names in section_headers.
    Note: Uses the function tools.remove_illegal_link_chars() to sanitize raw strings, such that they can be used
    in markdown links.

    Args:
        sections (list<str>) : List of the section names, corresponding to the first line in the docstrings
        section_headers (dict<str, str>) : Dict mapping the section names to the section headers in the markdown file
        method_dict (dict<str, list<tuple<str, function>>>) : Mapping the section names to the list of (method name,
                                                                function) tuples that are in the section.
        include_other (bool, optional) : Whether to include the automatically generated 'Other' section in the toc.
                                        Should be set to True when generating the toc for the whole file, and set to
                                        False when generating toc for subsections.
    Returns:
        str : The string representation of the table of contents to be written to the markdown file.
    """
    # Make sure to not modify input
    sections = copy.deepcopy(sections)
    section_headers = copy.deepcopy(section_headers)
    if include_other:
        sections.append('Other')
        section_headers['Other'] = 'Other methods'

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

def get_markdown_contents(sections, section_headers, section_intro, method_dict):
    """
    Iterate through the sections, generate the markdown documentation for all methods in method_dict, and join them
    while adding section headers and section introductions

    Args:
        sections (list<str>) : List of the section names, corresponding to the first line in the docstrings
        section_headers (dict<str, str>) : Dict mapping the section names to the section headers in the markdown file
        section_intro (dict<str, str>) : Dict mapping the section names to the section introduction text.
        method_dict (dict<str, list<tuple<str, function>>>) : Mapping the section names to the list of (method name,
                                                                function) tuples that are in the section.
    Returns:
        str : The markdown text corresponding to the main contents of the file.
    """
    md_text = ''
    for sec in sections:
        if sec not in method_dict.keys():
            continue
        md_text += '## ' + section_headers[sec] + '\n\n'
        md_text += section_intro[sec] + '\n\n'
        md_text +=  '#' + get_toc([sec], section_headers, method_dict, include_other=False) + '\n'
        md_text += to_markdown(method_dict[sec])

    if 'Other' in method_dict.keys():
        print('Writing other section : ', method_dict['Other'])
        md_text += '## Other\n\n'
        md_text += 'Methods that do not have a section identifier in their docstring.\n\n'
        md_text += '#' + get_toc([], {}, method_dict, include_other=True) + '\n'
        md_text += to_markdown(method_dict['Other'])

    return md_text

def thermo_to_markdown():
    """
    Generate markdown documentation file for the thermo class.
    """

    sections = ['TV-property',
                'Tp-property',
                'TVp-property',
                'Other property',
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
                       'TVp-property' : 'TVp-property interfaces',
                       'Other property' : 'Other property interfaces',
                       'Flash interface' : 'Flash interfaces',
                       'Saturation interface' : 'Saturation interfaces',
                       'Isoline' : 'Isolines',
                       'Stability interface' : 'Stability interfaces',
                       'Virial interface' : 'Virial interfaces',
                       'Joule-Thompson interface' : 'Joule-Thompson interface'}

    section_intro = {'Internal': 'Methods for handling communication with the Fortran library.',
                       'Utility': 'Methods for setting ... and getting ...',
                       'TV-property': 'Computing properties as a function of temperature and volume. Derivatives '
                                      'returned by methods in this section are computed as functions of (T, V, n).',
                       'Tp-property': 'Computing properties as a function of temperature and pressure. Derivatives '
                                      'returned by methods in this section are computed as functions of (T, p, n).',
                       'TVp-property' : 'Computing properties given Temperature, volume and mole numbers, but evaluate'
                                        ' derivatives as functions of (T, p, n). See [Advanced Usage => The different'
                                        ' property interfaces](https://github.com/thermotools/thermopack/wiki/Advanced-usage#the-different-property-interfaces-tv--tp--and-tvp-) for further explanation.',
                       'Other property' : 'Property interfaces in other variables than $TV$ or $Tp$, for example computing density given $\mu - T$.',
                       'Flash interface': 'Methods for flash calculations.',
                       'Saturation interface': 'Bubble- and dew point calculations and phase envelopes.',
                       'Isoline': 'Computing isolines.',
                       'Stability interface': 'Critical point solver.',
                       'Virial interface': 'Retrieve various virial coefficients.',
                       'Joule-Thompson interface': 'Joule-Thompson inversion curves.'}

    thermo_methods = inspect.getmembers(thermo, predicate=inspect.isfunction)
    method_dict = split_methods_by_section(sections, thermo_methods)

    ofile_text = get_autogen_header('thermo')
    ofile_text += '# Methods in the thermo class (`thermo.py`)\n\n'
    ofile_text += 'The `thermo` class, found in `addon/pycThermopack/thermo.py`, is the core of the ThermoPack Python interface. ' \
                  'All equation of state classes inherit from `thermo`. This is the class that contains the interface to all ' \
                  'practical calculations that can be done from the python-side of ThermoPack. Derived classes only implement ' \
                  'specific functions for parameter handling etc.\n\n'
    ofile_text += get_toc(sections, section_headers, method_dict)
    ofile_text += get_markdown_contents(sections, section_headers, section_intro, method_dict)

    filename = 'thermo_methods.md'
    write_file(MARKDOWN_DIR + filename, ofile_text)

def saft_to_markdown():
    """
    Generate markdown documentation file for the saft class.
    """

    sections = ['Utility',
                'Internal']

    section_headers = {'Internal' : 'Internal methods',
                       'Utility' : 'Utility methods'}

    section_intro = {'Internal': 'Methods for handling communication with the Fortran library.',
                       'Utility': 'Methods for computing specific parameters and contributions to the residual\n'
                                  'Helmholtz energy for SAFT-type equations of state'}

    saft_methods = inspect.getmembers(saft, predicate=inspect.isfunction)
    thermo_methods = inspect.getmembers(thermo, predicate=inspect.isfunction)
    saft_specific_methods = sorted(list(set(saft_methods) - set(thermo_methods)))
    method_dict = split_methods_by_section(sections, saft_specific_methods)

    ofile_text = get_autogen_header('saft')
    ofile_text += '# Methods in the saft class (`saft.py`)\n\n'
    ofile_text += 'The `saft` class, found in `addon/pycThermopack/saft.py`, is an "abstract" class, that is inherited\n' \
                  'by the `saftvrmie`, `pcsaft` and `saftvrqmie` classes. It contains some generic utility methods to\n' \
                  'compute quantities of interest when investigating SAFT-type equations of state.\n\n'
    ofile_text += get_toc(sections, section_headers, method_dict)
    ofile_text += get_markdown_contents(sections, section_headers, section_intro, method_dict)

    filename = 'saft_methods.md'
    write_file(MARKDOWN_DIR + filename, ofile_text)

def saftvrmie_to_markdown():
    """
    Generate markdown documentation file for the saft class.
    """

    sections = ['Constructor',
                'Utility',
                'Model control',
                'Model performance']

    section_headers = {'Constructor': 'Constructor',
                       'Utility': 'Utility methods',
                       'Model control': 'Model control',
                       'Model performance' : 'Model performance'}

    section_intro= {'Constructor' : 'Methods to initialise SAFT-VR Mie model.',
                    'Utility' : 'Set- and get methods for interaction parameters and pure fluid parameters.',
                    'Model control' : 'Control which contributions to the residual Helmholtz energy are included,\n'
                                      'and the hard-sphere reference term.',
                    'Model performance' : 'Methods to tune computation efficiency etc.'}

    saft_methods = inspect.getmembers(saft, predicate=inspect.isfunction)
    saftvrmie_methods = inspect.getmembers(saftvrmie, predicate=inspect.isfunction)
    saftvrmie_specific_methods = sorted(list(set(saftvrmie_methods) - set(saft_methods)))
    method_dict = split_methods_by_section(sections, saftvrmie_specific_methods)

    ofile_text = get_autogen_header('saftvrmie')
    ofile_text += '# Methods in the saftvrmie class (`saftvrmie.py`)\n\n'
    ofile_text += 'The `saftvrmie` class, found in `addon/pycThermopack/saftvrmie.py`, is the interface to the \n' \
                  'SAFT-VR Mie Equation of State. This class inherits the `saft` class, which in turn inherits the\n' \
                  '`thermo` class. This class implements methods for modifying fluid parameters and which terms\n' \
                  'are included in the model.\n\n'
    ofile_text += get_toc(sections, section_headers, method_dict)
    ofile_text += get_markdown_contents(sections, section_headers, section_intro, method_dict)

    filename = 'saftvrmie_methods.md'
    write_file(MARKDOWN_DIR + filename, ofile_text)

def saftvrqmie_to_markdown():
    """
    Generate markdown documentation file for the saftvrqmie class.
    """

    sections = ['Constructor',
                'Utility']

    section_headers = {'Constructor': 'Constructor',
                       'Utility': 'Utility methods'}

    section_intro= {'Constructor' : 'Methods to initialise SAFT-VRQ Mie model.',
                        'Utility' : 'Set- and get methods for interaction parameters and pure fluid parameters.'}

    saftvrmie_methods = inspect.getmembers(saftvrmie, predicate=inspect.isfunction)
    saftvrqmie_methods = inspect.getmembers(saftvrqmie, predicate=inspect.isfunction)
    saftvrqmie_specific_methods = sorted(list(set(saftvrqmie_methods) - set(saftvrmie_methods)))
    method_dict = split_methods_by_section(sections, saftvrqmie_specific_methods)

    ofile_text = get_autogen_header('saftvrqmie')
    ofile_text += '# Methods in the saftvrqmie class (`saftvrqmie.py`)\n\n'
    ofile_text += 'The `saftvrmie` class, found in `addon/pycThermopack/saftvrqmie.py`, is the interface to the \n' \
                  'SAFT-VRQ Mie Equation of State.\n*NOTE*: This class inherits the `saftvrmie` class, and thereby has\n' \
                  'access to the `model control` and `utility` methods found there. The `saftvrmie` class inherits\n' \
                  'the `saft` class, which in turn inherits the `thermo` class.\n' \
                  'This class implements utility methods specific to the SAFT-VRQ Mie EoS.\n\n'

    ofile_text += get_toc(sections, section_headers, method_dict)
    ofile_text += get_markdown_contents(sections, section_headers, section_intro, method_dict)

    filename = 'saftvrqmie_methods.md'
    write_file(MARKDOWN_DIR + filename, ofile_text)

def pcsaft_to_markdown():
    """
    Generate markdown documentation file for the saft class.
    """

    sections = ['Constructor',
                'Utility']

    section_headers = {'Constructor': 'Constructor',
                       'Utility': 'Utility methods'}

    section_intro= {'Constructor' : 'Methods to initialise PC-SAFT model.',
                        'Utility' : 'Set- and get methods for interaction parameters and pure fluid parameters.'}

    saft_methods = inspect.getmembers(saft, predicate=inspect.isfunction)
    pcsaft_methods = inspect.getmembers(pcsaft, predicate=inspect.isfunction)
    pcsaft_specific_methods = sorted(list(set(pcsaft_methods) - set(saft_methods)))
    method_dict = split_methods_by_section(sections, pcsaft_specific_methods)

    ofile_text = get_autogen_header('pcsaft')
    ofile_text += '# Methods in the pcsaft class (`pcsaft.py`)\n\n'
    ofile_text += 'The `pcsaft` class, found in `addon/pycThermopack/pcsaft.py`, is the interface to the \n' \
                  'PC-SAFT Equation of State. This class inherits the `saft` class, which in turn inherits the\n' \
                  '`thermo` class. This class implements utility methods to access mixing parameters etc.\n\n'
    ofile_text += get_toc(sections, section_headers, method_dict)
    ofile_text += get_markdown_contents(sections, section_headers, section_intro, method_dict)

    filename = 'pcsaft_methods.md'
    write_file(MARKDOWN_DIR + filename, ofile_text)

if __name__ == '__main__':
    thermo_to_markdown()
    saft_to_markdown()
    saftvrmie_to_markdown()
    saftvrqmie_to_markdown()
    pcsaft_to_markdown()