"""
Scripts to generate readme's and wiki-documentation files by joining various markdown files in thermopack/doc/markdown

Intent: Several "submodules" of documentation are used both in the wiki, the main github README, the pypi README and
        possibly other places. Each of these "submodules" should be contained in a single markdown file in the
        thermopack/doc/markdown directory. Each function in this file generates a single markdown file by joining
        the appropriate submodules, and prepending the header generated by the get_header(files) function.

Usage: To add new documentation, create a new markdown file in thermopoack/doc/markdown, and add the filename
        (sans the file ending) to the appropriate `files` lists in the functions in this file.
"""
import warnings
from datetime import datetime
from tools import THERMOPACK_ROOT, MARKDOWN_DIR, write_file
import re

def print_finished_report(header, out_file_path):
    printcolwidth = 100
    print('-' * printcolwidth)
    print('Wrote documentation file with the header :')
    print('#' * printcolwidth)
    print(header)
    print('#' * printcolwidth)
    print('-' * printcolwidth)
    print('To:', out_file_path)
    print('-' * printcolwidth)
    print()

def format_no_html(file):
    """
    Markdown files that are intended to be compiled to html may contain some syntax that is unfriendly to e.g.
    pages intended to render GitHub flavoured markdown. Specifically, the title is in the page metadata, not as a
    header. This function reads the file, and returns a string formatted to be friendly for "pure" markdown pages
    without html-templates.

    Args:
        file (file handle) : The file to read
    Returns
        str : The contents of the file, formatted to be nice.
    """
    line = file.readline()
    if '---' not in line:
        return line + file.read()
    metadata = line + '\n'
    line = file.readline()
    title = ''
    description = ''

    while '---' not in line:
        if 'title' in line:
            title = line.split(':')[-1].strip()
        elif 'description' in line:
            description = line.split(':')[-1].strip()

        metadata += line
        line = file.readline()

    line = file.readline() # Move past the '---' at the end of the header section.

    if title:
        main_header = title
    elif description:
        main_header = description
    else:
        main_header = ''
        warnings.warn(f'File with metadata : {metadata} \nDid not contain a title or description.', SyntaxWarning, stacklevel=2)

    outstr = f'# {main_header}\n'
    return outstr + line + repair_links(file.read())


def repair_links(filestr):
    """
    Because the markdown files used to generate the GH pages use relative paths, we need to prepend the appropriate
    url when making the Readme.
    """
    pattern_vcurrent = r"\]\((?!http)(.*?\.html)\)"
    replacement_vcurrent = r"](https://thermotools.github.io/thermopack/vcurrent/\1)"
    pattern_abspath = r"\]\(/thermopack(?!http)(.*?\.html)\)"
    replacement_abspath = r"](https://thermotools.github.io/thermopack\1)"
    filestr = re.sub(pattern_abspath, replacement_abspath, filestr)
    return re.sub(pattern_vcurrent, replacement_vcurrent, filestr)

def gen_file_str(files):
    out_file_str = ''
    for file in files:
        file_path = MARKDOWN_DIR + '../' + file + '.md'

        with open(file_path, 'r') as in_file:
            out_file_str += format_no_html(in_file) + '\n\n'

    return out_file_str

def get_header(files):
    header = '<!--- \n'
    header += 'Generated at: ' + datetime.today().isoformat() + '\n'
    header += 'This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/join_docs.py\n'
    header += 'The file is created by joining the contents of the files\n'
    header += f'    {MARKDOWN_DIR}\n'
    for fname in files:
        header += '    ' * 2 + fname + '.md\n'
    header += '--->\n\n'
    return header

def write_pypi_readme():
    files = ['readme_parts/header', 'readme_parts/pypi_toc', 'metapages/please_cite', 'readme_parts/pypi_structure',
             'v2.2.0/getting_started', 'v2.2.0/more_advanced', 'v2.2.0/Component-name-mapping']
    header = get_header(files)
    contents = gen_file_str(files)
    out_file_str = header + contents
    out_file_path = THERMOPACK_ROOT + '/addon/pycThermopack/README_pypi.md'
    write_file(out_file_path, out_file_str)

def write_github_readme():
    files = ['readme_parts/header', 'readme_parts/github_toc', 'metapages/please_cite', 'readme_parts/structure',
             'vCurrent/source_build', 'vCurrent/getting_started', 'vCurrent/more_advanced', 'vCurrent/new_fluids',
             'vCurrent/Component-name-mapping']
    header = get_header(files)
    content = gen_file_str(files)
    out_file_str = header + content
    out_file_path = THERMOPACK_ROOT + '/README.md'
    write_file(out_file_path, out_file_str)

if __name__ == '__main__':
    write_pypi_readme()
    write_github_readme()

