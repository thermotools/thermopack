import os

THERMOPACK_ROOT = os.path.dirname(__file__) + '/../../..'
MARKDOWN_DIR = THERMOPACK_ROOT + '/doc/markdown/'

def gen_file_str(files):
    out_file_str = ''
    for file in files:
        file_path = MARKDOWN_DIR + file + '.md'

        with open(file_path, 'r') as in_file:
            out_file_str += in_file.read() + '\n'

    return out_file_str

def write_pypi_readme():
    files = ['header', 'pypi_toc', 'cite_acknowl_licence', 'structure', 'getting_started']

    out_file_str = gen_file_str(files)
    with open('README_pypi.md', 'w') as out_file:
        out_file.write(out_file_str)

def write_github_readme():
    files = ['header', 'github_toc', 'cite_acknowl_licence', 'source_build', 'getting_started']

    out_file_str = gen_file_str(files)
    with open('README_github.md', 'w') as out_file:
        out_file.write(out_file_str)

if __name__ == '__main__':
    write_pypi_readme()
    write_github_readme()

