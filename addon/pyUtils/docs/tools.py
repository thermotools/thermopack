import os

THERMOPACK_ROOT = os.path.dirname(__file__) + '/../../..'
MARKDOWN_DIR = THERMOPACK_ROOT + '/docs/vCurrent/'

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def remove_illegal_link_chars(link_str):
    """
    Remove characters that are illegal in url-links from the string 'link_str', and return the new string.
    """
    return (link_str.replace(' ', '-').replace('(', '').replace(')', '').replace('=', '').replace(',', '').replace(
             "'", '').replace('.','')).lower()

def check_is_changed(old_file, new_file_str):
    """
    Check if the contents of old_file is equal to new_file_str, excluding the timestamp

    Args:
        old_file: (str) Path to the file to be written
        new_file_str: (str) Contents to be written to the file

    Returns:
        (bool) True if the contents have changed or the file does not exist, False otherwise
    """
    if not os.path.isfile(old_file):
        return True

    new_lines = new_file_str.split('\n')
    with open(old_file, 'r') as ofile:
        old_lines = ofile.read().split('\n')
    if len(new_lines) != len(old_lines):
        return True

    for old_line, new_line in zip(old_lines, new_lines):
        if (('Generated at' in old_line) and ('Generated at' in new_line)) \
            or (('Time stamp' in old_line) and ('Time stamp' in new_line)):
            continue
        if old_line != new_line:
            return True

    return False

def write_file(ofile_path, ofile_text):
    """
    Write to a file, but first check if the file exists, and if it exists, check whether the contents to be
    written to the file are equal to the contents of the existing file (exluding a timestamp). If the content
    to be written is equal to the existing file content, report this and leave the file alone.

    Args:
        ofile_path (str) : Path to write to
        ofile_text (str) : Contents to write

    Returns:
        None : Feedback is sent to stdout.
    """
    filename = ofile_path.split('/')[-1]
    if check_is_changed(ofile_path, ofile_text):
        with open(ofile_path, 'w') as ofile:
            ofile.write(ofile_text)
            print(f'{bcolors.OKGREEN}** Wrote {filename} to {ofile_path}{bcolors.ENDC}')
    else:
        print(f'{bcolors.OKCYAN}* File at {ofile_path} is unchanged.{bcolors.ENDC}')

def update_docfile_versions(vnew, doc_dir):
    """
    Intended for use when you copy the doc directory vCurrent to a new directory with a specific version number
    This function iterates over all the markdown files in the directory, and changes the version number and the
    permalink in the header from (blank) and /vcurrent/... to the specified version number.

    Args:
        vnew (str) : New version number (e.g. '2.2.0')
        doc_dir (str) : Path to the directory to modify (e.g. {THERMOPACK_ROOT}/docs/v{vnew}/)
    """
    files = os.listdir(doc_dir)
    for file in files:
        if file[-3:] != '.md':
            continue
        with open(f'{doc_dir}/{file}', 'r') as fh:
            lines = fh.readlines()
            if '---' not in lines[0]:
                continue
            for i in range(1, len(lines)):
                if '---' in lines[i]:
                    break
                if 'version:' in lines[i]:
                    lines[i] = f'version: {vnew}\n'
                if '/vcurrent/' in lines[i]:
                    lines[i] = lines[i].replace('/vcurrent/', f'/v{vnew}/')

        with open(f'{doc_dir}/{file}', 'w') as fh:
            for line in lines:
                fh.write(line)

def update_v220_method_docs():
    files = os.listdir(f'{THERMOPACK_ROOT}/docs/vCurrent/')
    for filename in files:
        if '_methods.md' not in filename:
            continue
        with open(f'{THERMOPACK_ROOT}/docs/vCurrent/{filename}', 'r') as ifile:
            lines = ifile.readlines()

        for i in range(1, len(lines)):
            if '---' in lines[i]:
                break
            if 'version:' in lines[i]:
                lines[i] = f'version: 2.2.0\n'
            if '/vcurrent/' in lines[i]:
                lines[i] = lines[i].replace('/vcurrent/', f'/v2.2.0/')

        with open(f'{THERMOPACK_ROOT}/docs/v2.2.0/{filename}', 'w') as ofile:
            for line in lines:
                ofile.write(line)

    print('Consolidated Method docs for v2.2.0 with current version.')

if __name__ == '__main__':
    update_v220_method_docs()
    # update_docfile_versions('2.2.0', f'{THERMOPACK_ROOT}/docs/v2.2.0/')