import os

THERMOPACK_ROOT = os.path.dirname(__file__) + '/../../..'
MARKDOWN_DIR = THERMOPACK_ROOT + '/docs/vCurrent/'

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
        if ('Generated at' in old_line) and ('Generated at' in new_line):
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
            print('** Wrote', filename, 'to', ofile_path)
    else:
        print('* File at', ofile_path, 'is unchanged.')