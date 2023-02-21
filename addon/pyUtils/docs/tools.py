import os

THERMOPACK_ROOT = os.path.dirname(__file__) + '/../../..'
MARKDOWN_DIR = THERMOPACK_ROOT + '/doc/markdown/'

def remove_illegal_link_chars(link_str):
    return link_str.replace(' ', '-').replace('(', '').replace(')', '').replace('=', '').replace(',', '').replace("'", '')
