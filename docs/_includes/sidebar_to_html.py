import re

link_pattern = r'\[([^\]]+)\]\(#([^\)]+)\)'

filename = 'sidebar'
ifilename = f'{filename}.md'
ofilename = f'{filename}.html'

def get_table(line, indent, file, level):
    if line is None:
        line = file.readline()
    table = {}
    while line:
        current_indent, content = line.split('* ')
        head, link = content.split('](')
        head = head[1:]
        link = link[:-2]
        head = (head, link)
        if len(current_indent) == len(indent):
            table[head] = {}
            current_head = head
            line = file.readline()
            continue
        elif len(current_indent) > len(indent):
            line, table[current_head] = get_table(line, current_indent, ifile, level + 1)
            continue
        else:
            return line, table

    return table

with open(ifilename, 'r') as ifile:
    table = get_table(None, '', ifile, 0)

def write_table(file, tab, level):
    file.write('\t' * level + '<ul>\n')
    for k, v in tab.items():
        if len(v) > 0:
            file.write('\t' * level + '<li>\n')
            file.write('\t' * (level + 1) + f'<a href="{k[1]}">{k[0]}</a><button class="expand-btn"></button>\n')
            file.write('\t' * (level + 1) + '<ul class="submenu">\n')
            write_table(file, v, level + 2)
            file.write('\t' * (level + 1) + '</ul>\n')
            file.write('\t' * level + '</li>\n')
        else:
            file.write('\t' * level + f'<li><a href="{k[1]}">{k[0]}</a></li>\n')
    file.write('\t' * level + '</ul>\n')

with open(ofilename, 'w') as ofile:
    write_table(ofile, table, 0)



