import os
import re

link_pattern = r"\[([^]]+)\]\(([^)]+)\)"
code_pattern = r'`([^`]+)`'
code_replace = r'<code class="language-plaintext highlighter-rouge">\1</code>'
sidebar_file_pattern = r"^sidebar.*\.md$"

filename = 'sidebar'
ifilename = f'{filename}.md'
ofilename = f'{filename}.html'

def parse_line_content(line_content):
    content = re.findall(link_pattern, line_content)
    if len(content) == 0:
        head, link = line_content, ''
    else:
        head, link = content[0]

    head = re.sub(code_pattern, code_replace, head)
    return head, link

def get_table(line, indent, file, level):
    if line is None:
        line = file.readline()
    table = {}
    while line:
        current_indent, content = line.split('* ')
        key = parse_line_content(content)
        if len(current_indent) == len(indent):
            table[key] = {}
            current_key = key
            line = file.readline()
            continue
        elif len(current_indent) > len(indent):
            line, table[current_key] = get_table(line, current_indent, ifile, level + 1)
            continue
        else:
            return line, table

    return table

def get_list_element(key):
    if len(key[1]) > 0:
        return f'<a href="{key[1]}">{key[0]}</a>'
    return key[0]

def write_table(file, tab, level):
    file.write('\t' * level + '<ul>\n')
    for k, v in tab.items():
        if len(v) > 0:
            file.write('\t' * level + '<li>\n')
            file.write('\t' * (level + 1) + f'{get_list_element(k)} <button class="expand-btn">&#9668;</button>\n')
            file.write('\t' * (level + 1) + '<ul class="submenu">\n')
            write_table(file, v, level + 2)
            file.write('\t' * (level + 1) + '</ul>\n')
            file.write('\t' * level + '</li>\n')
        else:
            file.write('\t' * level + f'<li>{get_list_element(k)}</li>\n')
    file.write('\t' * level + '</ul>\n')

if __name__ == '__main__':
    for file in os.listdir(os.path.dirname(__file__)):
        if not re.match(sidebar_file_pattern, file):
            continue

        filename = file.strip('.md')
        ifilename = f'{filename}.md'
        ofilename = f'{filename}.html'

        with open(ifilename, 'r') as ifile:
            table = get_table(None, '', ifile, 0)

        with open(ofilename, 'w') as ofile:
            write_table(ofile, table, 0)
            ofile.write("""<script>
            // Get all expand buttons
            var expandButtons = document.querySelectorAll('.expand-btn');
        
            // Add click event listener to each expand button
            expandButtons.forEach(function(button) {
                button.addEventListener('click', function() {
                    // Toggle the submenu visibility by toggling the 'active' class
                    var submenu = this.nextElementSibling;
                    submenu.classList.toggle('active');
                    button.classList.toggle('active');
                });
            });
        </script>""")



