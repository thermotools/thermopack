import markdown
import re

# Read the sidebar.md content
with open('sidebar.md', 'r') as f:
    markdown_content = f.read()

# Convert Markdown to HTML
html_content = markdown.markdown(markdown_content)

with open('sidebar.html', 'w') as f:
    f.write(html_content)

# Split the HTML content into items
items = re.split(r'<li>|</li>', html_content)
items = [item.strip() for item in items if item.strip()]

print(items)

# Initialize the sidebar HTML
sidebar_html = '<div class="sidebar">\n    <ul>\n'
current_indent = 0

# Process each item
for item in items:
    line = item
    if r'<ul>' in line:
        current_indent += 1
    if r'<\ul>' in line:
        current_indent -= 1

    if current_indent == 1:
        line = r"<li><span class='expand'>▶</span>" + item + '\n'
        continue
    indent = item.count('▶')
    if current_indent < indent:
        sidebar_html += '        ' * (current_indent) + '<ul class="sub-items">\n'
    elif current_indent > indent:
        sidebar_html += '        ' * (current_indent - 1) + '</ul>\n'
    sidebar_html += '        ' * indent + f'<li>{item}</li>\n'
    current_indent = indent

exit(0)
# Close any open ul tags
while current_indent > 0:
    sidebar_html += '        ' * (current_indent - 1) + '</ul>\n'
    current_indent -= 1

sidebar_html += '    </ul>\n</div>'

# Print or save the generated sidebar.html
with open('sidebar.html', 'w') as f:
    f.write(sidebar_html)

print("Sidebar HTML generated and saved as 'sidebar.html'")
