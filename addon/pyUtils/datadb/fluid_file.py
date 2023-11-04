import json


class fluid_file(object):
    """Read fluid file, manipulate and save
    """

    def __init__(self, filepath):
        """Load file from data-base
        Arguments:
        filename - path to file
        """
        self.filepath = filepath
        with open(filepath) as f:
            self.fluid = json.load(f)

    def save(self):
        """Save to file
        """
        with open(self.filepath, "w") as f:
            json.dump(self.fluid,f,indent=2)

    def fluid_file_get_lines(self):
        """Read fluid lines
        """
        f=open(self.filepath,"r")
        lines = f.readlines()
        f.close()
        return lines

    def fluid_file_write_lines(self, lines):
        """Write lines to fluid file
        """
        with open(self.filepath, "w") as f:
            for line in lines:
                f.write(line)

    def fluid_file_swap_words(self, old_word, new_word):
        """Swap words in fluid file
        """
        lines = self.fluid_file_get_lines()
        i = -1
        for il, line in enumerate(lines):
            if line.find(old_word) != -1:
                i = il
                lines[i] = lines[i].replace(old_word,new_word)

        self.fluid_file_write_lines(lines)

    def fluid_file_remove_line_containing_word(self, word):
        """Remove line from fluid file containig specific word
        """
        lines = self.fluid_file_get_lines()
        i = -1
        for il, line in enumerate(lines):
            if line.find(word) != -1:
                i = il
                break
        if lines[i].strip()[-1] != ",": # Last line in entry
            lines[i-1] = lines[i-1].strip()[:-1] # Strip "," from previous line
        lines.pop(i)
        self.fluid_file_write_lines(lines)

    def fluid_file_remove_key(self, key):
        """Remove key from fluid file
        """
        lines = self.fluid_file_get_lines()
        i = -1
        for il, line in enumerate(lines):
            if line.find(key) != -1:
                i = il
                break
        i_end = i
        if lines[i].find("{") != -1: # Look for closing bracket
            n_bracket = 1
            n_end_bracket = 0
            n_lines = len(lines)
            for k in range(i+1,len(lines)):
                if lines[k].find("{") != -1:
                    n_bracket += 1
                if lines[k].find("}") != -1:
                    n_end_bracket += 1
                if n_bracket == n_end_bracket:
                    i_end = k
                    break
        for k in range(i,i_end+1):
            lines.pop(i)
        self.fluid_file_write_lines(lines)

    def fluid_file_line_substitute(self, line_sub_string, old_word, new_word):
        """Replace word in fluid file
        """
        lines = self.fluid_file_get_lines()
        i = -1
        for il, line in enumerate(lines):
            if line.find(line_sub_string) != -1:
                i = il
                lines[i] = lines[i].replace(old_word,new_word)
        self.fluid_file_write_lines(lines)

    def fluid_file_line_with_two_substrings(self, sub_string1, sub_string2):
        """Locate and print line containing two sub-strings
        """
        lines = self.fluid_file_get_lines()
        i = -1
        for il, line in enumerate(lines):
            if line.find(sub_string1) != -1:
                if line.find(sub_string2) != -1:
                    print(self.filepath, line)
        self.fluid_file_write_lines(lines)

    def fluid_file_add_new_subkey(self, after_key, key, value_str):
        """Add single key to fluid file
        """
        lines = self.fluid_file_get_lines()
        i = -1
        for il, line in enumerate(lines):
            if line.find(after_key) != -1:
                i = il
                break
        n_spaces = lines[i].split("\"").count(' ')
        line_end = ",\n"
        if lines[i+1].lstrip()[0] == "}":
            # Add comma to previous line
            lines[i] = lines[i].replace("\n",",\n")
            line_end = "\n"

        line = " "*n_spaces + "\"" + key + "\": " + value_str + line_end
        lines.insert(i+1,line)
        self.fluid_file_write_lines(lines)
