import json
import os


def get_json_data(file_name):
    with open(file_name) as file:
        return json.load(file)


def save_json_data(new_data, file_name):
    if os.path.exists(file_name):
        data = get_json_data(file_name)
        data.update(new_data)
    else:
        data = new_data

    with open(file_name, "w") as file:
        json.dump(data, file, sort_keys=True, indent=4, separators=(',', ': '))


def get_unique_name(prefix, lst):
    i = 1
    while True:
        name = prefix + " " + str(i)

        if i >= 100:
            break

        elif name not in lst:
            return name

        else:
            i += 1
