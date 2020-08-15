from gui.classes.component import Component

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


def get_unique_id(data):
    all_ids = []
    for settings in data["Model setups"].values():
        all_ids.append(settings["id"])

    id = 1
    while id in all_ids:
        id += 1

    return id


def get_fluids():

    fluids = {}

    for root, dirs, files in os.walk("../../../../fluids"):
        for file_name in files:
            file = open(os.path.join(root, file_name), "r")

            component_data = json.load(file)
            name = component_data["name"]

            component = Component(name, component_data)
            fluids[name] = component

    return fluids
