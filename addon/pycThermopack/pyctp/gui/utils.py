from gui.classes.component import Component

import json
import os


def get_json_data(file_name):
    """
    Opens a JSON-file and returns a Python dictionary containing the file data
    :param file_name: Name of file to be opened
    :return: Python-dict
    """
    with open(file_name) as file:
        return json.load(file)


def save_json_data(new_data, file_name):
    """
    Opens a JSON-file, and saves the data to it. If there already is some data in the file, it is updated
    :param new_data: Data to be saved
    :param file_name: Name of file
    """
    if os.path.exists(file_name):
        data = get_json_data(file_name)
        data.update(new_data)
    else:
        data = new_data

    with open(file_name, "w") as file:
        json.dump(data, file, sort_keys=True, indent=4, separators=(',', ': '))


def get_unique_name(prefix, lst):
    """
    Appending numbers to a prefix until a unique name is obtained, which is not contained in the passed list.
    If the number gets over 100, the function breaks and does not return anything
    :param prefix: Prefix for the uniqe name
    :param lst: List to which the name is compared
    :return: Uniqe name
    """
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
    """
    Returns a number (id) which is not an id for existing model setups in the current session data
    :param data: Session data
    :return: Unique id
    """
    all_ids = []
    for settings in data["Model setups"].values():
        all_ids.append(settings["id"])

    id = 1
    while id in all_ids:
        id += 1

    return id


def get_fluids():
    """
    Opens the fluids folder, and returns a dict containing the data for the components
    :return: fluids dict
    """
    fluids = {}

    for root, dirs, files in os.walk("../../../../fluids"):
        for file_name in files:
            file = open(os.path.join(root, file_name), "r")

            component_data = json.load(file)
            name = component_data["name"]

            component = Component(name, component_data)
            fluids[name] = component

    return fluids


def get_comp_id(comp_list_data, comp_name):
    """
    Returns the identity (which is used by thermopack) corresponding to a given component name
    :param comp_list_data: Session component list data (dict)
    :param comp_name: Name of component of which the identity is desired
    :return: Component identity
    """
    index = comp_list_data["Names"].index(comp_name)
    return comp_list_data["Identities"][index]


def valid_float_input(input):
    try:
        float(input)
    except ValueError:
        return False
    return True
