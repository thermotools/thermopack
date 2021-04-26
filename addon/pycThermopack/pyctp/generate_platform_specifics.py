#!/usr/bin/env python
# Module for generating file with platform specifics
from map_platform_specifics import get_platform_specifics_by_trial_and_error


def write_platform_specifics_file(pf_specifics, filename):
    """ Write file for platform specifics"""
    lines = []
    lines.append("# Module for platform specific stuff. Automatically generated.")
    lines.append("\n")
    lines.append("def get_platform_specifics():")
    lines.append("    pf_specifics = {}")
    lines.append(f'    pf_specifics["prefix"] = "{pf_specifics["prefix"]}"')
    lines.append(f'    pf_specifics["module"] = "{pf_specifics["module"]}"')
    lines.append(f'    pf_specifics["postfix"] = "{pf_specifics["postfix"]}"')
    lines.append(f'    pf_specifics["postfix_no_module"] = "{pf_specifics["postfix_no_module"]}"')
    lines.append(f'    pf_specifics["dyn_lib"] = "{pf_specifics["dyn_lib"]}"')
    lines.append(f'    pf_specifics["os_id"] = "{pf_specifics["os_id"]}"')
    lines.append("    return pf_specifics")

    with open(filename, "w") as f:
        for line in lines:
            f.write(line)
            f.write("\n")


if __name__ == "__main__":
    platform_specifics = get_platform_specifics_by_trial_and_error()
    write_platform_specifics_file(platform_specifics, "platform_specifics.py")
