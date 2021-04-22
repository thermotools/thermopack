#!/usr/bin/env python
# Module for generating file with platform specifics
from map_platform_specifics import get_platform_specifics_by_trial_and_error


def write_platform_specifics_file(prefix, module, postfix, postfix_no_module, dyn_lib, os_id, filename):
    """ Write file for platform specifics"""
    lines = []
    lines.append("# Module for platform specific stuff. Automatically generated.")
    lines.append("PREFIX = \"" + prefix + "\"")
    lines.append("MODULE = \"" + module + "\"")
    lines.append("POSTFIX = \"" + postfix + "\"")
    lines.append("POSTFIX_NO_MODULE = \"" + postfix_no_module + "\"")
    lines.append("DYN_LIB = \"" + dyn_lib + "\"")
    lines.append("OS_ID = \"" + os_id + "\"")
    lines.append("\n")
    lines.append("def get_platform_specifics():")
    lines.append("    return PREFIX, MODULE, POSTFIX, POSTFIX_NO_MODULE, DYN_LIB, OS_ID")

    with open(filename, "w") as f:
        for line in lines:
            f.write(line)
            f.write("\n")


res = get_platform_specifics_by_trial_and_error()
write_platform_specifics_file(*res, "platform_specifics.py")
