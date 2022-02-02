"""Utility functions."""

import re
from subprocess import check_output
import sys


def gcc_major_version_greater_than(GCC_version):
    """Returns if GCC major version number is greater than specefied version

    Args:
        GCC_version (int): Major GCC version

    Returns:
        bool: GCC version is greater than specified version
    """
    is_gt = True
    sys_arr_gcc = re.split('Clang|clang|GCC|gcc', sys.version)
    if len(sys_arr_gcc) > 1:
        out = check_output(["gcc", "-dumpfullversion"])
        out_str = out.decode("utf8").split("\n", maxsplit=1)[0]
        match = re.search(r'([0-9]+)\.[0-9]\.[0-9]', out_str)
        gcc_mv_str = match.group(1)
        try:
            gcc_mv = int(gcc_mv_str)
        except ValueError:
            print("Not able to determine GCC major version. Exiting.")
            sys.exit(1)

        is_gt = gcc_mv > GCC_version

    return is_gt
