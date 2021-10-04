from subprocess import check_output
from sys import exit, version
import re

def gcc_major_version_greater_than(GCC_version):
    """Returns if GCC major version number is greater than specefied version

    Args:
        GCC_version (int): Major GCC version

    Returns:
        bool: GCC version is greater than specified version
    """
    is_gt = False
    sys_arr_gcc = version.split("GCC")
    if len(sys_arr_gcc) > 1:
        # GCC on system
        out = check_output(["gcc", "--version"])
        out_str = out.decode("utf8").split("\n")[0]
        match = re.search('[0-9]+\.[0-9]\.[0-9]', out_str)
        first_match_str = match.group(0)
        gcc_mv_str = first_match_str.split(".")[0]
        try:
            gcc_mv = int(gcc_mv_str)
        except ValueError:
            print("Not able to determine GCC major version. Exiting.")
            exit(1)

        is_gt = gcc_mv > GCC_version

    return is_gt
