# Import sys
import sys

def gcc_major_version_greater_than(version):
    "Returns gcc major version, or False if no GCC."
    is_gt = False
    sys_arr_gcc = sys.version.split("GCC")
    if len(sys_arr_gcc) > 1:
        gcc_mv = int(sys_arr_gcc[1].split(".")[0])
        is_gt = gcc_mv > version
    return is_gt
