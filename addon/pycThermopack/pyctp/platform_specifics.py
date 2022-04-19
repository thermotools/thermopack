# Module for platform specific stuff.
from . import map_platform_specifics


def get_platform_specifics():
    return map_platform_specifics.get_platform_specifics_from_platform()
