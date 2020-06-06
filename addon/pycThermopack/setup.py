from distutils.core import setup
import shutil
from sys import exit

thermopack = "../../bin/dynamic/libthermopack_optim_gfortran_Linux.so"
if os.path.exists(thermopack):
    shutil.copy2(thermopack, "./pyctp/libthermopack.so")
else:
    print(thermopack + " does not exist. Have you compiled thermopack?")
    exit(1)

setup(name='pyctp',
      version='1.0.0',
      description='pycThermopack',
      author='Morten Hammer',
      author_email='hammer.morten@sintef.no',
      url='https://github.com/SINTEF/thermopack',
      packages=['pyctp'],
      package_data={'pyctp':['libthermopack.*']}
)

