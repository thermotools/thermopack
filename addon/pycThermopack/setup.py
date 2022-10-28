from distutils.core import setup
import sys

sys.argv.extend(['--plat-name', 'linux_x86_64'])
#sys.argv.extend(['--plat-name', 'win_amd64'])

setup(name='pyctp',
      version='2.0.0',
      description='pycThermopack',
      author='Morten Hammer',
      author_email='hammer.morten@sintef.no',
      url='https://github.com/thermotools/thermopack',
      packages=['pyctp'],
      package_data={'pyctp':['*thermopack.*']}
)

