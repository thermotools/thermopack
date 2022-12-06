from distutils.core import setup
import sys


setup(name='thermopack',
      version='2.1.0',
      description='Python interface to thermopack',
      author='Morten Hammer',
      author_email='morten.hammer@sintef.no',
      url='https://github.com/thermotools/thermopack',
      packages=['thermopack'],
      package_data={'thermopack':['*thermopack.*']}
)

