from distutils.core import setup
import sys


setup(name='pyctp',
      version='2.0.0',
      description='Python interface to thermopack',
      author='Morten Hammer',
      author_email='morten.hammer@sintef.no',
      url='https://github.com/thermotools/thermopack',
      packages=['pyctp'],
      package_data={'pyctp':['*thermopack.*']}
)

