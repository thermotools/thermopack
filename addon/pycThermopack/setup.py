from distutils.core import setup
from pathlib import Path
import sys

root_dir = Path(__file__).parent # thermopack root directory
readme = (root_dir / 'README_pypi.md').read_text()
setup(name='thermopack',
      version='2.1.0',
      description='Python interface to thermopack',
      long_description=readme,
      long_description_content_type='text/markdown',
      author='Morten Hammer',
      author_email='morten.hammer@sintef.no',
      url='https://github.com/thermotools/thermopack',
      packages=['thermopack'],
      package_data={'thermopack':['*thermopack.*']}
)

