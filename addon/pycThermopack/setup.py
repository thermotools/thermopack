from distutils.core import setup

setup(name='pyctp',
      version='1.0.0',
      description='pycThermopack',
      author='Morten Hammer',
      author_email='hammer.morten@sintef.no',
      url='https://github.com/SINTEF/thermopack',
      packages=['pyctp'],
      package_data={'pyctp':['libthermopack.*']}
)

