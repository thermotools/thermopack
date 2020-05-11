from distutils.core import setup

setup(name='pytp',
            version='1.0.0',
            description='pyThermopack',
            author='Eskil Aursand',
            author_email='eskil.aursand@sintef.no',
            url='',
            packages=['pytp'],
            package_data={'pytp':['tp*.so']}
           )

