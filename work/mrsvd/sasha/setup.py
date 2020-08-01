from distutils.core import setup
import sys, shutil

if sys.argv[1] == 'clean':
    shutil.rmtree("./build")
    shutil.rmtree("/usr/local/lib/python2.7/dist-packages/sasha/")

setup(
    name='Sasha',
    version='0.0.1',
    author='Burak Bayramli',
    author_email='mail@post.ce',
    packages=['sasha'],
    scripts=[],
    url='https://github.com/burakbayramli/sasha',
    license='GPL3',
    description='Sasha',
    long_description="Sasha",
)
