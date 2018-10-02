import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="namelist",
    version="0.0.1",
    author="Martin Claus",
    author_email="mclaus@geomar.de",
    description="Parsing Fortran namelists to Python dictionaries and back.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://git.geomar.de/martin-claus/py-namelist",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v2 (GPLv2)",
        "Operating System :: OS Independent",
        "Development Status :: 4 - Beta",
    ],
)
