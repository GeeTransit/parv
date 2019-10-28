try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

setup(
    name="parv",
    description="Parv - PEG Parsing and Visiting",
    long_description="Parv is a library with parsers and visitors for PEG grammars.",
    license="MIT",
    version="0.1",
    author="George Zhang",
    author_email="geetransit@gmail.com",
    url="https://github.com/GeeTransit/parv",
    packages=["parv"],
    classifiers=[
        "Programming Language :: Python :: 3",
    ],
)
