"""namelist: Parsing Fortran namelists to Python dictionaries and back."""

__name__ = "namelist"
__version__ = "0.0.1"

from .namelist import (Namelist,
                       parse_namelist_file,
                       parse_namelist_string,
                       )
