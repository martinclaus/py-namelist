"""Test paring of namelists to dictionary."""

import pytest

from context import namelist


@pytest.mark.parametrize(
    "string",
    ["&nml &end",
     "&nml\n&end",
     "&nml /", "&nml/",
     ]
)
def test_parse_string(string):
    nml = namelist.parse_namelist_string(string)[0]
    assert nml.name == "nml"
