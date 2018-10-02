"""Test paring of namelists to dictionary."""

import pytest

from context import namelist


@pytest.mark.parametrize(
    "string",
    ["&nml &end",
     "&nml\n&end",
     "&nml &",
     "&nml /", "&nml/",
     ]
)
def test_parse_string(string):
    nml = namelist.parse_namelist_string(string)[0]
    assert nml.name == "nml"


@pytest.mark.parametrize(
    "string",
    ["&nml2 val=34 &end",
     "&nml2\n val=34 \n&end",
     ]
)
def test_match_name_val(string):
    nml = namelist.parse_namelist_string(string)[0]
    assert nml.name == "nml2"
    assert nml["val"] == 34


@pytest.mark.parametrize(
    "string",
    ["&nml3 val1=34, val2=35 &end",
     "&nml3 val1=34 val2=35 &end",
     "&nml3\nval1=34\nval2=35\n&end",
     ]
)
def test_match_multiple_name_val(string):
    nml = namelist.parse_namelist_string(string)[0]
    assert nml.name == "nml3"
    assert nml["val1"] == 34
    assert nml["val2"] == 35
