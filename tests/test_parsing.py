"""Test paring of namelists to dictionary."""

import pytest

from context import namelist


@pytest.mark.parametrize(
    "string,res",
    [("&nml3 &end", ["&nml3", "&end"]),
     ("&n val1=34,\nval2=35/",
      ['&n', 'val1', '=', '34', 'val2', '=', '35/']),
     ("&n val1=34,\nval2=35 /",
      ['&n', 'val1', '=', '34', 'val2', '=', '35', '/']),
     ("&n val1=34,!this is a comment\nval2=35 /",
      ['&n', 'val1', '=', '34', 'val2', '=', '35', '/']),
     ]
)
def test_tokenize(string, res):
    nml = namelist.namelist._tokenize(string)
    assert nml == res


@pytest.mark.parametrize(
    "string",
    ["&nml &end",
     "&nml\n&end",
     "&nml\n&end\n",
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


@pytest.mark.parametrize(
    "string,val",
    [("&nml val=.T./", True),
     ("&nml val=.t./", True),
     ("&nml val=.TRUE./", True),
     ("&nml val=.true./", True),
     ("&nml val=.F./", False),
     ("&nml val=.f./", False),
     ("&nml val=.FALSE./", False),
     ("&nml val=.false./", False),
     ]
)
def test_var_bool(string, val):
    nml = namelist.parse_namelist_string(string)[0]
    assert nml["val"] is val


@pytest.mark.parametrize(
    "string,val",
    [("&nml val='this is a string'/", "this is a string"),
     ("&nml val='this is \na string'/", "this is \na string"),
     ("&nml val=\"this is \na string\"/", "this is \na string"),
     ("&nml val=\"this 'is' \na string\"/", "this 'is' \na string"),
     ("&nml val='this \"is\" \na string'/", "this \"is\" \na string"),
     ]
)
def test_var_string(string, val):
    nml = namelist.parse_namelist_string(string)[0]
    assert nml["val"] == val
