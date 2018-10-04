"""Test paring of namelists to dictionary."""

import pytest

from context import namelist, io


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
    ["&nml &end",
     "&nml\n&end",
     "&nml\n&end\n",
     "&nml &",
     "&nml /", "&nml/",
     ]
)
def test_parse_file(string):
    f = io.StringIO(string)
    nml = namelist.parse_namelist_file(f)[0]
    assert nml.name == "nml"


@pytest.mark.parametrize(
    "string",
    ["&nml\n/\n",
     "&nml\nval1 = 3\n/\n",
     "&nml\nval1 = .TRUE.\n/\n",
     "&nml\nval1 = .FALSE.\n/\n",
     "&nml\nval1 = (/ 1,2,3,4,5,6 /)\n/\n",
     "&nml\nval1 = (/ .TRUE.,.FALSE.,.TRUE.,.TRUE. /)\n/\n",
     ]
)
def test_string_out(string):
    nml = namelist.parse_namelist_string(string)[0]
    assert str(nml) == string


@pytest.mark.parametrize(
    "string,arr",
    [("&nml\nval = (/ {} /)\n/\n", range(1, 30)),
     ("&nml\nval = (/ {} /)\n/\n", range(1, 100)),
     ]
)
def test_string_out_linebreak(string, arr):
    arr_string = ",".join([str(a) for a in arr])
    nml_string = string.format(arr_string)
    assert_arr_string = ""
    while True:
        lnbrk = namelist.namelist.NML_LINE_LENGTH
        if len(arr_string) <= lnbrk:
            assert_arr_string += arr_string
            break
        lnbrk_offset = arr_string[lnbrk:].find(',')
        lnbrk += max(lnbrk_offset, 0) + 1
        assert_arr_string += arr_string[:lnbrk] + " &\n"
        arr_string = arr_string[lnbrk:]

    assert_string = string.format(assert_arr_string)
    nml = namelist.parse_namelist_string(nml_string)[0]
    print(str(nml), assert_string)
    assert str(nml) == assert_string


@pytest.mark.parametrize(
    "string",
    ["&nml\n/\n",
     "&nml\nval1 = 3\n/\n",
     "&nml\nval1 = .TRUE., val2 = 1.\n/\n",
     "&nml\nval1 = .FALSE.\n/\n",
     "&nml\nval1 = (/ 1,2,3,4,5,6 /)\n/\n",
     "&nml\nval1 = (/ .TRUE.,.FALSE.,.TRUE.,.TRUE. /)\n/\n",
     "&nml\nval = (/ {} /)\n/\n".format(range(1, 100)),
     ]
)
def test_repr(string):
    nml = namelist.parse_namelist_string(string)[0]
    nml_copy = eval(repr(nml))
    assert nml == nml_copy
    assert nml.name == nml_copy.name


@pytest.mark.parametrize(
    "name",
    ["nml",
     "nml_other",
     "nml2",
     ]
)
def test_has_name(name):
    nml_string = "&{} /".format(name)
    nml = namelist.parse_namelist_string(nml_string)[0]
    assert nml.has_name(name)


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
    "string, val",
    [("&nml2 val=34 &end", 34),
     ("&nml2\n val=34. \n&end", 34.),
     ]
)
def test_val_conversion(string, val):
    nml = namelist.parse_namelist_string(string)[0]
    assert nml.name == "nml2"
    assert nml["val"] == val
    assert type(nml["val"]) == type(val)


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


@pytest.mark.parametrize(
    "string,arr",
    [("&nml val=(/ {} /)/", list(range(1, 10))),
     ("&nml val=(/{}/)/", list(range(1, 10))),
     ("&nml val=(/{}/)/", [float(n) for n in range(1, 10)]),
     ]
)
def test_var_array(string, arr):
    nml_string = string.format(",".join([str(a) for a in arr]))
    nml = namelist.parse_namelist_string(nml_string)[0]

    # do elementwise identity check to also check types
    for a, b in zip(nml["val"], arr):
        assert a == b
        assert type(a) == type(b)

    nml_string = string.format(" , ".join([str(a) for a in arr]))
    nml = namelist.parse_namelist_string(nml_string)[0]
    for a, b in zip(nml["val"], arr):
        assert a == b
        assert type(a) == type(b)


@pytest.mark.parametrize("string", ["&nml val= {}, val2='lsl'/"])
@pytest.mark.parametrize("op", ["+", "-", "/", "*", "**"])
@pytest.mark.parametrize(
    "expression",
    ["2.0 {} 2",
     "1e3 {} 1e2",
     "1 {} 2 - 3",
     "(1 + 2) {} 3",
     "((1 + 2)) {} 3",
     "(((1 {} 2))* 3)",
     ]
)
def test_var_expression(string, op, expression):
    nml_string = string.format(expression.format(op))
    nml = namelist.parse_namelist_string(nml_string)[0]
    assert nml["val"] == eval(expression.format(op))
