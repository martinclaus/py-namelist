py-namelist
===========
Parse Fortran Namelists to dict-like objects and back

## Download
To get the latest git version do
```
$ git clone https://git.geomar.de:martin-claus/py-namelist.git
```

## Usage
To parse a namelist file you call `parse_namelist(fobj)` where fobj is a file-like object offering a `read()` and `seek()` method (usually the standard python file object).
```
import namelist
with open(your_nml_file) as fobj:
	nmls = namelist.parse_namelist_file(fobj)
```
`nmls` will be a list of instances of the `Namelist` class.

`Namelist` is a subclass of `dict` or `OrderedDict` if you use Python >= 2.7. A `Namelist` instance, `nml`, is initialized with an name and optionally with initial values.
```
nml = Namelist("param", (("key1", val1), ...))
```

The name attribute will set the read-only property name of `nml`. To change, add or delete values the same methods are available as for dict.
```
print nml.name
nml.update({"eggs": 1, "spam": [1, 2, 3]})
del(nml["param"])
```

To create a Fortran readable string representation of the Namelist instance, just use the `str()` build-in
```
s = str(nml)
```
or just
```
print(nml)
```

A string representation of the Namelist instance that can be used by `eval()` to create copies of it can be created by `repr()`
```
print repr(nml)
```

**Note**: The parsing of namelist does not have to strictly follow the Fortran standard. Hence, some namelists that are perfectly accepted by some Fortran version are not guaranteed to be correctly parsed by `parse_namelist()`. Always check the content of your Namelist object.
