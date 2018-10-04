# -*- coding: utf-8 -*-
"""
Created on Tue Sep 23 10:27:57 2014

@author: mclaus
"""
import re

try:
    from collections import OrderedDict as DictClass
except ImportError:  # pragma: no cover # Python < 2.7
    DictClass = dict

MODULE_NAME = "namelist"
NML_LINE_LENGTH = 70
# Config file parser, called from the class initialization
varname = r'[a-zA-Z][a-zA-Z0-9_]*'
valueBool = re.compile(r"(\.(true|false|t|f)\.)", re.I)
quote = re.compile(r"([\']{1}[^\']*[\']{1}|[\"]{1}[^\"]*[\"]{1})",
                   re.MULTILINE)
namelistname = re.compile(r"&(" + varname + r")")
paramname = re.compile(r"^(" + varname + r")")
namlistend = re.compile(r'^(&(end)?|/)$', re.I)
comment = re.compile(r"!.*$", re.MULTILINE)
equalsign = re.compile(r"^=$")
computation = re.compile(
    r"([\(]*[0-9\.e]+[\)\s]*([\*\+\-/]{1}|[\*]{2})\s*)+[0-9\.e]+[\)]*", re.I
)


class Namelist(DictClass):
    """Class to handle Fortran Namelists.

    Namelist(string) -> new namelist with fortran namelist group name
    Namelist(string, init_val) -> new initialized namelist with namelist group
        name and init_val beeing a valid initialisation object for the parent
        class (either OrderedDict for Python >= 2.7 or else dict).
    A fortran readable string representation of the namelist can be generated
    via str() build-in function. A string representation of the Python object
    that can be used with eval or string. Template substitution can be obtained
    by repr() build-in function.
    """

    @property
    def name(self):
        """Namelist group name."""
        return self._name

    def __init__(self, name, init_val=()):
        """Create a `Namelist` instance.

        See help(type(x)) for signature.
        """
        self._name = name
        super(self.__class__, self).__init__(init_val)

    def __str__(self):
        """Fortran readable string representation of the namelist.

        If a value v is a sequence, an 1D fortran array representation
        is created using iter(v).
        """
        retstr = "&%s\n" % str(self.name)
        for k, v in self.items():
            if hasattr(v, '__iter__'):
                retstr += "%s = (/ " % k
                tmpstr = ""
                for vv in v:
                    if isinstance(vv, bool):
                        if vv:
                            rvv = ".TRUE."
                        else:
                            rvv = ".FALSE."
                    else:
                        rvv = repr(vv)
                    tmpstr += "%s," % rvv
                    if len(tmpstr) > NML_LINE_LENGTH:
                        if vv == v[-1]:
                            tmpstr = tmpstr[:-1]
                        retstr += tmpstr + " &\n"
                        tmpstr = ""
                retstr = retstr + tmpstr[:-1] + " /)\n"
            else:
                if isinstance(v, bool):
                    if v:
                        rv = ".TRUE."
                    else:
                        rv = ".FALSE."
                else:
                    rv = repr(v)
                retstr += "%s = %s\n" % (str(k), rv)
        retstr += "/\n"
        return retstr

    def __repr__(self):
        """Return a string that can be used by eval to create a copy."""
        retstr = "%s.%s(%s, (" % (MODULE_NAME, self.__class__.__name__,
                                  repr(self.name))
        for k, v in self.items():
            retstr += "%s, " % repr((k, v))
        retstr += "))"
        return retstr

    def has_name(self, name):
        """Return `True` if `name` matches the namelist group name.

        Parameters
        ----------
        name : str
            name to test against.

        Returns
        -------
        bool : `True` if `name` matches the namelist group name.

        """
        return name == self.name


def parse_namelist_file(in_file):
    """Parse namelists from file object.

    Parameters
    ----------
    in_file : :obj:
        Any object that implements pythons file object API, i.e. that offers a
        `read` and `seek` method.

    Returns
    -------
    :obj:`List` of :obj:`Namelist`

    """
    namelist_string = in_file.read()
    in_file.seek(0, 0)
    return parse_namelist_string(namelist_string)


def parse_namelist_string(in_string):
    """Parse namelists from string.

    Parameters
    ----------
    in_string : str
        String containing one or more namelist definitions.

    Returns
    -------
    :obj:`List` of :obj:`Namelist`

    """
    retlist = []
    content = _tokenize(in_string)
    for item in content:
        match = re.match(namelistname, item)
        if match:
            nmlname = match.group(1)
            nml = Namelist(nmlname)
            retlist.append(nml)
            continue
        match = re.match(paramname, item)
        if match:
            pname = match.group(1)
            nml[pname] = []
            continue
        if re.match(namlistend, item):
            continue
        if re.match(equalsign, item):
            continue
        match = re.match(valueBool, item)
        if match:
            nml[pname].append(match.group(1)[1].lower() == "t")
            continue
        match = re.match(quote, item)
        if match:
            nml[pname].append(match.group(1)[1:-1])
            continue
        try:
            nml[pname].append(int(item))
        except ValueError:
            pass
        else:
            continue  # pragma: no cover
        try:
            nml[pname].append(float(item))
        except ValueError:
            pass
        else:
            continue  # pragma: no cover
        match = re.match(computation, item)
        if match:
            nml[pname].append(eval(item))
    for nml in retlist:
        for k, v in nml.items():
            if len(v) == 1:
                nml[k] = v[0]
    return retlist


def _tokenize(text):
    """Extract syntax tokens."""
    fs = "$$$FS$$$"

    # remove comments
    text = re.sub(comment, '', text)

    hashed_tokens = {}

    # replace quoted strings by hash
    text = _hash_token(text, quote, hashed_tokens, fs)

    # replace numerical computations by hash
    text = _hash_token(text, computation, hashed_tokens, fs)

    for char, rep in zip(('\n', r',', ' ', '=', '(/', '/)'),
                         (fs, fs, fs, fs+'='+fs, fs, fs)):
        text = text.replace(char, rep)
    text = text.split(fs)
    tokens = [token.strip() for token in text if token.strip() != '']
    return [hashed_tokens[t] if t in hashed_tokens else t for t in tokens]


def _hash_token(text, pattern, hashed_tokens, fs):
    while True:
        match = re.search(pattern, text)
        if not match:
            break
        matched_str = match.group(0)
        hashed = str(hash(matched_str))
        hashed_tokens[hashed] = matched_str
        text = text.replace(matched_str, fs+hashed+fs, 1)
    return text
