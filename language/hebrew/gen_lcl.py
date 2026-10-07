# -*- coding: utf-8 -*-
"""Fill empty msgstr in language/lcl/lclstrconsts.he.po from the LCL dictionary.

Run from the repository root:
    python language/hebrew/gen_lcl.py

The dictionary lives in he_dict_lcl.py.
"""
import os
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
if SCRIPT_DIR not in sys.path:
    sys.path.insert(0, SCRIPT_DIR)

from he_dict_lcl import LCL  # noqa: E402

REPO_ROOT = os.path.dirname(os.path.dirname(SCRIPT_DIR))
PO = os.path.join(REPO_ROOT, 'language', 'lcl', 'lclstrconsts.he.po')


def escape_po(s):
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t')


def unescape_po(s):
    out = []
    i = 0
    while i < len(s):
        c = s[i]
        if c == '\\' and i + 1 < len(s):
            n = s[i + 1]
            mapping = {'n': '\n', 't': '\t', 'r': '\r', '\\': '\\', '"': '"', "'": "'"}
            out.append(mapping.get(n, n))
            i += 2
        else:
            out.append(c)
            i += 1
    return ''.join(out)


with open(PO, encoding='utf-8') as f:
    content = f.read()

# Preserve the file's original line-ending style (CRLF or LF).
crlf = '\r\n' in content
newline = '\r\n' if crlf else '\n'

lines = content.split('\n')
# strip the trailing '\r' from each line when CRLF is used
if crlf:
    lines = [l.rstrip('\r') for l in lines]
out = []
i = 0
missing = []
changed = 0
total = 0
while i < len(lines):
    line = lines[i]
    if line.startswith('msgid '):
        # keep the msgid line and its continuation lines
        out.append(line)
        # collect msgid value (possibly multi-line)
        parts = []
        parts.append(line[len('msgid '):].strip())
        j = i + 1
        while j < len(lines) and lines[j].startswith('"'):
            parts.append(lines[j].strip())
            out.append(lines[j])
            j += 1
        msgid = ''.join(unescape_po(p[1:-1]) for p in parts)
        # locate the msgstr line (must be j)
        k = j
        while k < len(lines) and not lines[k].startswith('msgstr '):
            out.append(lines[k])
            k += 1
        if k < len(lines) and lines[k].startswith('msgstr '):
            val = lines[k][len('msgstr '):].strip()
            if val == '""':
                total += 1
                if msgid in LCL:
                    out.append('msgstr "{}"'.format(escape_po(LCL[msgid])))
                    changed += 1
                else:
                    missing.append(msgid)
                    out.append(lines[k])
            else:
                out.append(lines[k])
            i = k + 1
        else:
            # no msgstr - keep everything as-is
            out.append(line)
            i += 1
    else:
        out.append(line)
        i += 1

with open(PO, 'w', encoding='utf-8', newline='') as f:
    f.write(newline.join(out) + newline)

print('empty msgstr found:', total)
print('filled:', changed)
print('still missing:', len(missing))
for m in missing:
    print('  MISSING:', repr(m))
