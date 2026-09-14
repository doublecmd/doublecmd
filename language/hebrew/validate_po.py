# -*- coding: utf-8 -*-
"""Strict structural validation of .po files."""
import re
import sys


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


def parse(path):
    with open(path, encoding='utf-8') as f:
        content = f.read()
    errors = []
    lines = content.split('\n')
    i = 0
    n = len(lines)
    entries = 0
    while i < n:
        line = lines[i]
        if line.startswith('msgid '):
            entries += 1
            parts = []
            parts.append(line[len('msgid '):].strip())
            j = i + 1
            while j < n and lines[j].startswith('"'):
                parts.append(lines[j].strip())
                j += 1
            if j >= n or not lines[j].startswith('msgstr '):
                errors.append(f'line {i+1}: msgid without following msgstr')
                i = j
                continue
            # msgstr
            sparts = []
            sparts.append(lines[j][len('msgstr '):].strip())
            k = j + 1
            while k < n and lines[k].startswith('"'):
                sparts.append(lines[k].strip())
                k += 1
            msgid = ''.join(unescape_po(p[1:-1]) for p in parts)
            msgstr = ''.join(unescape_po(p[1:-1]) for p in sparts)
            if msgid != '' and msgstr == '':
                errors.append(f'line {i+1}: empty translation for: {msgid[:60]!r}')
            i = k
        else:
            i += 1
    print(f'{path}: {entries} entries, {len(errors)} problems')
    for e in errors[:30]:
        print('  ', e)
    return len(errors)


total = 0
for path in sys.argv[1:]:
    total += parse(path)
sys.exit(1 if total else 0)
