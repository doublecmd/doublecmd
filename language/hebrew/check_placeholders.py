# -*- coding: utf-8 -*-
"""Verify all format placeholders in msgid appear in msgstr."""
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


def check(path):
    with open(path, encoding='utf-8') as f:
        content = f.read()
    lines = content.split('\n')
    i = 0
    n = len(lines)
    problems = 0
    while i < n:
        line = lines[i]
        if line.startswith('msgid '):
            parts = [line[len('msgid '):].strip()]
            j = i + 1
            while j < n and lines[j].startswith('"'):
                parts.append(lines[j].strip())
                j += 1
            msgid = ''.join(unescape_po(p[1:-1]) for p in parts)
            if j < n and lines[j].startswith('msgstr '):
                sparts = [lines[j][len('msgstr '):].strip()]
                k = j + 1
                while k < n and lines[k].startswith('"'):
                    sparts.append(lines[k].strip())
                    k += 1
                msgstr = ''.join(unescape_po(p[1:-1]) for p in sparts)
                # find format specs in msgid: %d %s %f %x %e %g, %%, %1:d etc
                specs = re.findall(r'%(?:\d+\$)?[-+ #0]*\d*(?:\.\d+)?[diouxXeEfFgGsc%]', msgid)
                for s in specs:
                    if s != '%%' and s not in msgstr:
                        print(f'{path}: missing {s!r} in {msgid[:80]!r}')
                        problems += 1
            i = j
        else:
            i += 1
    print(f'{path}: {problems} placeholder problems')
    return problems


total = 0
for p in sys.argv[1:]:
    total += check(p)
sys.exit(1 if total else 0)
