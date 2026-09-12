# -*- coding: utf-8 -*-
"""Generate language/doublecmd.he.po from language/doublecmd.pot + the Hebrew dictionary.

Run from the repository root:
    python language/hebrew/gen_he.py

The dictionary lives in he_dict.py (merged from he_dict_01..04.py).
"""
import os
import re
import sys

# Make sure the dict modules (he_dict.py, he_dict_01.py, ...) are importable
# no matter from which directory the script is invoked.
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
if SCRIPT_DIR not in sys.path:
    sys.path.insert(0, SCRIPT_DIR)

from he_dict import TRANSLATIONS  # noqa: E402

# Paths relative to the repository root.
REPO_ROOT = os.path.dirname(os.path.dirname(SCRIPT_DIR))
POT = os.path.join(REPO_ROOT, 'language', 'doublecmd.pot')
OUT = os.path.join(REPO_ROOT, 'language', 'doublecmd.he.po')

HEADER = '''msgid ""
msgstr ""
"Project-Id-Version: Double Commander 1.1.0 alpha\\n"
"Report-Msgid-Bugs-To: \\n"
"POT-Creation-Date: 2022-11-15 11:15+0300\\n"
"PO-Revision-Date: 2026-08-24 12:00+0300\\n"
"Last-Translator: lodbig <lodbig@users.noreply.github.com>\\n"
"Language-Team: Hebrew\\n"
"Language: he\\n"
"MIME-Version: 1.0\\n"
"Content-Type: text/plain; charset=UTF-8\\n"
"Content-Transfer-Encoding: 8bit\\n"
"X-Native-Language: עברית\\n"

'''


def unescape_po(s):
    """Unescape a .po string literal (content between quotes)."""
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


def escape_po(s):
    """Escape a plain string for a single-line .po msgstr."""
    return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t')


def parse_entries(content):
    """Split pot content into list of entry blocks.
    Each block is a list of lines belonging to one entry (comments included)."""
    lines = content.split('\n')
    blocks = []
    cur = None
    in_msgstr = False
    for line in lines:
        if line.startswith('msgid ') or line.startswith('msgctxt '):
            if cur is not None:
                blocks.append(cur)
            cur = [line]
            in_msgstr = False
        elif cur is not None:
            if line.startswith('msgstr '):
                in_msgstr = True
            elif in_msgstr and line.startswith('"'):
                pass  # continuation of msgstr - keep but we'll replace anyway
            cur.append(line)
    if cur is not None:
        blocks.append(cur)
    return blocks


def entry_msgid(block):
    """Return (msgid_real_string, msgctxt_string)."""
    msgid_parts = []
    msgctxt = None
    in_msgid = False
    for line in block:
        if line.startswith('msgctxt '):
            msgctxt = unescape_po(line[len('msgctxt '):].strip().strip('"'))
        elif line.startswith('msgid '):
            in_msgid = True
            val = line[len('msgid '):].strip()
            if val.startswith('"'):
                msgid_parts.append(unescape_po(val[1:-1]))
        elif in_msgid and line.startswith('"'):
            msgid_parts.append(unescape_po(line.strip()[1:-1]))
        elif line.startswith('msgstr '):
            in_msgid = False
    return ''.join(msgid_parts), msgctxt


def rebuild_block(block, translation):
    """Replace the msgstr in block with the translation."""
    out = []
    in_msgstr = False
    replaced = False
    for line in block:
        if line.startswith('msgstr '):
            in_msgstr = True
            if not replaced:
                out.append('msgstr "{}"'.format(escape_po(translation)))
                replaced = True
        elif in_msgstr and line.startswith('"'):
            # continuation of old msgstr - drop
            continue
        else:
            if in_msgstr and not replaced:
                out.append('msgstr ""')
                replaced = True
            in_msgstr = False
            out.append(line)
    if not replaced:
        out.append('msgstr "{}"'.format(escape_po(translation)))
    return out


def main():
    with open(POT, encoding='utf-8') as f:
        content = f.read()
    blocks = parse_entries(content)
    print('blocks:', len(blocks))

    missing = []
    out_lines = []
    first = True
    for block in blocks:
        joined = '\n'.join(block)
        if first and '"Project-Id-Version' in joined:
            out_lines.append(HEADER.rstrip('\n'))
            out_lines.append('')
            first = False
            continue
        msgid, msgctxt = entry_msgid(block)
        if msgid == '':
            out_lines.append('\n'.join(block))
            continue
        if msgid in TRANSLATIONS:
            trans = TRANSLATIONS[msgid]
            new_block = rebuild_block(block, trans)
            out_lines.append('\n'.join(new_block))
        else:
            missing.append(msgid)
            out_lines.append('\n'.join(block))

    with open(OUT, 'w', encoding='utf-8', newline='') as f:
        f.write('\n'.join(out_lines) + '\n')
    print('missing:', len(missing))
    for m in missing[:60]:
        print('  MISSING:', repr(m))


if __name__ == '__main__':
    main()
