# -*- coding: utf-8 -*-
"""Merge the Hebrew translation dictionary parts (he_dict_01..04.py)."""
import importlib
import os
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
if SCRIPT_DIR not in sys.path:
    sys.path.insert(0, SCRIPT_DIR)

TRANSLATIONS = {}

for i in range(1, 12):
    try:
        mod = importlib.import_module('he_dict_%02d' % i)
        TRANSLATIONS.update(mod.PART)
        print('loaded he_dict_%02d: %d entries' % (i, len(mod.PART)))
    except ImportError:
        pass
