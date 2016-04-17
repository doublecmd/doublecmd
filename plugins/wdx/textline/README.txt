Text Line
Content plugin for Double Commander

Description
-----------
Plugin is intended to show one line of a text file.
You can select line number and text encoding, you can replace one substring
by another.

Settings are stored in textline.ini.
Without settings file plugin handles all files, and doesn't replace anything.

textline.ini example:

[Options]
;list of supported extensions, separated by a space, if empty - all files
Extensions=txt ini inf
;skip empty lines
SkipEmpty=0

;a list of substitutions in the format S<n>=<original_text>=<new_text>
[Replaces]
;replace "hello" by nothing
;S1=hello=
;replace "test" by "hello"
;S2=test=hello
S1=
S2=
S3=
S4=
S5=
S6=
S7=
S8=
S9=
S10=
