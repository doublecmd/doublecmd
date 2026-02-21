{*
 * Copyright © 2008 Kristian Høgsberg
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *}

{** \file wayland-util.pas
 *
 * \brief Utility classes, functions, and macros.
 *}
unit wayland_util;

{$mode objfpc}{$H+}
{$packrecords c}
{$linklib wayland-client}

interface

uses
  Classes, SysUtils, ctypes;

type
  PPwl_interface = ^Pwl_interface;
  Pwl_interface = ^Twl_interface;
  Pwl_message = ^ Twl_message;
  Twl_message = record
    name: PChar;
    signature: PChar;
    types: PPwl_interface;
  end;


  Twl_interface = record
    name: PChar;
    version: cint;
    method_count: cint;
    methods: Pwl_message;
    event_count: cint;
    events: Pwl_message;
  end;

  Pwl_argument = ^Twl_argument;
  Twl_argument = record
    case integer of
      0: (i: cint32);
      1: (u: cuint32);
      2: (f: cint32);
      3: (s: PChar);
      4: (o : Pointer {wl_object});
      5: (n: cuint32);
      6: (a: Pointer {wl_array});
      7: (h: cuint32);
  end;

  wl_dispatcher_func_t = function (arg0: Pointer; arg1: Pointer; arg2: cuint32;
				    {const} message: Pwl_message;
				    argument: Pwl_argument): cint; cdecl;


  wl_log_func_t = procedure (char :pchar; va_list: Pointer); cdecl; // not sure how to use va_list, it's a record type afaik.

implementation

end.

