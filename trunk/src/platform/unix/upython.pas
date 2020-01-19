{
   Double Commander
   -------------------------------------------------------------------------
   Simple interface to the Python language

   Copyright (C) 2014-2015 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
}

unit uPython;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils, CTypes, DCOSUtils;

type
  PPyObject = ^TPyObject;
  PPyTypeObject = ^TPyTypeObject;

  TPyTypeObject = record
    ob_refcnt: csize_t;
    ob_type: PPyTypeObject;
    ob_size: csize_t;
    tp_name: PAnsiChar;
    tp_basicsize, tp_itemsize: csize_t;
    //* Methods to implement standard operations */
    tp_dealloc: procedure(obj: PPyObject); cdecl;
  end;

  TPyObject = record
    ob_refcnt: csize_t;
    ob_type: PPyTypeObject;
  end;

var
  // pythonrun.h
  Py_Initialize: procedure; cdecl;
  Py_Finalize: procedure; cdecl;
  PyErr_Print: procedure; cdecl;
  PyRun_SimpleString: function(s: PAnsiChar): cint; cdecl;
  // import.h
  PyImport_Import: function(name: PPyObject): PPyObject; cdecl;
  // object.h
  PyCallable_Check: function(ob: PPyObject): cint; cdecl;
  PyObject_GetAttrString: function (ob: PPyObject; c: PAnsiChar): PPyObject; cdecl;
  // abstract.h
  PyObject_CallObject: function(callable_object, args: PPyObject): PPyObject; cdecl;
  PyObject_CallFunctionObjArgs: function(callable: PPyObject): PPyObject; cdecl; varargs;
  PyObject_CallMethodObjArgs: function(o, name: PPyObject): PPyObject; cdecl; varargs;
  // stringobject.h
  PyString_AsString: function(ob: PPyObject): PAnsiChar; cdecl;
  PyString_FromString: function(s: PAnsiChar): PPyObject; cdecl;
  // listobject.h
  PyList_New: function(size: csize_t): PPyObject; cdecl;
  PyList_Size: function (ob: PPyObject): csize_t; cdecl;
  PyList_GetItem: function(ob: PPyObject; index: csize_t): PPyObject; cdecl;
  PyList_SetItem: function(ob: PPyObject; index: csize_t; item: PPyObject): cint; cdecl;
  // tupleobject.h
  PyTuple_New: function(size: csize_t): PPyObject; cdecl;
  PyTuple_SetItem: function(ob: PPyObject; index: csize_t; item: PPyObject): cint; cdecl;

procedure Py_DECREF(op: PPyObject);
procedure Py_XDECREF(op: PPyObject);
function  PyStringToString(S: PPyObject): String;


procedure PythonAddModulePath(const Path: String);
function  PythonLoadModule(const ModuleName: String): PPyObject;
function  PythonRunFunction(Module: PPyObject; const FunctionName: String): PPyObject; overload;
function  PythonRunFunction(Module: PPyObject; const FunctionName, FunctionArg: String): PPyObject; overload;
function  PythonRunFunction(Module: PPyObject; const FunctionName: String; FileList: TStrings): PPyObject; overload;

var
  PythonExe: String;
  HasPython: Boolean = False;

implementation

uses
  dynlibs, dl, uMyUnix;

procedure Py_DECREF(op: PPyObject);
begin
  with op^ do begin
    Dec(ob_refcnt);
    if ob_refcnt = 0 then begin
      ob_type^.tp_dealloc(op);
    end;
  end;
end;

procedure Py_XDECREF(op: PPyObject); inline;
begin
  if Assigned(op) then Py_DECREF(op);
end;

function PyStringToString(S: PPyObject): String;
begin
  if not Assigned(S) then
    Result:= EmptyStr
  else begin
    Result:= StrPas(PyString_AsString(S));
    Py_DECREF(S);
  end;
end;

function StringsToPyList(Strings: TStrings): PPyObject;
var
  I: LongInt;
begin
  Result:= PyList_New(Strings.Count);
  if not Assigned(Result) then Exit;
  for I:= 0 to Strings.Count - 1 do
  begin
    PyList_SetItem(Result, I, PyString_FromString(PAnsiChar(Strings[I])));
  end;
end;

function PyObjectsToPyTuple(Values: array of PPyObject): PPyObject;
var
  Index: csize_t;
begin
  Result:= PyTuple_New(Length(Values));
  if not Assigned(Result) then Exit;
  for Index:= Low(Values) to High(Values) do
  begin
    PyTuple_SetItem(Result, Index, Values[Index]);
  end;
end;

procedure PythonAddModulePath(const Path: String);
begin
  PyRun_SimpleString('import sys');
  PyRun_SimpleString(PAnsiChar('sys.path.append("' + Path + '")'));
end;

function PythonLoadModule(const ModuleName: String): PPyObject;
var
  pyName: PPyObject;
begin
  pyName:= PyString_FromString(PAnsiChar(ModuleName));
  Result:= PyImport_Import(pyName);
  Py_DECREF(pyName);
end;

function PythonCallFunction(Module: PPyObject; const FunctionName: String; FunctionArg: PPyObject): PPyObject; overload;
var
  pyFunc, pyArgs: PPyObject;
begin
  if Assigned(Module) then
  begin
    pyFunc:= PyObject_GetAttrString(Module, PAnsiChar(FunctionName));
    if (Assigned(pyFunc) and (PyCallable_Check(pyFunc) <> 0)) then
    begin
      if (FunctionArg = nil) then
        pyArgs:= nil
      else begin
        pyArgs:= PyObjectsToPyTuple([FunctionArg]);
      end;
      Result:= PyObject_CallObject(pyFunc, pyArgs);
      Py_XDECREF(pyArgs);
      if (Result = nil) then begin
        PyErr_Print()
      end;
      Py_DECREF(pyFunc);
    end;
  end;
end;

function PythonRunFunction(Module: PPyObject; const FunctionName: String): PPyObject;
begin
  Result:= PythonCallFunction(Module, FunctionName, nil);
end;

function PythonRunFunction(Module: PPyObject; const FunctionName, FunctionArg: String): PPyObject;
var
  pyArgs: PPyObject;
begin
  pyArgs:= PyString_FromString(PAnsiChar(FunctionArg));
  Result:= PythonCallFunction(Module, FunctionName, pyArgs);
end;

function PythonRunFunction(Module: PPyObject; const FunctionName: String; FileList: TStrings): PPyObject;
var
  pyArgs: PPyObject;
begin
  pyArgs:= StringsToPyList(FileList);
  Result:= PythonCallFunction(Module, FunctionName, pyArgs);
end;

function FindPythonExecutable: String;
begin
  if ExecutableInSystemPath('python2') then
    Result:= 'python2'
  else begin
    Result:= 'python';
  end;
end;

var
  libpython: TLibHandle;

procedure Initialize;
begin
  PythonExe:= FindPythonExecutable;
  libpython:= TLibHandle(dlopen('libpython2.7.so.1.0', RTLD_NOW or RTLD_GLOBAL));
  HasPython:= libpython <> NilHandle;
  if HasPython then
  try
    @Py_Initialize:= SafeGetProcAddress(libpython, 'Py_Initialize');
    @Py_Finalize:= SafeGetProcAddress(libpython, 'Py_Finalize');
    @PyErr_Print:= SafeGetProcAddress(libpython, 'PyErr_Print');
    @PyRun_SimpleString:= SafeGetProcAddress(libpython, 'PyRun_SimpleString');
    @PyImport_Import:= SafeGetProcAddress(libpython, 'PyImport_Import');
    @PyCallable_Check:= SafeGetProcAddress(libpython, 'PyCallable_Check');
    @PyObject_GetAttrString:= SafeGetProcAddress(libpython, 'PyObject_GetAttrString');
    @PyObject_CallObject:= SafeGetProcAddress(libpython, 'PyObject_CallObject');
    @PyObject_CallMethodObjArgs:= SafeGetProcAddress(libpython, 'PyObject_CallMethodObjArgs');
    @PyObject_CallFunctionObjArgs:= SafeGetProcAddress(libpython, 'PyObject_CallFunctionObjArgs');
    @PyString_AsString:= SafeGetProcAddress(libpython, 'PyString_AsString');
    @PyString_FromString:= SafeGetProcAddress(libpython, 'PyString_FromString');
    @PyList_New:= SafeGetProcAddress(libpython, 'PyList_New');
    @PyList_Size:= SafeGetProcAddress(libpython, 'PyList_Size');
    @PyList_GetItem:= SafeGetProcAddress(libpython, 'PyList_GetItem');
    @PyList_SetItem:= SafeGetProcAddress(libpython, 'PyList_SetItem');
    @PyTuple_New:= SafeGetProcAddress(libpython, 'PyTuple_New');
    @PyTuple_SetItem:= SafeGetProcAddress(libpython, 'PyTuple_SetItem');
    // Initialize the Python interpreter
    Py_Initialize();
  except
    HasPython:= False;
  end;
end;

procedure Finalize;
begin
  if HasPython then Py_Finalize();
  if libpython <> NilHandle then FreeLibrary(libpython);
end;

initialization
  Initialize;

finalization
  Finalize;

end.

