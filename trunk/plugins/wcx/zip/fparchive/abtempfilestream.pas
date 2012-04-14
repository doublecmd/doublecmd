(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is : Turbopower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * Robert J. Love
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Robert J. Love
 *
 * ***** END LICENSE BLOCK ***** *)
unit AbTempFileStream;

interface

uses SysUtils,Classes,DCClassesUtf8;

type
  TAbTempFileStream = class (TFileStreamEx)
  private
    FFileName: string;
    FDeleteFile : Boolean;
  public
    constructor Create(aDeleteFile : Boolean);
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

implementation

uses
  AbUtils,
  DCOSUtils;

{ TAbTempFileStream }

constructor TAbTempFileStream.Create(aDeleteFile : Boolean);
begin
  FDeleteFile := aDeleteFile;
  FFileName := AbGetTempFile(GetTempDir,True);
  inherited Create(FFileName,fmOpenReadWrite + fmShareExclusive);
end;

destructor TAbTempFileStream.Destroy;
var
 ScopeFileName : String;
 ScopeDel : Boolean;
begin
  ScopeFileName := FFileName;
  ScopeDel := FDeleteFile;
  if Not FDeleteFile then // This file will not be deleted here
     AbFlushOsBuffers(Handle); // Flush Buffers to make sure commited to disk.
  inherited;
  if ScopeDel then
     mbDeleteFile(ScopeFileName);
end;

end.
