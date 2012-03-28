{
    Double Commander
    -------------------------------------------------------------------------
    Miscellaneous functions for file source operations and queues.

    Copyright (C) 2012       Przemysław Nagay (cobines@gmail.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uFileSourceOperationMisc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFileSourceOperation, uOperationsManager;

function GetOperationStateString(OperationState: TFileSourceOperationState): String;
function GetProgressString(const Progress: Double): String;
procedure ShowOperation(OpManItem: TOperationsManagerItem);

implementation

uses
  fFileOpDlg;

function GetOperationStateString(OperationState: TFileSourceOperationState): String;
begin
  if OperationState <> fsosRunning then
    Result := ' [' + FileSourceOperationStateText[OperationState] + ']'
  else
    Result := '';
end;

function GetProgressString(const Progress: Double): String;
begin
  Result := FloatToStrF(Progress * 100, ffFixed, 0, 0) + '%';
end;

procedure ShowOperation(OpManItem: TOperationsManagerItem);
begin
  if OpManItem.Queue.IsFree or (OpManItem.Queue.Count = 1) then
    TfrmFileOp.ShowFor(OpManItem.Handle);
end;

end.

