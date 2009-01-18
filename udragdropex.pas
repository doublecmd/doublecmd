{
    Double Commander
    -------------------------------------------------------------------------
    Interface unit for Drag&Drop to external applications.

    Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit uDragDropEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFileList;

procedure DoDragDropEx(FileList: TFileList);

implementation

{$IFDEF MSWINDOWS}
uses
  Windows, ActiveX, uOleDragDrop;
{$ENDIF}


procedure DoDragDropEx(FileList: TFileList);
{$IFDEF MSWINDOWS}
var
  DropSource: TFileDropSource;
  DropData: THDropDataObject;
  Rslt: HRESULT;
  dwEffect: LongWord;
  DropPoint: TPoint;
  I: Integer;
begin
    // Create source-object
    DropSource:= TFileDropSource.Create;

    // and data object
    DropPoint.x:= 0;
    DropPoint.y:= 0;
    DropData:= THDropDataObject.Create(DropPoint, True);

    for I:= 0 to FileList.Count - 1 do
      DropData.Add (FileList.GetFileName(I));

    // Start OLE Drag&Drop
    // if Shift pressed then move else copy
    if (GetKeyState(VK_SHIFT) and $8000) <> 0 then
      Rslt:= DoDragDrop(DropData, DropSource, DROPEFFECT_MOVE, @dwEffect)
    else
      Rslt:= DoDragDrop(DropData, DropSource, DROPEFFECT_COPY, @dwEffect);

    if ((Rslt <> DRAGDROP_S_DROP) and (Rslt <> DRAGDROP_S_CANCEL)) then
    begin
      case Rslt of
        E_OUTOFMEMORY:
          MessageBox(0, 'Out of memory', 'Error!', 16);
        else
          MessageBox(0, 'Something bad happened', 'Error!', 16);
      end;
    end;

    { Освобождаем использованные ресурсы

    после завершения работы }

   { DropSource.Free;

    DropData.Free; }
end;
{$ELSE UNIX}
begin
  //TODO: Add Drag&Drop to external applications under Linux
end;
{$ENDIF}

end.

