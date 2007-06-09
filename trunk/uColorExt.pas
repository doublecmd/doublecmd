{
   Double Commander
   -------------------------------------------------------------------------
   Load colors of files in file panels

   Copyright (C) 2003-2004 Radek Cervinka (radek.cervinka@centrum.cz)
   Copyright (C) 2006-2007  Koblov Alexander (Alexx2000@mail.ru)

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

unit uColorExt;

interface
uses
  Classes, Graphics;
type
  TColorExt=Class
  protected
    lsExts:TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function ColorByExt(const sExt:String):TColor;
    procedure Load;
    procedure Save;
  end;

implementation

uses
  SysUtils, uGlobs;

constructor TColorExt.Create;
begin
  inherited;
  lsExts:=TStringList.Create;
end;

destructor TColorExt.Destroy;
begin
  if assigned(lsExts) then
    FreeAndNil(lsExts);
end;

function TColorExt.ColorByExt(const sExt:String):TColor;
var
  iIndex:Integer;
begin
  Result:= gForeColor; //$0000ff00;
  if sExt='' then Exit;
  if sExt[1]='.' then
    iIndex:= lsExts.IndexOf(UpperCase(Copy(sExt,2, Length(sExt)-1)))
   else
    iIndex:= lsExts.IndexOf(UpperCase(sExt));
  if iIndex=-1 then Exit;
  Result:=TColor(lsExts.Objects[iIndex]);
end;

(* Load colors of files from doublecmd.ini *)

{  format of colors storage as in Total Commander:
   doublecmd.ini
     [Colors]
     ColorFilter1=*.o;*.ppu;*.rst;*.bak;*.dcu
     ColorFilter1Color=16711680
     ColorFilter2=*.pas
     ColorFilter2Color=16711000
   etc...
}

procedure TColorExt.Load;
var
  sExt,
  sExtMask : String;
  iColor,
  iPos,
  iBegin,
  I : Integer;
begin
  I := 1;
  iBegin := 1;
  lsExts.Clear;

  while gIni.ReadString('Colors', 'ColorFilter' + IntToStr(I), '') <> '' do
    begin
      sExtMask := gIni.ReadString('Colors', 'ColorFilter' + IntToStr(I), '');
      iColor := gIni.ReadInteger('Colors', 'ColorFilter' + IntToStr(I) + 'Color', clText);

      if pos(';', sExtMask) <> 0 then // if some extensions
      repeat
        begin
          iPos := pos(';', sExtMask);

          if iPos = 0 then  // if last extension
            iPos := Length(sExtMask) + 1
          else
            begin
              Delete(sExtMask, iPos, 1);
              Insert(' ', sExtMask, iPos); // change ';' to space
            end;

          sExt := Copy(sExtMask, iBegin, iPos - 1);
          sExt := ExtractFileExt(sExt);
          if sExt[1] = '.' then
            Delete(sExt,1,1);
          lsExts.AddObject(sExt,TObject(iColor));
          
          iBegin := iPos + 1;
        end
      until pos(';', sExtMask) = 0
      else  // if one extension
        begin
          sExt := ExtractFileExt(sExtMask);
          if sExt[1] = '.' then
            Delete(sExt,1,1);
          lsExts.AddObject(sExt,TObject(iColor));
          end;
      Inc(I);
    end; // while gIni.ReadString();
end;

procedure TColorExt.Save;
begin


end;

end.
