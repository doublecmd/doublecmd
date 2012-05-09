{
   Double Commander Components
   -------------------------------------------------------------------------
   Extended ComboBox classes

   Copyright (C) 2012  Przemyslaw Nagay (cobines@gmail.com)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

unit KASComboBox;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TComboBoxWithDelItems }

  {en
     Combo box that allows removing items with Shift+Delete.
  }
  TComboBoxWithDelItems = class(TComboBox)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

procedure Register;

implementation

uses
  LCLType;

procedure Register;
begin
  RegisterComponents('KASComponents',[TComboBoxWithDelItems]);
end;

{ TComboBoxWithDelItems }

procedure TComboBoxWithDelItems.KeyDown(var Key: Word; Shift: TShiftState);
var
  Index: Integer;
begin
  if DroppedDown and (Key = VK_DELETE) and (Shift = [ssShift]) then
  begin
    Index := ItemIndex;
    if (Index >= 0) and (Index < Items.Count) then
    begin
      Items.Delete(Index);
      ItemIndex := Index;
      Key := 0;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

end.
