unit uQtWSMenus;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Menus, QtWSMenus;

type

  { TQtWSMenuItemEx }

  TQtWSMenuItemEx = class(TQtWSMenuItem)
  published
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
  end;

implementation

uses
  Classes, Graphics, ImgList, WSMenus, WSLCLClasses, qt6, qtwidgets, qtobjects;

type
  TComponentEx = class(TComponent);

{ TQtWSMenuItemEx }

class function TQtWSMenuItemEx.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  Bmp: TBitmap;
  AIcon: QIconH;
  APixmap: QPixmapH;
  ImgList: TCustomImageList;
  GlyphShowMode: TGlyphShowMode;
  Menu: TQtWidget absolute Result;
  AResolution: TCustomImageListResolution;
begin
  ImgList:= AMenuItem.GetImageList;

  if (ImgList = nil) or AMenuItem.IsLine then
  begin
    Result:= inherited CreateHandle(AMenuItem);
    Exit;
  end;

  GlyphShowMode:= AMenuItem.GlyphShowMode;
{$PUSH}{$WARNINGS OFF}
  TComponentEx(AMenuItem).Loading;
{$POP}
  AMenuItem.GlyphShowMode:= gsmNever;

  Result:= inherited CreateHandle(AMenuItem);

  AMenuItem.GlyphShowMode:= GlyphShowMode;
{$PUSH}{$WARNINGS OFF}
  TComponentEx(AMenuItem).Loaded;
{$POP}

  if not (Menu is TQtMenu) then Exit;

  if (ImgList <> nil) and (AMenuItem.ImageIndex >= 0) and
     (AMenuItem.ImageIndex < ImgList.Count) then
  begin
    Bmp:= TBitmap.Create;
    AIcon:= QIcon_create();
    APixmap:= QPixmap_create();
    try
      for AResolution in ImgList.Resolutions do
      begin
        AResolution.GetBitmap(AMenuItem.ImageIndex, Bmp);
        QPixmap_fromImage(APixmap, TQtImage(Bmp.Handle).Handle);
        QIcon_addPixmap(AIcon, APixmap, QIconNormal, QIconOn);
      end;
      TQtMenu(Menu).setIcon(AIcon);
    finally
      Bmp.Free;
      QIcon_destroy(AIcon);
      QPixmap_destroy(APixmap);
    end;
  end;
end;

initialization
  WSMenus.RegisterMenuItem;
  RegisterWSComponent(TMenuItem, TQtWSMenuItemEx);

end.

