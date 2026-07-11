unit uGtk3WSMenus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Graphics, LCLType, Gtk3WSMenus, Gtk3Widgets,
  LazGtk3, LazCairo1, LazGlib2;

type

  { TGtk3MenuItemEx }

  TGtk3MenuItemEx = class(TGtk3MenuItem)
  strict private
    class function MenuItemDraw(AWidget: PGtkWidget; ACairo: Pcairo_t; AData: GPointer): gboolean; cdecl; static;
  protected
    function CreateWidget(const Params: TCreateParams): PGtkWidget; override;
  end;

  { TGtk3WSMenuItemEx }

  TGtk3WSMenuItemEx = class(TGtk3WSMenuItem)
  published
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
  end;

implementation

uses
  Forms, ImgList, WSMenus, WSLCLClasses, Gtk3Int, Gtk3Objects,
  Gtk3Procs, LazGObject2, LazGdk3, LazGdkPixbuf2;

function gtk_image_menu_item_get_image(image_menu_item: PGtkImageMenuItem): PGtkWidget; external;

{ TGtk3MenuItemEx }

class function TGtk3MenuItemEx.MenuItemDraw(AWidget: PGtkWidget; ACairo: Pcairo_t;
  AData: GPointer): gboolean; cdecl;
var
  ARect: TRect;
  AFactor: Double;
  ACanvas: TCanvas;
  AForm: TCustomForm;
  AParentMenu: TMenu;
  AMenuItem: TMenuItem;
  GtkItem: TGtk3MenuItem;
  AAlloc: TGtkAllocation;
  ADC: TGtk3DeviceContext;
  AState: TOwnerDrawState;
  ImageWidth, APPI: Integer;
  ImageList: TCustomImageList;
  Res: TScaledImageListResolution;
begin
  Result := False;

  AMenuItem := TMenuItem(AData);
  if Assigned(AMenuItem.OnDrawItem) then
    Exit;

  AParentMenu := AMenuItem.GetParentMenu;
  if Assigned(AParentMenu) and Assigned(AParentMenu.OnDrawItem) then
    Exit;

  AWidget^.get_allocation(@AAlloc);
  ARect := Rect(0, 0, AAlloc.width, AAlloc.height);

  ADC := TGtk3DeviceContext.CreateFromCairo(AWidget, ACairo);
  ACanvas := TCanvas.Create;
  ACanvas.Handle := HDC(ADC);
  try
    ImageWidth := 0;
    ImageList := nil;
    AMenuItem.GetImageList(ImageList, ImageWidth);
    if ImageWidth <= 0 then
    begin
      ImageWidth := ImageList.Width;
    end;
    APPI := 96;
    AFactor := 1;
    AForm := nil;
    if Assigned(AParentMenu) then
    begin
      if AParentMenu.Owner is TCustomForm then
        AForm := TCustomForm(AParentMenu.Owner)
      else if AParentMenu.Parent is TCustomForm then
        AForm := TCustomForm(AParentMenu.Parent);
    end;
    if (AForm = nil) then
    begin
      AForm := Screen.ActiveCustomForm;
    end;
    if Assigned(AForm) then
    begin
      APPI := AForm.PixelsPerInch;
      AFactor := AForm.GetCanvasScaleFactor;
    end;

    Res := ImageList.ResolutionForPPI[ImageWidth, APPI, AFactor];

    if Assigned(Res.Resolution) then
    begin
      Res.Draw(ACanvas, 0, 0, AMenuItem.ImageIndex, AMenuItem.Enabled);
      Result := True;
    end;
  finally
    ACanvas.Handle := 0;
    ACanvas.Free;
    ADC.Free;
  end;
end;

function TGtk3MenuItemEx.CreateWidget(const Params: TCreateParams): PGtkWidget;
var
  AMenuIcon: PGtkImage;
  AGtkImage: PGtkWidget;
  ImgPixbuf: PGdkPixbuf;
  ImageWidth, APPI: Integer;
  ImageList: TCustomImageList;
  AGtkMenuItem: PGtkImageMenuItem;
begin
  if (MenuItem.HasIcon) then
  begin
    ImageWidth:= 0;
    ImageList:= nil;
    MenuItem.GetImageList(ImageList, ImageWidth);
    if (ImageList <> nil) and (MenuItem.ImageIndex >= 0)
       and (MenuItem.ImageIndex < ImageList.Count) then
    begin
      if ImageWidth <= 0 then
      begin
        ImageWidth := ImageList.Width;
      end;
      ImgPixbuf := gdk_pixbuf_new(GDK_COLORSPACE_RGB, True, 8, ImageWidth, ImageWidth);
      AMenuIcon := TGtkImage.new_from_pixbuf(ImgPixbuf);
      g_object_unref(PGObject(ImgPixbuf));

      AGtkMenuItem := gtk_image_menu_item_new();
      AGtkMenuItem^.set_image(AMenuIcon);
      AGtkMenuItem^.set_always_show_image(true);
      AGtkImage:= gtk_image_menu_item_get_image(AGtkMenuItem);
      g_signal_connect_data(AGtkImage, 'draw', TGCallback(@MenuItemDraw), MenuItem, nil, G_CONNECT_DEFAULT);

      if (MenuItem.Caption <> cLineCaption) then
      begin
        PGtkMenuItem(AGtkMenuItem)^.use_underline := True;
        PGtkMenuItem(AGtkMenuItem)^.set_label(Pgchar(ReplaceAmpersandsWithUnderscores(MenuItem.Caption)));
        PGtkMenuItem(AGtkMenuItem)^.set_sensitive(MenuItem.Enabled);
      end;

      Exit(AGtkMenuItem)
    end;
  end;
  Result:= inherited CreateWidget(Params);
end;

{ TGtk3WSMenuItemEx }

class function TGtk3WSMenuItemEx.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := HMENU(TGtk3MenuItemEx.Create(AMenuItem));

  if AMenuItem.Visible then
  begin
    TGtk3MenuItemEx(Result).show;
  end;
end;

initialization
  WSMenus.RegisterMenuItem;
  RegisterWSComponent(TMenuItem, TGtk3WSMenuItemEx);

end.

