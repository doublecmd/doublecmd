Unit uFastBitmap;
(*==============================================================================
 DESCRIPTION   : Classe de manipulation basique de bitmap en 32 bit.
                 Basic Class for manipulating 32 bit Bitmap
 DATE          : 17/06/2018
 VERSION       : 1.0
 AUTEUR        : J.Delauney (BeanzMaster)
 LICENCE       : MPL
================================================================================
*)

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

Interface

Uses
  LCLType, LCLIntf, Classes, SysUtils, GraphType, Graphics, Contnrs, Dialogs,
  IntfGraphics, FPimage;

Const
  { Constantes utiles pour le calcul sur les masques de couleur }
  { Useful constants for calculation on color masks }
  {$IFDEF WINDOWS} // Format BGRA
    cBlueOrder = 0;
    cGreenOrder = 1;
    cRedOrder = 2;
    cAlphaOrder = 3;
  {$ELSE} // Format RGBA
    cRedOrder = 0;
    cGreenOrder = 1;
    cBlueOrder = 2;
    cAlphaOrder = 3;
  {$ENDIF}
  cRedShift = cRedOrder * 8;
  cGreenShift = cGreenOrder * 8;
  cBlueShift = cBlueOrder * 8;
  cAlphaShift = cAlphaOrder * 8;

  maskRed = 1;
  maskGreen = 2;
  maskBlue = 4;
  maskAlpha = 8;
  maskRGB = maskRed Or maskGreen Or maskBlue;
  maskRGBA = maskRGB Or maskAlpha;

Type
  { TColorRGB24 : Définition d'un pixel sur 24 bits au format RGB }
  { TColorRGB24 : Definition of a 24-bit pixel in RGB format }
  TColorRGB24Type = packed array[0..2] of byte;
  TColorRGB24 = packed record
    { Creation de la couleur / Create Color }
    procedure Create(R,G,B : Byte); Overload;
    procedure Create(Color:TColor); Overload;

    { Conversion vers un TColor / Convert to TColor }
    function ToColor : TColor;

    Case Integer of
     0 : (V:TColorRGB24Type);     // Acces via Tableau / Array
     1 : (Red, Green, Blue:Byte); // Acces via Composantes / Channel
  end;

  { TColor32 : Définition d'un pixel sur 32 bits au format RGBA ou BGRA suivant l'OS }
  { TColor32: Definition of a 32-bit pixel in RGBA or BGRA format depending on the OS }
  TColor32Type = packed array[0..3] of byte;
  TColor32 = Packed Record
  private
    function getColorComponent(Index : Integer): byte;
    procedure SetColorComponent(Index : Integer; aValue:Byte);
  public
    { Creation de la couleur / Create Color }
    procedure Create(R,G,B,A : Byte); Overload;
    procedure Create(R,G,B : Byte);   Overload;
    procedure Create(Color : TColor); Overload;
    procedure Create(Color : TColorRGB24); Overload;

    { Conversion vers un TColor / Convert to TColor }
    function ToColor : TColor;
    { Conversion vers un TColorRGB24 / Convert to TColorRGB24 }
    function ToColorRGB24 : TColorRGB24;
    { Conversion vers un TFPColor / Convert to TFPColor }
    function ToFPColor : TFPColor;

    { Mixage de la couleur courrante avec la couleur "Color" avec prise en charge du canal Alpha }
    { Mix current color with 'Color' color with Alpha channel support }
    function Blend(Color : TColor32): TColor32;

    { Vérifie si 2 valeurs sont identiques / Check if 2 colors are equal }
    class operator =(Color1,Color2 : TColor32):Boolean;

    { Accès aux composantes de la couleur / Color channel access }
    property Red:Byte Index cRedOrder read GetColorComponent Write SetColorComponent;
    property Green:Byte Index cGreenOrder read GetColorComponent Write SetColorComponent;
    property Blue:Byte Index cBlueOrder read GetColorComponent Write SetColorComponent;
    property Alpha:Byte Index cAlphaOrder read GetColorComponent Write SetColorComponent;

    Case Integer of
     0 : (V:TColor32Type);  // Acces via tableau / Array
     1 : (AsInteger : Integer); // Acces via Integer
  End;
  PColor32 = ^TColor32;

  { TColor32Item : Objet persistant englobant une couleur de type TColor32 }
  { TColor32Item: Persistent object that includes a TColor32 color }
  TColor32Item = Class(TPersistent)
  Private
    FColor: TColor32;
    FName:  String;
    FTag:   Integer;

    Procedure SetRed(Const AValue: Byte);
    Procedure SetGreen(Const AValue: Byte);
    Procedure SetBlue(Const AValue: Byte);
    Procedure SetAlpha(Const AValue: Byte);
    Procedure SetValue(Const AValue: TColor32);
    Procedure SetColorName(Const aName: String);

    Function getRed: Byte;
    Function getGreen: Byte;
    Function getBlue: Byte;
    Function getAlpha: Byte;
    Function getValue: TColor32;

  Protected
  Public
    Constructor Create;
    Destructor Destroy; override;

    { Valeur de la couleur / Value of the color }
    Property Value: TColor32 read getValue write setValue;
    { Nom de la couleur eg : clrRed  / Name of the color}
    Property Name: String read FName write setColorName;
  Published
    { Valeur du canal rouge / Red channel }
    Property Red: Byte read getRed write setRed;
    { Valeur du canal vert / Green channel }
    Property Green: Byte read getRed write setGreen;
    { Valeur du canal Bleu / Blue channel }
    Property Blue: Byte read getRed write setBlue;
    { Valeur du canal alpha pour la transparence / Alpha channel for transparency }
    Property Alpha: Byte read getRed write setAlpha;
    {  Valeur complémentaire personnel / User define value }
    Property Tag: Integer read FTag write FTag;
  End;

  { TColor32List : Classe pour la gestion d'une palette (liste) de couleurs }
  { TColor32List : Class for managing a palette (list) of colors }
  TColor32List = Class(TObjectList)
  Private
  Protected
    Function GetColorItem(index: Integer): TColor32Item;
    Procedure SetColorItem(index: Integer; val: TColor32Item);
  Public
    { Efface la liste / Clear the list }
    procedure Clear; override;
    { Ajoute une couleur à la liste / Add a color to the list }
    Function AddColor(Const aColor: TColor32): Integer; Overload;
    { Ajoute une couleur à la liste /Add a color to the list }
    Function AddColor(Const aName: String; Const aColor: TColor32): Integer; Overload;
    { Ajoute une couleur à la liste / Add a color to the list}
    Function AddColor(Const aColorItem: TColor32Item): Integer; Overload;
    { Supprime une couleur de la liste / Delete a color of the list }
    Procedure RemoveColor(Const aName: String);
    { Recherche une couleur dans la liste / Search color in list }
    Function FindColorByName(Const aName: String; Out Index: Integer):TColor32; Overload;
    { Recherche une couleur dans la liste  / Search color in list }
    Function FindColorByName(Const aName: String): TColor32; Overload;

    { Colors : Acceder à la couleur "Index" de la liste / Color access with Index }
    Property Colors[Index: Integer]: TColor32Item read GetColorItem write setColorItem;
  End;

Const
  clrTransparent : TColor32 = (v:($00,$00,$00,$00));
  clrBlack       : TColor32 = (v:($00,$00,$00,$FF));
  clrWhite       : TColor32 = (v:($FF,$FF,$FF,$FF));


Type
  { TFastBitmapDrawMode : Mode d'Affichage pour la fonction PutImage de TFastBitmap }
  { TFastBitmapDrawMode : Display Mode for the PutImage Function of TFastBitmap }
  TFastBitmapDrawMode = ( dmSet, dmAlpha, dmAlphaCheck);

  { TFastBitmap }
  { Classe d'aide à la manipulation d'une image }
  { Help class for image manipulation }
  TFastBitmap = Class
  Strict private
    FTransparentColor : TColor; // Couleur transparent à pour l'affichage via TBitmap de la LCL si besoin / Transparent color for display via TBitmap of the LCL if needed

    FData     : PDWord;    // Tampon de stockage des données d'un bitmap / Buffer for storing data from a bitmap
    FWidth    : Integer;   // Largeur du bitmap / Width
    FHeight   : Integer;   // Hauteur du Bitmap / Height
    FSize     : Int64;     // Taille du tampon en octet / Size in byte

  protected

    procedure SetWidth(NewWidth : Integer);
    procedure SetHeight(NewHeight : Integer);

    function BuildBitmap : Graphics.TBitmap;
    function IsClipped(X,Y:Integer) : Boolean;


  Public
    Constructor Create; Overload;
    Constructor Create(NewWidth, NewHeight : Integer); Overload;
    Destructor Destroy; Override;

    { Assigne les donnée d'un autre TFastBitmap / Assign another TFastBitmap }
    procedure Assign(aFastBitmap : TFastBitmap);
    { Modifie les dimensions du bitmap / Change size of bitmap }
    procedure SetSize(NewWidth, NewHeight : Integer);
    { Importation des données d'un TRawImage. Retourne "TRUE" en cas de succès }
    { Import from RawImage. Return TRUE on success }
    function ImportFromRawImage(Const ARawImage : TRawImage):Boolean;
    { Importation des données d'un TBitmap. Retourne "TRUE" en cas de succès }
    { Import from TBitmap. Return TRUE on success }
    function ImportFromBitmap(Const ABitmap :Graphics.TBitmap):Boolean;
    { Efface le bitmap avec la couleur "Color" / Clear bitmap with Color }
    procedure Clear(Color : TColor32);
    { Retourne le tampon du bitmap / Return bitmap buffer }
    function GetSurfaceBuffer : PColor32;
    { Retourne l'adresse de la ligne "Y" dans le tampon / Return address in buffer of a line }
    function GetScanLine(Y : Integer) : PColor32;
    { Retourne l'adresse du pixel à la position "X,Y" dans le tampon / Return address at X,Y}
    function GetPixelPtr(X, Y : Integer) : PColor32;
    { Ecrit un pixel de couleur "Color" à la position "X,Y / Put pixel X,Y with Color }
    procedure PutPixel(X,Y:Integer; Color : TColor32);
    { Lit un pixel de couleur "Color" à la position "X,Y / Get color of pixel at X,Y }
    function GetPixel(X,Y:Integer): TColor32;
    { Ecrit un pixel de en mixant couleur "Color" avec la couleur du pixel présent dans le tampon à la position "X,Y }
    { Writes a pixel by mixing 'Color' color with the color of the pixel present in the buffer at the 'X, Y' position }
    procedure PutPixelBlend(X,Y : Integer; Color : TColor32);
    { Copie une image source "Src" depuis la position "SrcX,SrcY" et de dimension "SrcWidthxSrcHeight" dans le bitmap à la position "DstX, DstY
      et suivant le "Mode"
       Mode : TFastBitmapDrawMode
        - dmSet : Copie brute de l'image
        - dmAlpha : Copie les pixel de l'image source en mixant les couleurs avec celles du bitmap en fonction de leur valeur Alpha
        - dmAlphaCheck : Copie les pixels de l'image source seulement si le pixel est visible (Alpha <> 0)
       Note : les dimensions et les positions entre le bitmap et l'image source sont automatiquement ajustées si besoin.

    --------------------------
      Copy a source image 'Src' from the position 'SrcX, SrcY' and dimension 'SrcWidthxSrcHeight' into the bitmap at the position 'DstX, DstY
      and following the 'Mode'
       Mode: TFastBitmapDrawMode
        - dmSet: Raw copy of the image
        - dmAlpha: Copy the pixels of the source image by mixing the colors with those of the bitmap according to their Alpha value
        - dmAlphaCheck: Copy the pixels of the source image only if the pixel is invisible (Alpha <> 0)
       Note: The dimensions and positions between the bitmap and the source image are automatically adjusted if necessary.
    }
    procedure PutImage(Src : TFastBitmap; SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY : Integer; Mode : TFastBitmapDrawMode);
    { Creation  d'un clone du bitmap (nouvelle instance) / Create clone (new instance) }
    function Clone : TFastBitmap;
    { Retourne un bitmap de type LCL ==> Graphics.TBitmap / Return a TBitmap}
    function GetBitmap : Graphics.TBitmap;
    { Dessine le bitmap sur un canvas à la position "X,Y" / Draw the bitmap on a canvas }
    procedure Draw(ACanvas : TCanvas; X,Y : Integer);  Overload;
    { Dessine le bitmap sur un canvas délimité par "Rect" / Draw the bitmap on a canvas delimited by "Rect" }
    procedure Draw(ACanvas : TCanvas; Rect : TRect);   Overload;
    { Inverse les composante de couleur Rouge et Bleu du bitmap  / Swap Red and Blue channel }
    procedure SwapRB;

   // procedure HLine(X,Y,X2 : Integer; aColor : TColor32);
    { Information sur la couleur assignée à la transparence (seulement valable si différent de clrTransparent) / Return the transparency color }
    property TransparentColor : TColor Read FTransparentColor Write FTransparentColor;
    { Largeur du bitmap / Width }
    property Width : Integer Read FWidth Write SetWidth;
    { Hauteur du bitmap / Height }
    property Height : Integer Read FHeight Write SetHeight;
    { Taille du tampon en octet / Size of the buffer }
    property Size : Int64 Read FSize;
  End;

Implementation

Uses Types, Math, GifViewerStrConsts;



{%region=====[ TColorRGB24 ]====================================================}

Procedure TColorRGB24.Create(R, G, B : Byte);
Begin
 Red := R;
 Green := G;
 Blue := B;
End;

Procedure TColorRGB24.Create(Color : TColor);
Var
  lr,lg,lb : Byte;
Begin
  lr := Color;
  lg := Color shr 8;
  lb := Color shr 16;
  Create(lr,lg,lb);
End;

Function TColorRGB24.ToColor : TColor;
Begin
  Result := Red + (Green shl 8) + (Blue shl 16);
End;

{%endregion%}

{%region=====[ TColor32 ]===================================================}

function TColor32.getColorComponent(Index: Integer): byte;
Begin
  result := v[Index];
End;

procedure TColor32.SetColorComponent(Index: Integer; aValue: Byte);
Begin
  v[Index] := aValue;
End;

procedure TColor32.Create(R, G, B, A: Byte);
Begin
  Red := R;
  Green := G;
  Blue := B;
  Alpha := A;
End;

procedure TColor32.Create(R, G, B: Byte);
Begin
  Create(R,G,B,255);
End;

procedure TColor32.Create(Color: TColor);
Var
  ColorRGB24 : TColorRGB24;
Begin
  {%H-}ColorRGB24.Create(Color);
  Create(ColorRGB24);
End;

procedure TColor32.Create(Color: TColorRGB24);
Begin
  Create(Color.Red,Color.Green,Color.Blue);
End;

function TColor32.ToColor: TColor;
Begin
 Result := ToColorRGB24.ToColor;
End;

function TColor32.ToColorRGB24: TColorRGB24;
Begin
 Result.Red := Red;
 Result.Green := Green;
 Result.Blue := Blue;
End;

function TColor32.ToFPColor: TFPColor;
begin
  Result.Red   := Self.Red shl 8 + Self.Red;
  Result.Green := Self.Green shl 8 + Self.Green;
  Result.Blue  := Self.Blue shl 8 + Self.Blue;
  Result.Alpha := Self.Alpha shl 8 + Self.Alpha;
end;

function TColor32.Blend(Color: TColor32): TColor32;
var
  factor, factor2:single;
begin

  if Color.Alpha = 255 then Result := Color
  else  if (Color.Alpha = 0) or (Self = Color) then Result:= Self
  else
  begin
    factor := Color.Alpha / 255;
    factor2 := 1 - Factor;
    Result.Red  := Round((Self.Red*Factor)+(Color.Red*factor2));
    Result.Green  := Round((Self.Green*Factor)+(Color.Green*Factor2));
    Result.Blue  := Round((Self.Blue*Factor)+(Color.Blue*Factor2));
    Result.alpha := Round((Self.Alpha*Factor)+(Color.Alpha*Factor2));
  End;
end;

class operator TColor32.=(Color1, Color2: TColor32): Boolean;
Begin
  Result := False;
  if (Color1.Alpha = 0) and (Color2.Alpha = 0) then Result :=True
  else Result := ((Color1.Red = Color2.Red) and (Color1.Green = Color2.Green) and (Color1.Blue = Color2.Blue) and (Color1.Alpha = Color2.Alpha))
End;

{%endregion%}

{%region=====[ TColor32Item ]===============================================}

Constructor TColor32Item.Create;
Begin
  Inherited Create;
  FName := 'Black';
  FColor.Create(0,0,0);
  FTag := 0;
End;

Destructor TColor32Item.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TColor32Item.SetRed(Const AValue: Byte);
Begin
  If AValue = FColor.red Then exit;
  FColor.Red := AValue;
End;

Procedure TColor32Item.SetGreen(Const AValue: Byte);
Begin
  If AValue = FColor.Green Then exit;
  FColor.Green := AValue;
End;

Procedure TColor32Item.SetBlue(Const AValue: Byte);
Begin
  If AValue = FColor.Blue Then exit;
  FColor.Blue := AValue;
End;

Procedure TColor32Item.SetAlpha(Const AValue: Byte);
Begin
  If AValue = FColor.Alpha Then exit;
  FColor.Alpha := AValue;
End;

Procedure TColor32Item.SetValue(Const AValue: TColor32);
Begin
  If AValue = FColor Then exit;
  FColor := AValue;
End;

Function TColor32Item.getRed: Byte;
Begin
  Result := FColor.Red;
End;

Function TColor32Item.getGreen: Byte;
Begin
  Result := FColor.Green;
End;

Function TColor32Item.getBlue: Byte;
Begin
  Result := FColor.Blue;
End;

Function TColor32Item.getAlpha: Byte;
Begin
  Result := FColor.Alpha;
End;

Function TColor32Item.getValue: TColor32;
Begin
  Result := FColor;
End;

Procedure TColor32Item.SetColorName(Const aName: String);
Begin
  If FName = aName Then exit;
  FName := aName;
End;

{%endregion%}

{%region ====[ TColor32List ]===============================================}

Function TColor32List.GetColorItem(index: Integer): TColor32Item;
Begin
  Result := TColor32Item(Get(Index));
End;

Procedure TColor32List.SetColorItem(index: Integer; val: TColor32Item);
Begin
  Put(Index, Val);
End;

procedure TColor32List.Clear;
Var
  anItem: TColor32Item;
  i : Integer;
Begin
  inherited Clear;
  If Count > 0 then
  begin
    For i :=Count -1 downto 0 do
    begin
      AnItem:= Colors[i];
      if anItem<>nil then anItem.Free;
    End;
  End;
End;

Function TColor32List.AddColor(Const aColor: TColor32): Integer;
Var
  aColorItem: TColor32Item;
Begin
  aColorItem := TColor32Item.Create;
  aColorItem.Value := aColor;
  Result := Add(aColorItem);
End;

Function TColor32List.AddColor(Const aName: String; Const aColor: TColor32): Integer;
Var
  aColorItem: TColor32Item;
Begin
  aColorItem := TColor32Item.Create;
  aColorItem.Value := aColor;
  aColorItem.Name := aName;
  Result := Add(aColorItem);
End;

Function TColor32List.AddColor(Const aColorItem: TColor32Item): Integer;
Begin
  Result := Add(aColorItem);
End;

Procedure TColor32List.RemoveColor(Const aName: String);
Var
  I:   Integer;
  Col: TColor32Item;
Begin
  FindColorByName(aName, I);
  If I > -1 Then
  Begin
    Col := GetColorItem(I);
    If Assigned(Col) Then
      Col.Free;
    Delete(I);
  End;
End;

Function TColor32List.FindColorByName(Const aName: String; Out Index: Integer): TColor32;
Var
  i: Integer;
Begin
  Result := clrTransparent;
  Index := -1;
  For i := 0 To Count - 1 Do
    If TColor32Item(Items[i]).Name = aName Then
    Begin
      Index := I;
      Result := TColor32Item(Items[i]).Value;
      break;
    End;
End;

Function TColor32List.FindColorByName(Const aName: String): TColor32;
Var
  i: Integer;
Begin
  Result := FindColorByName(aName, I);
End;

{%endregion%}

{%region=====[ TFastBitmap ]====================================================}

Constructor TFastBitmap.Create(NewWidth, NewHeight : Integer);
Begin
 inherited Create;
  FWidth  := Max(1,NewWidth);
  FHeight := Max(1,NewHeight);
  FData := Nil;
  FSize := (int64(FWidth) * int64(FHeight))*4;
  ReAllocMem(FData,FSize);
  FTransparentColor := clBlack;
End;

Constructor TFastBitmap.Create;
Begin
  Create(1,1);
End;

Destructor TFastBitmap.Destroy;
Begin
  FreeMem(FData);
  FData := Nil;
  inherited Destroy;
End;

Procedure TFastBitmap.SetWidth(NewWidth : Integer);
Begin
  if NewWidth = FWidth then Exit;
  SetSize(NewWidth, FHeight);
End;

Procedure TFastBitmap.SetHeight(NewHeight : Integer);
Begin
  if NewHeight = FHeight then Exit;
  SetSize(FWidth, NewHeight);
End;

Function TFastBitmap.BuildBitmap: Graphics.TBitmap;
Var
  Temp : Graphics.TBitmap;
  IntfBmp : TLazIntfImage;
  ImgFormatDescription: TRawImageDescription;
  W,H,X,Y : Integer;
  SrcPix : PColor32;
Begin

  (* /!\ Le code si dessous fonctionne parfaitement sous Windows et Mac.
     Mais sous Linux ce code produit des erreur au niveau de la transparence

    BmpHandle := 0;
    MskHandle := 0;
    W := FWidth;
    H := FHeight;
    Buffer := PByte(GetSurfaceBuffer);

    RawImage.Init;
    {$IFDEF WINDOWS}
    RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(W,H);
    {$ELSE}
    RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(W,H);
    {$ENDIF}

    RawImage.Data := Buffer;
    RawImage.DataSize := FSize;

    if not RawImage_CreateBitmaps(RawImage, BmpHandle, MskHandle,False) then
      Raise Exception.Create('Impossible de créer le TBitmap')
    else
    begin
      Temp := Graphics.TBitmap.Create;
      Temp.Width := W;
      Temp.Height := H;
      Temp.PixelFormat := pf32bit;
      Temp.Handle := BmpHandle;
      Temp.MaskHandle := MskHandle;
      Temp.Transparent := True;
      //Temp.TransparentColor := FTransparentColor;
      //temp.TransparentMode := tmAuto;
      Result := Temp;
    End;
  *)

  Result := nil;

  W := FWidth;
  H := FHeight;

  // Pour que la transparence soit gérée correctement sous Linux on est obligé de passer par TLazIntfImage
  IntfBmp := TLazIntfImage.Create(W,H);
  ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(W, H);
  IntfBmp.DataDescription := ImgFormatDescription;

  SrcPix := Self.GetSurfaceBuffer;
  For Y:=0 to H-1 do
    For X:=0 to W-1 do
    begin
      IntfBmp.Colors[x, y]:=SrcPix^.ToFPColor;
      inc(SrcPix);
    end;

  begin
    Temp := Graphics.TBitmap.Create;
    Temp.LoadFromIntfImage(IntfBmp);
    Result := Temp;
    IntfBmp.Free;
  End;
  if Result = nil then
    Raise Exception.Create(rsBitmapCreateError);
End;

Function TFastBitmap.IsClipped(X, Y : Integer) : Boolean;
Begin
  Result := ((X>=0) and (Y>=0) and (X<FWidth) and (Y<FHeight));
End;

Procedure TFastBitmap.SwapRB;
var
  Pixptr: PColor32;
  AIntColor : Cardinal;
  PixelCount : Integer;
begin
  PixPtr := GetSurfaceBuffer;
  PixelCount := (FWidth * FHeight)-1;
  while pixelCount > 0 do
  begin
    AIntColor := PixPtr^.AsInteger;
    PixPtr^.AsInteger := AIntColor and $FF00FF00 or (AintColor and $000000FF SHL 16) or (AIntColor and $00FF0000 SHR 16);
    Inc(PixPtr);
    Dec(pixelCount);
  end;
end;

Procedure TFastBitmap.Assign(aFastBitmap : TFastBitmap);
Begin
  SetSize(aFastBitMap.Width, aFastBitmap.Height);
  Move(PByte(aFastBitmap.GetSurfaceBuffer)^, PByte(FData)^, FSize);
End;

Procedure TFastBitmap.SetSize(NewWidth, NewHeight : Integer);
Begin
  FWidth  := Max(1,NewWidth);
  FHeight := Max(1,NewHeight);
  FSize :=(int64(FWidth) * int64(FHeight))*4;
  if (FData<>nil) then
  begin
    FreeMem(FData);
    FData := Nil;
  End;
  ReAllocMem(FData,FSize);
  Clear(clrTransparent);
End;

Function TFastBitmap.ImportFromRawImage(Const ARawImage: TRawImage): Boolean;
var
  BufferData : PByte;
begin
  SetSize(ARawImage.Description.Width,ARawImage.Description.Height);
  result:=false;
  // On verifie si la taille des deux tampons sont identique
  // Si ce n'est pas le cas, cela veut dire que le TRawImage n'est pas au format 32bit
  if (ARawImage.DataSize= FSize) then
  begin
    try
      BufferData := PByte(Self.getSurfaceBuffer);
      Move(ARawImage.Data^, BufferData^, self.Size);
      {$IFDEF WINDOWS}
        if (ARawImage.Description.RedShift = 0) and ((ARawImage.Description.BlueShift = 16)) then Self.SwapRB; // Le RawImage est-il en RGB, si oui on échange
      {$ELSE}
        if (ARawImage.Description.RedShift = 16) and ((ARawImage.Description.BlueShift = 0)) then Self.SwapRB; // Le RawImage est-il en BGR, si oui on échange
      {$ENDIF}
    finally
      result:=true;
    end;
  end;
End;

Function TFastBitmap.ImportFromBitmap(Const ABitmap: Graphics.TBitmap): Boolean;
var
  LTempBitmap: Graphics.TBitmap;
  ok,ResetAlpha:Boolean;

  procedure SetAlpha(Value : Byte);
  var
    i : Integer;
    PixPtr : PColor32;
    maxi : Integer;
  begin
    i:=0;
    Maxi := (FWidth * FHeight)-1;
    PixPtr :=PColor32(FData);// Self.GetScanLine(0);
    While i<Maxi do
    begin
      PixPtr^.Alpha:= Value;
      inc(PixPtr);
      inc(i);
    end;
  end;

begin
  ResetAlpha:=False;
  result:=false;
  if (ABitmap.PixelFormat <> pf32bit)  then
  begin
    LTempBitmap := Graphics.TBitmap.Create;
    try
      ResetAlpha:=True;
      LTempBitmap.SetSize(ABitmap.Width, ABitmap.Height);
      LTempBitmap.PixelFormat := pf32bit;
      LTempBitmap.Canvas.Draw(0, 0, ABitmap);
    finally
      ok:=Self.ImportFromRawImage(LTempBitmap.RawImage);
      if ResetAlpha then SetAlpha(255);
      FreeAndNil(LTempBitmap);
      result:=true and (ok);
    end;
  end
  else
  begin
   ok:=Self.ImportFromRawImage(ABitmap.RawImage);
   result:=true and (ok);
  end;
End;

Procedure TFastBitmap.Clear(Color : TColor32);
Begin
  FillDWord(FData^,FWidth * FHeight, DWord(Color));
End;

Function TFastBitmap.GetSurfaceBuffer: PColor32;
Begin
   Result := PColor32(FData);
End;

Function TFastBitmap.GetScanLine(Y : Integer) : PColor32;
Var
  yy : DWord;
Begin
  If (Y<0) or (Y>=FHeight) then
    Raise Exception.Create(rsBitmapScanlineOutOfRange)
  else
  begin
    yy := DWord(FWidth) * DWord(Y);
    Result := PColor32(FData + YY);
  End;
End;

Function TFastBitmap.GetPixelPtr(X, Y : Integer) : PColor32;
Begin
  Result := nil;
  if IsClipped(X,Y) then
  Begin
    Result := PColor32(FData + (FWidth * Y) + X);
  End;
End;

Procedure TFastBitmap.PutPixel(X, Y : Integer; Color : TColor32);
Var
  PixelPtr : PColor32;
Begin
  if IsClipped(X,Y) then
  Begin
    PixelPtr := PColor32(FData + DWord(FWidth * Y));
    Inc(PixelPtr,X);
    PixelPtr^:= Color;
  End;
End;

Function TFastBitmap.GetPixel(X, Y : Integer) : TColor32;
Var
  PixelPtr : PColor32;
Begin
  Result := clrTransparent;
  if IsClipped(X,Y) then
  Begin
    PixelPtr := PColor32(FData + (FWidth * Y) + X);
    Result := PixelPtr^;
  End;
End;

Procedure TFastBitmap.PutPixelBlend(X, Y : Integer; Color : TColor32);
Var
  PixelPtr : PColor32;
Begin
  if IsClipped(X,Y) then
  Begin
    PixelPtr := PColor32(FData + (FWidth * Y) + X);
    PixelPtr^:= PixelPtr^.Blend(Color);
  End;
End;

Procedure TFastBitmap.PutImage(Src : TFastBitmap; SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY : Integer; Mode : TFastBitmapDrawMode);
Var
  SrcPtr, DstPtr : PColor32;
  NextSrcLine, NextDstLine : Integer;
  DstCol, SrcCol : TColor32;
  LineSize,TotalSize,xx,yy,i : Integer;

  Procedure ClipCopyRect(Var SrcX, SrcY, rWidth, rHeight, DstX, DstY: Integer; SrcImageWidth, SrcImageHeight: Integer; Const DstClip: Types.TRect);
    Var
      diff, OldDstPosX, OldDstPosY: Integer;
    Begin
      OldDstPosX := 0;
      If (DstX < 0) Then OldDstPosX := DstX;
      OldDstPosY := 0;
      If (DstY < 0) Then OldDstPosY := DstY;

      If DstX < DstClip.Left Then
      Begin
        Diff := DstClip.Left - DstX;
        rWidth := rWidth - Diff;
        SrcX := SrcX + Diff;
        DstX := DstClip.Left;
      End;

      If DstY < DstClip.Top Then
      Begin
        Diff := DstClip.Top - DstY;
        rHeight := rHeight - Diff;
        SrcY := SrcY + Diff;
        DstY := DstClip.Bottom;
      End;

      If SrcX < 0 Then
      Begin
        Width := Width + SrcX - OldDstPosX;
        DstX := DstX - SrcX + OldDstPosX;
        SrcX := 0;
      End;
      If SrcY < 0 Then
      Begin
        rHeight := rHeight + SrcX - OldDstPosY;
        DstY := DstY - SrcY + OldDstPosY;
        SrcY := 0;
      End;

      If ((SrcX + rWidth) > SrcImageWidth) Then rWidth := SrcImageWidth - SrcX;
      If ((SrcY + rHeight) > SrcImageHeight) Then rHeight := SrcImageHeight - SrcY;

      if DstX > FWidth then DstX := 0;
      if DstY > FHeight then DstY := 0;

      If ((DstX + rWidth) >  (DstClip.Right+1)) Then rWidth := DstClip.Right - DstX;
      If ((DstY + rHeight) > (DstClip.Bottom+1)) Then rHeight := DstClip.Bottom - DstY;

    End;
Begin

  if (SrcWidth = 0) and (SrcHeight = 0) then exit;
  ClipCopyRect(SrcX, SrcY, SrcWidth,SrcHeight, DstX, DstY, Src.Width, Src.Height, Types.Rect(0,0,FWidth-1, FHeight-1));


  if (SrcWidth = 1) and (SrcHeight = 1) then
  begin
    Case Mode of
      dmSet :
        begin
          SrcCol := Src.GetPixel(0,0);
          PutPixel(0,0,SrcCol);
        End;
      dmAlpha :
      begin
        SrcCol := Src.GetPixel(0,0);
        DstCol := GetPixel(0,0);
        PutPixel(0,0,DstCol.Blend(SrcCol));
      End;
      dmAlphaCheck :
        begin
          If SrcCol.Alpha > 0 Then
          begin
            SrcCol := Src.GetPixel(0,0);
            DstCol := GetPixel(0,0);
            PutPixel(0,0,DstCol.Blend(SrcCol));
          End
          Else
          begin
            DstCol := GetPixel(0,0);
            PutPixel(0,0,DstCol);
          End;
        End;
    End;
    exit;
  End;

  SrcPtr := Src.GetPixelPtr(SrcX,SrcY);
  DstPtr := GetPixelPtr(DstX, DstY);

  if SrcWidth <= Src.Width then
    nextSrcLine := Src.Width
  else
    nextSrcLine := SrcX + (Src.Width - (SrcX + SrcWidth));
  if Mode = dmSet then
  begin
    if (((Src.Width = FWidth) and (Src.Height = FHeight)) and ((SrcWidth = FWidth) and (SrcHeight = FHeight))) then
      Move(SrcPtr^,DstPtr^,DWord(Src.Size))
    else
    begin
      LineSize := SrcWidth * 4;
      For I := 0 to SrcHeight-1 do
      begin
        Move(SrcPtr^, DstPtr^, LineSize);
        Inc(SrcPtr, NextSrcLine);
        Inc(DstPtr, FWidth);
      End;
    End;
  End
  else
  begin
    totalsize := (Src.Width * Src.Height) - 1;
    Dec(SrcHeight);
    xx := 0;
    Dec(SrcWidth);
    nextSrcLine := SrcX + (Src.Width - (SrcX + SrcWidth));
    nextDstLine := DstX + (FWidth - (DstX + SrcWidth));
    yy := 0;
    xx := 0;
    SrcCol := clrTransparent;
    DstCol := clrTransparent;
    While (yy <= TotalSize) Do
    Begin
      DstCol := DstPtr^;
      SrcCol := SrcPtr^;
      Case Mode of
        dmAlpha :
        begin
          DstPtr^ := DstCol.Blend(SrcCol);
        End;
        dmAlphaCheck :
        begin
          If SrcCol.Alpha > 0 Then
            DstPtr^ := DstCol.Blend(SrcCol)
          Else
            DstPtr^ := DstCol;
        End;
      End;
      Inc(xx);
      Inc(yy);
      If (xx > SrcWidth) Then
      Begin
        xx := 0;
        Inc(DstPtr, NextDstLine);
        Inc(SrcPtr, NextSrcLine);
      End
      Else
      Begin
         Inc(SrcPtr);
         Inc(DstPtr);
      End;
    End;
  End;
End;

Function TFastBitmap.Clone : TFastBitmap;
Var
  NewBmp : TFastBitmap;
Begin
 NewBmp := TFastBitmap.Create;
 NewBmp.Assign(Self);
 Result := NewBmp;
End;

Function TFastBitmap.GetBitmap : Graphics.TBitmap;
Begin
  Result := BuildBitmap;
End;

Procedure TFastBitmap.Draw(ACanvas : TCanvas; X, Y : Integer);
Var
  Tmp : Graphics.TBitmap;
Begin
  Tmp :=  BuildBitmap;
  ACanvas.Draw(X,Y,Tmp);
  FreeAndNil(Tmp);
End;

Procedure TFastBitmap.Draw(ACanvas : TCanvas; Rect : TRect);
Var
  Tmp : Graphics.TBitmap;
Begin
  Tmp :=  BuildBitmap;
  ACanvas.StretchDraw(Rect, Tmp);
  FreeAndNil(Tmp);
End;

{%endregion%}
End.


