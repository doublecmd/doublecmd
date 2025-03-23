unit Img32.Fmt.SVG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  SVG file format extension for TImage32                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IF NOT DEFINED(NEWPOSFUNC) OR DEFINED(FPC)} StrUtils, {$IFEND}
  {$IFDEF UNICODE} AnsiStrings, {$ENDIF}
  SysUtils, Classes, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults, {$ENDIF}
  Img32, Img32.Vector, Img32.SVG.Core, Img32.SVG.Reader
  {$IF DEFINED(USING_LCL)}, Types{$IFEND}
  ;

type
  TImageFormat_SVG = class(TImageFormat)
  public
    class function IsValidImageStream(stream: TStream): Boolean; override;
    function LoadFromStream(stream: TStream;
      img32: TImage32; imgIndex: integer = 0): Boolean; override;
    // SaveToStream: not implemented for SVG streams
    procedure SaveToStream(stream: TStream;
      img32: TImage32; quality: integer = 0); override;
    class function CanCopyToClipboard: Boolean; override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
  end;

  TSvgListObject = class
    xml           : string;
    name          : string;
  end;

  TSvgImageList32 = class(TInterfacedObj, INotifySender)
  private
    fReader     : TSvgReader;
{$IFDEF XPLAT_GENERICS}
    fList       : TList<TSvgListObject>;
{$ELSE}
    fList       : TList;
{$ENDIF}
    fDefWidth       : integer;
    fDefHeight      : integer;
    fRecipientList  : TRecipients;
    fUpdateCnt      : integer;
{$IFDEF MSWINDOWS}
    fResName        : string;
    procedure SetResName(const resName: string);
{$ENDIF}
    procedure SetDefWidth(value: integer);
    procedure SetDefHeight(value: integer);
  protected
    procedure Changed; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NotifyRecipients(notifyFlag: TImg32Notification);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function Find(const aName: string): integer;
    procedure AddRecipient(recipient: INotifyRecipient);
    procedure DeleteRecipient(recipient: INotifyRecipient);
    function CreateImage(index: integer): TImage32;
    procedure GetImage(index: integer; image: TImage32); overload;
    procedure GetImage(index: integer; image: TImage32; out aName: string); overload;
    procedure Add(const aName, xml: string);
    procedure AddFromFile(const aName, filename: string);
    procedure AddFromResource(const aName, resName: string; resType: PChar);
    procedure Insert(index: integer; const name, xml: string);
    procedure Move(currentIndex, newIndex: integer);
    procedure Delete(index: integer);
    property DefaultWidth: integer read fDefWidth write SetDefWidth;
    property DefaultHeight: integer read fDefHeight write SetDefHeight;
{$IFDEF MSWINDOWS}
    property ResourceName: string read fResName write SetResName;
{$ENDIF}
  end;

implementation

//------------------------------------------------------------------------------
// Three routines used to enumerate a resource type
//------------------------------------------------------------------------------

function Is_IntResource(lpszType: PChar): Boolean;
begin
  Result := NativeUInt(lpszType) shr 16 = 0;
end;
//------------------------------------------------------------------------------

function ResourceNameToString(lpszName: PChar): string;
begin
  if Is_IntResource(lpszName) then
    Result := '#' + IntToStr(NativeUInt(lpszName)) else
    Result := lpszName;
end;
//------------------------------------------------------------------------------

function EnumResNameProc(hModule: HMODULE; lpszType, lpszName: PChar;
  lParam: NativeInt): Boolean; stdcall;
var
  n: string;
begin
  n:= ResourceNameToString(lpszName);
  TSvgImageList32(lParam).AddFromResource(n, n, lpszType);
  Result := true;
end;

//------------------------------------------------------------------------------
// TSvgImageList32
//------------------------------------------------------------------------------

constructor TSvgImageList32.Create;
begin
  fReader := TSvgReader.Create;
{$IFDEF XPLAT_GENERICS}
  fList := TList<TSvgListObject>.Create;
{$ELSE}
  fList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TSvgImageList32.Destroy;
begin
  NotifyRecipients(inDestroy);
  Clear;
  fList.Free;
  fReader.Free;
  inherited;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
procedure TSvgImageList32.SetResName(const resName: string);
begin
  if fResName = resName then Exit;
  fResName := resName;
  BeginUpdate;
  try
    Clear;
    EnumResourceNames(HInstance, PChar(resName), @EnumResNameProc, lParam(self));
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TSvgImageList32.Count: integer;
begin
  result := fList.Count;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
    TSvgListObject(fList[i]).Free;
  fList.Clear;
  Changed;
end;
//------------------------------------------------------------------------------

function TSvgImageList32.Find(const aName: string): integer;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
    with TSvgListObject(fList[i]) do
      if SameText(name, aName) then
      begin
        Result := i;
        Exit;
      end;
  Result := -1;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.GetImage(index: integer; image: TImage32; out aName: string);
begin
  if not Assigned(image) or (index < 0) or (index >= count) then Exit;
  if image.IsEmpty then
   image.SetSize(fDefWidth, fDefHeight);
  with TSvgListObject(fList[index]) do
  begin
    fReader.LoadFromString(xml);
    aName := name;
  end;
  fReader.DrawImage(image, true);
end;
//------------------------------------------------------------------------------

function TSvgImageList32.CreateImage(index: integer): TImage32;
begin
  Result := TImage32.Create(DefaultWidth, DefaultHeight);
  GetImage(index, Result);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.GetImage(index: integer; image: TImage32);
var
  dummy: string;
begin
  GetImage(index, image, dummy);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Add(const aName, xml: string);
begin
  Insert(count, aName, xml);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.AddFromFile(const aName, filename: string);
begin
  if not FileExists(filename) then Exit;
  with TStringList.Create do
  try
    LoadFromFile(filename);
    Self.Insert(Self.Count, aName, Text);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.AddFromResource(const aName, resName: string; resType: PChar);
var
  rs: TResourceStream;
  ansi: AnsiString;
begin
  rs := TResourceStream.Create(hInstance, resName, resType);
  try
    SetLength(ansi, rs.Size);
    rs.Read(ansi[1], rs.Size);
    Self.Insert(Self.Count, aName, string(ansi));
  finally
    rs.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Insert(index: integer; const name, xml: string);
var
  lo: TSvgListObject;
begin
  if index < 0 then index := 0
  else if index > Count then index := Count;

  lo := TSvgListObject.Create;
  lo.name := name;
  lo.xml := xml;
  fList.Insert(index, lo);
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Move(currentIndex, newIndex: integer);
begin
  fList.Move(currentIndex, newIndex);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Delete(index: integer);
begin
  TSvgListObject(fList[index]).Free;
  fList.Delete(index);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.BeginUpdate;
begin
  inc(fUpdateCnt);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.EndUpdate;
begin
  dec(fUpdateCnt);
  if fUpdateCnt = 0 then Changed;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.Changed;
begin
  if (fUpdateCnt = 0) then
    NotifyRecipients(inStateChange);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.SetDefWidth(value: integer);
begin
  if fDefWidth = value then Exit;
  fDefWidth := value;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.SetDefHeight(value: integer);
begin
  if fDefHeight = value then Exit;
  fDefHeight := value;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.AddRecipient(recipient: INotifyRecipient);
var
  len: integer;
begin
  len := Length(fRecipientList);
  SetLength(fRecipientList, len+1);
  fRecipientList[len] := Recipient;
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.DeleteRecipient(recipient: INotifyRecipient);
var
  i, highI: integer;
begin
  highI := High(fRecipientList);
  i := highI;
  while (i >= 0) and (fRecipientList[i] <> Recipient) do dec(i);
  if i < 0 then Exit;
  if i < highI then
    System.Move(fRecipientList[i+1], fRecipientList[i],
      (highI - i) * SizeOf(INotifyRecipient));
  SetLength(fRecipientList, highI);
end;
//------------------------------------------------------------------------------

procedure TSvgImageList32.NotifyRecipients(notifyFlag: TImg32Notification);
var
  i: integer;
begin
  if fUpdateCnt > 0 then Exit;
  for i := High(fRecipientList) downto 0 do
    try
      //when destroying in a finalization section
      //it's possible for recipients to have been destroyed
      //without their destructors being called.
      fRecipientList[i].ReceiveNotification(self, notifyFlag);
    except
    end;
end;

//------------------------------------------------------------------------------
// Loading (reading) SVG images from file ...
//------------------------------------------------------------------------------

function TImageFormat_SVG.LoadFromStream(stream: TStream;
  img32: TImage32; imgIndex: integer = 0): Boolean;
var
  r: TRectWH;
  sx: double;
begin
  with TSvgReader.Create do
  try
    Result := LoadFromStream(stream);
    if not Result then Exit;

    r := RootElement.viewboxWH;
    img32.BeginUpdate;
    try
      if img32.IsEmpty then
      begin
        with RootElement do
          if Width.IsValid and Height.IsValid then
            img32.SetSize(
              Round(Width.GetValue(defaultSvgWidth, 0)),
              Round(Height.GetValue(defaultSvgHeight, 0)))
          else if not r.IsEmpty then
            img32.SetSize(Round(r.Width), Round(r.Height))
          else
            img32.SetSize(defaultSvgWidth, defaultSvgHeight);
      end
      else if not r.IsEmpty then
      begin
        // scale the SVG to best fit the image dimensions
        sx := GetScaleForBestFit(r.Width, r.Height, img32.Width, img32.Height);
        img32.SetSize(Round(r.Width * sx), Round(r.Height * sx));
      end;

      //draw the SVG image to fit inside the canvas
      DrawImage(img32, True);
    finally
      img32.EndUpdate;
    end;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) SVG images to file (not currently implemented) ...
//------------------------------------------------------------------------------

class function TImageFormat_SVG.IsValidImageStream(stream: TStream): Boolean;
var
  i, savedPos, len: integer;
  buff: array [1..1024] of AnsiChar;
begin
  Result := false;
  savedPos := stream.Position;
  len := Min(1024, stream.Size - savedPos);
  stream.Read(buff[1], len);
  stream.Position := savedPos;
  for i := 1 to len -4 do
  begin
    if buff[i] < #9 then Exit
    else if (buff[i] = '<') and
      (buff[i +1] = 's') and
      (buff[i +2] = 'v') and
      (buff[i +3] = 'g') then
    begin
      Result := true;
      break;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImageFormat_SVG.SaveToStream(stream: TStream;
  img32: TImage32; quality: integer);
begin
  //not enabled
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CanCopyToClipboard: Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CopyToClipboard(img32: TImage32): Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.CanPasteFromClipboard: Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_SVG.PasteFromClipboard(img32: TImage32): Boolean;
begin
  Result := false;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('SVG', TImageFormat_SVG, cpLow);

end.
