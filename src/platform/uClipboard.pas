unit uClipboard;

{$mode objfpc}{$H+}
{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

{$IF DEFINED(UNIX) and not DEFINED(DARWIN)}
  {$Define UNIX_not_DARWIN}
{$ENDIF}


interface

uses
  Classes, SysUtils, LCLType;

  type TClipboardOperation = ( ClipboardCopy, ClipboardCut );

  function CopyToClipboard(const filenames:TStringList):Boolean;
  function CutToClipboard(const filenames:TStringList):Boolean;
  function PasteFromClipboard(out ClipboardOp: TClipboardOperation;
                              out filenames:TStringList):Boolean;

  function URIDecode(encodedUri: String): String;
  function URIEncode(path: String): String;

{$IF DEFINED(UNIX_not_DARWIN)}
  function ExtractFilenames(uriList: String): TStringList;
  function FileNameToURI(const FileName: String): String;

  function FormatUriList(FileNames: TStringList): String;
  function FormatTextPlain(FileNames: TStringList): String;
{$ENDIF}

  procedure ClearClipboard;
  procedure ClipboardSetText(AText: String);

const

  fileScheme      = 'file:';   // for URI

{$IF DEFINED(MSWINDOWS)}
  CFSTR_PREFERRED_DROPEFFECT      = 'Preferred DropEffect';
  CFSTR_FILENAME                  = 'FileName';
  CFSTR_FILENAMEW                 = 'FileNameW';
  CFSTR_UNIFORM_RESOURCE_LOCATOR  = 'UniformResourceLocator';
  CFSTR_UNIFORM_RESOURCE_LOCATORW = 'UniformResourceLocatorW';
  CFSTR_SHELL_IDLIST_ARRAY        = 'Shell IDList Array';
  CFSTR_FILEDESCRIPTOR = 'FileGroupDescriptor';
  CFSTR_FILEDESCRIPTORW = 'FileGroupDescriptorW';
  CFSTR_FILECONTENTS = 'FileContents';
  CFSTR_HTMLFORMAT = 'HTML Format';
  CFSTR_RICHTEXTFORMAT = 'Rich Text Format';

{$ELSEIF DEFINED(UNIX_not_DARWIN)}

  // General MIME
  uriListMime     = 'text/uri-list';
  textPlainMime   = 'text/plain';

  // Gnome
  cutText = 'cut';
  copyText = 'copy';
  gnomeClipboardMime = 'x-special/gnome-copied-files';

  // Kde
  kdeClipboardMime = 'application/x-kde-cutselection';

{$ELSEIF DEFINED(DARWIN)}

  TClipboardOperationName : array[TClipboardOperation] of string = (
      'copy', 'cut'
    );

  darwinPasteboardOpMime = 'application/x-darwin-doublecmd-PbOp';

{$ENDIF}


{$IF DEFINED(MSWINDOWS)}

var
  CFU_PREFERRED_DROPEFFECT,
  CFU_FILENAME,
  CFU_FILENAMEW,
  CFU_UNIFORM_RESOURCE_LOCATOR,
  CFU_UNIFORM_RESOURCE_LOCATORW,
  CFU_SHELL_IDLIST_ARRAY,
  CFU_FILECONTENTS,
  CFU_FILEGROUPDESCRIPTOR,
  CFU_FILEGROUPDESCRIPTORW,
  CFU_HTML,
  CFU_RICHTEXT: TClipboardFormat;

{$ELSEIF DEFINED(UNIX_not_DARWIN)}

var
  CFU_KDE_CUT_SELECTION,
  CFU_GNOME_COPIED_FILES,
  CFU_TEXT_PLAIN,
  CFU_URI_LIST: TClipboardFormat;

{$ENDIF}


implementation

uses
{$IF DEFINED(MSWINDOWS)}
  Clipbrd, Windows, ActiveX, uOleDragDrop, fMain, uShellContextMenu, uOSForms
{$ELSEIF DEFINED(UNIX_not_DARWIN)}
  Clipbrd, LCLIntf
{$ELSEIF DEFINED(DARWIN)}
  DCStrUtils, CocoaAll, CocoaUtils, uMyDarwin
{$ENDIF}
  ;


procedure RegisterUserFormats;
begin
{$IF DEFINED(MSWINDOWS)}

  CFU_PREFERRED_DROPEFFECT      := RegisterClipboardFormat(CFSTR_PREFERRED_DROPEFFECT);
  CFU_FILENAME                  := RegisterClipboardFormat(CFSTR_FILENAME);
  CFU_FILENAMEW                 := RegisterClipboardFormat(CFSTR_FILENAMEW);
  CFU_UNIFORM_RESOURCE_LOCATOR  := RegisterClipboardFormat(CFSTR_UNIFORM_RESOURCE_LOCATOR);
  CFU_UNIFORM_RESOURCE_LOCATORW := RegisterClipboardFormat(CFSTR_UNIFORM_RESOURCE_LOCATORW);
  CFU_SHELL_IDLIST_ARRAY        := RegisterClipboardFormat(CFSTR_SHELL_IDLIST_ARRAY);
  CFU_FILECONTENTS := $8000 OR RegisterClipboardFormat(CFSTR_FILECONTENTS) And $7FFF;
  CFU_FILEGROUPDESCRIPTOR := $8000 OR RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR) And $7FFF;
  CFU_FILEGROUPDESCRIPTORW := $8000 OR RegisterClipboardFormat(CFSTR_FILEDESCRIPTORW) And $7FFF;
  CFU_HTML := $8000 OR RegisterClipboardFormat(CFSTR_HTMLFORMAT) And $7FFF;
  CFU_RICHTEXT := $8000 OR RegisterClipboardFormat(CFSTR_RICHTEXTFORMAT) And $7FFF;

{$ELSEIF DEFINED(UNIX_not_DARWIN)}

  CFU_GNOME_COPIED_FILES        := RegisterClipboardFormat(gnomeClipboardMime);
  CFU_KDE_CUT_SELECTION         := RegisterClipboardFormat(kdeClipboardMime);
  CFU_TEXT_PLAIN                := RegisterClipboardFormat(textPlainMime);
  CFU_URI_LIST                  := RegisterClipboardFormat(uriListMime);

{$ENDIF}

end;

{ Changes all '%XX' to bytes (XX is a hex number). }
function URIDecode(encodedUri: String): String;
var
  i, oldIndex: Integer;
  len: Integer;
begin
  len := Length(encodedUri);
  Result := '';

  oldIndex := 1;
  i := 1;
  while i <= len-2 do // must be at least 2 more characters after '%'
  begin
    if encodedUri[i] = '%' then
    begin
      Result := Result + Copy(encodedUri, oldIndex, i-oldIndex)
                       + Chr(StrToInt('$' + Copy(encodedUri, i+1, 2)));
      i := i + 3;
      oldIndex := i;
    end
    else
      Inc(i);
  end;

  Result := Result + Copy(encodedUri, oldIndex, len - oldIndex + 1 );
end;

{ Escapes forbidden characters to '%XX' (XX is a hex number). }
function URIEncode(path: String): String;
const
{
  Per RFC-3986, what's allowed in uri-encoded path.

  path-absolute = "/" [ segment-nz *( "/" segment ) ]
  segment       = *pchar
  segment-nz    = 1*pchar
  pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"      <--
  pct-encoded   = "%" HEXDIG HEXDIG
  unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
  reserved      = gen-delims / sub-delims
  gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
  sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                / "*" / "+" / "," / ";" / "="

  We'll also allow "/" in pchar, because it happens to also be the OS path delimiter.
}
  allowed : set of char
          = [ '-', '.', '_', '~', // 'A'..'Z', 'a'..'z', '0'..'9',
              '!', '$', '&', #39 {'}, '(', ')', '*', '+', ',', ';', '=',
              ':', '@', '/' ];
var
  i, oldIndex: Integer;
  len: Integer;
begin
  len := Length(path);
  Result := '';

  oldIndex := 1;
  i := 1;
  for i := 1 to len do
  begin
    if not ((path[i] >= 'a') and (path[i] <= 'z')) and
       not ((path[i] >= 'A') and (path[i] <= 'Z')) and
       not ((path[i] >= '0') and (path[i] <= '9')) and
       not (path[i] in allowed) then
    begin
      Result := Result + Copy(path, oldIndex, i-oldIndex)
                       + '%' + Format('%2x', [Ord(path[i])]);

      oldIndex := i + 1;
    end;
  end;

  Result := Result + Copy(path, oldIndex, len - oldIndex + 1 );
end;


{$IFDEF UNIX_not_DARWIN}
{ Extracts a path from URI }
function ExtractPath(uri: String): String;
var
  len: Integer;
  i, j: Integer;
begin
  len := Length(uri);
  if (len >= Length(fileScheme)) and
     (CompareChar(uri[1], fileScheme, Length(fileScheme)) = 0) then
  begin
    i := 1 + Length(fileScheme);

    // Omit case where we would have a root-less path - it is useless to us.
    if (i <= len) and (uri[i] = '/') then
    begin
      // Check if we have a: - "//" authority - part.
      if (i+1 <= len) and (uri[i+1] = '/') then
      begin
        // Authority (usually a hostname) may be empty.
        for j := i + 2 to len do
          if uri[j] = '/' then
          begin
            Result := Copy(uri, j, len - j + 1);
            Break;
          end;
      end
      else
      begin
        // We have only a path.
        Result := Copy(uri, i, len - i + 1);
      end;
    end;
  end
  else
    Result := '';
end;

{ Retrieves file names delimited by line ending characters. }
function ExtractFilenames(uriList: String): TStringList;
var
  i, oldIndex: Integer;
  len: Integer;
  path: String;
begin
  // Format should be:      file://hostname/path/to/file
  // Hostname may be empty.

  len := Length(uriList);
  Result := TStringList.Create;

  // For compatibility with apps that end the string with zero.
  while (len > 0) and (uriList[len] = #0) do Dec(len);
  if len = 0 then Exit;

  oldIndex := 1;
  for i := 1 to len do
  begin
    // Search for the end of line.
    if uriList[i] in [ #10, #13 ] then
    begin
      if i > oldIndex then
      begin
        path := ExtractPath(Copy(uriList, oldIndex, i - oldIndex));
        if Length(path) > 0 then
          Result.Add(path);
      end;

      oldIndex := i + 1;
    end
  end;

  if i >= oldIndex then
  begin
    // copy including 'i'th character
    path := ExtractPath(Copy(uriList, oldIndex, i - oldIndex + 1));
    if Length(path) > 0 then
      Result.Add(path);
  end;
end;

function FileNameToURI(const FileName: String): String;
begin
  Result := fileScheme + '//' + URIEncode(FileName);
end;

function FormatUriList(FileNames: TStringList): String;
var
  i : integer;
begin
  Result := '';
  for i := 0 to filenames.Count-1 do
  begin
    // Separate previous uris with line endings,
    // but do not end the whole string with it.
    if i > 0 then
      Result := Result + LineEnding;

    Result := Result
            + fileScheme + '//'  { don't put hostname }
            + URIEncode(filenames[i]);
  end;
end;

function FormatTextPlain(FileNames: TStringList): String;
var
  i : integer;
begin
  Result := '';
  for i := 0 to filenames.Count-1 do
  begin
    if i > 0 then
      Result := Result + LineEnding;

    Result := Result
            + fileScheme + '//'  { don't put hostname }
            + filenames[i];
  end;
end;

function GetClipboardFormatAsString(formatId: TClipboardFormat): String;
var
  PBuffer: PChar = nil;
  stream: TMemoryStream;
begin
  Result := '';

  stream := TMemoryStream.Create;
  if stream <> nil then
  try
    Clipboard.GetFormat(formatId, stream);
    stream.Seek(0, soFromBeginning);

    PBuffer := AllocMem(stream.Size);
    if PBuffer <> nil then
    begin
      stream.Read(PBuffer^, stream.Size);
      SetString(Result, PBuffer, stream.Size);
    end;

  finally
    if PBuffer <> nil then
    begin
      FreeMem(PBuffer);
      PBuffer := nil;
    end;
    FreeAndNil(stream);
  end;
end;

function GetClipboardFormatAsString(formatName: String): String;
var
  formatId: Integer;
begin
  formatId := Clipboard.FindFormatID(formatName);
  if formatId <> 0 then
    Result := GetClipboardFormatAsString(formatId)
  else
    Result := '';
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function SendToClipboard(const filenames:TStringList; ClipboardOp: TClipboardOperation):Boolean;
var
  DragDropInfo: TDragDropInfo;
  i: Integer;
  hGlobalBuffer: HGLOBAL;
  PreferredEffect: DWORD = DROPEFFECT_COPY;
  formatEtc: TFormatEtc = (CfFormat: 0; Ptd: nil; dwAspect: 0; lindex: 0; tymed: TYMED_HGLOBAL);
begin

  Result := False;

  if filenames.Count = 0 then Exit;

  if OpenClipboard(GetWindowHandle(frmMain)) = False then Exit;

  // Empty clipboard, freeing handles to data inside it.
  // Assign ownership of clipboard to self (frmMain.Handle).
  EmptyClipboard;

  { Create a helper object. }

  DragDropInfo := TDragDropInfo.Create(PreferredEffect);

  try

    for i := 0 to filenames.Count - 1 do
      DragDropInfo.Add(filenames[i]);


    { Now, set preferred effect. }

    if CFU_PREFERRED_DROPEFFECT <> 0 then
    begin
      if ClipboardOp = ClipboardCopy then
        PreferredEffect := DROPEFFECT_COPY
      else if ClipboardOp = ClipboardCut then
        PreferredEffect := DROPEFFECT_MOVE;

      hGlobalBuffer := DragDropInfo.CreatePreferredDropEffect(PreferredEffect);

      if hGlobalBuffer <> 0 then
      begin
        if SetClipboardData(CFU_PREFERRED_DROPEFFECT, hGlobalBuffer) = 0 then
        begin
          // Failed.
          GlobalFree(hGlobalBuffer);
          CloseClipboard;
          Exit;
        end
        // else SetClipboardData succeeded,
        // so hGlobalBuffer is now owned by the operating system.
      end
      else
      begin
        CloseClipboard;
        Exit;
      end;
    end;

    { Now, set clipboard data. }

    formatEtc.CfFormat := CF_HDROP;
    hGlobalBuffer := DragDropInfo.MakeDataInFormat(formatEtc);
    if SetClipboardData(CF_HDROP, hGlobalBuffer) = 0 then
      GlobalFree(hGlobalBuffer);

    formatEtc.CfFormat := CFU_SHELL_IDLIST_ARRAY;
    hGlobalBuffer := DragDropInfo.MakeDataInFormat(formatEtc);
    if SetClipboardData(CFU_SHELL_IDLIST_ARRAY, hGlobalBuffer) = 0 then
      GlobalFree(hGlobalBuffer);

    CloseClipboard;

    Result := True;

  finally
    FreeAndNil(DragDropInfo);

  end;
end;
{$ENDIF}

{$IFDEF UNIX_not_DARWIN}
function SendToClipboard(const filenames:TStringList; ClipboardOp: TClipboardOperation):Boolean;
var
  s: String;
  uriList: String;
  plainList: String;
begin

  Result := False;

  if filenames.Count = 0 then Exit;

  // Prepare filenames list.
  uriList := FormatUriList(filenames);
  plainList := FormatTextPlain(filenames);

  Clipboard.Open;
  Clipboard.Clear;

  { Gnome }
  if CFU_GNOME_COPIED_FILES <> 0 then
  begin
    case ClipboardOp of
      ClipboardCopy:
        s := copyText;

      ClipboardCut:
        s := cutText;

      else
        // unsupported operation
        s := '';
    end;

    if s <> '' then
    begin
      s := s + LineEnding + uriList;
      Clipboard.AddFormat(CFU_GNOME_COPIED_FILES, s[1], Length(s));
    end;
  end;

  { KDE }
  if CFU_KDE_CUT_SELECTION <> 0 then
  begin
    case ClipboardOp of
      ClipboardCopy:
        s := '0';

      ClipboardCut:
        s := '1';

      else
        // unsupported operation
        s := '';
    end;

    if s <> '' then
      Clipboard.AddFormat(CFU_KDE_CUT_SELECTION, s[1], Length(s));
  end;

  // Common to all, plain text.
  Clipboard.AddFormat(PredefinedClipboardFormat(pcfText),
                      plainList[1], Length(plainList));

  // Send also as URI-list.
  if CFU_URI_LIST <> 0 then
    Clipboard.AddFormat(CFU_URI_LIST, uriList[1], Length(uriList));

  Clipboard.Close;

  Result := True;

end;
{$ENDIF}

// MacOs 10.5 compatibility
{$IFDEF DARWIN}
function FilenamesToString(const filenames:TStringList): String;
begin
  Result := TrimRightLineEnding( filenames.Text, filenames.TextLineBreakStyle);
end;

procedure NSPasteboardAddFiles(const filenames:TStringList; pb:NSPasteboard);
begin
  pb.addTypes_owner(NSArray.arrayWithObject(NSFileNamesPboardType), nil);
  pb.setPropertyList_forType(ListToNSArray(filenames), NSFileNamesPboardType);
end;

procedure NSPasteboardAddFiles(const filenames:TStringList);
begin
  NSPasteboardAddFiles( filenames, NSPasteboard.generalPasteboard );
end;

procedure NSPasteboardAddString(const value:String; const pbType:NSString );
var
  pb: NSPasteboard;
begin
  pb:= NSPasteboard.generalPasteboard;
  pb.addTypes_owner(NSArray.arrayWithObject(pbType), nil);
  pb.setString_forType(StringToNSString(value), pbType);
end;

procedure NSPasteboardAddString(const value:String);
begin
  NSPasteboardAddString( value , NSStringPboardType );
end;

function SendToClipboard(const filenames:TStringList; ClipboardOp: TClipboardOperation):Boolean;
var
   s : string;
begin
  Result := false;
  if filenames.Count = 0 then Exit;

  ClearClipboard;
  NSPasteboardAddFiles( filenames );
  NSPasteboardAddString( FilenamesToString(filenames) );
  NSPasteboardAddString( TClipboardOperationName[ClipboardOp] , StringToNSString(darwinPasteboardOpMime) );

  Result := true;
end;
{$ENDIF}

function CopyToClipboard(const filenames:TStringList):Boolean;
begin
  Result := SendToClipboard(filenames, ClipboardCopy);
end;

function CutToClipboard(const filenames:TStringList):Boolean;
begin
  Result := SendToClipboard(filenames, ClipboardCut);
end;


{$IFDEF MSWINDOWS}
function PasteFromClipboard(out ClipboardOp: TClipboardOperation; out filenames:TStringList):Boolean;
var
  hGlobalBuffer: HGLOBAL;
  pBuffer: LPVOID;
  PreferredEffect: DWORD;
begin

  filenames := nil;
  Result := False;

  // Default to 'copy' if effect hasn't been given.
  ClipboardOp := ClipboardCopy;

  if OpenClipboard(0) = False then Exit;

  if CFU_PREFERRED_DROPEFFECT <> 0 then
  begin
    hGlobalBuffer := GetClipboardData(CFU_PREFERRED_DROPEFFECT);
    if hGlobalBuffer <> 0 then
    begin
      pBuffer := GlobalLock(hGlobalBuffer);
      if pBuffer <> nil then
      begin
        PreferredEffect := PDWORD(pBuffer)^;
        if PreferredEffect = DROPEFFECT_COPY then ClipboardOp := ClipboardCopy
        else if PreferredEffect = DROPEFFECT_MOVE then ClipboardOp := ClipboardCut;

        GlobalUnlock(hGlobalBuffer);
      end;
    end;
  end;

  { Now, retrieve file names. }

  hGlobalBuffer := GetClipboardData(CF_HDROP);

  if hGlobalBuffer = 0 then
  begin
    with frmMain do
    begin
      CloseClipboard;
      uShellContextMenu.PasteFromClipboard(Handle, ActiveFrame.CurrentPath);
      Exit(False);
    end;
  end;

  filenames := uOleDragDrop.TFileDropTarget.GetDropFilenames(hGlobalBuffer);

  if Assigned(filenames) then
    Result := True;

  CloseClipboard;

end;
{$ENDIF}

{$IFDEF UNIX_not_DARWIN}
function PasteFromClipboard(out ClipboardOp: TClipboardOperation; out filenames:TStringList):Boolean;
var
  formatId: TClipboardFormat;
  uriList: String;
  s: String;
begin

  filenames := nil;
  Result := False;

  // Default to 'copy' if effect hasn't been given.
  ClipboardOp := ClipboardCopy;

  uriList := '';

  // Check if clipboard is not empty.
  if Clipboard.FormatCount = 0 then Exit;

  { Gnome }
  formatId := Clipboard.FindFormatID(gnomeClipboardMime);
  if formatId <> 0 then
  begin
    s := GetClipboardFormatAsString(formatId);

    { Format is:
      'cut' or 'copy' + line ending character,
      followed by an URI-list delimited with line ending characters.
      Filenames may be UTF-8 encoded.
      e.g.
      cut#10file://host/path/to/file/name%C4%85%C3%B3%C5%9B%C5%BA%C4%87 }

    { Check operation }

    if (Length(s) >= Length(CutText)) and
       (CompareChar(s[1], CutText, Length(CutText)) = 0) then
    begin
        ClipboardOp := ClipboardCut;
        uriList := Copy(s, 1 + Length(CutText), Length(s)-Length(CutText));
    end
    else if (Length(s) >= Length(CopyText)) and
            (CompareChar(s[1], CopyText, Length(CopyText)) = 0) then
    begin
        ClipboardOp := ClipboardCopy;
        uriList := Copy(s, 1 + Length(CopyText), Length(s)-Length(CopyText));
    end;

    if Length(uriList) > 0 then
      uriList := URIDecode(Trim(uriList));
  end

  else

  { KDE }
  begin
    formatId := Clipboard.FindFormatID(kdeClipboardMime);
    if formatId <> 0 then
    begin
      s := GetClipboardFormatAsString(formatId);

      { We should have a single char: '1' if 'cut', '0' if 'copy'. }
      { No uri-list in this target. }
      if Length(s) > 0 then
      begin
        if s[1] = '1' then
          ClipboardOp := ClipboardCut
        else
          ClipboardOp := ClipboardCopy;
      end;
    end;
  end;

  { Common formats }

  if uriList = '' then
  begin
    // Try to read one of the text formats.
    // The URIs in targets like STRING, UTF8_STRING, etc. are not encoded.

    // First try default target choosing behaviour.
    // Some buggy apps, however, supply UTF8_STRING or other targets
    // with 0 size and it's not detected by this function under gtk.
    uriList := Clipboard.AsText;

    // Next, try URI encoded list.
    if uriList = '' then
    begin
      uriList := GetClipboardFormatAsString(uriListMime);
      if Length(uriList) > 0 then
        uriList := URIDecode(Trim(uriList));
    end;

    // Try plain texts now.
    // On non-UTF8 systems these should be encoded in system locale,
    // and may be displayed badly, but will be copied successfully.

    if uriList = '' then
    begin
      uriList := GetClipboardFormatAsString('STRING');
    end;

    if uriList = '' then
    begin
      uriList := GetClipboardFormatAsString(textPlainMime);
    end;

    // If still nothing, then maybe the clipboard has no data in text format.
    if uriList = '' then Exit;
  end;

  filenames := ExtractFilenames(uriList);

  if (filenames <> nil) and (filenames.Count > 0) then
    Result := True;

end;
{$ENDIF}

// MacOs 10.5 compatibility
{$IFDEF DARWIN}
function getStringFromPasteboard( pbType : NSString ) : String;
var
  pb : NSPasteboard;
begin
  pb := NSPasteboard.generalPasteboard;
  Result := NSStringToString( pb.stringForType( pbType ) );
end;

function getOpFromPasteboard() : TClipboardOperation;
var
  opString : String;
begin
  Result := ClipboardCopy;
  opString := getStringFromPasteboard( StringToNSString(darwinPasteboardOpMime) );
  if TClipboardOperationName[ClipboardCut].CompareTo(opString) = 0 then Result := ClipboardCut;
end;

function getFilenamesFromPasteboard() : TStringList;
var
  pb : NSPasteboard;
  filenameArray{, lClasses}: NSArray;
begin
  Result := nil;
  pb := NSPasteboard.generalPasteboard;
  filenameArray := pb.propertyListForType(NSFilenamesPboardType);
  if filenameArray <> nil then Result := NSArrayToList( filenameArray );
end;

function PasteFromClipboard(out ClipboardOp: TClipboardOperation; out filenames:TStringList):Boolean;
begin
  Result := false;
  ClipboardOp := ClipboardCopy;
  filenames := getFilenamesFromPasteboard();
  if filenames <> nil then
  begin
    ClipboardOp := getOpFromPasteboard();
    Result := true;
  end;
end;
{$ENDIF}




{$IFDEF MSWINDOWS}
procedure ClearClipboard;
begin
  if OpenClipboard(0) then
  begin
    EmptyClipboard;
    CloseClipboard;
  end;
end;
{$ENDIF}

{$IFDEF UNIX_not_DARWIN}
procedure ClearClipboard;
begin
  Clipboard.Open;
  Clipboard.AsText := '';
  Clipboard.Close;
end;
{$ENDIF}

// MacOs 10.5 compatibility
{$IFDEF DARWIN}
procedure ClearClipboard( pb:NSPasteboard );
begin
  pb.clearContents;
end;

procedure ClearClipboard;
begin
  ClearClipboard( NSPasteboard.generalPasteboard );
end;
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
procedure ClipboardSetText(AText: String);
begin
  Clipboard.AsText := AText;
end;
{$ENDIF}

{$IF DEFINED(UNIX_not_DARWIN)}
procedure ClipboardSetText(AText: String);
begin
{$IFNDEF LCLGTK2}
  Clipboard.AsText := AText;
{$ELSE}
  // Workaround for Lazarus bug #0021453. LCL adds trailing zero to clipboard in Clipboard.AsText.
  if Length(AText) = 0 then
    Clipboard.AsText := ''
  else begin
    Clipboard.Clear;
    Clipboard.AddFormat(PredefinedClipboardFormat(pcfText), AText[1], Length(AText));
  end;
{$ENDIF}
end;
{$ENDIF}

// MacOs 10.5 compatibility
{$IFDEF DARWIN}
procedure ClipboardSetText(AText: String);
begin
  ClearClipboard;
  NSPasteboardAddString(AText);
end;
{$ENDIF}

initialization

  RegisterUserFormats;

end.

