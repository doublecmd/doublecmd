unit uClipboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  type TClipboardOperation = ( ClipboardCopy, ClipboardCut );

  function CopyToClipboard(filenames:TStringList):Boolean;
  function CutToClipboard(filenames:TStringList):Boolean;
  function PasteFromClipboard(out ClipboardOp: TClipboardOperation;
                              out filenames:TStringList):Boolean;

{$IF DEFINED(UNIX)}
  function URIDecode(encodedUri: String): String;
  function URIEncode(path: String): String;
  function ExtractFilenames(uriList: String): TStringList;

const
  // General MIME
  uriListMime = 'text/uri-list';
  textPlainMime = 'text/plain';
  fileScheme = 'file:';   // for URI
{$ENDIF}

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, ActiveX, uOleDragDrop, fMain,
{$ELSE IFDEF UNIX}
  LCLIntf, LCLType,
{$ENDIF}
  Clipbrd;

const
{$IFDEF MSWINDOWS}

  CFSTR_PREFERREDDROPEFFECT = 'Preferred DropEffect';

{$ELSE IFDEF UNIX}

  // Gnome
  cutText = 'cut';
  copyText = 'copy';
  gnomeClipboardMime = 'x-special/gnome-copied-files';

  // Kde
  kdeClipboardMime = 'application/x-kde-cutselection';

{$ENDIF}


{$IFDEF UNIX}

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
  while (uriList[len] = #0) and (len > 0) do Dec(len);
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

function GetClipboardFormatAsString(formatId: TClipboardFormat): String;
var
  PBuffer: PChar;
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  if stream <> nil then
  begin
    Clipboard.GetFormat(formatId, stream);
    stream.Seek(0, soFromBeginning);

    PBuffer := nil;
    try
      PBuffer := AllocMem(stream.GetSize);
      if PBuffer <> nil then
      begin
        stream.Read(PBuffer^, stream.GetSize);
        SetString(Result, PBuffer, stream.GetSize);
      end;
    finally
      if PBuffer <> nil then
      begin
        FreeMem(PBuffer);
        PBuffer := nil;
      end;
    end;
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

function SendToClipboard(filenames:TStringList; ClipboardOp: TClipboardOperation):Boolean;
{$IFDEF MSWINDOWS}
var
  DragDropInfo: TDragDropInfo;
  i: Integer;
  hGlobalBuffer: HGLOBAL;
  pBuffer: LPVOID;
  CF_EFFECT: UINT;
  PreferredEffect: DWORD = DROPEFFECT_COPY;

const
  DummyPoint: TPoint = (x: 0; y: 0);
{$ENDIF}

{$IFDEF UNIX}
var
  formatId: Integer;
  i: Integer;
  s: String;
  uriList: String;
  plainList: String;
{$ENDIF}

begin

  Result := False;

  if filenames.Count = 0 then Exit;

{$IFDEF MSWINDOWS}

  { First, try to acquire preferred effect (move, copy) handle. }

  CF_EFFECT := RegisterClipboardFormat(PChar(CFSTR_PREFERREDDROPEFFECT));

  if CF_EFFECT = 0 then Exit;

  if OpenClipboard(frmMain.Handle) = False then Exit;

  // Empty clipboard, freeing handles to data inside it.
  // Assign ownership of clipboard to self (frmMain.Handle).
  EmptyClipboard;

  { Now, set preferred effect. }

  if ClipboardOp = ClipboardCopy then
    PreferredEffect := DROPEFFECT_COPY
  else if ClipboardOp = ClipboardCut then
    PreferredEffect := DROPEFFECT_MOVE;

  hGlobalBuffer := GlobalAlloc(GMEM_MOVEABLE, SizeOf(DWORD));
  if hGlobalBuffer = 0 then
  begin
    CloseClipboard;
    Exit;
  end;

  pBuffer := GlobalLock(hGlobalBuffer);
  if pBuffer <> nil then
  begin
    CopyMemory(pBuffer, PDWORD(@PreferredEffect), SizeOf(DWORD));
    GlobalUnlock(hGlobalBuffer);

    if SetClipboardData(CF_EFFECT, hGlobalBuffer) = 0 then
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
    // Could not lock allocated memory, so free it.
    GlobalFree(hGlobalBuffer);
    CloseClipboard;
    Exit;
  end;

  { Now, set clipboard data in CF_HDROP format. }

  DragDropInfo := TDragDropInfo.Create(DummyPoint, True);

  for i := 0 to filenames.Count - 1 do
    DragDropInfo.Add(filenames[i]);

  hGlobalBuffer := DragDropInfo.CreateHDrop;
  if SetClipboardData(CF_HDROP, hGlobalBuffer) = 0 then
    GlobalFree(hGlobalBuffer);

  CloseClipboard;

  Result := True;

{$ENDIF}

{$IFDEF UNIX}

  // Prepare filenames list.
  uriList := '';
  plainList := '';
  for i := 0 to filenames.Count-1 do
  begin
    plainList := plainList
               + fileScheme + '//'  { don't put hostname }
               + filenames[i]
               + LineEnding;

    uriList   := uriList
               + fileScheme + '//'  { don't put hostname }
               + URIEncode(filenames[i])
               + LineEnding;
  end;


  Clipboard.Open;
  Clipboard.Clear;

  { Gnome }
  formatId := RegisterClipboardFormat(gnomeClipboardMime);
  if formatId <> 0 then
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
      Clipboard.AddFormat(formatId, s[1], Length(s));
    end;
  end;

  { KDE }
  formatId := RegisterClipboardFormat(kdeClipboardMime);
  if formatId <> 0 then
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
      Clipboard.AddFormat(formatId, s[1], Length(s));
  end;

  // Common to all, plain text.
  Clipboard.AddFormat(PredefinedClipboardFormat(pcfText),
                      plainList[1], Length(plainList));

  // Send also as URI-list.
  formatId := RegisterClipboardFormat(uriListMime);
  if formatId <> 0 then
    Clipboard.AddFormat(formatId, uriList[1], Length(uriList));

  Clipboard.Close;

  Result := True;

{$ENDIF}

end;

function CopyToClipboard(filenames:TStringList):Boolean;
begin
  Result := SendToClipboard(filenames, ClipboardCopy);
end;

function CutToClipboard(filenames:TStringList):Boolean;
begin
  Result := SendToClipboard(filenames, ClipboardCut);
end;

function PasteFromClipboard(out ClipboardOp: TClipboardOperation; out filenames:TStringList):Boolean;
{$IFDEF MSWINDOWS}
var
  hGlobalBuffer: HGLOBAL;
  pBuffer: LPVOID;
  NumFiles: Integer;
  i: Integer;
  szFilename: array [0..MAX_PATH] of char;
  bWideStrings: boolean;
  CF_EFFECT: UINT;
  PreferredEffect: DWORD;
{$ELSE IF DEFINED(UNIX)}
var
  formatId: TClipboardFormat;
  uriList: String;
  s: String;
{$ENDIF}
begin

  filenames := nil;
  Result := False;

  // Default to 'copy' if effect hasn't been given.
  ClipboardOp := ClipboardCopy;

{$IFDEF MSWINDOWS}

  if OpenClipboard(0) = False then Exit;

  { First, try to acquire preferred effect (move, copy) handle. }

  CF_EFFECT := RegisterClipboardFormat(PChar(CFSTR_PREFERREDDROPEFFECT));

  if CF_EFFECT <> 0 then
  begin
    hGlobalBuffer := GetClipboardData(CF_EFFECT);
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

  if hGlobalBuffer <> 0 then
  begin

    NumFiles := DragQueryFile(hGlobalBuffer, $FFFFFFFF, nil, 0);
    bWideStrings := DragQueryWide(hGlobalBuffer);

    filenames := TStringList.Create;

    for i := 0 to NumFiles - 1 do
    begin

      DragQueryFile(hGlobalBuffer, i, szFilename, sizeof(szFilename));

      // If Wide strings, then do Wide to UTF-8 transform
      if bWideStrings then
        filenames.Add(UTF8Encode(szFileName))
      else
        filenames.Add(szFilename);

    end;

    Result := True;

  end;

  CloseClipboard;

{$ELSE IF DEFINED(UNIX)}

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

{$ENDIF}
end;

end.

