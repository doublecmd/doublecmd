unit uURIHandling;

{$mode ObjFPC}{$H+}

{$IF DEFINED(UNIX) and not DEFINED(DARWIN)}
  {$Define UNIX_not_DARWIN}
{$ENDIF}

interface

uses
  Classes, SysUtils;

  function URIDecode(encodedUri: String): String;
  function URIEncode(const path: String): String;

  {$IF DEFINED(UNIX_not_DARWIN)}
  function ExtractFilenames(uriList: String): TStringList;
  function FileNameToURI(const FileName: String): String;

  function FormatUriList(FileNames: TStringList): String;
  function FormatTextPlain(FileNames: TStringList): String;
  {$ENDIF}

const
  fileScheme      = 'file:';   // for URI
  // General MIME
  uriListMime     = 'text/uri-list';
  textPlainMime   = 'text/plain';


implementation

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
function URIEncode(const path: String): String;
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
          = [ 'A'..'Z', 'a'..'z', '0'..'9', '-', '.', '_', '~',
              '!', '$', '&', '''' {'}, '(', ')', '*', '+', ',', ';', '=',
              ':', '@', '/' ];
var
  i, oldIndex: Integer;
  len: Integer;
begin
  len := Length(path);
  Result := '';

  oldIndex := 1;
  for i := 1 to len do
  begin
    if not (path[i] in allowed) then
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
{$ENDIF}

end.

