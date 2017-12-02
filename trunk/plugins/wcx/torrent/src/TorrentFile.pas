unit TorrentFile;

interface

uses
  SysUtils, Contnrs, Hashes, Classes, BDecode;

type
  TBitfield = array of boolean;

  TTorrentPiece = class(TObject)
  private
    _Hash: String;
    _HashBin: String;
    _Valid: Boolean;
  public
    property Hash: String read _Hash;
    property HashBin: String read _HashBin;
    property Valid: Boolean read _Valid write _Valid;
    constructor Create(Hash: String; HashBin:String; Valid: Boolean);
  end;

  TTorrentSubFile = class(TObject)
  private
    _Name: String;
    _Path: String;
    _Filename: String;
    _Length: Int64;
    _Offset: Int64;
    _Left: Int64;
  public
    property Name: String read _Name write _Name;
    property Path: String read _Path write _Path;
    property Length: Int64 read _Length;
    property Offset: Int64 read _Offset;
    property Left: Int64 read _Left write _Left;
    property Filename: String read _Filename write _Filename;
    constructor Create(Name: String; Path: String; Length: Int64; Offset: Int64);
  end;

  TTorrentFile = class(TObject)
  published
  private
    _Announce : String;
    _Name : String;
    _Comment : String;
    _Length : Int64;
    _CreationTime : Int32;
    _Count : Integer;
    _Err : TStringList;
    _Tree : TObjectHash;
    _SHA1Hash : String;
    _HashBin : String;
    _Multifile : Boolean;
    _Files : TObjectList;
  public
    Pieces : array of TTorrentPiece;
    PieceLength : Integer;
    BackupTrackers : TStringList;

    property Announce: String read _Announce write _Announce;
    property Name: String read _Name write _Name;
    property CreationTime: Int32 read _CreationTime write _CreationTime;
    property Length: Int64 read _Length;
    property Count: Integer read _Count;
    property Tree: TObjectHash read _Tree;
    property Errors: TStringList read _Err;
    property Hash: String read _SHA1Hash;
    property Comment: String read _Comment write _Comment;
    property HashBin: String read _HashBin;
    property Multifile: Boolean read _Multifile;
    property Files: TObjectList read _Files write _Files;
    procedure Clear();
    function Load(Stream: TStream): Boolean;
    procedure Save(Stream: TStream; Pieces : array of TTorrentPiece);
    procedure Init(Announce, Name, Comment, HashBin:String; Length:Int64; Multifile:Boolean);
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  DCDateTimeUtils, SHA1;

{ TTorrentSubFile }

constructor TTorrentSubFile.Create(Name, Path: String; Length: Int64; Offset: Int64);
begin
  _Name := Name;
  _Path := Path;
  _Length := Length;
  _Offset := Offset;
  _Left := Length;
  
  inherited Create();
end;

procedure TTorrentFile.Clear();
var
  i : Integer;
begin
  _Announce := '';
  _Name := '';
  _SHA1Hash := '';
  _Length := 0;
  _Count := 0;
  _Files.Clear();
  _Tree.Clear();
  _Err.Clear();
  for i := Low(Pieces) to High(Pieces) do FreeAndNil(Pieces[i]);
  SetLength(Pieces,0);
  _Multifile := False;
end;

constructor TTorrentFile.Create();
begin
  _Files := TObjectList.Create();
  _Tree := TObjectHash.Create();
  _Err := TStringList.Create();
  BackupTrackers := TStringList.Create;
  inherited Create();
end;

destructor TTorrentFile.Destroy();
begin
  Clear();
  FreeAndNil(_Files);
  FreeAndNil(_Tree);
  FreeAndNil(_Err);
  FreeAndNil(BackupTrackers);
  inherited;
end;

procedure TTorrentFile.Init(Announce, Name, Comment, HashBin:String; Length:Int64; Multifile:Boolean);
begin
  _Announce := Announce;
  _Name := Name;
  _Comment := Comment;
  _HashBin := HashBin;
  _Length := Length;
  _Multifile := Multifile;
  _CreationTime := DateTimeToUnixFileTime(Now);
end;

function TTorrentFile.Load(Stream: TStream): Boolean;
var
  info, thisfile: TObjectHash;
  files, path, backup, backup2: TObjectList;
  fp, fn: String;
  i, j, pcount: Integer;
  sz, fs, fo: Int64;
  digest: TSHA1Digest;
  r: Boolean;
  o: TObject;
  s:string;
begin
  Clear();
  r := False;
  sz := 0;
  try
    o := bdecodeStream(Stream);
    if(Assigned(o)) then begin
      _Tree := o as TObjectHash;
      if(_Tree.Exists('announce')) then begin
        _Announce := (_Tree['announce'] as TIntString).StringPart;
      end else begin
        _Err.Add('Corrupt File: Missing "announce" segment');
      end;
      if(_Tree.Exists('announce-list')) then begin
         backup := _Tree['announce-list'] as TObjectList;
         for i := 0 to backup.Count - 1 do begin
              backup2 := (backup[i] as TObjectList);
              for j:=0 to backup2.Count -1 do BackupTrackers.Add((backup2[j] as TIntString).StringPart);
         end;
      end;
      if(_Tree.Exists('comment')) then begin
        _Comment := (_Tree['comment'] as TIntString).StringPart;
      end;
      if(_Tree.Exists('creation date')) then begin
        _CreationTime := (_Tree['creation date'] as TIntString).IntPart;
      end;
      if(_Tree.Exists('info')) then begin
        info := _Tree['info'] as TObjectHash;
        if(info.Exists('name')) then begin
          _Name := (info['name'] as TIntString).StringPart;
          if copy(_Name,system.length(_Name)-7,8)='.torrent' then
            _Name:=copy(_Name,0,system.length(_Name)-8);
        end else begin
          _Err.Add('Corrupt File: Missing "info.name" segment');
        end;
        if(info.Exists('piece length')) then begin
          PieceLength := (info['piece length'] as TIntString).IntPart;
        end else begin
          _Err.Add('Corrupt File: Missing "info.piece length" segment');
        end;
        if(info.Exists('pieces')) then begin
          fp := (info['pieces'] as TIntString).StringPart;
          pcount := System.Length(fp) div 20;
          SetLength(Pieces,pcount);
          for i := 0 to pcount - 1 do begin
            s:=copy(fp,(i * 20) + 1,20);
            Pieces[i] := TTorrentPiece.Create(bin2hex(s), s, False);
          end;
        end else begin
          _Err.Add('Corrupt File: Missing "info.pieces" segment');
        end;
        if(info.Exists('length')) then begin // single-file archive
          sz := (info['length'] as TIntString).IntPart;
          _Count := 1;
          _Files.Add(TTorrentSubFile.Create(_Name,'',sz,Int64(0)));
        end else begin
          if(info.Exists('files')) then begin
            _Multifile := True;
            files := info['files'] as TObjectList;
            for i := 0 to files.Count - 1 do begin
              thisfile := files[i] as TObjectHash;
              if(thisfile.Exists('length')) then begin
                fs := (thisfile['length'] as TIntString).IntPart;
              end else begin
                fs := Int64(0);
                _Err.Add('Corrupt File: files[' + IntToStr(i) + '] is missing a "length" segment');
              end;
              fp := '';
              fn := '';
              if(thisfile.Exists('path')) then begin
                path := thisfile['path'] as TObjectList;
                for j := 0 to path.Count - 2 do
                  fp := fp + (path[j] as TIntString).StringPart + PathDelim;
                if(path.Count > 0) then fn := (path[path.Count - 1] as TIntString).StringPart;
              end else begin
                _Err.Add('Corrupt File: files[' + IntToStr(i) + '] is missing a "path" segment');
              end;
              _Files.Add(TTorrentSubFile.Create(fn,fp,fs,sz));
              sz := sz + fs;
            end;
            _Count := _Files.Count;
          end else begin
            _Err.Add('Corrupt File: Missing both "info.length" and "info.files" segments (should have one or the other)');
          end;
        end;
        if(_Tree.Exists('_info_start') and _Tree.Exists('_info_length')) then begin
          fo := Stream.Position;
          Stream.Seek((_Tree['_info_start'] as TIntString).IntPart,soFromBeginning);
          fs := (_Tree['_info_length'] as TIntString).IntPart;
          SetLength(fp,fs);
          Stream.Read(PChar(fp)^,fs);
          digest := SHA1String(fp);
          _SHA1Hash := SHA1Print(digest);
          SetLength(_HashBin, 20);
          Move(digest[0], _HashBin[1], 20);
          Stream.Seek(fo,soFromBeginning);
        end;
      end else begin
        _Err.Add('Corrupt File: Missing "info" segment');
      end;
      _Length := sz;
      r := True;
    end else begin
      _Err.Add('Error parsing file; does not appear to be valid bencoded metainfo');
    end;
  except
    _Err.Add('Something bad happened while trying to load the file, probably corrupt metainfo');
  end;
  Result := r;
end;

procedure TTorrentFile.Save(Stream: TStream; Pieces : array of TTorrentPiece);
var i:integer;
    s,s2:string;

procedure WStrm(s:string);
begin
  Stream.WriteBuffer(s[1],system.length(s));
end;

procedure WStrg(s:string);
var t:String;
begin
  t:=inttostr(system.length(s))+':'+s;
  WStrm(t);
end;

procedure WInt(i:int64);
begin
  WStrm('i'); WStrm(IntToStr(i)); WStrm('e');
end;

begin
  WStrm('d');
  WStrg('announce'); WStrg(Announce);
  if BackupTrackers.Count > 0 then
  begin
    WStrg('announce-list');
    WStrm('l');
     // Primary Tracker
     WStrm('l');
      WStrg(Announce);
     WStrm('e');
     // Backup Tracker
     for i:=0 to BackupTrackers.Count-1 do
      if BackupTrackers[i] <> Announce then
      begin
      WStrm('l');
      WStrg(BackupTrackers[i]);
      WStrm('e');
      end;
    WStrm('e');
  end;
  if Comment <> '' then
  begin
    WStrg('comment');
    WStrg(comment);
  end;
  if Date <> 0 then
  begin
    WStrg('creation date');
    WInt(CreationTime);
  end;
  WStrg('info'); WStrm('d');

  if Multifile then
  begin
  WStrg('files'); WStrm('l');

  for i:=0 to Files.Count-1 do
   with (Files[i] as TTorrentSubFile) do
    begin
       WStrm('d');
         WStrg('length');
         WInt(Length);
         WStrg('path');
         WStrm('l');

         if Path <> '' then
         begin
           s:=path;
           repeat
            if pos('\',s) <> 0 then
            begin
              s2:=copy(s,1,pos('\',s)-1); WStrg(s2);
              Delete(s,1,pos('\',s));
            end;

            if (pos('\',s)=0) and (s <>'') then WStrg(s);
           until pos('\',s)=0;
         end;
         WStrg(Name);

         WStrm('e');
       WStrm('e');
    end;

  WStrm('e');
  end
  else
  begin
     WStrg('length');
     WInt(Length);
  end;

  WStrg('name');
  WStrg(Name);
  WStrg('piece length');
  WInt(PieceLength);

  WStrg('pieces');
  WStrm(IntToStr((high(pieces)+1)*20));
  WStrm(':');
  for i:=0 to high(pieces) do WStrm(pieces[i].HashBin);

  WStrm('e');
  WStrm('e');
end;

constructor TTorrentPiece.Create(Hash, HashBin: String; Valid: Boolean);
begin
  _Hash := Hash;
  _HashBin := HashBin;
  _Valid := Valid;
  inherited Create();
end;

end.
