unit dpkg_deb;


//Debian Linux package unpacker implemented as TFileStream
//v1.0.1  add option ReadAfterDataMember, to avoid unnecessary read after data.tar.gz 

interface

uses
    Classes;

const
   MEMBER_CONTROL = 1;
   MEMBER_DATA = 2;

type
  ar_hdr = record //converted from /usr/include/ar.h
    ar_name: array [0..Pred(16)] of char; (* name *)
    ar_date: array [0..Pred(12)] of char; (* modification time *)
    ar_uid: array [0..Pred(6)] of char; (* user id *)
    ar_gid: array [0..Pred(6)] of char; (* group id *)
    ar_mode: array [0..Pred(8)] of char; (* octal file permissions *)
    ar_size: array [0..Pred(10)] of char; (* size in bytes *)
    ar_fmag: array [0..Pred(2)] of char; (* consistency check *)
  end;  
  TDebianPackage = class
  private
    //FMemberList: array of ar_hdr;
    FPkgVersion: string;
    //FDebStrm: TFileStream;
    FCheckHeader : boolean;
    //constructor Create; override;
    //destructor Destory; override;
    FFileName : string;
    function DoCheckHeader(arh: ar_hdr; infobuf: PChar; memberlen: integer): boolean;
    //function GetFileList: TStrings;
    function ParseHeaderLength(inh: PChar;  Len: integer): integer;
    function SkipMember(Strm: TStream; memberlen: integer): boolean;
  public
    FMemberList: array of ar_hdr;
    ReadAfterDataMember: boolean;
    constructor Create;
    function ReadFromFile(DebPkgFile: string): integer;
    function ExtractMemberToStream(MemberIdx: integer; OutputStrm: TStream): boolean;
    function ExtractMemberToFile(idx: integer; OutputFile: string): boolean;
  published
    property PkgVersion: string read FPkgVersion;
    property CheckHeader: boolean read FCheckHeader write FCheckHeader default false;
    //property MemberList: array of ar_hdr read FMemberList; default;
    //property FileList: TStrings read GetFileList; 
    //property GetControlFile:  //need tar+gzip to implement this
    
  end;
  
implementation

uses
    SysUtils{$IFDEF GDEBUG}, dbugintf {$ENDIF};
    
const
  (* Pre-4BSD archives had these magic numbers in them. *)
  OARMAG1 = $FF6D; 
  OARMAG2 = $FF65; 
  ARMAG = '!<arch>'#10; (* ar "magic number" *)
  SARMAG = 8; (* strlen(ARMAG); *)
  AR_EFMT1 = '#1/'; (* extended format #1 *)
  ARFMAG = '`'#10''; 

(*
static void skipmember(FILE *ar, const char *fn, long memberlen) {
  int c;
  
  memberlen += (memberlen&1);
  while (memberlen > 0) {
    if ((c= getc(ar)) == EOF) readfail(ar,fn,"skipped member data");
    memberlen--;
  }
}  *)
function TDebianPackage.SkipMember(Strm: TStream; memberlen: integer): boolean;
begin
    Result := false;
    Inc(memberlen, (memberlen and 1)); 
    if Strm.Position + memberlen > Strm.Size then exit;
    Strm.Seek(memberlen, soFromCurrent);
    Result := true;   
end;    

//return the number of members found
function TDebianPackage.ReadFromFile(DebPkgFile: string): integer;
var
    debStrm: TFileStream;
    MagicHeaderBuf: array[0..10] of char;
    //memberbuf: PChar;
    verinfobuf : array [0..100] of char;
    arh: ar_hdr;
    memberlen, memberidx: integer;
    n: integer;
begin
    Result := 0;
    SetLength(FMemberList, 0);
    
    //if not Assigned(DebStrm) then raise Exception.Create('Stream not assigned');
    if not FileExists(DebPkgFile) then raise Exception.Create('File not exists!');
    FFileName := DebPkgFile;
    debStrm := TFileStream.Create(DebPkgFile, fmOpenRead or fmShareDenyWrite);

  try    
    //debStrm.LoadFromFile(DebPkgFile);
    if DebStrm.Size < sizeof(ARMAG) + 2*sizeof(ar_hdr) then
        raise Exception.Create('Size of file is too small. maybe its not a debian package');
    
    DebStrm.Seek(0, soFromBeginning); //rewind
    DebStrm.Read(MagicHeaderBuf, SARMAG);
    if StrLComp(MagicHeaderBuf, ARMAG, SARMAG)=0 then //if MagicHeaderBuf='!<arch>\n'
    begin
        memberidx:=0;
        repeat
            n := DebStrm.Read(arh, sizeof(arh));
            if n=0 then break
            else if n<sizeof(ar_hdr) then raise Exception.Create('corrputed package');
            (* if (memcmp(arh.ar_fmag,ARFMAG,sizeof(arh.ar_fmag)))
                    ohshit("file `%.250s' is corrupt - bad magic at end of first header",debar); *)
            if StrLComp(arh.ar_fmag, ARFMAG, sizeof(arh.ar_fmag))<>0 then
                raise Exception.Create('bad magic at end of first header');

            memberlen := ParseHeaderLength(arh.ar_size, sizeof(arh.ar_size));
            if memberlen<0 then  raise Exception.Create('corrputed package'); 
                //ohshit("file `%.250s' is corrupt - negative member length %ld",debar,memberlen);            

            //save header (member info) into list
            SetLength(FMemberList, memberidx+1);
            FMemberList[memberidx] := arh;
            Inc(memberidx);

            if (memberidx=0) and FCheckHeader then //package header
            begin
                //GetMem(memberbuf, memberlen + 1);
                try
                    //if DebStrm.Read(memberbuf, memberlen + (memberlen and 1))<memberlen then exit; //failed to read header info member
                    if DebStrm.Read(verinfobuf, memberlen + (memberlen and 1))<memberlen then exit;
{$IFDEF GDEBUG}
                    SendDebug(StrPas(verinfobuf));
{$ENDIF}
                    //if CheckHeader(arh, memberbuf, memberlen) then exit;
                    if not DoCheckHeader(arh, verinfobuf, memberlen) then exit;
                finally
                    //FreeMem(memberbuf, memberlen + 1);
                end;
            end
            else
                if (Trim(arh.ar_name)='data.tar.gz') and (not ReadAfterDataMember) then
                    break
                else
                    SkipMember(DebStrm, memberlen)

        until (DebStrm.Position>=DebStrm.Size);
        Result := memberidx + 1;
    end
    else if StrLComp(MagicHeaderBuf,'!<arch>',7)=0 then
            raise Exception.Create('Bad magic header. maybe it''s not a debian package')
                //"file looks like it might be an archive which has been\n"
                //"corrupted by being downloaded in ASCII mode.\n"
    else if StrLComp(MagicHeaderBuf,'0.93',4)=0 then
            raise Exception.Create('Old format debian package not supported')
    else
            raise Exception.Create('Bad magic header. maybe it''s not a debian package');
  finally
    DebStrm.Free;
  end;
                      
end;

function TDebianPackage.ExtractMemberToStream(MemberIdx: integer; OutputStrm: TStream): boolean;
var
    idx, memberlen: integer;
    DebStrm : TFileStream;
    arh: ar_hdr;
begin
    Result := false;
    if not Assigned(OutputStrm) then exit;
    if MemberIdx > High(FMemberList) then exit;

    DebStrm := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);

  try    
    DebStrm.Seek(SARMAG, soFromBeginning); //rewind to the first member 
    idx := 0;
    while(idx<=memberidx) do
    begin
            arh := FMemberList[idx];
            if DebStrm.Read(arh, sizeof(arh))<sizeof(ar_hdr) then 
                raise Exception.Create('corrputed package');
            memberlen := ParseHeaderLength(arh.ar_size, sizeof(arh.ar_size));
            if memberlen<0 then  raise Exception.Create('corrputed package'); 
                    //ohshit("file `%.250s' is corrupt - negative member length %ld",debar,memberlen);            
                                
            //if (idx=1) then //header
            //    if ReadControlFile(DebStrm, arh, memberlen)<0 then raise....
            if (idx=MemberIdx) then
            begin
                if OutputStrm.CopyFrom(DebStrm, memberlen)<memberlen then exit;
                Result := True;
                break;
            end
            else                      
                SkipMember(DebStrm, memberlen); 

           Inc(idx);
    end;
  finally
    DebStrm.Free;
  end;
            
end;

function TDebianPackage.ExtractMemberToFile(idx: integer; OutputFile: string): boolean;
var
    AFileStrm: TFileStream;
begin
    Result := false;
    if idx>High(FMemberList) then exit;
    
    AFileStrm := TFileStream.Create(OutputFile, fmCreate or fmOpenWrite	or fmShareDenyWrite);
    try
        Result := ExtractMemberToStream(idx, AFileStrm);
    finally
        AFileStrm.Free;
    end;
end;    

function TDebianPackage.ParseHeaderLength(inh: PChar;  Len: integer): integer;
(*static unsigned long parseheaderlength(const char *inh, size_t len,
                                       const char *fn, const char *what) {
  char lintbuf[15];
  unsigned long r;
  char *endp;

  if (memchr(inh,0,len))
    ohshit("file `%.250s' is corrupt - %.250s length contains nulls",fn,what);
  assert(sizeof(lintbuf) > len);
  memcpy(lintbuf,inh,len);
  lintbuf[len]= ' ';
  *strchr(lintbuf,' ')= 0;
  r= strtoul(lintbuf,&endp,10);
  if ( *endp )
    ohshit("file `%.250s' is corrupt - bad digit (code %d) in %s",fn,*endp,what);
  return r; *)
var
   lintbuf: array[0..14] of char;
begin
   if len> sizeof(lintbuf) then raise Exception.Create('ParseMemberLength');
   StrLCopy(lintbuf, inh, len);
   lintbuf[len] := #0;
   StrScan(lintbuf, ' ')^ := #0;
   Result := StrToInt(StrPas(lintbuf));
end;


//return the length of control.tar.gz
(*function TDebianPackage.CheckControlFile(DebStrm: TMemoryStream; arh: ar_hdr; memberlen: integer; 
        OutputFile: String): integer;
begin
    
end; *)

//if any error encounted, return false
function TDebianPackage.DoCheckHeader(arh: ar_hdr; infobuf: PChar; memberlen: integer): boolean;
const
    DebianSign = 'debian-binary   ';
var
    verinfobuf: array[0..20] of char;
    cur: PChar;
begin
    Result := false;
   
    if StrLComp(arh.ar_name, DebianSign, sizeof(arh.ar_name))<>0 then exit;
          //ohshit("file `%.250s' is not a debian binary archive (try dpkg-split?)",debar);
    //memberlen =  ParseArchiveLength(arh.ar_size,sizeof(arh.ar_size));
       
    infobuf[memberlen] := #0;
    
    cur := StrScan(infobuf, #10);
    if (cur=nil) then exit;  //ohshit("archive has no newlines in header");
    cur^ := #0;
    
    cur := StrScan(infobuf,'.');
    if (cur=nil) then exit; //ohshit("archive has no dot in version number");
    cur^ := #0;
    
    if (StrComp(infobuf,'2')<>0) then exit;
          //ohshit("archive version %.250s not understood, get newer " BACKEND, infobuf);
    cur^ := '.'; //restore version delimiter
       
    //StrLCopy(verinfobuf, infobuf, min(sizeof(verinfobuf)-1, memberlen); //got the package version info
    StrLCopy(verinfobuf, infobuf, sizeof(verinfobuf)-1);
    verinfobuf[sizeof(verinfobuf)-1] := #0; 
    FPkgVersion := StrPas(verinfobuf);
    
    Result := true;
        
end;

constructor TDebianPackage.Create;
begin
    CheckHeader := True;
    ReadAfterDataMember := false;
end;

end.