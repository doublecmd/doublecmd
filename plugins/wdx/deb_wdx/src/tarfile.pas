unit tarfile;

interface

uses classes, sysutils, math;

const EXP_FILENAME = 0;
      EXP_SIZE     = 1;
      EXP_DATE     = 2;
      EXP_BODY     = 3;
      EXP_ERROR    = 4;
      EXP_EOF      = 5;

      SECSIZE = 512;
//      SECSPERBLOCK = 120;
      BUFSIZE = SECSIZE;  // * SECSPERBLOCK;

type
    TBuffer = Array [0..Pred(BUFSIZE)] Of byte;
    TDateTimeRec = record 
                sec : integer;
                min : integer;
                hour : integer;
                day : integer;
                month : integer;
                year : integer;
    end; 

    TTarFile = class
    private
       FTarF : TFileStream;
       FExpecting : byte;
       FName : string;
       FBuffer : TBuffer;
       FLen : longint;
       FUnreadSec : integer;
       function CrackUnixDateTime( UnixDate : longint) : TDateTimeRec;
       procedure AdjustFilename( var filename : string);
    public
       constructor Create( filename : string);
       destructor Free;
       function EOF : boolean;
       function Progress : integer;
       function GetNextFilename : string;
       function GetNextSize : longint;
       function GetNextDate : TDateTimeRec;
       function ReadFile( var buffer; maximum : longint) : longint;
       Procedure SkipFile;
    protected
end;

implementation

// **************************************************
// Private part
// **************************************************
{$WRITEABLECONST ON}

function TTarFile.CrackUnixDateTime( UnixDate : longint) : TDateTimeRec;
Const monlen : Array [1..12] Of byte
               = (31,28,31,30,31,30,31,31,30,31,30,31);
var dt : TDateTimeRec;
begin
   dt.sec := UnixDate mod 60;
   UnixDate := UnixDate div 60;
   dt.min := UnixDate mod 60;
   UnixDate := UnixDate div 60;
   dt.hour := UnixDate mod 24;
   UnixDate := UnixDate div 24;

   dt.year := 1970;
   while ((UnixDate>=365) and (dt.year mod 4 <> 0)) or
         ((UnixDate>=366) and (dt.year mod 4 = 0 )) do
   begin
      if dt.year mod 4 = 0 then UnixDate := UnixDate - 1;
      UnixDate := UnixDate - 365;
      Inc(dt.year)
   end;

   dt.month := 1;
   if dt.year mod 4 = 0 then Inc(monlen[2]);
   while UnixDate>=monlen[dt.month] do
   begin
      UnixDate := UnixDate - monlen[dt.month];
      Inc(dt.month)
   end;
   if dt.year mod 4 = 0 then Dec(monlen[2]);

   dt.day := UnixDate + 1;

   Result := dt
end;

Procedure TTarFile.AdjustFilename(Var filename : string);

Const badletter : Set Of char = ['+',' ',':','<','>','|'];
Var i : byte;
Begin                                                           { openfile }
   For i := Length(filename) DownTo 1 Do
   Begin
      If filename[i] = '/' Then filename[i] := '\';
      If filename[i] In badletter Then filename[i] := '_';
   End
end;

// **************************************************
// Public part
// **************************************************

constructor TTarFile.Create( filename : string);
begin
   FTarF := TFileStream.Create( filename, fmOpenRead or fmShareDenyWrite);
end;

destructor TTarFile.Free;
begin
   FTarF.Free;
end;

function TTarFile.EOF : boolean;
begin
   EOF := FTarF.Size = FTarF.Position;
end;

function TTarFile.Progress : integer;
begin
   Progress := Floor((FTarF.Position / FTarF.Size) * 100)
end;

function TTarFile.GetNextFilename : string;
var iread : integer;
    i : integer;
begin
   FName := '';
   if (not(EOF) and (FExpecting = EXP_FILENAME)) then
   begin
      iread := FTarF.Read( FBuffer, SECSIZE);
      If iread <> SECSIZE Then FExpecting := EXP_ERROR
      else begin
           i := 0;
           While (FBuffer[i] <> 0) And (i < 254) Do
           begin
              FName := FName + char(FBuffer[i]);
              Inc(i);
           end;
           if i > 0 then
           begin
              FExpecting := EXP_SIZE;
              AdjustFilename( FName)
           end
           else begin
              i := 0;
              // Lazy evaluation needed to prvent reading from FBuffer[SECSIZE]
              while (i < SECSIZE) and (FBuffer[i]=0) do Inc(i);
              if i < SECSIZE then
                 FExpecting := EXP_FILENAME
              else begin
                 FExpecting := EXP_EOF;
                 FTarF.Position := FTarF.Size
              end
           end
      end
   end;
   Result := FName;
end;

function TTarFile.GetNextSize : longint;
var i : byte;
begin
   FLen := 0;
   GetNextSize := 0;
   if (not(EOF) and (FExpecting = EXP_SIZE)) then
   begin
      For i := $7C To $86 Do
          If (FBuffer[i] >= 48) And (FBuffer[i] <= 55) Then
                    FLen := 8*FLen + FBuffer[i] - 48;
      if FLen > 0 then
         FExpecting := EXP_DATE
      else
         FExpecting := EXP_FILENAME;

      GetNextSize := FLen
   end;
   FUnreadSec := (SECSIZE - (FLen mod SECSIZE)) mod SECSIZE
end;

function TTarFile.GetNextDate : TDateTimeRec;
var UnixDate : longint;
    i : byte;
begin
   UnixDate := 0;
   if FExpecting = EXP_DATE then
   begin
      For i := $88 To $92 Do
          If (FBuffer[i] >= 48) And (FBuffer[i] <= 55) Then
                      UnixDate := 8*UnixDate + FBuffer[i] - 48;

      FExpecting := EXP_BODY
   end;
   Result := CrackUnixDateTime( UnixDate)
end;

function TTarFile.ReadFile( var buffer; maximum : longint) : longint;
var iread : longint;
    buff : TBuffer;
begin
   iread := 0;
   if (FLen > FTarF.Size - FTarF.Position) or
      (FExpecting <> EXP_BODY)
   then FExpecting := EXP_ERROR
   else begin
      iread := FTarF.Read( buffer, min(maximum,FLen));
      FLen := FLen - iread;
      if FLen = 0 then
      begin
           FExpecting := EXP_FILENAME;
           if FUnreadSec > 0 then FTarF.Read( buff, FUnreadSec)
      end
   end;
   ReadFile := iread
end;

procedure TTarFile.SkipFile;
begin
   if (FLen > FTarF.Size - FTarF.Position) or
      (FExpecting <> EXP_BODY)
   then FExpecting := EXP_ERROR
   else begin
      FTarF.Position := FTarF.Position + FLen + FUnreadSec;
      FExpecting := EXP_FILENAME
   end
end;

end.
