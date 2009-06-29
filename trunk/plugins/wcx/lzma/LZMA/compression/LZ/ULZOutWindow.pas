unit ULZOutWindow;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes;

type TLZOutWindow=class
       public
         buffer: array of byte;
         pos:integer;
         windowSize:integer;
         streamPos:integer;
         stream:TStream;
         procedure _Create(const windowSize:integer);
         procedure SetStream(const stream:TStream);
         procedure ReleaseStream;
         procedure Init(const solid:boolean);
         procedure Flush;
         procedure CopyBlock(const distance:integer; len:integer);
         procedure PutByte(const b:byte);
         function GetByte(const distance:integer):byte;
       end;

implementation

procedure TLZOutWindow._Create(const windowSize:integer);
begin
if (length(buffer)=0) or (self.windowSize <> windowSize) then
   setlength(buffer,windowSize);
self.windowSize := windowSize;
pos := 0;
streamPos := 0;
end;

procedure TLZOutWindow.SetStream(const stream:TStream);
begin
ReleaseStream;
self.stream:=stream;
end;

procedure TLZOutWindow.ReleaseStream;
begin
flush;
self.stream:=nil;
end;

procedure TLZOutWindow.Init(const solid:boolean);
begin
if not solid then begin
   streamPos:=0;
   Pos:=0;
   end;
end;

procedure TLZOutWindow.Flush;
var size:integer;
begin
size := pos - streamPos;
if (size = 0) then
   exit;
stream.write(buffer[streamPos], size);
if (pos >= windowSize) then
   pos := 0;
streamPos := pos;
end;

procedure TLZOutWindow.CopyBlock(const distance:integer;len:integer);
var pos:integer;
begin
pos := self.pos - distance - 1;
if pos < 0 then
   pos := pos + windowSize;
while len<>0 do begin
      if pos >= windowSize then
         pos := 0;
      buffer[self.pos] := buffer[pos];
      inc(self.pos);
      inc(pos);
      if self.pos >= windowSize then
         Flush();
    dec(len);
    end;
end;

procedure TLZOutWindow.PutByte(const b:byte);
begin
buffer[pos] := b;
inc(pos);
if (pos >= windowSize) then
   Flush();
end;

function TLZOutWindow.GetByte(const distance:integer):byte;
var pos:integer;
begin
pos := self.pos - distance - 1;
if (pos < 0) then
   pos := pos + windowSize;
result:=buffer[pos];
end;

end.
