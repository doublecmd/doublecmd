unit uLog;

interface
{$DEFINE LOG} // this temporally

procedure logWrite(const sText:String);


implementation
{$IFDEF LOG}
uses
  SysUtils;
const
  cLogName='./sc.log';
{$ENDIF}

procedure logWrite(const sText:String);
{$IFDEF LOG}
var
  lf:TextFile;
{$ENDIF}
begin
{$IFDEF LOG}
  assignFile(lf,cLogName);
  try
    if FileExists(cLogName) then
      Append(lf)
    else
      rewrite(lf);  
    writeln(lf,Format('%s %s',[DateTimeToStr(now),sText]));
    writeln(Format('%s %s',[DateTimeToStr(now),sText]));
    CloseFile(lf);
  except
    on E:Exception do
      writeln('error writing to log:'+E.Message);
  end;
{$ENDIF}
end;
end.
