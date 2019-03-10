unit uCmdLineParams;

{$mode objfpc}{$H+}

interface

type
  TCommandLineParams = packed record
    NewTab: Boolean;
    NoSplash: Boolean;
    ActivePanelSpecified: Boolean;
    ActiveRight: Boolean;
    LeftPath: array[0..1023] of AnsiChar;
    RightPath: array[0..1023] of AnsiChar;
    ActivePanelPath: array[0..1023] of AnsiChar;
    Client: Boolean;
    Servername: array[0..1023] of AnsiChar;
  end;

procedure ProcessCommandLineParams;

var
  CommandLineParams: TCommandLineParams;

implementation

uses
  Forms, Dialogs, SysUtils, uOSUtils, uDCUtils, uGlobsPaths, getopts, uDebug,
  uLng, uClipboard, uAdministrator, DCStrUtils;

function DecodePath(const Path: String): String;
begin
  Result := TrimQuotes(Path);
  if Pos(fileScheme, Result) = 1 then
  begin
    Result:= URIDecode(Copy(Result, 8, MaxInt));
  end;
  Result:= GetAbsoluteFileName(IncludeTrailingBackslash(GetCurrentDir), Result);
end;

procedure ProcessCommandLineParams;
var
  Option: AnsiChar = #0;
  OptionIndex: LongInt = 0;
  Options: array[1..6] of TOption;
  OptionUnknown: String;
begin
  FillChar(Options, SizeOf(Options), #0);
  with Options[1] do
  begin
    Name:= 'no-console';
  end;
  with Options[2] do
  begin
    Name:= 'config-dir';
    Has_arg:= 1;
  end;
  with Options[3] do
  begin
    Name:= 'client';
  end;
  with Options[4] do
  begin
    Name:= 'servername';
    Has_arg:= 1;
  end;
  with Options[5] do
  begin
    Name:= 'no-splash';
  end;
  with Options[6] do
  begin
    Name:= 'operation';
    Has_arg:= 1;
  end;
  FillChar(CommandLineParams, SizeOf(TCommandLineParams), #0);
  repeat
    try
      Option:= GetLongOpts('L:l:R:r:P:p:TtCc', @Options[1], OptionIndex);
    except
      MessageDlg(Application.Title, rsMsgInvalidCommandLine, mtError, [mbOK], 0, mbOK);
      Exit;
    end;
    case Option of
      #0:
        begin
          case OptionIndex of
            1:
              begin
                {$IF DEFINED(NIGHTLY_BUILD)}
                HideConsoleWindow;
                {$ENDIF}
              end;
            2:
              begin
                gpCmdLineCfgDir:= ParamStrU(TrimQuotes(OptArg));
              end;
            3:
              begin
                CommandLineParams.Client:= True;
                CommandLineParams.NoSplash:= True;
              end;
            4:
              begin
                CommandLineParams.Servername:= ParamStrU(TrimQuotes(OptArg));
              end;
            5:
              begin
                CommandLineParams.NoSplash:= True;
              end;
            6:
              begin
                ExecuteOperation(ParamStrU(TrimQuotes(OptArg)));
              end;
          end;
        end;
      'L', 'l': CommandLineParams.LeftPath:= DecodePath(ParamStrU(OptArg));
      'R', 'r': CommandLineParams.RightPath:= DecodePath(ParamStrU(OptArg));
      'P', 'p': begin
        CommandLineParams.ActivePanelSpecified:= True;
        CommandLineParams.ActiveRight:= (UpperCase(OptArg) = 'R');
      end;
      'T', 't': CommandLineParams.NewTab:= True;
      'C', 'c': begin
        CommandLineParams.Client:= True;
        CommandLineParams.NoSplash:= True;
      end;
      '?', ':': DCDebug ('Error with opt : ', OptOpt);
    end; { case }
  until Option = EndOfOptions;

  if OptInd <= ParamCount then
  begin
    // If also found one parameter then use it as path of active panel
    if ParamCount - OptInd = 0 then
      begin
        CommandLineParams.ActivePanelPath:= DecodePath(ParamStrU(OptInd));
        Inc(OptInd, 1);
      end
    // If also found two parameters then use it as paths in panels
    else if ParamCount - OptInd = 1 then
      begin
        CommandLineParams.LeftPath:= DecodePath(ParamStrU(OptInd));
        CommandLineParams.RightPath:= DecodePath(ParamStrU(OptInd + 1));
        Inc(OptInd, 2);
      end;
    // Unknown options, print to console
    if OptInd <= ParamCount then
    begin
      while OptInd <= ParamCount do
      begin
        OptionUnknown:= ParamStrU(OptInd) + ' ';
        Inc(OptInd)
      end;
      DCDebug ('Non options : ', OptionUnknown);
    end;
  end;
end;

end.
