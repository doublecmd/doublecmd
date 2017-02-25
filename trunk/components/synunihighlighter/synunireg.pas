unit SynUniReg;

(*

Tom Lisjac <vlx@users.sourceforge.net> http://theseus.sf.net
  Initially adapted for use with Lazarus and FPC - 2003-06-12
  Changes can be found by searching for: ////TL
  Issues that need review are flagged with one or more ! suffixes after TL
*)
interface

{$I SynEdit.inc}

uses
  ////TL DsgnIntf,
  componenteditors, ////TL added
  Classes,
  SynUniHighlighter,
  SynUniDesigner;

type
  ////TL! TDefaultEditor doesn't appear to exist... changed to TDefaultComponentEditor
  ////TL! TSynUniEditor = class(TDefaultEditor)
  TSynUniEditor = class(TDefaultComponentEditor)
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  // ToDo: port the component editor to lazarus and register it
  //RegisterComponentEditor(TSynUniSyn, TSynUniEditor);
end;

{ TSynUniEditor }

procedure TSynUniEditor.Edit;
begin
  ////TL explicitly passed null string to the formerly optional parm
  TSynUniDesigner.EditHighlighter(Component as TSynUniSyn, '' );
end;

procedure TSynUniEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

////TL FPC wants resources defined globally... moved from the function below
resourcestring
  sEditUni = 'Edit ...';

function TSynUniEditor.GetVerb(Index: Integer): string;
////TL resourcestring
////TL  sEditUni = 'Edit ...';
begin
  Result := sEditUni;
end;

function TSynUniEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

