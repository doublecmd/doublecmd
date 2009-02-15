unit UnTar;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes,
  {$IFDEF ABOUTDLG}DsgnIntf, {$ENDIF}tarfile;

CONST BUFSIZE = 512 * 128; // 512 = SECSIZE in unit tarfile

type
  TZeroHundred = 0..100;
  TOverwriteMode = ( omSkip, omRename, omReplace );
  TNextFile = record
                name : string;
                size : longint;
                timestamp : TDateTime
  end;

{$IFDEF ABOUTDLG}
  TAboutProperty = class(TPropertyEditor)
  public
	procedure Edit; override;
	function GetAttributes: TPropertyAttributes; override;
	function GetValue: string; override;
  end; 
{$ENDIF}  

  TUnTar = class(TComponent)
  private
    { Private declarations }
{$IFDEF ABOUTDLG}    
    FAbout : TAboutProperty;
{$ENDIF}    
    FOnNextFile : TNotifyEvent;
    FNextFile : TNextFile;
    FCreateEmptyDir: boolean;
    FOnExtractOverwrite : TNotifyEvent;
    FOnProgress : TNotifyEvent;
    FProgress : integer;
    FProgressStep : TZeroHundred;
    FOverwriteMode : TOverwriteMode;
    FOverwriteThisTime : TOverwriteMode;
    FOverwriteFilename : String;
    FFileSource : string;
    FUnpackPath : string;
    FNewFileName : string;
    procedure DoProgress( tarfile : TTarFile);
    procedure CreateNextFile( tarfile: TTarfile);
    function TranslateDate(dt: TDateTimeRec): longint;
  protected
    { Protected declarations }
    procedure DoOnNextFile; virtual;
    procedure DoOnExtractOverwrite; virtual;
    procedure DoOnProgress; virtual;
  public
    { Public declarations }
    constructor Create( AOwner: TComponent); override;
//    destructor Free;
    procedure UnTar;
    procedure UnTarSelected( list: TStringList);
    procedure GetInfo;
    property Progress : integer
		 read FProgress;
    property NextFile : TNextfile
                 read FNextFile;
    property OverwriteThisTime : TOverwriteMode
                 read FOverwriteThisTime write FOverwriteThisTime;
    property OverwriteFilename : String
                 read FOverwriteFilename write FOverwriteFilename;
  published
    { Published declarations }
{$IFDEF ABOUTDLG}    
    property About: TAboutProperty	 read FAbout write FAbout;
{$ENDIF}    		 
    property FileSource : String
		 read FFileSource write FFileSource;
  	property UnpackPath : String
		 read FUnpackPath write FUnpackPath;
    property ProgressStep : TZeroHundred
		 read FProgressStep write FProgressStep;
	property OnProgress : TNotifyEvent
		 read FOnProgress write FOnProgress;
    Property OverwriteMode : TOverwriteMode
                 read FOverwriteMode write FOverwriteMode;
    Property CreateEmptyDir: boolean
                 read FCreateEmptyDir write FCreateEmptyDir;
    Property OnExtractOverwrite : TNotifyEvent
                 read FOnExtractOverwrite write FOnExtractOverwrite;
    Property OnNextFile : TNotifyEvent
                 read FOnNextFile write FOnNextFile;

  end;

procedure Register;

implementation

{$IFDEF ABOUTDLG}uses utils;{$ENDIF}

{$IFDEF ABOUTDLG}
procedure TAboutProperty.Edit;
var utils : TUtils;
begin
   ShowMessage(utils.CreateAboutMsg('DelphiUnTar'))
end;

function TAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutProperty.GetValue: string;
begin
  Result := 'DelphiUnTar';
end;
{$ENDIF}

constructor TUnTar.Create( AOwner: TComponent);
begin
   inherited Create( AOwner);
   FFileSource := '';
   FUnpackPath := '';
   FProgressStep := 0;
   FCreateEmptyDir := false;
   //FOverwriteMode := omRename
   FOverwriteMode := omReplace;
end;

procedure TUnTar.DoOnNextFile;
begin
   if Assigned (FOnNextFile) then
      FOnNextFile (self)
end;

procedure TUnTar.DoOnExtractOverwrite;
begin
     if Assigned (FOnExtractOverwrite) then
        FOnExtractOverwrite (self)
end;

procedure TUnTar.DoOnProgress;
begin
   if Assigned (FOnProgress) then
      FOnProgress (self)
end;

procedure TUnTar.DoProgress( tarfile : TTarFile);
var dummy : integer;
begin
   if FProgressStep > 0 then
   begin
      dummy := tarfile.Progress;
      if (dummy >= FProgress + FProgressStep) or
         (dummy = 100) then
      begin
         FProgress := dummy - (dummy mod FProgressStep);
         if dummy = 100 then FProgress := dummy;
         DoOnProgress
      end
   end
end;

function TUnTar.TranslateDate( dt : TDateTimeRec) : longint;
begin
   Result := DateTimeToFileDate(
                EncodeDate( dt.year, dt.month, dt.day) +
                EncodeTime( dt.hour, dt.min, dt.sec, 0))
end;

procedure TUnTar.CreateNextFile( tarfile: TTarfile);
type TBuffer = Array [0..Pred(BUFSIZE)] Of byte;
var outfiledir: string;
    outf: TFileStream;
    iread: longint;
    buffer: TBuffer;
begin
   outfileDir := ExtractFileDir(FNextFile.name);
   // Check if sub-dir exists, if not create
   if not(DirectoryExists(outfileDir)) and (outfileDir<>'') then
   begin
      outfileDir := ExpandFileName(outfileDir);
      ForceDirectories(outfileDir);
   end;

   if outfileDir <> '' then outfileDir := outfileDir + PathDelim;
   FNewFilename := outfileDir+ExtractFileName(FNextFile.name);

   FOverwriteThisTime := omRename;
   while (FileExists( FNewFilename)) and
         (FOverwriteMode = omRename)and
         (FOverwriteThisTime = omRename) do
   begin
      FOverwriteFilename := '';
      // Raise event to ask what should be done
      DoOnExtractOverwrite;
      if (FOverwriteThisTime = omRename) and
         (FOverwriteFilename <> '') then
             FNewFilename := FOverwriteFilename
   end;

   if (not FileExists( FNewFilename)) or
      (FOverwriteMode = omReplace) or
      (FOverwriteThisTime = omReplace) then
   begin
      outf := TFileStream.Create(FNewFilename, fmCreate or fmShareDenyWrite);

      while FNextFile.size > 0 do
      begin
         iread := tarfile.ReadFile( buffer, BUFSIZE);
         outf.Write( buffer, iread);
         FNextFile.size := FNextFile.size - iread;
         DoProgress(tarfile)
      end;
      FileSetDate(outf.Handle, DateTimeToFileDate(FNextFile.timestamp));
      outf.Free
   end else
   begin
      tarfile.SkipFile;   // We do not need the file
      DoProgress(tarfile)
   end
end;

procedure TUnTar.UnTar;
var oldDir, outfileDir : string;
    tarfile : TTarFile;
begin
   FProgress := 0;
   oldDir := getCurrentDir;
   // check if destination-path exists
   if FUnpackPath <> '' then
   begin
      if not(DirectoryExists(FUnpackPath)) then
         ForceDirectories(FUnpackPath);
            setCurrentDir(FUnpackPath);
   end;

   tarfile := TTarFile.Create( FFileSource);
   DoProgress(tarfile);
   while not( tarfile.EOF) do
   begin
      FNextFile.name := tarfile.GetNextFilename;
      DoProgress( tarfile);
      outfileDir := ExtractFileDir(FNextFile.name);

      if FCreateEmptyDir then
         // Check if sub-dir exists, if not create
         if not(DirectoryExists(outfileDir)) and (outfileDir<>'') then
         begin
            outfileDir := ExpandFileName(outfileDir);
            ForceDirectories(outfileDir);
         end;

      FNextFile.size := tarfile.GetNextSize;
      if FNextFile.size > 0 then
      begin
         //FNextFile.timestamp := tarfile.GetNextDate;
         FNextFile.timestamp := TranslateDate(tarfile.GetNextDate);

         DoOnNextFile;   // raise event that we start with new file
                         // Info is now read and in FNextFile
         // Create the file
         CreateNextFile(tarfile)
      end
   end;
   DoProgress( tarfile);
   tarfile.Free;

   setCurrentDir(oldDir)
end;

procedure TUnTar.UnTarSelected( list: TStringList);
var oldDir, outfileDir : string;
    tarfile : TTarFile;
begin
   FProgress := 0;
   oldDir := getCurrentDir;
   // check if destination-path exists
   if FUnpackPath <> '' then
   begin
      if not(DirectoryExists(FUnpackPath)) then
         ForceDirectories(FUnpackPath);
            setCurrentDir(FUnpackPath);
   end;

   tarfile := TTarFile.Create( FFileSource);
   DoProgress(tarfile);
   while not( tarfile.EOF) do
   begin
      FNextFile.name := tarfile.GetNextFilename;
      DoProgress( tarfile);
      outfileDir := ExtractFileDir(FNextFile.name);

      FNextFile.size := tarfile.GetNextSize;
      if FNextFile.size > 0 then
      begin
         //FNextFile.timestamp := tarfile.GetNextDate;
         FNextFile.timestamp := TranslateDate(tarfile.GetNextDate);

         if list.IndexOf(FNextFile.Name) > -1 then
         begin
            DoOnNextFile;   // raise event that we start with new file
                            // Info is now read and in FNextFile
            // Create the file
            CreateNextFile(tarfile)
         end else
            tarFile.SkipFile
      end
   end;
   DoProgress( tarfile);
   tarfile.Free;

   setCurrentDir(oldDir)
end;

procedure TUnTar.GetInfo;
var tarfile : TTarFile;
begin
   FProgress := 0;
   tarfile := TTarFile.Create( FFileSource);
   DoProgress( tarfile);
   while not( tarfile.EOF) do
   begin
      FNextFile.name := tarfile.GetNextFilename;
      FNextFile.size := tarfile.GetNextSize;
      DoProgress( tarfile);
      if FNextFile.size > 0 then
      begin
         //FNextFile.timestamp := tarfile.GetNextDate;
         FNextFile.timestamp := TranslateDate(tarfile.GetNextDate);
         tarfile.SkipFile;
         DoOnNextFile;
         DoProgress( tarfile)
      end
   end;
   DoProgress( tarfile);
   tarfile.Free;
end;

procedure Register;
begin
  RegisterComponents('Samples', [TUnTar]);
{$IFDEF ABOUTDLG}  
  RegisterPropertyEditor(TypeInfo(TAboutProperty), TUnTar, 'ABOUT', TAboutProperty);
{$ENDIF}  
end;

end.
