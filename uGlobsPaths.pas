unit uGlobsPaths;

interface
var

  gpExePath:String ='';
  gpIniDir:String =''; // local for user
  gpCfgDir:String =''; // global for all user
  gpLngDir:String =''; // path to language *.po files
  gpPixmapPath:String ='';
  
procedure LoadPaths;

implementation
uses
  SysUtils;

procedure LoadPaths;
begin
  gpExePath:=ExtractFilePath(ParamStr(0));
  Writeln('executable directory:',gpExePath);

//  gpExePath:=gpExePath+DirectorySeparator;
  gpIniDir:=gpExePath;
  gpCfgDir:=gpExePath;
  gpLngDir:=gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath:= gpExePath + 'pixmaps' + DirectorySeparator;
end;

end.
