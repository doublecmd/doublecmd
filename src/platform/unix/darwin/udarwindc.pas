unit uDarwinDC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uGlobs,
  uFileSourceWatcher,
  uDarwinFSWatch;

type

  { TDarwinFSWatcherUtil }

  TDarwinFSWatcherUtil = class
    class function convertToFileSourceEvent(
      const event: TDarwinFSWatchEvent;
      var fileSourceEvent: TFSWatcherEventData ): Boolean;
  end;

implementation

{ TDarwinFSWatcherUtil }

class function TDarwinFSWatcherUtil.convertToFileSourceEvent(
  const event: TDarwinFSWatchEvent;
  var fileSourceEvent: TFSWatcherEventData ): Boolean;
begin
  Result:= False;
  if [watch_file_name_change, watch_attributes_change] * gWatchDirs = [] then exit;
  if event.isDropabled then exit;

  fileSourceEvent.Path := event.watchPath;
  fileSourceEvent.FileName := EmptyStr;
  fileSourceEvent.NewFileName := EmptyStr;
  fileSourceEvent.OriginalEvent := event;
  fileSourceEvent.EventType := fswUnknownChange;

  if TDarwinFSWatchEventCategory.ecRootChanged in event.categories then begin
    fileSourceEvent.EventType := fswSelfDeleted;
  end else if event.fullPath.Length >= event.watchPath.Length+2 then begin
    // 1. file-level update only valid if there is a FileName,
    //    otherwise keep directory-level update
    // 2. the order of the following judgment conditions must be preserved
    if (not (watch_file_name_change in gWatchDirs)) and
       ([ecStructChanged, ecAttribChanged] * event.categories = [ecStructChanged])
         then exit;
    if (not (watch_attributes_change in gWatchDirs)) and
       ([ecStructChanged, ecAttribChanged] * event.categories = [ecAttribChanged])
         then exit;

    fileSourceEvent.FileName := ExtractFileName( event.fullPath );

    if TDarwinFSWatchEventCategory.ecRemoved in event.categories then
      fileSourceEvent.EventType := fswFileDeleted
    else if TDarwinFSWatchEventCategory.ecRenamed in event.categories then begin
      if ExtractFilePath(event.fullPath)=ExtractFilePath(event.renamedPath) then begin
        // fswFileRenamed only when FileName and NewFileName in the same dir
        // otherwise keep fswUnknownChange
        fileSourceEvent.EventType := fswFileRenamed;
        fileSourceEvent.NewFileName := ExtractFileName( event.renamedPath );
      end;
    end else if TDarwinFSWatchEventCategory.ecCreated in event.categories then
      fileSourceEvent.EventType := fswFileCreated
    else if TDarwinFSWatchEventCategory.ecAttribChanged in event.categories then
      fileSourceEvent.EventType := fswFileChanged
    else
      exit;
  end;

  Result:= True;
end;

end.

