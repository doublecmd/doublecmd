unit uFileSourceOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  TFileSourceOperationOptionSymLink =
    (fsooslNone, fsooslFollow, fsooslDontFollow);

  TFileSourceOperationOptionFileExists =
    (fsoofeNone, fsoofeSkip, fsoofeOverwrite, fsoofeOverwriteOlder, fsoofeAppend);

  TFileSourceOperationOptionDirectoryExists =
    (fsoodeNone, fsoodeSkip, fsoodeDelete, fsoodeCopyInto);

  TFileSourceOperationOptionGeneral =
    (fsoogNone, fsoogYes, fsoogNo);

implementation

end.

