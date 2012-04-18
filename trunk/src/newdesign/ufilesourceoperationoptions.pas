unit uFileSourceOperationOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  TFileSourceOperationOptionGeneral =
    (fsoogNone, fsoogYes, fsoogNo);

  TFileSourceOperationOptionSymLink =
    (fsooslNone, fsooslFollow, fsooslDontFollow);

  TFileSourceOperationOptionFileExists =
    (fsoofeNone, fsoofeSkip, fsoofeOverwrite, fsoofeOverwriteOlder, fsoofeAppend, fsoofeResume);

  TFileSourceOperationOptionDirectoryExists =
    (fsoodeNone, fsoodeSkip, fsoodeDelete, fsoodeCopyInto);

  TFileSourceOperationOptionSetPropertyError =
    (fsoospeNone, fsoospeDontSet, fsoospeIgnoreErrors);

implementation

end.

