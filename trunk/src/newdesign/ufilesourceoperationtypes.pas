unit uFileSourceOperationTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  // Capabilities.
  // (or make a separate type TFileSourceCapability with fsc... ?)
  TFileSourceOperationType = (
    fsoList,
    fsoCopyIn,
    fsoCopyOut,
    fsoMove,            // Move/rename files within the same file source.
    fsoDelete,
    fsoWipe,
    fsoCreateDirectory,
    //fsoCreateFile,
    //fsoCreateLink,
    fsoCalcChecksum,
    fsoCalcStatistics,  // Should probably always be supported if fsoList is supported.
    fsoSetFileProperty,
    fsoSetDateTime,
    fsoSetAttribute,
    fsoExecute
  );

  TFileSourceOperationTypes = set of TFileSourceOperationType;

implementation

end.

