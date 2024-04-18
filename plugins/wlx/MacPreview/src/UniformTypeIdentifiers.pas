unit UniformTypeIdentifiers;

{$mode delphi}
{$modeswitch objectivec1}

interface

{$linkframework UniformTypeIdentifiers}

uses
  SysUtils, CocoaAll;

type
  UTType = objcclass external(NSObject, NSCopyingProtocol, NSSecureCodingProtocol)
  public
    class function typeWithIdentifier(identifier: NSString): id; message 'typeWithIdentifier:';
    class function typeWithFilenameExtension(filenameExtension: NSString): id; message 'typeWithFilenameExtension:';

    function identifier: NSString; message 'identifier';
    function conformsToType(type_: UTType): ObjCBOOL; message 'conformsToType:';

    // NSCopyingProtocol
    function copyWithZone(zone: NSZonePtr): id; message 'copyWithZone:';
    // NSCodingProtocol
    procedure encodeWithCoder(aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder(aDecoder: NSCoder): id; message 'initWithCoder:';
    // NSSecureCodingProtocol
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;

implementation

end.
