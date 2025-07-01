unit uDarwinFileUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils;

type

  { TDarwinFileUtil }

  TDarwinFileUtil = class
    class function cloneFile( const fromPath: String; const toPath: String ): Boolean;
  end;

implementation

type
  copyfile_state_t_o = record
  end;
  copyfile_state_t = ^copyfile_state_t_o;
  copyfile_flags_t = UInt32;

  function copyfile( const fromPath: pchar; const toPath: pchar; state: copyfile_state_t; flags: copyfile_flags_t ): Integer;
    cdecl; external name 'copyfile';

const
  COPYFILE_ACL   = 1 shl 0;
  COPYFILE_STAT	 = 1 shl 1;
  COPYFILE_XATTR = 1 shl 2;
  COPYFILE_DATA	 = 1 shl 3;

  COPYFILE_SECURITY = COPYFILE_STAT or COPYFILE_ACL;
  COPYFILE_METADATA = COPYFILE_SECURITY or COPYFILE_XATTR;
  COPYFILE_ALL	    = COPYFILE_METADATA or COPYFILE_DATA;

  COPYFILE_UNLINK      = 1 shl 21;
  COPYFILE_CLONE       = 1 shl 24;
  COPYFILE_CLONE_FORCE = 1 shl 25;

{ TDarwinFileUtil }

class function TDarwinFileUtil.cloneFile( const fromPath: String; const toPath: String ): Boolean;
var
  ret: Integer;
begin
  ret:= copyfile( pchar(fromPath), pchar(toPath), nil, COPYFILE_ALL or COPYFILE_UNLINK or COPYFILE_CLONE_FORCE );
  Result:= (ret=0);
end;

end.

