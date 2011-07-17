library rpm_wdx;

{$MODE Delphi}

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  rpm_io in 'rpm_io.pas',
  rpm_wdx_intf in 'rpm_wdx_intf.pas',
  rpm_def in 'rpm_def.pas';

{$E wdx}

exports
  ContentGetDetectString    name 'ContentGetDetectString',
  ContentGetSupportedField  name 'ContentGetSupportedField',
  ContentGetValue           name 'ContentGetValue';
  
begin
end.
