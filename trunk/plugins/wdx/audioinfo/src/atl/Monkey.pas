{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TMonkey - for manipulating with Monkey's Audio file information       }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 1.7 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.6 (11 April 2004) by Gambit                                       }
{   - Added Ratio property again                                              }
{                                                                             }
{ Version 1.5 (22 August 2003) by MaDah                                       }
{   - Added support for Monkey's Audio 3.98                                   }
{   - Added/changed/removed some stuff                                        }
{                                                                             }
{ Version 1.4 (29 July 2002)                                                  }
{   - Correction for calculating of duration                                  }
{                                                                             }
{ Version 1.1 (11 September 2001)                                             }
{   - Added property Samples                                                  }
{   - Removed WAV header information                                          }
{                                                                             }
{ Version 1.0 (7 September 2001)                                              }
{   - Support for Monkey's Audio files                                        }
{   - Class TID3v1: reading & writing support for ID3v1 tags                  }
{   - Class TID3v2: reading & writing support for ID3v2 tags                  }
{   - Class TAPEtag: reading & writing support for APE tags                   }
{                                                                             }
{ This library is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU Lesser General Public                  }
{ License as published by the Free Software Foundation; either                }
{ version 2.1 of the License, or (at your option) any later version.          }
{                                                                             }
{ This library is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ Lesser General Public License for more details.                             }
{                                                                             }
{ You should have received a copy of the GNU Lesser General Public            }
{ License along with this library; if not, write to the Free Software         }
{ Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                             }
{ *************************************************************************** }

unit Monkey;

interface

uses
  Classes, SysUtils, ID3v1, ID3v2, APEtag, DCClassesUtf8;

const
  { Compression level codes }
  MONKEY_COMPRESSION_FAST       = 1000;    { Fast (poor) }
  MONKEY_COMPRESSION_NORMAL     = 2000;    { Normal (good) }
  MONKEY_COMPRESSION_HIGH       = 3000;    { High (very good) }
  MONKEY_COMPRESSION_EXTRA_HIGH = 4000;    { Extra high (best) }
  MONKEY_COMPRESSION_INSANE     = 5000;    { Insane }
  MONKEY_COMPRESSION_BRAINDEAD  = 6000;    { BrainDead }
  { Compression level names }
  MONKEY_COMPRESSION: array [0..6] of string =
    ('Unknown', 'Fast', 'Normal', 'High', 'Extra High', 'Insane', 'BrainDead');

  { Format flags, only for Monkey's Audio <= 3.97 }
  MONKEY_FLAG_8_BIT          = 1;    // Audio 8-bit
  MONKEY_FLAG_CRC            = 2;    // New CRC32 error detection
  MONKEY_FLAG_PEAK_LEVEL     = 4;    // Peak level stored
  MONKEY_FLAG_24_BIT         = 8;    // Audio 24-bit
  MONKEY_FLAG_SEEK_ELEMENTS  = 16;   // Number of seek elements stored
  MONKEY_FLAG_WAV_NOT_STORED = 32;   // WAV header not stored

  { Channel mode names }
  MONKEY_MODE: array [0..2] of string =
    ('Unknown', 'Mono', 'Stereo');

type
  { Class TMonkey }
  TMonkey = class(TObject)
  private
    { Private declarations }
		FValid     		      : boolean;
    // Stuff loaded from the header:
    FVersion             : integer;
    FVersionStr          : string;
		FChannels  		      : integer;
		FSampleRate		      : integer;
		FBits      		      : integer;
    FPeakLevel           : longword;
    FPeakLevelRatio      : double;
    FTotalSamples        : int64;
		FBitrate  		      : double;
		FDuration		      : double;
		FCompressionMode     : integer;
    FCompressionModeStr  : string;
    // FormatFlags, only used with Monkey's <= 3.97
    FFormatFlags         : integer;
    FHasPeakLevel        : boolean;
    FHasSeekElements     : boolean;
    FWavNotStored        : boolean;
    // Tagging
    FID3v1               : TID3v1;
    FID3v2               : TID3v2;
    FAPEtag              : TAPEtag;
    //
   	FFileSize  		      : int64;

    procedure FResetData;

    function FGetRatio: Double;
    function FGetChannelMode: string;

  public
    { Public declarations }
    constructor Create;                                       { Create object }
    destructor Destroy; override;                            { Destroy object }

    function ReadFromFile(const FileName: String): Boolean; { Load header }

    property FileSize 	      : int64		read FFileSize;
    property Valid     	      : boolean	read FValid;
    property Version           : integer   read FVersion;
    property VersionStr        : string    read FVersionStr;
    property Channels  	      : integer	read FChannels;
    property SampleRate	      : integer	read FSamplerate;
    property Bits      	      : integer	read FBits;
    property Bitrate  	      : double		read FBitrate;
    property Duration		      : double		read FDuration;
    property PeakLevel 	      : longword	read FPeakLevel;
    property PeakLevelRatio    : double 	read FPeakLevelRatio;
    property TotalSamples 	   : int64	   read FTotalSamples;
    property CompressionMode 	: integer	read FCompressionMode;
    property CompressionModeStr: string 	read FCompressionModeStr;
    property ChannelMode: string read FGetChannelMode;  { Channel mode name }
    // FormatFlags, only used with Monkey's <= 3.97
    property FormatFlags 	   : integer	read FFormatFlags;
    property HasPeakLevel 	   : boolean	read FHasPeakLevel;
    property HasSeekElements 	: boolean	read FHasSeekElements;
    property WavNotStored 	   : boolean	read FWavNotStored;
    // Tagging
    property ID3v1: TID3v1 read FID3v1;                    { ID3v1 tag data }
    property ID3v2: TID3v2 read FID3v2;                    { ID3v2 tag data }
    property APEtag: TAPEtag read FAPEtag;                   { APE tag data }

    property Ratio: Double read FGetRatio;          { Compression ratio (%) }
  end;

implementation

type
  { Real structure of Monkey's Audio header }
  // common header for all versions
  APE_HEADER = packed record
    cID: array[0..3] of byte;        // should equal 'MAC '
    nVersion : WORD;                 // version number * 1000 (3.81 = 3810)
  end;
  // old header for <= 3.97
  APE_HEADER_OLD = packed record
    nCompressionLevel,               // the compression level
    nFormatFlags,                    // any format flags (for future use)
    nChannels: word;                 // the number of channels (1 or 2)
    nSampleRate,                     // the sample rate (typically 44100)
    nHeaderBytes,                    // the bytes after the MAC header that compose the WAV header
    nTerminatingBytes,               // the bytes after that raw data (for extended info)
    nTotalFrames,                    // the number of frames in the file
    nFinalFrameBlocks: longword;     // the number of samples in the final frame
    nInt : integer;
  end;
  // new header for >= 3.98
  APE_HEADER_NEW = packed record
    nCompressionLevel : word;		    // the compression level (see defines I.E. COMPRESSION_LEVEL_FAST)
    nFormatFlags      : word;		    // any format flags (for future use) Note: NOT the same flags as the old header!
    nBlocksPerFrame   : longword;		// the number of audio blocks in one frame
    nFinalFrameBlocks : longword;		// the number of audio blocks in the final frame
    nTotalFrames      : longword;		// the total number of frames
    nBitsPerSample    : word;		    // the bits per sample (typically 16)
    nChannels         : word;		    // the number of channels (1 or 2)
    nSampleRate       : longword;   // the sample rate (typically 44100)
  end;
  // data descriptor for >= 3.98
  APE_DESCRIPTOR = packed record
    padded : Word;                   // padding/reserved (always empty)
	  nDescriptorBytes,		            // the number of descriptor bytes (allows later expansion of this header)
    nHeaderBytes,			              // the number of header APE_HEADER bytes
	  nSeekTableBytes,		              // the number of bytes of the seek table
	  nHeaderDataBytes,		            // the number of header data bytes (from original file)
	  nAPEFrameDataBytes,		          // the number of bytes of APE frame data
	  nAPEFrameDataBytesHigh,	        // the high order number of APE frame data bytes
	  nTerminatingDataBytes : longword;// the terminating data of the file (not including tag data)
    cFileMD5 : array[0..15] of Byte; // the MD5 hash of the file (see notes for usage... it's a littly tricky)
  end;

{ ********************** Private functions & procedures ********************* }

procedure TMonkey.FResetData;
begin
  { Reset data }
	FValid     		      := false;
  FVersion            := 0;
  FVersionStr         := '';
	FChannels  		      := 0;
	FSampleRate		      := 0;
	FBits      		      := 0;
  FPeakLevel          := 0;
  FPeakLevelRatio     := 0.0;
  FTotalSamples       := 0;
	FBitrate  		      := 0.0;
  FDuration		        := 0.0;
	FCompressionMode    := 0;
  FCompressionModeStr := '';
  FFormatFlags        := 0;
  FHasPeakLevel       := false;
  FHasSeekElements    := false;
  FWavNotStored       := false;
 	FFileSize  		      := 0;
  FID3v1.ResetData;
  FID3v2.ResetData;
  FAPEtag.ResetData;
end;

{ ********************** Public functions & procedures ********************** }

constructor TMonkey.Create;
begin
  { Create object }
  inherited;
  FID3v1 := TID3v1.Create;
  FID3v2 := TID3v2.Create;
  FAPEtag := TAPEtag.Create;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

destructor TMonkey.Destroy;
begin
  { Destroy object }
  FID3v1.Free;
  FID3v2.Free;
  FAPEtag.Free;
  inherited;
end;

{ --------------------------------------------------------------------------- }

function TMonkey.ReadFromFile(const FileName: String): Boolean;
var
   f              : TFileStreamEx;
   APE            : APE_HEADER;     // common header
   APE_OLD        : APE_HEADER_OLD; // old header   <= 3.97
   APE_NEW        : APE_HEADER_NEW; // new header   >= 3.98
   APE_DESC       : APE_DESCRIPTOR; // extra header >= 3.98
   BlocksPerFrame : integer;
   LoadSuccess    : boolean;
   TagSize        : integer;
begin
   Result := FALSE;
   FResetData;
   // load tags first
   FID3v2.ReadFromFile(FileName);
   FID3v1.ReadFromFile(FileName);
   FAPEtag.ReadFromFile(FileName);
   // calculate total tag size
   TagSize := 0;
   if FID3v1.Exists  then inc(TagSize, 128);
   if FID3v2.Exists  then inc(TagSize, FID3v2.Size);
   if FAPEtag.Exists then inc(TagSize, FAPETag.Size);
   // begin reading data from file
   LoadSuccess := FALSE;
   f:=nil;
   try
      try
         f := TFileStreamEx.create(FileName, fmOpenRead or fmShareDenyWrite);
         FFileSize := f.Size;
         // seek past id3v2-tag
         if FID3v2.Exists then begin
            f.Seek(FID3v2.Size, soFromBeginning);
         end;
         // Read APE Format Header
         fillchar(APE, sizeof(APE), 0);
         if (f.Read(APE, sizeof(APE)) = sizeof(APE)) and ( StrLComp(@APE.cID[0],'MAC ',4)=0) then begin
            FVersion       := APE.nVersion;
            Str(FVersion / 1000 : 4 : 2, FVersionStr);
            // Load New Monkey's Audio Header for version >= 3.98
            if APE.nVersion >= 3980 then begin
               fillchar(APE_DESC, sizeof(APE_DESC), 0);
               if (f.Read(APE_DESC, sizeof(APE_DESC)) = sizeof(APE_DESC)) then begin
                  // seek past description header
                  if APE_DESC.nDescriptorBytes <> 52 then f.Seek(APE_DESC.nDescriptorBytes - 52, soFromCurrent);
                  // load new ape_header
                  if APE_DESC.nHeaderBytes > sizeof(APE_NEW) then APE_DESC.nHeaderBytes := sizeof(APE_NEW);
                  fillchar(APE_NEW, sizeof(APE_NEW), 0);
                  if (longword(f.Read(APE_NEW, APE_DESC.nHeaderBytes)) = APE_DESC.nHeaderBytes ) then begin
                     // based on MAC SDK 3.98a1 (APEinfo.h)
                     FSampleRate       := APE_NEW.nSampleRate;
                     FChannels         := APE_NEW.nChannels;
                     FFormatFlags      := APE_NEW.nFormatFlags;
                     FBits             := APE_NEW.nBitsPerSample;
                     FCompressionMode  := APE_NEW.nCompressionLevel;
                     // calculate total uncompressed samples
                     if APE_NEW.nTotalFrames>0 then begin
                        FTotalSamples     := Int64(APE_NEW.nBlocksPerFrame) *
                                             Int64(APE_NEW.nTotalFrames-1) +
                                             Int64(APE_NEW.nFinalFrameBlocks);
                     end;
                     LoadSuccess := TRUE;
                  end;
               end;
            end else begin
               // Old Monkey <= 3.97
               fillchar(APE_OLD, sizeof(APE_OLD), 0);
               if (f.Read(APE_OLD, sizeof(APE_OLD)) = sizeof(APE_OLD) ) then begin
                  FCompressionMode  := APE_OLD.nCompressionLevel;
                  FSampleRate       := APE_OLD.nSampleRate;
                  FChannels         := APE_OLD.nChannels;
                  FFormatFlags      := APE_OLD.nFormatFlags;
                  FBits := 16;
                  if APE_OLD.nFormatFlags and MONKEY_FLAG_8_BIT  <>0 then FBits :=  8;
                  if APE_OLD.nFormatFlags and MONKEY_FLAG_24_BIT <>0 then FBits := 24;
                  FHasSeekElements  := APE_OLD.nFormatFlags and MONKEY_FLAG_PEAK_LEVEL    <>0;
                  FWavNotStored     := APE_OLD.nFormatFlags and MONKEY_FLAG_SEEK_ELEMENTS <>0;
                  FHasPeakLevel     := APE_OLD.nFormatFlags and MONKEY_FLAG_WAV_NOT_STORED<>0;
                  if FHasPeakLevel then begin
                     FPeakLevel        := APE_OLD.nInt;
                     FPeakLevelRatio   := (FPeakLevel / (1 shl FBits) / 2.0) * 100.0;
                  end;
                  // based on MAC_SDK_397 (APEinfo.cpp)
                  if (FVersion >= 3950) then
                     BlocksPerFrame := 73728 * 4
                  else if (FVersion >= 3900) or ((FVersion >= 3800) and (APE_OLD.nCompressionLevel = MONKEY_COMPRESSION_EXTRA_HIGH)) then
                     BlocksPerFrame := 73728
                  else
                     BlocksPerFrame := 9216;
                  // calculate total uncompressed samples
                  if APE_OLD.nTotalFrames>0 then begin
                     FTotalSamples :=  Int64(APE_OLD.nTotalFrames-1) *
                                       Int64(BlocksPerFrame) +
                                       Int64(APE_OLD.nFinalFrameBlocks);
                  end;
                  LoadSuccess := TRUE;
               end;
            end;
            if LoadSuccess then begin
               // compression profile name
               if ((FCompressionMode mod 1000) = 0) and (FCompressionMode<=6000) then begin
                  FCompressionModeStr := MONKEY_COMPRESSION[FCompressionMode div 1000];
               end else begin
                  FCompressionModeStr := IntToStr(FCompressionMode);
               end;
               // length
               if FSampleRate>0 then FDuration := FTotalSamples / FSampleRate;
               // average bitrate
               if FDuration>0 then FBitrate := (FFileSize - Int64(TagSize))*8.0 / (FDuration/1000.0);
               // some extra sanity checks
               FValid   := (FBits>0) and (FSampleRate>0) and (FTotalSamples>0) and (FChannels>0);
               Result   := FValid;
            end;
         end;
      finally
         f.free;
      end;
   except
   end;
end;
   
{ --------------------------------------------------------------------------- }

function TMonkey.FGetRatio: Double;
begin
  { Get compression ratio }
  if FValid then
    Result := FFileSize / (FTotalSamples * (FChannels * FBits / 8) + 44) * 100
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TMonkey.FGetChannelMode: string;
begin
  if FChannels < Length(MONKEY_MODE) then
    Result:= MONKEY_MODE[FChannels]
  else begin
    Result:= EmptyStr;
  end;
end;
      
{ --------------------------------------------------------------------------- }

end.
