{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TFLACfile - for manipulating with FLAC file information               }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 1.4 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.3 (13 August 2004) by jtclipper                                   }
{   - unit rewritten, VorbisComment is obsolete now                           }
{                                                                             }
{ Version 1.2 (23 June 2004) by sundance                                      }
{   - Check for ID3 tags (although not supported)                             }
{   - Don't parse for other FLAC metablocks if FLAC header is missing         }
{                                                                             }
{ Version 1.1 (6 July 2003) by Erik                                           }
{   - Class: Vorbis comments (native comment to FLAC files) added             }
{                                                                             }
{ Version 1.0 (13 August 2002)                                                }
{   - Info: channels, sample rate, bits/sample, file size, duration, ratio    }
{   - Class TID3v1: reading & writing support for ID3v1 tags                  }
{   - Class TID3v2: reading & writing support for ID3v2 tags                  }
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

unit FLACfile;

interface

uses
  Classes, SysUtils, StrUtils, ID3v2,
  DCClassesUtf8, DCBasicTypes, DCOSUtils;

const
  META_STREAMINFO      = 0;
  META_PADDING         = 1;
  META_APPLICATION     = 2;
  META_SEEKTABLE       = 3;
  META_VORBIS_COMMENT  = 4;
  META_CUESHEET        = 5;
  META_PICTURE         = 6;

type
  TFlacHeader = record
    StreamMarker: array[1..4] of Char; //should always be 'fLaC'
    MetaDataBlockHeader: array[1..4] of Byte;
    Info: array[1..18] of Byte;
    MD5Sum: array[1..16] of Byte;
  end;

  TMetaData = record
    MetaDataBlockHeader: array[1..4] of Byte;
    Data: TMemoryStream;
  end;


  { TFLACfile }

  TFLACfile = class(TObject)
  private

    FHeader: TFlacHeader;
    FFileName: String;
    FPaddingIndex: integer;
    FPaddingLast: boolean;
    FPaddingFragments: boolean;
    FVorbisIndex: integer;
    FPadding: integer;
    FVCOffset: integer;
    FAudioOffset: integer;
    FChannels: byte;
    FSampleRate: integer;
    FBitsPerSample: byte;
    FBitrate: integer;
    FFileLength: integer;
    FSamples: Int64;

    aMetaBlockOther: array of TMetaData;

    // tag data
    FVendor: string;
    FTagSize: integer;
    FExists: boolean;

    FID3v2: TID3v2;

    function FGetHasLyrics: boolean;

    procedure FResetData( const bHeaderInfo, bTagFields :boolean );
    function FIsValid: Boolean;
    function FGetDuration: Double;
    function FGetTrack: Word;
    function FGetRatio: Double;
    function FGetChannelMode: string;

    function GetInfo( sFile: String; bSetTags: boolean ): boolean;
    procedure AddMetaDataOther( aMetaHeader: array of Byte; stream: TFileStreamEx; const iBlocklength,iIndex: integer );
    procedure ReadTag( Source: TFileStreamEx; bSetTagFields: boolean );
    function RebuildFile( const sFile: String; VorbisBlock: TStringStream ): Boolean;

  public

    TrackString: string;
    Title: string;
    Artist: string;
    Album: string;
    Year: string;
    Genre: string;
    Comment: string;
    //extra
    xTones: string;
    xStyles: string;
    xMood: string;
    xSituation: string;
    xRating: string;
    xQuality: string;
    xTempo: string;
    xType: string;

    //
    Composer: string;
    Language: string;
    Copyright: string;
    Link: string;
    Encoder: string;
    Lyrics: string;
    Performer: string;
    License: string;
    Organization: string;
    Description: string;
    Location: string;
    Contact: string;
    ISRC: string;
    aExtraFields: array of array of string;

    constructor Create;
    destructor Destroy; override;

    function ReadFromFile( const sFile: String ): boolean;
    function SaveToFile( const sFile: String; const bBasicOnly: boolean = false ): boolean;
    function RemoveFromFile( const sFile: String ):boolean;
    procedure AddExtraField(const sID, sValue: string);

    property Channels: Byte read FChannels;                     // Number of channels
    property SampleRate: Integer read FSampleRate;              // Sample rate (hz)
    property BitsPerSample: Byte read FBitsPerSample;           // Bits per sample
    property FileLength: integer read FFileLength;              // File length (bytes)
    property Samples: Int64 read FSamples;                      // Number of samples
    property Valid: Boolean read FIsValid;                      // True if header valid
    property Duration: Double read FGetDuration;                // Duration (seconds)
    property Ratio: Double read FGetRatio;                      // Compression ratio (%)
    property Track: Word read FGetTrack;                        // Track number
    property Bitrate: integer read FBitrate;
    property ChannelMode: string read FGetChannelMode;
    property Exists: boolean read FExists;
    property Vendor: string read FVendor;
    property FileName: String read FFileName;
    property AudioOffset: integer read FAudioOffset;           //offset of audio data
    property HasLyrics: boolean read FGetHasLyrics;
  end;

var
  bTAG_PreserveDate: boolean;


implementation

(* -------------------------------------------------------------------------- *)

procedure TFLACfile.FResetData( const bHeaderInfo, bTagFields :boolean );
var
  i: integer;
begin

   if bHeaderInfo then begin
      FFileName := '';
      FPadding := 0;
      FPaddingLast := false;
      FPaddingFragments := false;
      FChannels := 0;
      FSampleRate := 0;
      FBitsPerSample := 0;
      FFileLength := 0;
      FSamples := 0;
      FVorbisIndex := 0;
      FPaddingIndex := 0;
      FVCOffset := 0;
      FAudioOffset := 0;

      for i := 0 to Length( aMetaBlockOther ) - 1 do aMetaBlockOther[ i ].Data.Free;
      SetLength( aMetaBlockOther, 0 );
   end;

   //tag data
   if bTagFields then begin
      FVendor := '';
      FTagSize := 0;
      FExists := false;

      Title := '';
      Artist := '';
      Album := '';
      TrackString := '';
      Year := '';
      Genre := '';
      Comment := '';
      //extra
      xTones := '';
      xStyles := '';
      xMood := '';
      xSituation := '';
      xRating := '';
      xQuality := '';
      xTempo := '';
      xType := '';

      //
      Composer := '';
      Language := '';
      Copyright := '';
      Link := '';
      Encoder := '';
      Lyrics := '';
      Performer := '';
      License := '';
      Organization := '';
      Description := '';
      Location := '';
      Contact := '';
      ISRC := '';
      SetLength( aExtraFields, 0 );
  end;    
end;

(* -------------------------------------------------------------------------- *)
// Check for right FLAC file data
function TFLACfile.FIsValid: Boolean;
begin
  result := (FHeader.StreamMarker = 'fLaC') and
            (FChannels > 0) and
            (FSampleRate > 0) and
            (FBitsPerSample > 0) and
            (FSamples > 0);
end;

(* -------------------------------------------------------------------------- *)

function TFLACfile.FGetDuration: Double;
begin
  if (FIsValid) and (FSampleRate > 0) then begin
     result := FSamples / FSampleRate
  end else begin
     result := 0;
  end;
end;

(* -------------------------------------------------------------------------- *)

function TFLACfile.FGetTrack: Word;
var
  Index, Value, Code: Integer;
begin
  { Extract track from string }
  Index := Pos('/', TrackString);
  if Index = 0 then Val(TrackString, Value, Code)
  else Val(Copy(TrackString, 1, Index - 1), Value, Code);
  if Code = 0 then Result := Value
  else Result := 0;
end;

(* -------------------------------------------------------------------------- *)
//   Get compression ratio
function TFLACfile.FGetRatio: Double;
begin
  if FIsValid then begin
     result := FFileLength / (FSamples * FChannels * FBitsPerSample / 8) * 100
  end else begin
     result := 0;
  end;
end;

(* -------------------------------------------------------------------------- *)
//   Get channel mode
function TFLACfile.FGetChannelMode: string;
begin
  if FIsValid then begin
     case FChannels of
      1 : result := 'Mono';
      2 : result := 'Stereo';
      else result := 'Multi Channel';
     end;
  end else begin
     result := '';
  end;
end;

(* -------------------------------------------------------------------------- *)

function TFLACfile.FGetHasLyrics: boolean;
begin
  result := ( Trim( Lyrics ) <> '' );
end;

(* -------------------------------------------------------------------------- *)

constructor TFLACfile.Create;
begin
  inherited;
  FID3v2 := TID3v2.Create;
  FResetData( true, true );
end;
destructor TFLACfile.Destroy;
begin
  FResetData( true, true );
  FID3v2.Free;
  inherited;
end;

(* -------------------------------------------------------------------------- *)

function TFLACfile.ReadFromFile( const sFile: String ): boolean;
begin
  FResetData( false, true );
  result := GetInfo( sFile, true );
end;

(* -------------------------------------------------------------------------- *)

function TFLACfile.GetInfo( sFile: String; bSetTags: boolean ): boolean;
var
  SourceFile: TFileStreamEx;
  aMetaDataBlockHeader: array[1..4] of byte;
  iBlockLength, iMetaType, iIndex: integer;
  bPaddingFound: boolean;
begin

  result := true;
  bPaddingFound := false;
  FResetData( true, false );  
  try
    { Read data from ID3 tags }
    FID3v2.ReadFromFile(sFile);

    // Set read-access and open file
    SourceFile := TFileStreamEx.Create(sFile, fmOpenRead or fmShareDenyWrite);
    FFileLength := SourceFile.Size;
    FFileName := sFile;

    { Seek past the ID3v2 tag, if there is one }
    if FID3v2.Exists then begin
      SourceFile.Seek(FID3v2.Size, soFromBeginning)
    end;

    // Read header data
    FillChar( FHeader, SizeOf(FHeader), 0 );
    SourceFile.Read( FHeader, SizeOf(FHeader) );

    // Process data if loaded and header valid
    if FHeader.StreamMarker = 'fLaC' then begin

       with FHeader do begin
         FChannels      := ( Info[13] shr 1 and $7 + 1 );
         FSampleRate    := ( Info[11] shl 12 or Info[12] shl 4 or Info[13] shr 4 );
         FBitsPerSample := ( Info[13] and 1 shl 4 or Info[14] shr 4 + 1 );
         FSamples       := ( Info[15] shl 24 or Info[16] shl 16 or Info[17] shl 8 or Info[18] );
       end;

       if (FHeader.MetaDataBlockHeader[1] and $80) <> 0 then exit; //no metadata blocks exist
       iIndex := 0;
       repeat // read more metadata blocks if available

          SourceFile.Read( aMetaDataBlockHeader, 4 );

          iIndex := iIndex + 1; // metadatablock index
          iBlockLength := (aMetaDataBlockHeader[2] shl 16 or aMetaDataBlockHeader[3] shl 8 or aMetaDataBlockHeader[4]); //decode length
          if iBlockLength <= 0 then exit; // can it be 0 ?

          iMetaType := (aMetaDataBlockHeader[1] and $7F); // decode metablock type

          if iMetaType = META_VORBIS_COMMENT then begin  // read vorbis block
             FVCOffset := SourceFile.Position;
             FTagSize := iBlockLength;
             FVorbisIndex := iIndex;
             ReadTag(SourceFile, bSetTags); // set up fields
          end else if (iMetaType = META_PADDING) and not bPaddingFound then begin // we have padding block
             FPadding := iBlockLength;                                            // if we find more skip & put them in metablock array
             FPaddingLast := ((aMetaDataBlockHeader[1] and $80) <> 0);
             FPaddingIndex := iIndex;
             bPaddingFound := true;
             SourceFile.Seek(FPadding, soCurrent); // advance into file till next block or audio data start
          end else begin // all other
             if iMetaType <= META_PICTURE then begin // is it a valid metablock ?
                if (iMetaType = META_PADDING) then begin // set flag for fragmented padding blocks
                   FPaddingFragments := true;
                end;
                AddMetaDataOther(aMetaDataBlockHeader, SourceFile, iBlocklength, iIndex);
             end else begin
                FSamples := 0; // ops...
                Exit;
             end;
          end;

       until ((aMetaDataBlockHeader[1] and $80) <> 0); // until is last flag ( first bit = 1 )

    end;
  finally
    if FIsValid then begin
       FAudioOffset := SourceFile.Position;  // we need that to rebuild the file if nedeed
       FBitrate := Round( ( ( FFileLength - FAudioOffset ) / 1000 ) * 8 / FGetDuration ); //time to calculate average bitrate
    end else begin
       result := false;
    end;
    FreeAndNil(SourceFile);
  end;

end;

(* -------------------------------------------------------------------------- *)

procedure TFLACfile.AddMetaDataOther( aMetaHeader: array of Byte; stream: TFileStreamEx; const iBlocklength,iIndex: integer );
var
  iMetaLen: integer;
begin
  // enlarge array
  iMetaLen := Length( aMetaBlockOther ) + 1;
  SetLength( aMetaBlockOther, iMetaLen );
  // save header
  aMetaBlockOther[ iMetaLen - 1 ].MetaDataBlockHeader[1] := aMetaHeader[0];
  aMetaBlockOther[ iMetaLen - 1 ].MetaDataBlockHeader[2] := aMetaHeader[1];
  aMetaBlockOther[ iMetaLen - 1 ].MetaDataBlockHeader[3] := aMetaHeader[2];
  aMetaBlockOther[ iMetaLen - 1 ].MetaDataBlockHeader[4] := aMetaHeader[3];
  // save content in a stream
  aMetaBlockOther[ iMetaLen - 1 ].Data := TMemoryStream.Create;
  aMetaBlockOther[ iMetaLen - 1 ].Data.Position := 0;
  aMetaBlockOther[ iMetaLen - 1 ].Data.CopyFrom( stream, iBlocklength );
end;

(* -------------------------------------------------------------------------- *)

procedure TFLACfile.ReadTag( Source: TFileStreamEx; bSetTagFields: boolean );
var
  i, iCount, iSize, iSepPos: Integer;
  Data, sFieldID, sFieldData: String;
begin

  Source.Read( iSize, SizeOf( iSize ) ); // vendor
  SetLength( Data, iSize );
  Source.Read( Data[ 1 ], iSize );
  FVendor := String( Data );

  Source.Read( iCount, SizeOf( iCount ) ); //fieldcount

  FExists := ( iCount > 0 );

  for i := 0 to iCount - 1 do begin
      Source.Read( iSize, SizeOf( iSize ) );
      SetLength( Data , iSize );
      Source.Read( Data[ 1 ], iSize );

      if not bSetTagFields then Continue; // if we don't want to re asign fields we skip
      
      iSepPos := Pos( '=', String( Data ) );
      if iSepPos > 0 then begin

         sFieldID := UpperCase( Copy( String( Data ), 1, iSepPos - 1) );
         sFieldData := Copy( String( Data ), iSepPos + 1, MaxInt );

         if (sFieldID = 'TRACKNUMBER') and (TrackString = '') then begin
            TrackString := sFieldData;
         end else if (sFieldID = 'ARTIST') and (Artist = '') then begin
            Artist := sFieldData;
         end else if (sFieldID = 'ALBUM') and (Album = '') then begin
            Album := sFieldData;
         end else if (sFieldID = 'TITLE') and (Title = '') then begin
            Title := sFieldData;
         end else if (sFieldID = 'DATE') and (Year = '') then begin
            Year := sFieldData;
         end else if (sFieldID = 'GENRE') and (Genre = '') then begin
            Genre := sFieldData;
         end else if (sFieldID = 'COMMENT') and (Comment = '') then begin
            Comment := sFieldData;
         end else if (sFieldID = 'COMPOSER') and (Composer = '') then begin
            Composer := sFieldData;
         end else if (sFieldID = 'LANGUAGE') and (Language = '') then begin
            Language := sFieldData;
         end else if (sFieldID = 'COPYRIGHT') and (Copyright = '') then begin
            Copyright := sFieldData;
         end else if (sFieldID = 'URL') and (Link = '') then begin
            Link := sFieldData;
         end else if (sFieldID = 'ENCODER') and (Encoder = '') then begin
            Encoder := sFieldData;
         end else if (sFieldID = 'TONES') and (xTones = '') then begin
            xTones := sFieldData;
         end else if (sFieldID = 'STYLES') and (xStyles = '') then begin
            xStyles := sFieldData;
         end else if (sFieldID = 'MOOD') and (xMood = '') then begin
            xMood := sFieldData;
         end else if (sFieldID = 'SITUATION') and (xSituation = '') then begin
            xSituation := sFieldData;
         end else if (sFieldID = 'RATING') and (xRating = '') then begin
            xRating := sFieldData;
         end else if (sFieldID = 'QUALITY') and (xQuality = '') then begin
            xQuality := sFieldData;
         end else if (sFieldID = 'TEMPO') and (xTempo = '') then begin
            xTempo := sFieldData;
         end else if (sFieldID = 'TYPE') and (xType = '') then begin
            xType := sFieldData;
         end else if (sFieldID = 'LYRICS') and (Lyrics = '') then begin
            Lyrics := sFieldData;
         end else if (sFieldID = 'PERFORMER') and (Performer = '') then begin
            Performer := sFieldData;
         end else if (sFieldID = 'LICENSE') and (License = '') then begin
            License := sFieldData;
         end else if (sFieldID = 'ORGANIZATION') and (Organization = '') then begin
            Organization := sFieldData;
         end else if (sFieldID = 'DESCRIPTION') and (Description = '') then begin
            Description := sFieldData;
         end else if (sFieldID = 'LOCATION') and (Location = '') then begin
            Location := sFieldData;
         end else if (sFieldID = 'CONTACT') and (Contact = '') then begin
            Contact := sFieldData;
         end else if (sFieldID = 'ISRC') and (ISRC = '') then begin
            ISRC := sFieldData;
         end else begin // more fields
            AddExtraField( sFieldID, sFieldData );
         end;

      end;

  end;

end;

(* -------------------------------------------------------------------------- *)

procedure TFLACfile.AddExtraField(const sID, sValue: string);
var
  iExtraLen: integer;
begin
  iExtraLen := Length( aExtraFields ) + 1;
  SetLength( aExtraFields, iExtraLen );
  SetLength( aExtraFields[ iExtraLen - 1 ], 2 );

  aExtraFields[ iExtraLen - 1, 0 ] := sID;
  aExtraFields[ iExtraLen - 1, 1 ] := sValue;
end;

(* -------------------------------------------------------------------------- *)

function TFLACfile.SaveToFile( const sFile: String; const bBasicOnly: boolean = false ): boolean;
var
  i, iFieldCount, iSize: Integer;
  VorbisBlock, Tag: TStringStream;

  procedure _WriteTagBuff( sID, sData: string );
  var
    sTmp: string;
    iTmp: integer;
  begin
    if sData <> '' then  begin
       sTmp := sID + '=' + sData;
       iTmp := Length( sTmp );
       Tag.Write( iTmp, SizeOf( iTmp ) );
       Tag.WriteString( sTmp );
       iFieldCount := iFieldCount + 1;
    end;
  end;

begin

  try
    result := false;

    Tag := TStringStream.Create('');
    VorbisBlock := TStringStream.Create('');
    if not GetInfo( sFile, false ) then exit; //reload all except tag fields

    iFieldCount := 0;

    _WriteTagBuff( 'TRACKNUMBER', TrackString );
    _WriteTagBuff( 'ARTIST', Artist );
    _WriteTagBuff( 'ALBUM', Album );
    _WriteTagBuff( 'TITLE', Title );
    _WriteTagBuff( 'DATE', Year );
    _WriteTagBuff( 'GENRE', Genre );
    _WriteTagBuff( 'COMMENT', Comment );
    _WriteTagBuff( 'COMPOSER', Composer );
    _WriteTagBuff( 'LANGUAGE', Language );
    _WriteTagBuff( 'COPYRIGHT', Copyright );
    _WriteTagBuff( 'URL', Link );
    _WriteTagBuff( 'ENCODER', Encoder );

    _WriteTagBuff( 'TONES', xTones );
    _WriteTagBuff( 'STYLES', xStyles );
    _WriteTagBuff( 'MOOD', xMood );
    _WriteTagBuff( 'SITUATION', xSituation );
    _WriteTagBuff( 'RATING', xRating );
    _WriteTagBuff( 'QUALITY', xQuality );
    _WriteTagBuff( 'TEMPO', xTempo );
    _WriteTagBuff( 'TYPE', xType );

    if not bBasicOnly then begin
       _WriteTagBuff( 'PERFORMER', Performer );
       _WriteTagBuff( 'LICENSE', License );
       _WriteTagBuff( 'ORGANIZATION', Organization );
       _WriteTagBuff( 'DESCRIPTION', Description );
       _WriteTagBuff( 'LOCATION', Location );
       _WriteTagBuff( 'CONTACT', Contact );
       _WriteTagBuff( 'ISRC', ISRC );
       _WriteTagBuff( 'LYRICS', Lyrics );

       for i := 0 to Length( aExtraFields ) - 1 do begin
           if Trim( aExtraFields[ i, 0 ] ) <> '' then _WriteTagBuff( aExtraFields[ i, 0 ], aExtraFields[ i, 1 ] );
       end;
    end;

    // Write vendor info and number of fields
    with VorbisBlock do begin
      if FVendor = '' then FVendor := 'reference libFLAC 1.1.0 20030126'; // guess it
      iSize := Length( FVendor );
      Write( iSize, SizeOf( iSize ) );
      WriteString( FVendor );
      Write( iFieldCount, SizeOf( iFieldCount ) );
    end;

    VorbisBlock.CopyFrom( Tag, 0 ); // All tag data is here now
    VorbisBlock.Position := 0;

    result := RebuildFile( sFile, VorbisBlock );
    FExists := result and (Tag.Size > 0 );

  finally
    FreeAndNil( Tag );
    FreeAndNil( VorbisBlock );
  end;

end;

(* -------------------------------------------------------------------------- *)

function TFLACfile.RemoveFromFile( const sFile: String ):boolean;
begin
  FResetData( false, true );
  result := SaveToFile( sFile );
  if FExists then FExists := not result;
end;

(* -------------------------------------------------------------------------- *)
// saves metablocks back to the file
// always tries to rebuild header so padding exists after comment block and no more than 1 padding block exists
function TFLACfile.RebuildFile( const sFile: String; VorbisBlock: TStringStream ): Boolean;
var
  iFileAge: TFileTime;
  Source, Destination: TFileStreamEx;
  i, iNewPadding, iMetaCount, iExtraPadding: Integer;
  BufferName, sTmp: string;
  MetaDataBlockHeader: array[1..4] of Byte;
  oldHeader: TFlacHeader;
  MetaBlocks: TMemoryStream;
  bRebuild, bRearange: boolean;
begin

  result := false;
  bRearange := false;
  iExtraPadding := 0;
  if (not mbFileExists(FileName)) or (not mbFileSetReadOnly(FileName, False)) then exit;

  try
    iFileAge := 0;
    if bTAG_PreserveDate then iFileAge := mbFileAge( FileName );

    // re arrange other metadata in case of
    // 1. padding block is not aligned after vorbis comment
    // 2. insufficient padding - rearange upon file rebuild
    // 3. fragmented padding blocks
    iMetaCount := Length( aMetaBlockOther );
    if (FPaddingIndex <> FVorbisIndex + 1) or (FPadding <= VorbisBlock.Size - FTagSize ) or FPaddingFragments then begin
       MetaBlocks := TMemoryStream.Create;
       for i := 0 to iMetaCount - 1 do begin
           aMetaBlockOther[ i ].MetaDataBlockHeader[ 1 ] := ( aMetaBlockOther[ i ].MetaDataBlockHeader[ 1 ] and $7f ); // not last

           if aMetaBlockOther[ i ].MetaDataBlockHeader[ 1 ] = META_PADDING then begin
              iExtraPadding := iExtraPadding + aMetaBlockOther[ i ].Data.Size + 4; // add padding size plus 4 bytes of header block
           end else begin
              aMetaBlockOther[ i ].Data.Position := 0;
              MetaBlocks.Write( aMetaBlockOther[ i ].MetaDataBlockHeader[ 1 ], 4 );
              MetaBlocks.CopyFrom( aMetaBlockOther[ i ].Data, 0 );
           end;

       end;
       MetaBlocks.Position := 0;
       bRearange := true;
    end;

    // set up file
    if (FPadding <= VorbisBlock.Size - FTagSize ) then begin // no room rebuild the file from scratch
       bRebuild := true;
       BufferName := FileName + '~';
       Source := TFileStreamEx.Create( FileName, fmOpenRead ); // Set read-only and open old file, and create new
       Destination := TFileStreamEx.Create( BufferName, fmCreate );
       Source.Read( oldHeader, sizeof( oldHeader ) );
       oldHeader.MetaDataBlockHeader[ 1 ] := (oldHeader.MetaDataBlockHeader[ 1 ] and $7f ); //just in case no metadata existed
       Destination.Write( oldHeader, Sizeof( oldHeader ) );
       Destination.CopyFrom( MetaBlocks, 0 );
    end else begin
       bRebuild := false;
       Source := nil;
       Destination := TFileStreamEx.Create( FileName, fmOpenWrite); // Set write-access and open file
       if bRearange then begin
          Destination.Seek( SizeOf( FHeader ), soFromBeginning );
          Destination.CopyFrom( MetaBlocks, 0 );
       end else begin
          Destination.Seek( FVCOffset - 4, soFromBeginning );
       end;
    end;

    // finally write vorbis block
    MetaDataBlockHeader[1] := META_VORBIS_COMMENT;
    MetaDataBlockHeader[2] := Byte(( VorbisBlock.Size shr 16 ) and 255 );
    MetaDataBlockHeader[3] := Byte(( VorbisBlock.Size shr 8 ) and 255 );
    MetaDataBlockHeader[4] := Byte( VorbisBlock.Size and 255 );
    Destination.Write( MetaDataBlockHeader[ 1 ], SizeOf( MetaDataBlockHeader ) );
    Destination.CopyFrom( VorbisBlock, VorbisBlock.Size );

    // and add padding
    if FPaddingLast or bRearange then begin
       MetaDataBlockHeader[1] := META_PADDING or $80;
    end else begin
       MetaDataBlockHeader[1] := META_PADDING;
    end;
    if bRebuild then begin
       iNewPadding := 4096; // why not...
    end else begin
       if FTagSize > VorbisBlock.Size then begin // tag got smaller increase padding
          iNewPadding := (FPadding + FTagSize - VorbisBlock.Size) + iExtraPadding;
       end else begin // tag got bigger shrink padding
          iNewPadding := (FPadding - VorbisBlock.Size + FTagSize ) + iExtraPadding;
       end;
    end;
    MetaDataBlockHeader[2] := Byte(( iNewPadding shr 16 ) and 255 );
    MetaDataBlockHeader[3] := Byte(( iNewPadding shr 8 ) and 255 );
    MetaDataBlockHeader[4] := Byte( iNewPadding and 255 );
    Destination.Write(MetaDataBlockHeader[ 1 ], 4);
    if (FPadding <> iNewPadding) or bRearange then begin // fill the block with zeros
       sTmp := DupeString( #0, iNewPadding );
       Destination.Write( sTmp[1], iNewPadding );
    end;

    // finish
    if bRebuild then begin // time to put back the audio data...
       Source.Seek( FAudioOffset, soFromBeginning );
       Destination.CopyFrom( Source, Source.Size - FAudioOffset );
       Source.Free;
       Destination.Free;
       if ( mbDeleteFile( FileName ) ) and ( mbRenameFile( BufferName, FileName ) ) then begin //Replace old file and delete temporary file
          result := true
       end else begin
          raise Exception.Create('');
       end;
    end else begin
       result := true;
       Destination.Free;
    end;

    // post save tasks
    if bTAG_PreserveDate then mbFileSetTime( FileName, iFileAge );
    if bRearange then FreeAndNil( MetaBlocks );

  except
    // Access error
    if mbFileExists( BufferName ) then mbDeleteFile( BufferName );
  end;
end;

(* -------------------------------------------------------------------------- *)

end.

