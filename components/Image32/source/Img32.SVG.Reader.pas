unit Img32.SVG.Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.8                                                             *
* Date      :  12 January 2025                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
*                                                                              *
* Purpose   :  Read SVG 2.0 files                                              *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Types, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.SVG.Core, Img32.SVG.Path, Img32.Vector,
  Img32.Draw, Img32.Text, Img32.Transform;

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

type

  TBaseElement = class;
  TElementClass = class of TBaseElement;

  TDrawData = record
    currentColor  : TColor32;
    fillColor     : TColor32;
    fillOpacity   : double;
    fillRule      : TFillRule;
    fillEl        : UTF8String;
    strokeColor   : TColor32;
    strokeOpacity : double;
    strokeWidth   : TValue;
    strokeCap     : TEndStyle;
    strokeJoin    : TJoinStyle;
    strokeMitLim  : double;
    strokeEl      : UTF8String;
    dashArray     : TArrayOfDouble;
    dashOffset    : double;
    fontInfo      : TSVGFontInfo;
    markerStart   : UTF8String;
    markerMiddle  : UTF8String;
    markerEnd     : UTF8String;
    filterElRef   : UTF8String;
    maskElRef     : UTF8String;
    clipElRef     : UTF8String;
    matrix        : TMatrixD;
    visible       : Boolean;
    useEl         : TBaseElement; // to check for and prevent <USE> recursion
    bounds        : TRectD;
  end;

  PSvgIdNameHashMapItem = ^TSvgIdNameHashMapItem;
  TSvgIdNameHashMapItem = record
    Hash: Cardinal;
    Next: Integer;
    Name: UTF8String;
    Element: TBaseElement;
  end;

  TSvgIdNameHashMap = class(TObject)
  private
    FItems: array of TSvgIdNameHashMapItem;
    FBuckets: TArrayOfInteger;
    FCount: Integer;
    FMod: Cardinal;
    procedure Grow;
    function FindItemIndex(const Name: UTF8String): Integer;
  public
    procedure AddOrIgnore(const idName: UTF8String; element: TBaseElement);
    function FindElement(const idName: UTF8String): TBaseElement;
    procedure Clear;
  end;

  TSvgReader = class;

  TBaseElement = class
  private
    fParent         : TBaseElement;
    fXmlEl          : TSvgXmlEl;
    fSvgReader      : TSvgReader;
{$IFDEF XPLAT_GENERICS}
    fChilds         : TList<TBaseElement>;
{$ELSE}
    fChilds         : TList;
{$ENDIF}
    fId             : UTF8String;
    fDrawData       : TDrawData;    // currently both static and dynamic vars
    function FindRefElement(const refname: UTF8String): TBaseElement;
    function GetChildCount: integer;
    function GetChild(index: integer): TBaseElement;
    function FindChild(const idName: UTF8String): TBaseElement;
  protected
    elRectWH        : TValueRecWH;  // multifunction variable
    function  IsFirstChild: Boolean;
    procedure LoadAttributes;
    procedure LoadAttribute(attrib: PSvgAttrib);
    function  LoadContent: Boolean; virtual;
    // GetRelFracLimit: ie when to assume untyped vals are relative vals
    function  GetRelFracLimit: double; virtual;
    procedure Draw(image: TImage32; drawDat: TDrawData); virtual;
    procedure DrawChildren(image: TImage32; const drawDat: TDrawData);
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); virtual;
    destructor  Destroy; override;
    property Child[index: integer]: TBaseElement read GetChild; default;
    property ChildCount: integer read GetChildCount;
    property DrawData: TDrawData read fDrawData write fDrawData;
    property Id: UTF8String read fId;
  end;

  TShapeElement = class(TBaseElement)
  protected
    hasPaths    : Boolean;
    pathsLoaded : Boolean;
    drawPathsO  : TPathsD; //open only
    drawPathsC  : TPathsD; //closed only
    function GetBounds: TRectD; virtual;
    function  HasMarkers: Boolean;
    procedure GetPaths(const drawDat: TDrawData); virtual;
    // GetSimplePath: is only required for markers
    function  GetSimplePath(const drawDat: TDrawData): TPathsD; virtual;
    procedure DrawFilled(img: TImage32;
      const paths: TPathsD; drawDat: TDrawData);
    procedure DrawStroke(img: TImage32;
      const paths: TPathsD; drawDat: TDrawData; isClosed: Boolean);
    procedure DrawMarkers(img: TImage32; drawDat: TDrawData);
    procedure Draw(image: TImage32; drawDat: TDrawData); override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TSvgElement = class(TShapeElement)
  protected
    procedure Draw(image: TImage32; drawDat: TDrawData); override;
  public
    viewboxWH           : TRectWH;
    function  Width     : TValue;
    function  Height    : TValue;
  end;

  TSvgReader = class
  private
    fSvgParser        : TSvgParser;
    fBkgndColor       : TColor32;
    fBackgndImage     : TImage32;
    fTempImage        : TImage32;
    fBlurQuality      : integer;
    fIdList           : TSvgIdNameHashMap;
    fLinGradRenderer  : TLinearGradientRenderer;
    fRadGradRenderer  : TSvgRadialGradientRenderer;
    fCustomRendererCache: TCustomRendererCache;
    fRootElement      : TSvgElement;
    fFontCache        : TFontCache;
    fUsePropScale     : Boolean;
    fSimpleDraw       : Boolean;
    fSimpleDrawList   : TList;
    function  LoadInternal: Boolean;
    function  GetIsEmpty: Boolean;
    function  GetTempImage: TImage32;
    procedure SetBlurQuality(quality: integer);
  protected
    userSpaceBounds : TRectD;
    currentColor    : TColor32;
    procedure GetBestFont(const svgFontInfo: TSVGFontInfo);
    property  RadGradRenderer: TSvgRadialGradientRenderer read fRadGradRenderer;
    property  LinGradRenderer: TLinearGradientRenderer read fLinGradRenderer;
    property  BackgndImage   : TImage32 read fBackgndImage;
    property  TempImage      : TImage32 read GetTempImage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DrawImage(img: TImage32; scaleToImage: Boolean);
    function  LoadFromStream(stream: TStream): Boolean;
    function  LoadFromFile(const filename: string): Boolean;
    function  LoadFromString(const str: string): Boolean;

    // The following two methods are deprecated and intended only for ...
    // https://github.com/EtheaDev/SVGIconImageList
    procedure SetOverrideFillColor(color: TColor32); //deprecated;
    procedure SetOverrideStrokeColor(color: TColor32); //deprecated;

    function  FindElement(const idName: UTF8String): TBaseElement;
    property  BackgroundColor : TColor32 read fBkgndColor write fBkgndColor;
    property  BlurQuality     : integer read fBlurQuality write SetBlurQuality;
    property  IsEmpty         : Boolean read GetIsEmpty;
    // KeepAspectRatio: this property has also been added for the convenience of
    // the third-party SVGIconImageList. (IMHO it should always = true)
    property  KeepAspectRatio: Boolean
      read fUsePropScale write fUsePropScale;
    property  RootElement     : TSvgElement read fRootElement;
    // RecordSimpleDraw: record simple drawing instructions
    property  RecordSimpleDraw: Boolean read fSimpleDraw write fSimpleDraw;
    // SimpleDrawList: list of PSimpleDrawData records;
    property  SimpleDrawList  : TList read fSimpleDrawList;
  end;

  PSimpleDrawData = ^TSimpleDrawData;
  TSimpleDrawData = record
    paths     : TPathsD;
    fillRule  : TFillRule;
    color     : TColor32;
    tag       : integer;
  end;

var
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/width
  defaultSvgWidth: integer = 300;
  // https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/height
  defaultSvgHeight: integer = 150;

implementation

uses
  Img32.Extra, Img32.Clipper2;

type
  TFourDoubles = array [0..3] of double;

  TDefsElement = class(TBaseElement)
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TStyleElement = class(TBaseElement)
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  // TImageElement only supports *embedded* jpg & png images.
  // And it requires Img32.Fmt.JPG & Img32.Fmt.PNG to be included
  // in the USES clause of at least one of the application's units.
  // (nb: If using the FMX framework, then add Img32.FMX instead of
  // Img32.Fmt.JPG & Img32.Fmt.PNG to the USES clause.)
  TImageElement = class(TBaseElement)
  private
    fRefEl: UTF8String;
    fImage: TImage32;
    fTransparent: Boolean;
  protected
    procedure Draw(image: TImage32; drawDat: TDrawData); override;
  public
    destructor Destroy; override;
  end;

  TGroupElement = class(TShapeElement)
  protected
    procedure Draw(image: TImage32; drawDat: TDrawData); override;
  end;

  TSwitchElement = class(TShapeElement)
  protected
    procedure Draw(image: TImage32; drawDat: TDrawData); override;
  end;

  TUseElement = class(TShapeElement)
  private
    callerUse: TBaseElement;
    function ValidateNonRecursion(el: TBaseElement): Boolean;
  protected
    fRefEl: UTF8String;
    procedure GetPaths(const drawDat: TDrawData); override;
    procedure Draw(img: TImage32; drawDat: TDrawData); override;
  end;

  TMaskElement = class(TShapeElement)
  protected
    maskRec: TRect;
    procedure GetPaths(const drawDat: TDrawData); override;
    procedure ApplyMask(img: TImage32; const drawDat: TDrawData);
  end;

  TSymbolElement = class(TShapeElement)
  protected
    viewboxWH: TRectWH;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  //-------------------------------------

  TPathElement = class(TShapeElement)
  private
    fSvgPaths   : TSvgPath;
    procedure Flatten(index: integer; scalePending: double;
      out path: TPathD; out isClosed: Boolean);
  protected
    function  GetBounds: TRectD; override;
    procedure ParseDAttrib(const value: UTF8String);
    procedure GetPaths(const drawDat: TDrawData); override;
    function  GetSimplePath(const drawDat: TDrawData): TPathsD; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
    destructor Destroy; override;
  end;

  TPolyElement = class(TShapeElement) //polyline or polygon
  protected
    path        : TPathD;
    function  GetBounds: TRectD; override;
    procedure ParsePoints(const value: UTF8String);
    procedure GetPaths(const drawDat: TDrawData); override;
    function  GetSimplePath(const drawDat: TDrawData): TPathsD; override;
  end;

  TLineElement = class(TShapeElement)
  protected
    path      : TPathD;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawDat: TDrawData); override;
    function  GetSimplePath(const drawDat: TDrawData): TPathsD; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TCircleElement = class(TShapeElement)
  protected
    bounds          : TRectD;
    centerPt        : TValuePt;
    radius          : TValue;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawDat: TDrawData); override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TEllipseElement = class(TShapeElement)
  protected
    bounds    : TRectD;
    centerPt  : TValuePt;
    radius    : TValuePt;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawDat: TDrawData); override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TRectElement = class(TShapeElement)
  protected
    radius    : TValuePt;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawDat: TDrawData); override;
    function  GetSimplePath(const drawDat: TDrawData): TPathsD; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TTSpanElement = class;

  // TTextElement: although a TShapeElement descendant, it's really just
  // a container for other TShapeElements (<tspan>, <textpath> etc).
  TTextElement = class(TShapeElement)
  protected
    offset      : TValuePt;
    textDx      : double;
    angle       : TArrayOfDouble;
    currentPt   : TPointD;
    currSpanEl  : TTSpanElement; //the current 'real' <tspan>
    lastChrSpc  : Boolean;
    procedure Draw(img: TImage32; drawDat: TDrawData); override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TTextSubElement = class(TShapeElement)
  protected
    offset      : TValuePt;
    textEl      : TTextElement;
    function GetTextEl: TTextElement;
  end;

  TTSpanElement = class(TTextSubElement)
  protected
    chunkDx     : double;
    angle       : TArrayOfDouble;
    procedure GetPaths(const drawDat: TDrawData); override;
  public
    procedure Draw(image: TImage32; drawDat: TDrawData); override;
  end;

  TTextPathElement = class(TTextSubElement)
  private
    pathEl: TPathElement;
    scale: double;
  protected
    pathName  : UTF8String; //name (id) of path element
    procedure GetPathsInternal(el: TBaseElement; const drawDat: TDrawData);
    procedure GetPaths(const drawDat: TDrawData); override;
    function  GetBounds: TRectD; override;
  public
    procedure Draw(image: TImage32; drawDat: TDrawData); override;
  end;

  TTextAreaElement = class(TShapeElement)
  protected
    procedure GetPaths(const drawDat: TDrawData); override;
  end;

  TMarkerElement = class(TShapeElement)
  private
    fPoints     : TPathD;
  protected
    refPt       : TValuePt;
    angle       : double;
    angle2      : double;
    markerBoxWH : TRectWH;
    autoStartReverse  : Boolean;
    procedure SetEndPoint(const pt: TPointD; angle: double);
    function SetMiddlePoints(const points: TPathD): Boolean;
    procedure Draw(img: TImage32; drawDat: TDrawData); override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TSvgColorStop = record
    offset    : double;
    color     : TColor32;
  end;
  TSvgColorStops = array of TSvgColorStop;

  TFillElement = class(TBaseElement)
  protected
    refEl : UTF8String;
    units : Cardinal;
    function  GetRelFracLimit: double; override;
  end;

  TPatternElement = class(TFillElement)
  protected
    ImgRenderer : TImageRenderer;
    pattBoxWH   : TRectWH;
    function PrepareRenderer(renderer: TImageRenderer;
      drawDat: TDrawData): Boolean; virtual;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
    destructor Destroy; override;
  end;

  //nb: gradients with objectBoundingBox should not be applied to
  //elements without width and height.
  TGradientElement = class(TFillElement)
  protected
    stops         : TSvgColorStops;
    spreadMethod  : TGradientFillStyle;
    function LoadContent: Boolean; override;
    procedure AddStop(color: TColor32; offset: double);
    procedure AssignTo(other: TBaseElement);  virtual;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawDat: TDrawData): Boolean; virtual;
    procedure AddColorStopsToRenderer(renderer: TCustomGradientRenderer);
  end;

  TRadGradElement = class(TGradientElement)
  protected
    radius: TValuePt;
    F, C: TValuePt;
    procedure AssignTo(other: TBaseElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawDat: TDrawData): Boolean; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TLinGradElement = class(TGradientElement)
  protected
    startPt, endPt: TValuePt;
    procedure AssignTo(other: TBaseElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawDat: TDrawData): Boolean; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TGradStopElement = class(TBaseElement)
  protected
    offset: double;
    color: TColor32;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TFilterElement = class(TBaseElement)
  private
    fSrcImg       : TImage32;
    fLastImg      : TImage32;
    fScale        : double;
    fFilterBounds : TRect;
    fObjectBounds : TRect;
    fImages       : array of TImage32;
    fNames        : array of UTF8String;
  protected
    procedure Clear;
    function GetRelFracLimit: double; override;
    function GetAdjustedBounds(const bounds: TRectD): TRectD;
    function FindNamedImage(const name: UTF8String): TImage32;
    function AddNamedImage(const name: UTF8String): TImage32;
    function GetNamedImage(const name: UTF8String; isIn: Boolean): TImage32;
    procedure Apply(img: TImage32;
      const filterBounds: TRect; const matrix: TMatrixD);
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
    destructor Destroy; override;
  end;

  TFeBaseElement = class(TBaseElement)
  private
    function GetParentAsFilterEl: TFilterElement;
  protected
    in1: UTF8String;
    in2: UTF8String;
    res: UTF8String;
    srcImg, dstImg: TImage32;
    srcRec, dstRec: TRect;
    function GetSrcAndDst: Boolean;
    function GetBounds(img: TImage32): TRect;
    procedure Apply; virtual; abstract;
    property ParentFilterEl: TFilterElement read GetParentAsFilterEl;
  end;

  TFeBlendElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TFeImageElement  = class(TFeBaseElement)
  private
    refEl: UTF8String;
    fImage: TImage32;
  protected
    procedure Apply; override;
  public
    destructor Destroy; override;
  end;

  TCompositeOp = (coOver, coIn, coOut, coAtop, coXOR, coArithmetic);

  TFeCompositeElement  = class(TFeBaseElement)
  protected
    fourKs: TFourDoubles; //arithmetic constants
    compositeOp: TCompositeOp;
    procedure Apply; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TFeColorMatrixElement  = class(TFeBaseElement)
  protected
    values: TArrayOfDouble;
    procedure Apply; override;
  end;

  TFuncType = (ftIdentity, ftTable, ftDiscrete, ftLinear, ftGamma);

  TFeComponentTransferElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TFeComponentTransferChild = class(TBaseElement)
  protected
    bytes: TArrayOfByte;
  protected
    funcType: TFuncType;
    intercept: double;
    slope: double;
    tableValues: TArrayOfDouble;
  end;

  TFeFuncRElement = class(TFeComponentTransferChild)
  end;
  TFeFuncGElement = class(TFeComponentTransferChild)
  end;
  TFeFuncBElement = class(TFeComponentTransferChild)
  end;
  TFeFuncAElement = class(TFeComponentTransferChild)
  end;

  TFeDefuseLightElement = class(TFeBaseElement)
  protected
    color         : TColor32;
    surfaceScale  : double;
    diffuseConst  : double;
    kernelSize    : integer;
    procedure Apply; override;
  end;

  TFeDropShadowElement = class(TFeBaseElement)
  protected
    stdDev      : double;
    offset        : TValuePt;
    floodColor  : TColor32;
    procedure Apply; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TFeFloodElement  = class(TFeBaseElement)
  protected
    floodColor  : TColor32;
    procedure Apply; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TFeGaussElement  = class(TFeBaseElement)
  protected
    stdDev: double;
    procedure Apply; override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;

  TFeMergeElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TFeMergeNodeElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TFeOffsetElement = class(TFeBaseElement)
  protected
    offset        : TValuePt;
    procedure Apply; override;
  end;

  TFePointLightElement = class(TFeBaseElement)
  protected
    z             : double;
  end;

  TFeSpecLightElement = class(TFeBaseElement)
  protected
    exponent      : double;
    color         : TColor32;
    procedure Apply; override;
  end;

  TClipPathElement = class(TShapeElement)
  protected
    units: Cardinal;
    procedure GetPaths(const drawDat: TDrawData); override;
  public
    constructor Create(parent: TBaseElement; svgEl: TSvgXmlEl); override;
  end;
  //-------------------------------------


const
  buffSize    = 32;
  clAlphaSet  = $00010101;
  SourceImage   : UTF8String = 'SourceGraphic';
  //SourceAlpha   : UTF8String = 'SourceAlpha';
  tmpFilterImg  : UTF8String = 'tmp';

  //https://www.w3.org/TR/css-fonts-3/#font-family-prop
  emptyDrawInfo: TDrawData =
    (currentColor: clInvalid;
    fillColor: clInvalid; fillOpacity: InvalidD;
    fillRule: frNegative; fillEl: '';
    strokeColor: clInvalid; strokeOpacity: InvalidD;
    strokeWidth: (rawVal: InvalidD; unitType: utNumber);
    strokeCap: esPolygon; strokeJoin: jsMiter; strokeMitLim: 0.0; strokeEl : '';
    dashArray: nil; dashOffset: 0;
    fontInfo: (family: tfUnknown; familyNames: nil; size: 0; spacing: 0.0;
    spacesInText: sitUndefined; textLength: 0; italic: sfsUndefined;
    weight: -1; align: staUndefined; decoration: fdUndefined;
    baseShift: (rawVal: InvalidD; unitType: utNumber));
    markerStart: ''; markerMiddle: ''; markerEnd: '';
    filterElRef: ''; maskElRef: ''; clipElRef: '';
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); visible: true;
    useEl: nil; bounds: (Left:0; Top:0; Right:0; Bottom:0));

var
  //defaultFontHeight: this size will be used to retrieve ALL glyph contours
  //(and later scaled as necessary). This relatively large default ensures
  //that contours will have adequate detail.
  defaultFontHeight: double = 20.0;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function HashToElementClass(hash: Cardinal): TElementClass;
begin
  case hash of
    hClippath       : Result := TClipPathElement;
    hCircle         : Result := TCircleElement;
    hDefs           : Result := TDefsElement;
    hEllipse        : Result := TEllipseElement;
    hFilter         : Result := TFilterElement;
    hfeBlend        : Result := TFeBlendElement;
    hfeColorMatrix  : Result := TFeColorMatrixElement;
    hFeComponentTransfer : Result := TFeComponentTransferElement;
    hFeFuncR        : Result := TFeFuncRElement;
    hFeFuncG        : Result := TFeFuncGElement;
    hFeFuncB        : Result := TFeFuncBElement;
    hFeFuncA        : Result := TFeFuncAElement;
    hfeComposite    : Result := TFeCompositeElement;
    hfeDefuseLighting : Result := TFeDefuseLightElement;
    hfeDropShadow   : Result := TFeDropShadowElement;
    hfeFlood        : Result := TFeFloodElement;
    hFeGaussianBlur : Result := TFeGaussElement;
    hFeImage        : Result := TFeImageElement;
    hfeMerge        : Result := TFeMergeElement;
    hfeMergeNode    : Result := TFeMergeNodeElement;
    hfeOffset       : Result := TFeOffsetElement;
    hfePointLight   : Result := TFePointLightElement;
    hfeSpecularLighting : Result := TFeSpecLightElement;
    hG              : Result := TGroupElement;
    hImage           : Result := TImageElement;
    hLine           : Result := TLineElement;
    hLineargradient : Result := TLinGradElement;
    hMarker         : Result := TMarkerElement;
    hMask           : Result := TMaskElement;
    hPath           : Result := TPathElement;
    hPattern        : Result := TPatternElement;
    hPolyline       : Result := TPolyElement;
    hPolygon        : Result := TPolyElement;
    hRadialgradient : Result := TRadGradElement;
    hRect           : Result := TRectElement;
    hStop           : Result := TGradStopElement;
    hStyle          : Result := TStyleElement;
    hSvg            : Result := TSvgElement;
    hSwitch         : Result := TSwitchElement;
    hSymbol         : Result := TSymbolElement;
    hText           : Result := TTextElement;
    hTextArea       : Result := TTextAreaElement;
    hTextPath       : Result := TTextPathElement;
    hTSpan          : Result := TTSpanElement;
    hUse            : Result := TUseElement;
    else              Result := TBaseElement; //use generic class
  end;
end;
//------------------------------------------------------------------------------

procedure UpdateDrawInfo(var drawDat: TDrawData; thisElement: TBaseElement);
begin
  with thisElement.fDrawData do
  begin
    if currentColor <> clInvalid then
      thisElement.fSvgReader.currentColor := currentColor;
    if fillRule <> frNegative then
      drawDat.fillRule := fillRule;
    if (fillColor = clCurrent) then
      drawDat.fillColor := thisElement.fSvgReader.currentColor
    else if (fillColor <> clInvalid) then
      drawDat.fillColor := fillColor;
    if fillOpacity <> InvalidD then
      drawDat.fillOpacity := fillOpacity;
    if (fillEl <> '') then
      drawDat.fillEl := fillEl;
    if (strokeColor = clCurrent) then
      drawDat.strokeColor := thisElement.fSvgReader.currentColor
    else if strokeColor <> clInvalid then
      drawDat.strokeColor := strokeColor;
    if strokeOpacity <> InvalidD then
      drawDat.strokeOpacity := strokeOpacity;
    if strokeWidth.IsValid then
      drawDat.strokeWidth := strokeWidth;
    if strokeCap = esPolygon then
      drawDat.strokeCap := strokeCap;
    if strokeJoin <> jsMiter then
      drawDat.strokeJoin := strokeJoin;
    if strokeMitLim > 0 then
      drawDat.strokeMitLim := strokeMitLim;
    if Assigned(dashArray) then
      drawDat.dashArray := Copy(dashArray, 0, Length(dashArray));
    if dashOffset <> 0 then
      drawDat.dashOffset := dashOffset;
    if (strokeEl <> '') then
      drawDat.strokeEl := strokeEl;

    if (clipElRef <> '') then
      drawDat.clipElRef := clipElRef;
    if (maskElRef <> '') then
      drawDat.maskElRef := maskElRef;
    if (filterElRef <> '') then
      drawDat.filterElRef := filterElRef;
    if not IsIdentityMatrix(matrix) then
      MatrixMultiply2(matrix, drawDat.matrix);
  end;
end;
//------------------------------------------------------------------------------

procedure UpdateFontInfo(var drawDat: TDrawData; thisElement: TBaseElement);
begin
  with thisElement.fDrawData do
  begin
    if fontInfo.family <> tfUnknown then
    begin
      drawDat.fontInfo.family := fontInfo.family;
      drawDat.fontInfo.familyNames := nil;
    end;
    if Assigned(fontInfo.familyNames) then
      drawDat.fontInfo.familyNames := fontInfo.familyNames;

    if fontInfo.size > 0 then
      drawDat.fontInfo.size := fontInfo.size;
    if fontInfo.spacing <> 0 then
      drawDat.fontInfo.spacing := fontInfo.spacing;
    if fontInfo.textLength > 0 then
      drawDat.fontInfo.textLength := fontInfo.textLength;

    if (fontInfo.italic <> sfsUndefined) then
      drawDat.fontInfo.italic := fontInfo.italic;
    if (fontInfo.weight <> -1) then
      drawDat.fontInfo.weight := fontInfo.weight;

    if fontInfo.align <> staUndefined then
      drawDat.fontInfo.align := fontInfo.align;

    if fontInfo.spacesInText <> sitUndefined then
      drawDat.fontInfo.spacesInText := fontInfo.spacesInText;

    if (thisElement is TTextElement) or
      (fontInfo.decoration <> fdUndefined) then
      drawDat.fontInfo.decoration := fontInfo.decoration;
    if fontInfo.baseShift.IsValid then
      drawDat.fontInfo.baseShift := fontInfo.baseShift;
  end;
end;
//------------------------------------------------------------------------------

function IsFilled(const drawDat: TDrawData): Boolean;
begin
  with drawDat do
    Result := (fillOpacity <> 0) and
      ((fillColor <> clNone32) or (fillEl <> ''));
end;
//------------------------------------------------------------------------------

function IsStroked(const drawDat: TDrawData): Boolean;
begin
  with drawDat do
    if (strokeOpacity = 0) then
      Result := false
    else if (strokeEl = '') and
      ((strokeColor = clNone32) or (strokeColor = clInvalid)) then
        Result := false
    else
      Result := ((strokeWidth.rawVal = InvalidD) or (strokeWidth.rawVal > 0));
end;
//------------------------------------------------------------------------------

function MergeColorAndOpacity(color: TColor32; opacity: double): TColor32;
begin
  if (opacity < 0) or (opacity >= 1.0) then Result := color or $FF000000
  else if opacity = 0 then Result := clNone32
  else Result := (color and $FFFFFF) + Round(opacity * 255) shl 24;
end;
//------------------------------------------------------------------------------

function UTF8StringToFloat(const ansiValue: UTF8String;
  out value: double): Boolean;
var
  c: PUTF8Char;
begin
  c := PUTF8Char(ansiValue);
  Result := ParseNextNum(c, c + Length(ansiValue), false, value);
end;
//------------------------------------------------------------------------------

function UTF8StringToFloatEx(const ansiValue: UTF8String;
  var value: double; out measureUnit: TUnitType): Boolean;
var
  c: PUTF8Char;
begin
  c := PUTF8Char(ansiValue);
  Result := ParseNextNumEx(c, c + Length(ansiValue), false, value, measureUnit);
end;
//------------------------------------------------------------------------------

procedure UTF8StringToOpacity(const ansiValue: UTF8String; var color: TColor32);
var
  opacity: double;
begin
  if color = clNone32 then
  begin
    color := clAlphaSet;
    Exit;
  end;

  if color = clInvalid then color := clNone32;
  if not UTF8StringToFloat(ansiValue, opacity) then Exit;
  with TARGB(color) do
    if (opacity <= 0) then
    begin
      if Color = clNone32 then Color := clAlphaSet
      else A := 0;
    end
    else if (opacity >= 1) then A := 255
    else A := Round(255 * opacity);
end;
//------------------------------------------------------------------------------

// Note: This MatrixApply() is a function, whereas in Img32.Transform it's a procedure.
function MatrixApply(const paths: TPathsD; const matrix: TMatrixD): TPathsD; overload;
var
  i,j,len,len2: integer;
  pp,rr: PPointD;
begin
  if not Assigned(paths) then
    Result := nil
  else if IsIdentityMatrix(matrix) then
    Result := CopyPaths(paths)
  else
  begin
    len := Length(paths);
    SetLength(Result, len);
    for i := 0 to len -1 do
    begin
      len2 := Length(paths[i]);
      NewPointDArray(Result[i], len2, True);
      if len2 = 0 then Continue;
      pp := @paths[i][0];
      rr := @Result[i][0];
      for j := 0 to High(paths[i]) do
      begin
        rr.X := pp.X * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
        rr.Y := pp.X * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
        inc(pp); inc(rr);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function FixSpaces(const text: UnicodeString; trimLeadingSpace: Boolean): UnicodeString;
var
  i,j, len: integer;
begin
  //changes \r\n\t chars to spaces
  //and trims consecutive spaces
  len  := Length(text);
  SetLength(Result, len);
  if len = 0 then Exit;

  if trimLeadingSpace then
  begin
    i := 1;
    while (i <= len) and (text[i] <= #32) do inc(i);
    if i > len then
    begin
      Result := '';
      Exit;
    end;
    Result[1] := text[i];
    inc(i);
  end else
  begin
    // allow a single leading space char
    if text[1] <= #32 then
      Result[1] := #32
    else
      Result[1] := text[1];
    i := 2;
  end;

  j := 1;
  for i := i to len do
  begin
    if (text[i] <= #32) then
    begin
      if (Result[j] = #32) then Continue;
      inc(j);
      Result[j] := #32;
    end else
    begin
      inc(j);
      Result[j] := text[i];
    end;
  end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function IsBlankText(const text: UnicodeString): Boolean;
var
  i: integer;
begin
  Result := false;
  for i := 1 to Length(text) do
    if (text[i] > #32) and (text[i] <> #160) then Exit;
  Result := true;
end;
//------------------------------------------------------------------------------

function SvgTextAlignToTextAlign(svgAlign: TSvgTextAlign): TTextAlign;
begin
  case svgAlign of
    staCenter: Result := taCenter;
    staRight: Result := taRight;
    staJustify: Result := taJustify;
    else Result := taLeft;
  end;
end;

//------------------------------------------------------------------------------
// TSvgIdNameHashMap
//------------------------------------------------------------------------------

procedure TSvgIdNameHashMap.Grow;
var
  Len, I: Integer;
  Index: Integer;
begin
  Len := Length(FItems);
  if Len < 5 then
    Len := 5
  else
    Len := Len * 2;

  SetLength(FItems, Len);
  FMod := Cardinal(Len);
  if not Odd(FMod) then
    Inc(FMod);
  SetLengthUninit(FBuckets, FMod);
  FillChar(FBuckets[0], FMod * SizeOf(FBuckets[0]), $FF);

  // Rehash
  for I := 0 to FCount - 1 do
  begin
    Index := (FItems[I].Hash and $7FFFFFFF) mod FMod;
    FItems[I].Next := FBuckets[Index];
    FBuckets[Index] := I;
  end;
end;
//------------------------------------------------------------------------------

function TSvgIdNameHashMap.FindItemIndex(const Name: UTF8String): Integer;
var
  hash: Cardinal;
begin
  Result := -1;
  if FMod = 0 then Exit;
  Hash := GetHash(Name);
  Result := FBuckets[(Hash and $7FFFFFFF) mod FMod];
  while (Result <> -1) and
    ((FItems[Result].Hash <> Hash) or
    not IsSameUTF8String(FItems[Result].Name, Name)) do
      Result := FItems[Result].Next;
end;
//------------------------------------------------------------------------------

procedure TSvgIdNameHashMap.AddOrIgnore(const idName: UTF8String; element: TBaseElement);
var
  Index: Integer;
  Hash: Cardinal;
  Item: PSvgIdNameHashMapItem;
  Bucket: PInteger;
begin
  Index := FindItemIndex(idName);
  if Index >= 0 then
    Exit; // already exists so ignore;

  // add new item
  if FCount = Length(FItems) then Grow;
  Index := FCount;
  Inc(FCount);

  Hash := GetHash(idName);
  Bucket := @FBuckets[(Hash and $7FFFFFFF) mod FMod];
  Item := @FItems[Index];
  Item.Next := Bucket^;
  Item.Hash := Hash;
  Item.Name := idName;
  Item.Element := element;
  Bucket^ := Index;
end;
//------------------------------------------------------------------------------

function TSvgIdNameHashMap.FindElement(const idName: UTF8String): TBaseElement;
var
  Index: Integer;
begin
  if FCount = 0 then
    Result := nil
  else
  begin
    Index := FindItemIndex(idName);
    if Index < 0 then
      Result := nil else
      Result := FItems[Index].Element
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgIdNameHashMap.Clear;
begin
  FCount := 0;
  FMod := 0;
  FItems := nil;
  FBuckets := nil;
end;

//------------------------------------------------------------------------------
// TDefsElement
//------------------------------------------------------------------------------

constructor TDefsElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  fDrawData.visible := false;
end;

//------------------------------------------------------------------------------
// TStyleElement
//------------------------------------------------------------------------------

constructor TStyleElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  fDrawData.visible := false;
  // See ParseStyleElementContent in Img32.Core.
end;

//------------------------------------------------------------------------------
// TImageElement
//------------------------------------------------------------------------------

function TrimAnySpaces(const s: UTF8String): UTF8String;
var
  i, j, len: integer;
  dst: PUTF8Char;
begin
  len := Length(s);
  SetLength(Result, len);
  dst := PUTF8Char(Pointer(Result));
  j := 0;
  for i := 1 to len do
    if s[i] > #32 then
    begin
      dst[j] := s[i];
      inc(j);
    end;
  if j <> len then
    SetLength(Result, j);
end;
//------------------------------------------------------------------------------

procedure ReadRefElImage(const refEl: UTF8String; out img: TImage32);
var
  len, offset: integer;
  s: UTF8String;
  ms: TMemoryStream;
  c: PUTF8Char;
begin
  img := nil;

  // unfortunately white spaces are sometimes found inside encoded base64
  s := TrimAnySpaces(refEl);
  len := Length(s);

  // currently only accepts **embedded** images
  if (len = 0) then Exit;

  c := PUTF8Char(s);
  if not Match(c, 'data:image/') then Exit;

  if      Match(@c[11], 'jpg;base64,') then offset := 22
  else if Match(@c[11], 'jpeg;base64,') then offset := 23
  else if Match(@c[11], 'png;base64,') then offset := 22
  else Exit;

  ms := TMemoryStream.Create;
  try
    if not Base64Decode(@c[offset], len -offset, ms) then Exit;
    img := TImage32.Create;
    if not img.LoadFromStream(ms) then
    begin
      FreeAndNil(img);
      Exit;
    end;
  finally
    ms.Free;
  end;
end;

//------------------------------------------------------------------------------
// TImageElement
//------------------------------------------------------------------------------

destructor TImageElement.Destroy;
begin
  if Assigned(fImage) then fImage.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TImageElement.Draw(image: TImage32; drawDat: TDrawData);
var
  dstRecD: TRectD;
  tmp: TImage32;
  tmpScale: TPointD;
begin
  dstRecD := Self.elRectWH.GetRectD(0,0);
  MatrixMultiply2(fDrawData.matrix, drawDat.matrix);

  MatrixApply(drawDat.matrix, dstRecD);

  if (fRefEl <> '') and not Assigned(fImage) then
  begin
    ReadRefElImage(fRefEl, fImage);
    if Assigned(fImage) then
    begin
      fRefEl := ''; // ie avoid reloading fImage
      fTransparent := fImage.HasTransparency;
    end;
  end;

  if fImage <> nil then
  begin
    if elRectWH.IsValid then
    begin
      tmpScale.X := elRectWH.width.rawVal / fImage.Width;
      tmpScale.Y := elRectWH.Height.rawVal / fImage.Height;
      MatrixScale(drawDat.matrix, tmpScale.X, tmpScale.Y);
    end;

    tmp := TImage32.Create();
    try
      tmp.AssignSettings(fImage);
      MatrixApply(drawDat.matrix, fImage, tmp);
      // CopyBlend is slower than Copy, so only use it if we have a
      // transparent image.
      if fTransparent then
        image.CopyBlend(tmp, tmp.Bounds, Rect(dstRecD), BlendToAlphaLine)
      else
        image.Copy(tmp, tmp.Bounds, Rect(dstRecD));
    finally
      tmp.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TGroupElement
//------------------------------------------------------------------------------

procedure TGroupElement.Draw(image: TImage32; drawDat: TDrawData);
var
  clipEl    : TClipPathElement;
  maskEl    : TMaskElement;
  tmpImg    : TImage32;
  clipPaths : TPathsD;
  clipRec   : TRect;
  dstClipRec: TRect;
  offsetX, offsetY: integer;
  fr: TFillRule;
begin
  if fChilds.Count = 0 then Exit;

  UpdateDrawInfo(drawDat, self);
  UpdateFontInfo(drawDat, self);

  if drawDat.fillRule = frNegative then
    drawDat.fillRule := frNonZero;

  maskEl := TMaskElement(FindRefElement(drawDat.maskElRef));
  clipEl := TClipPathElement(FindRefElement(drawDat.clipElRef));
  if Assigned(clipEl) then
  begin
    drawDat.clipElRef := '';
    with clipEl do
    begin
      GetPaths(drawDat);
      clipPaths := CopyPaths(drawPathsC);
      AppendPath(clipPaths, drawPathsO);
      MatrixApply(drawDat.matrix, clipPaths);
      clipRec := Img32.Vector.GetBounds(clipPaths);
    end;
    if IsEmptyRect(clipRec) then Exit;
    dstClipRec := clipRec; // save for blending tmpImg to image

    // Translate the clipPaths, clipRec and matrix
    // to minimize the size of the mask image.
    offsetX := clipRec.Left;
    offsetY := clipRec.Top;
    if offsetX < 0 then offsetX := 0;
    if offsetY < 0 then offsetY := 0;
    if (offsetX > 0) or (offsetY > 0) then
    begin
      MatrixTranslate(drawDat.matrix, -offsetX, -offsetY); // for DrawChildren
      clipPaths := TranslatePath(clipPaths, -offsetX, -offsetY);
      TranslateRect(clipRec, -offsetX, -offsetY);
    end;

    //nb: it's not safe to use fReader.TempImage when calling DrawChildren
    tmpImg := TImage32.Create(Min(image.Width, clipRec.Right), Min(image.Height, clipRec.Bottom));
    try
      DrawChildren(tmpImg, drawDat);
      if clipEl.fDrawData.fillRule = frNegative then
        fr := frNonZero else
        fr := clipEl.fDrawData.fillRule;
      EraseOutsidePaths(tmpImg, clipPaths, fr, clipRec, fSvgReader.fCustomRendererCache);
      image.CopyBlend(tmpImg, clipRec, dstClipRec, BlendToAlphaLine);
    finally
      tmpImg.Free;
    end;
  end
  else if Assigned(maskEl) then
  begin
    drawDat.maskElRef := '';
    with maskEl do
    begin
      GetPaths(drawDat);
      clipRec := maskRec;
    end;

    // Translate the maskRec, the matix and the clipRec to minimize
    // the size of the mask image.
    dstClipRec := clipRec; // save for blending tmpImg to image
    offsetX := -clipRec.Left;
    offsetY := -clipRec.Top;
    if offsetX > 0 then offsetX := 0;
    if offsetY > 0 then offsetY := 0;
    if (offsetX < 0) or (offsetY < 0) then
    begin
      MatrixTranslate(drawDat.matrix, offsetX, offsetY); // for DrawChildren
      TranslateRect(clipRec, offsetX, offsetY);
      TranslateRect(maskEl.maskRec, offsetX, offsetY);
    end;

    tmpImg := TImage32.Create(Min(image.Width, clipRec.Right), Min(image.Height, clipRec.Bottom));
    try
      DrawChildren(tmpImg, drawDat);
      TMaskElement(maskEl).ApplyMask(tmpImg, drawDat);
      image.CopyBlend(tmpImg, clipRec, dstClipRec, BlendToAlphaLine);
    finally
      tmpImg.Free;
    end;
  end else
    DrawChildren(image, drawDat);
end;

//------------------------------------------------------------------------------
// TSwitchElement
//------------------------------------------------------------------------------

procedure TSwitchElement.Draw(image: TImage32; drawDat: TDrawData);
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    if TBaseElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
        if fDrawData.visible then
        begin
          Draw(image, drawDat);
          break; //break after the first successful drawing
        end;
end;

//------------------------------------------------------------------------------
// TUseElement
//------------------------------------------------------------------------------

procedure TUseElement.GetPaths(const drawDat: TDrawData);
var
  el: TBaseElement;
  dx, dy: double;
begin
  if pathsLoaded or (fRefEl = '') then Exit;

  el := FindRefElement(fRefEl);
  if not Assigned(el) or not (el is TShapeElement) then Exit;
  pathsLoaded := true;

  with TShapeElement(el) do
  begin
    GetPaths(drawDat);
    self.drawPathsC := CopyPaths(drawPathsC);
    self.drawPathsO := CopyPaths(drawPathsO);
  end;

  if elRectWH.left.IsValid then
    dx := elRectWH.left.rawVal else
    dx := 0;
  if elRectWH.top.IsValid  then
    dy := elRectWH.top.rawVal else
    dy := 0;

  if (dx <> 0) or (dy <> 0) then
  begin
    drawPathsC := TranslatePath(drawPathsC, dx, dy);
    drawPathsO := TranslatePath(drawPathsO, dx, dy);
  end;
end;
//------------------------------------------------------------------------------

function TUseElement.ValidateNonRecursion(el: TBaseElement): Boolean;
begin
  Result := false;
  while assigned(el) do
  begin
    if (el = Self) then Exit;
    if not (el is TUseElement) then break; //shouldn't happen
    el := TUseElement(el).callerUse;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

procedure TUseElement.Draw(img: TImage32; drawDat: TDrawData);
var
  el: TBaseElement;
  s, dx, dy: double;
  scale, scale2: TPointD;
  mat: TMatrixD;
begin

  //make sure there's not recursion, either directly or indirectly
  if not ValidateNonRecursion(drawDat.useEl) then Exit;

  callerUse := drawDat.useEl;
  drawDat.useEl := self;

  el := FindRefElement(fRefEl);
  if not Assigned(el) then Exit;

  UpdateDrawInfo(drawDat, self); //nb: <use> attribs override el's.
  MatrixExtractScale(drawDat.matrix, scale.X, scale.Y);

  if elRectWH.left.IsValid then dx := elRectWH.left.rawVal else dx := 0;
  if elRectWH.top.IsValid  then dy := elRectWH.top.rawVal  else dy := 0;

  if (dx <> 0) or (dy <> 0) then
  begin
    mat := IdentityMatrix;
    MatrixTranslate(mat, dx, dy);
    MatrixMultiply2(mat, drawDat.matrix);
  end;

  if el is TSymbolElement then
  begin
    with TSymbolElement(el) do
    begin
      if not viewboxWH.IsEmpty then
      begin
        //scale the symbol according to its width and height attributes
        if elRectWH.width.IsValid and elRectWH.height.IsValid then
        begin
          scale2.X := elRectWH.width.rawVal / viewboxWH.Width;
          scale2.Y := elRectWH.height.rawVal / viewboxWH.Height;
          if scale2.Y < scale2.X then s := scale2.Y else s := scale2.X;
          //the following 3 lines will scale without translating
          mat := IdentityMatrix;
          MatrixScale(mat, s, s);
          MatrixMultiply2(mat, drawDat.matrix);
          drawDat.bounds := RectD(0,0,viewboxWH.Width, viewboxWH.Height);
        end;

        if self.elRectWH.width.IsValid and
          self.elRectWH.height.IsValid then
        begin
          with viewboxWH do
          begin
            dx := -Left/Width * self.elRectWH.width.rawVal;
            dy := -Top/Height * self.elRectWH.height.rawVal;

            //scale <symbol> proportionally to fill the <use> element
            scale2.X := self.elRectWH.width.rawVal / Width;
            scale2.Y := self.elRectWH.height.rawVal / Height;
            if scale2.Y < scale2.X then s := scale2.Y else s := scale2.X;
          end;

          mat := IdentityMatrix;
          MatrixScale(mat, s, s);
          MatrixTranslate(mat, dx, dy);
          MatrixMultiply2(mat, drawDat.matrix);

          //now center after scaling
          if scale2.X > scale2.Y then
          begin
            if scale2.X > 1 then
            begin
              s := (self.elRectWH.width.rawVal - viewboxWH.Width) * 0.5;
              MatrixTranslate(drawDat.matrix, s * scale.X, 0);
            end;
          end else if scale2.Y > 1 then
          begin
            s := (self.elRectWH.height.rawVal - viewboxWH.Height) * 0.5;
            MatrixTranslate(drawDat.matrix, 0, s * scale.Y);
          end;

        end;
      end;
      DrawChildren(img, drawDat);
    end;
  end
  else if el is TImageElement then
    el.Draw(img, drawDat)
  else if el is TShapeElement then
    el.Draw(img, drawDat);
end;

//------------------------------------------------------------------------------
// TMaskElement
//------------------------------------------------------------------------------

procedure TMaskElement.GetPaths(const drawDat: TDrawData);
var
  i   : integer;
  el  : TShapeElement;
begin
  maskRec := NullRect;
  for i := 0 to fChilds.Count -1 do
    if TBaseElement(fChilds[i]) is TShapeElement then
    begin
      el := TShapeElement(fChilds[i]);
      el.GetPaths(drawDat);
      maskRec := Img32.Vector.UnionRect(maskRec, Img32.Vector.GetBounds(el.drawPathsC));
      Img32.Vector.UnionRect(maskRec, Img32.Vector.GetBounds(el.drawPathsO));
    end;
  MatrixApply(drawDat.matrix, maskRec);
end;
//------------------------------------------------------------------------------

procedure TMaskElement.ApplyMask(img: TImage32; const drawDat: TDrawData);
var
  tmpImg: TImage32;
begin
  tmpImg := TImage32.Create(Min(img.Width, maskRec.Right), Min(img.Height, maskRec.Bottom));
  try
    DrawChildren(tmpImg, drawDat);
    img.CopyBlend(tmpImg, maskRec, maskRec, BlendBlueChannelLine);
  finally
    tmpImg.Free;
  end;
end;

//------------------------------------------------------------------------------
// TSymbolElement
//------------------------------------------------------------------------------

constructor TSymbolElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  fDrawData.visible := false;
end;

//------------------------------------------------------------------------------
// TGradElement
//------------------------------------------------------------------------------

function TGradientElement.LoadContent: Boolean;
var
  i: integer;
begin
  Result := inherited LoadContent;
  for i := 0 to fChilds.Count -1 do
    if TBaseElement(fChilds[i]) is TGradStopElement then
      with TGradStopElement(fChilds[i]) do
        AddStop(color, offset);
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AddStop(color: TColor32; offset: double);
var
  len: integer;
begin
  //if a stop is less than previous stops, it is set equal to the largest stop.
  //If two stops are equal the last stop controls the color from that point.
  len := Length(stops);
  if (len > 0) and (stops[len-1].offset > offset) then
    offset := stops[len-1].offset;
  setLength(stops, len+1);
  stops[len].offset := Min(1,Max(0, offset));
  stops[len].color := color;
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AssignTo(other: TBaseElement);
var
  i, len: integer;
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;

  with TGradientElement(other) do
  begin
    if units = 0 then
      units := Self.units;

    if Length(stops) = 0 then
    begin
      len := Length(self.stops);
      SetLength(stops, len);
      for i := 0 to len -1 do
        stops[i] := Self.stops[i];
    end;

    if IsIdentityMatrix(fDrawData.matrix) then
      fDrawData.matrix := self.fDrawData.matrix;
  end;
end;
//------------------------------------------------------------------------------

function TGradientElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; drawDat: TDrawData): Boolean;
var
  el: TBaseElement;
begin
  if (refEl <> '') then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TGradientElement) then
      TGradientElement(el).AssignTo(self);
  end;
  Result := Length(stops) > 0;
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AddColorStopsToRenderer(renderer: TCustomGradientRenderer);
var
  i, hiStops: Integer;
begin
  hiStops := High(stops);
  if (hiStops = 0) or (renderer = nil) then Exit;

  // If vector boundary-stops are implicit, then boundary
  // and adjacent inner stop (explicit) should have the
  // same color
  if stops[0].offset > 0 then
    with stops[0] do
      renderer.InsertColorStop(offset, color);

  for i := 1 to hiStops -1 do
    with stops[i] do
      renderer.InsertColorStop(offset, color);

  // If vector boundary-stops are implicit, then boundary
  // and adjacent inner stop (explicit) should have the
  // same color
  if stops[hiStops].offset < 1 then
    with stops[hiStops] do
      renderer.InsertColorStop(offset, color);
end;

//------------------------------------------------------------------------------
// TRadGradElement
//------------------------------------------------------------------------------

constructor TRadGradElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  radius.Init;
  F.Init;
  C.Init;
end;
//------------------------------------------------------------------------------

procedure TRadGradElement.AssignTo(other: TBaseElement);
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;
  if other is TRadGradElement then
    with TRadGradElement(other) do
    begin
      if not radius.IsValid then radius := self.radius;
      if not C.IsValid then C := self.C;
      if not F.IsValid then F := self.F;
    end;
end;
//------------------------------------------------------------------------------

function TRadGradElement.PrepareRenderer(renderer: TCustomGradientRenderer;
  drawDat: TDrawData): Boolean;
var
  hiStops: integer;
  cp, fp, r: TPointD;
  scale, scale2: TPointD;
  rec2, rec3: TRectD;
begin
  inherited PrepareRenderer(renderer, drawDat);
  hiStops := High(stops);
  Result := hiStops >= 0;
  if not Result then Exit;

  if units = hUserSpaceOnUse then
    rec2 := fSvgReader.userSpaceBounds else
    rec2 := drawDat.bounds;

  if radius.IsValid then
  begin
    if radius.X.HasFontUnits then
      r := radius.GetPoint(drawDat.fontInfo.size, GetRelFracLimit) else
      r := radius.GetPoint(rec2, GetRelFracLimit);
  end else
  begin
    r.X := rec2.Width * 0.5;
    r.Y := rec2.Height * 0.5;
  end;
  MatrixExtractScale(drawDat.matrix, scale.X, scale.Y);
  MatrixExtractScale(fDrawData.matrix, scale2.X, scale2.Y);
  r := ScalePoint(r, scale.X * scale2.X, scale.Y * scale2.Y);

  if C.IsValid then
  begin
    if C.X.HasFontUnits then
      cp := C.GetPoint(drawDat.fontInfo.size, GetRelFracLimit) else
      cp := C.GetPoint(rec2, GetRelFracLimit);
    cp := TranslatePoint(cp, rec2.Left, rec2.Top);
  end else
    cp := rec2.MidPoint;
  MatrixApply(fDrawData.matrix, cp);
  MatrixApply(drawDat.matrix, cp);

  rec3 := RectD(cp.X-r.X, cp.Y-r.Y, cp.X+r.X, cp.Y+r.Y);

  if F.IsValid then
  begin
    if F.X.HasFontUnits then
      fp := F.GetPoint(drawDat.fontInfo.size, GetRelFracLimit) else
      fp := F.GetPoint(rec2, GetRelFracLimit);
    fp := TranslatePoint(fp, rec2.Left, rec2.Top);
    MatrixApply(fDrawData.matrix, fp);
    MatrixApply(drawDat.matrix, fp);
  end else
    fp := MidPoint(rec3);

  with renderer as TSvgRadialGradientRenderer do
  begin
    SetParameters(Rect(rec3), Point(fp),
      stops[0].color, stops[hiStops].color, spreadMethod);
    AddColorStopsToRenderer(renderer);
  end;
end;

//------------------------------------------------------------------------------
// TLinGradElement
//------------------------------------------------------------------------------

constructor TLinGradElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  startPt.Init;
  endPt.Init;
end;
//------------------------------------------------------------------------------

procedure TLinGradElement.AssignTo(other: TBaseElement);
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;
  if other is TLinGradElement then
    with TLinGradElement(other) do
    begin
      if not startPt.IsValid then startPt := self.startPt;
      if not endPt.IsValid then endPt := self.endPt;
    end;
end;
//------------------------------------------------------------------------------

function TLinGradElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; drawDat: TDrawData): Boolean;
var
  pt1, pt2: TPointD;
  hiStops: integer;
  rec2: TRectD;
begin
  inherited PrepareRenderer(renderer, drawDat);
  hiStops := High(stops);
  Result := (hiStops >= 0);
  if not Result then Exit;

  //w3c-coords-units-01-b.svg

  //if gradientUnits=objectBoundingBox (default) then all values must be
  //percentages. Also... when the object's bounding box is not square, the
  //gradient may render non-perpendicular relative to the gradient vector
  //unless the gradient vector is vertical or horizontal.
  //https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientUnits

  if units = hUserSpaceOnUse then
    rec2 := fSvgReader.userSpaceBounds else
    rec2 := drawDat.bounds;

  with TLinearGradientRenderer(renderer) do
  begin
    if startPt.X.HasFontUnits then
      pt1 := startPt.GetPoint(drawDat.fontInfo.size, GetRelFracLimit) else
      pt1 := startPt.GetPoint(rec2, GetRelFracLimit);

    if (startPt.X.unitType <> utPixel) or
      (units <> hUserSpaceOnUse) then
        pt1.X := pt1.X + rec2.Left;

    if (startPt.Y.unitType <> utPixel) or
      (units <> hUserSpaceOnUse) then
        pt1.Y := pt1.Y + rec2.Top;

    MatrixApply(fDrawData.matrix, pt1);
    MatrixApply(drawDat.matrix, pt1);


    if not endPt.X.IsValid then
      pt2.X := rec2.Width else
      pt2.X := endPt.X.GetValue(rec2.Width, GetRelFracLimit);
    pt2.Y := endPt.Y.GetValue(rec2.Height, GetRelFracLimit);
    pt2 := TranslatePoint(pt2, rec2.Left, rec2.Top);

    MatrixApply(fDrawData.matrix, pt2);
    MatrixApply(drawDat.matrix, pt2);

    if (units <> hUserSpaceOnUse) and
      ((pt2.X <> pt1.X) or (pt2.Y <> pt1.Y)) then
    begin
      //skew the gradient
    end;

    SetParameters(pt1, pt2, stops[0].color,
      stops[hiStops].color, spreadMethod);
    AddColorStopsToRenderer(renderer);
  end;
end;

//------------------------------------------------------------------------------
// TGradStopElement
//------------------------------------------------------------------------------

constructor TGradStopElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  color := clBlack32;
end;

//------------------------------------------------------------------------------
// TFilterElement
//------------------------------------------------------------------------------

constructor TFilterElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  fDrawData.visible := false;
  elRectWH.Init;
end;
//------------------------------------------------------------------------------

destructor TFilterElement.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TFilterElement.Clear;
var
  i: integer;
begin
  for i := 0 to High(fImages) do
    fImages[i].Free;
  fImages := nil;
  fNames := nil;
  fLastImg := nil;
end;
//------------------------------------------------------------------------------

function TFilterElement.GetRelFracLimit: double;
begin
  // assume fractional values below 2.5 are always relative
  Result := 2.5;
end;
//------------------------------------------------------------------------------

function TFilterElement.GetAdjustedBounds(const bounds: TRectD): TRectD;
var
  recWH: TRectWH;
  delta: TSizeD;
  d: double;
  pt: TPointD;
  i: integer;
  hasOffset: Boolean;
begin
  fObjectBounds := Rect(bounds);
  if elRectWH.IsValid then
  begin
    recWH := elRectWH.GetRectWH(bounds, GetRelFracLimit);
    Result.Left := bounds.Left + recWH.Left;
    Result.Top := bounds.Top + recWH.Top;
    Result.Right := Result.Left + recWH.Width;
    Result.Bottom := Result.Top + recWH.Height;
  end else
  begin
    Result := bounds;

    //when the filter's width and height are undefined then limit the filter
    //margin to 20% of the bounds when just blurring, not also offsetting.
    hasOffset := false;

    delta.cx := 0; delta.cy := 0;
    for i := 0 to ChildCount -1 do
    begin
      if Child[i] is TFeGaussElement then
      begin
        d := TFeGaussElement(Child[i]).stdDev * 3 * fScale;
        delta.cx := delta.cx + d;
        delta.cy := delta.cy + d;
      end
      else if Child[i] is TFeDropShadowElement then
        with TFeDropShadowElement(Child[i]) do
        begin
          d := stdDev * 0.75 * fScale;
          pt := offset.GetPoint(bounds, 1);
          delta.cx := delta.cx + d + Abs(pt.X) * fScale;
          delta.cy := delta.cy + d + Abs(pt.Y) * fScale;
          hasOffset := true;
        end
      else if Child[i] is TFeOffsetElement then
        with TFeOffsetElement(Child[i]) do
        begin
          pt := offset.GetPoint(bounds, 1);
          delta.cx := delta.cx + Abs(pt.X) * fScale;
          delta.cy := delta.cy + Abs(pt.Y) * fScale;
          hasOffset := true;
        end;
    end;

    if (delta.cx = InvalidD) or (delta.cy = InvalidD) then Exit;

    //limit the filter margin to 20% if only blurring
    if not hasOffset then
      with delta, bounds do
      begin
        if cx > Width * 0.2 then cx := Width * 0.2;
        if cy > Height * 0.2 then cy := Height * 0.2;
      end;
    Img32.Vector.InflateRect(Result, delta.cx, delta.cy);

  end;
end;
//------------------------------------------------------------------------------

function TFilterElement.FindNamedImage(const name: UTF8String): TImage32;
var
  i, len: integer;
begin
  Result := nil;
  len := Length(fNames);
  for i := 0 to len -1 do
    if name = fNames[i] then
    begin
      Result := fImages[i];
      Break;
    end;
end;
//------------------------------------------------------------------------------

function TFilterElement.AddNamedImage(const name: UTF8String): TImage32;
var
  len, w, h: integer;
begin
  len := Length(fNames);
  SetLength(fNames, len+1);
  SetLength(fImages, len+1);
  RectWidthHeight(fFilterBounds, w, h);
  Result := TImage32.Create(w, h);
  fImages[len] := Result;
  fNames[len] := name;
end;
//------------------------------------------------------------------------------

function TFilterElement.GetNamedImage(const name: UTF8String; isIn: Boolean): TImage32;
begin
  Result := FindNamedImage(name);
  if not Assigned(Result) then
    Result := AddNamedImage(name)
  else if not isIn then
    Exit;

  case GetHash(name) of
    hBackgroundImage:
      Result.Copy(fSvgReader.BackgndImage, fFilterBounds, Result.Bounds);
    hBackgroundAlpha:
      begin
        Result.Copy(fSvgReader.BackgndImage, fFilterBounds, Result.Bounds);
        Result.SetRGB(clNone32, Result.Bounds);
      end;
    hSourceGraphic:
      Result.Copy(fSrcImg, fFilterBounds, Result.Bounds);
    hSourceAlpha:
      begin
        Result.Copy(fSrcImg, fFilterBounds, Result.Bounds);
        Result.SetRGB(clBlack32, Result.Bounds);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TFilterElement.Apply(img: TImage32;
  const filterBounds: TRect; const matrix: TMatrixD);
var
  i: integer;
begin
  MatrixExtractScale(matrix, fScale);
  fFilterBounds := filterBounds;
  Types.IntersectRect(fObjectBounds, fObjectBounds, img.Bounds);
  fSrcImg := img;

  try
    for i := 0 to fChilds.Count -1 do
    begin
      case TBaseElement(fChilds[i]).fXmlEl.hash of
        hfeBlend            : TFeBlendElement(fChilds[i]).Apply;
        hfeColorMatrix      : TFeColorMatrixElement(fChilds[i]).Apply;
        hFeComponentTransfer : TFeComponentTransferElement(fChilds[i]).Apply;
        hfeComposite        : TFeCompositeElement(fChilds[i]).Apply;
        hfeDefuseLighting   : TFeDefuseLightElement(fChilds[i]).Apply;
        hfeDropShadow       : TFeDropShadowElement(fChilds[i]).Apply;
        hfeFlood            : TFeFloodElement(fChilds[i]).Apply;
        hfeImage            : TFeImageElement(fChilds[i]).Apply;
        hFeGaussianBlur     : TFeGaussElement(fChilds[i]).Apply;
        hfeMerge            : TFeMergeElement(fChilds[i]).Apply;
        hfeOffset           : TFeOffsetElement(fChilds[i]).Apply;
        hfeSpecularLighting : TFeSpecLightElement(fChilds[i]).Apply;
      end;
    end;
    if Assigned(fLastImg) then
      fSrcImg.Copy(fLastImg, fLastImg.Bounds, fFilterBounds);
  finally
    Clear;
  end;
end;

//------------------------------------------------------------------------------
// TFeBaseElement
//------------------------------------------------------------------------------

function TFeBaseElement.GetParentAsFilterEl: TFilterElement;
var
  el: TBaseElement;
begin
  el := fParent;
  while Assigned(el) and not (el is TFilterElement) do
    el := el.fParent;
  if not Assigned(el) then
    Result := nil else
    Result := TFilterElement(el);
end;
//------------------------------------------------------------------------------

function TFeBaseElement.GetBounds(img: TImage32): TRect;
var
  pfe: TFilterElement;
begin
  pfe := ParentFilterEl;
  if img = pfe.fSrcImg then
    Result := pfe.fFilterBounds else
    Result := img.Bounds;
end;
//------------------------------------------------------------------------------

function TFeBaseElement.GetSrcAndDst: Boolean;
var
  pfe: TFilterElement;
begin
  pfe := ParentFilterEl;
  if (in1 <> '') then
    srcImg := pfe.GetNamedImage(in1, true)
  else if Assigned(pfe.fLastImg) then
    srcImg := pfe.fLastImg
  else
    srcImg := pfe.GetNamedImage(SourceImage, false);

  if (res <> '') then
    dstImg := pfe.GetNamedImage(res, false) else
    dstImg := pfe.GetNamedImage(SourceImage, false);

  Result := Assigned(srcImg) and Assigned(dstImg);
  if not Result then Exit;
  pfe.fLastImg := dstImg;
  srcRec := GetBounds(srcImg);
  dstRec := GetBounds(dstImg);
end;

//------------------------------------------------------------------------------
// TFeBlendElement
//------------------------------------------------------------------------------

procedure TFeBlendElement.Apply;
var
  pfe: TFilterElement;
  srcImg2, dstImg2: TImage32;
  srcRec2, dstRec2: TRect;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  if (in2 = '') then Exit;
  if dstImg = srcImg then
    dstImg2 := pfe.AddNamedImage(tmpFilterImg) else
    dstImg2 := dstImg;
  dstRec2 := GetBounds(dstImg2);

  srcImg2 := pfe.GetNamedImage(in2, true);
  srcRec2 := GetBounds(srcImg2);
  dstImg2.CopyBlend(srcImg2, srcRec2, dstRec2, BlendToAlphaLine);
  dstImg2.CopyBlend(srcImg,  srcRec,  dstRec2, BlendToAlphaLine);
  if dstImg = srcImg then
    dstImg.Copy(dstImg2, dstRec2, dstRec);
end;

//------------------------------------------------------------------------------
// TFeImageElement
//------------------------------------------------------------------------------

destructor TFeImageElement.Destroy;
begin
  fImage.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TFeImageElement.Apply;
begin
  if GetSrcAndDst then
  begin
    if refEl <> '' then
      ReadRefElImage(refEl, fImage); // also clears refEl

    if fImage <> nil then
      dstImg.Copy(fImage, fImage.Bounds, dstRec);
  end;
end;

//------------------------------------------------------------------------------
// TFeCompositeElement
//------------------------------------------------------------------------------

constructor TFeCompositeElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  fourKs[0] := InvalidD; fourKs[1] := InvalidD;
  fourKs[2] := InvalidD; fourKs[3] := InvalidD;
end;
//------------------------------------------------------------------------------

procedure Arithmetic(p1, p2, r: PColor32; const ks: array of byte);
var
  c1  : PARGB absolute p1;
  c2  : PARGB absolute p2;
  res : PARGB absolute r;
begin
  res.A := (((c1.A xor 255) * (c2.A xor 255)) shr 8) xor 255;
  res.R := ClampByte((ks[0] * c1.R * c2.R +
    ks[1] * c1.R * 255 + ks[2] * c2.R * 255 + ks[3] * 65025) shr 16);
  res.G := ClampByte((ks[0] * c1.G * c2.G +
    ks[1] * c1.G * 255 + ks[2] * c2.G * 255 + ks[3] * 65025) shr 16);
  res.B := ClampByte((ks[0] * c1.B * c2.B +
    ks[1] * c1.B * 255 + ks[2] * c2.B * 255 + ks[3] * 65025) shr 16);
end;
//------------------------------------------------------------------------------

procedure ArithmeticBlend(src1, src2, dst: TImage32;
  const recS1, recS2, recDst: TRect; const ks: TFourDoubles);
var
  kk: array[0..3] of byte;
  w,h, w2,h2, w3,h3, i,j: integer;
  p1,p2,r: PColor32;
begin
  RectWidthHeight(recS1, w, h);
  RectWidthHeight(recS2, w2, h2);
  RectWidthHeight(recDst, w3, h3);
  if (w2 <> w) or (w3 <> w) or (h2 <> h) or (h3 <> h) or
    (ks[0] = InvalidD) or (ks[1] = InvalidD) or
    (ks[2] = InvalidD) or (ks[3] = InvalidD) then Exit;

  for i := 0 to 3 do
    kk[i] := ClampByte(ks[i]*255);

  for i := 0 to h -1 do
  begin
    p1 := @src1.Pixels[(recS1.Top + i) * src1.Width + recS1.Left];
    p2 := @src2.Pixels[(recS2.Top + i) * src2.Width + recS2.Left];
    r  := @dst.Pixels[(recDst.Top + i) * dst.Width + recDst.Left];
    for j := 0 to w -1 do
    begin
      Arithmetic(p1, p2, r, kk);
      inc(p1); inc(p2); inc(r);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TFeCompositeElement.Apply;
var
  pfe: TFilterElement;
  srcImg2, dstImg2: TImage32;
  srcRec2, dstRec2: TRect;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  if (in2 = '') then Exit;

  srcImg2 := pfe.GetNamedImage(in2, true);
  srcRec2 := GetBounds(srcImg2); //either filter bounds or image bounds

  if (dstImg = srcImg) or (dstImg = srcImg2) then
    dstImg2 := pfe.AddNamedImage(tmpFilterImg) else
    dstImg2 := dstImg;
  dstRec2 := GetBounds(dstImg2); //either filter bounds or image bounds

  case compositeOp of
    coIn:
      begin
        dstImg2.Copy(srcImg, srcRec, dstRec2);
        dstImg2.CopyBlend(srcImg2,  srcRec2,  dstRec2, BlendMaskLine);
      end;
    coOut:
      begin
        dstImg2.Copy(srcImg, srcRec, dstRec2);
        dstImg2.CopyBlend(srcImg2,  srcRec2,  dstRec2, BlendInvertedMaskLine);
      end;
    coAtop:
      begin
        dstImg2.Copy(srcImg2, srcRec2, dstRec2);
        dstImg2.CopyBlend(srcImg,  srcRec,  dstRec2, BlendToAlphaLine);
        dstImg2.CopyBlend(srcImg2,  srcRec2,  dstRec2, BlendMaskLine);
      end;
    coXOR:
      begin
        dstImg2.Copy(srcImg2, srcRec2, dstRec2);
        dstImg2.CopyBlend(srcImg, srcRec, dstRec2, BlendToAlphaLine);
        dstImg2.CopyBlend(srcImg2,  srcRec2,  dstRec2, BlendInvertedMaskLine);
      end;
    coArithmetic:
      begin
        ArithmeticBlend(srcImg, srcImg2, dstImg2,
          srcRec, srcRec2, dstRec2, fourKs);
      end;
    else     //coOver
      begin
        dstImg2.CopyBlend(srcImg2, srcRec2, dstRec2, BlendToAlphaLine);
        dstImg2.CopyBlend(srcImg,  srcRec,  dstRec2, BlendToAlphaLine);
      end;
  end;
  if (dstImg <> dstImg2) then
    dstImg.Copy(dstImg2, dstRec2, dstRec);
end;

//------------------------------------------------------------------------------
// TFeColorMatrixElement
//------------------------------------------------------------------------------

type
  TColorMatrix = array[0..19] of Byte;

function ApplyColorMatrix(color: TColor32; const mat: TColorMatrix): TColor32;
var
  clrIn : TARGB absolute color;
  clrOut: TARGB absolute Result;
begin
  clrOut.R := ClampByte(MulBytes(mat[0],clrIn.R) + MulBytes(mat[1],clrIn.G) +
    MulBytes(mat[2],clrIn.B) + MulBytes(mat[3],clrIn.A) + mat[4]);
  clrOut.G := ClampByte(MulBytes(mat[5],clrIn.R) + MulBytes(mat[6],clrIn.G) +
    MulBytes(mat[7],clrIn.B) + MulBytes(mat[8],clrIn.A) + mat[9]);
  clrOut.B := ClampByte(MulBytes(mat[10],clrIn.R) + MulBytes(mat[11],clrIn.G) +
    MulBytes(mat[12],clrIn.B) + MulBytes(mat[13],clrIn.A) + mat[14]);
  clrOut.A := ClampByte(MulBytes(mat[15],clrIn.R) + MulBytes(mat[16],clrIn.G) +
    MulBytes(mat[17],clrIn.B) + MulBytes(mat[18],clrIn.A) + mat[19]);
end;
//------------------------------------------------------------------------------

procedure TFeColorMatrixElement.Apply;
var
  i,j, dx1,dx2: integer;
  colorMatrix: TColorMatrix;
  p1, p2: PColor32;
begin
  if not GetSrcAndDst or not Assigned(values) then Exit;
  for i := 0 to 19 do
    colorMatrix[i] := ClampByte(Integer(Round(values[i]*255)));

  dx1 := srcImg.Width - RectWidth(srcRec);
  dx2 := dstImg.Width - RectWidth(dstRec);
  p1 := @srcImg.Pixels[srcRec.Top * srcImg.Width + srcRec.Left];
  p2 := @dstImg.Pixels[dstRec.Top * dstImg.Width + dstRec.Left];
  for i := srcRec.Top to srcRec.Bottom -1 do
  begin
    for j := srcRec.Left to srcRec.Right -1 do
    begin
      p2^ := ApplyColorMatrix(p1^, colorMatrix);
      inc(p1); inc(p2);
    end;
    inc(p1, dx1); inc(p2, dx2);
  end;
end;

//------------------------------------------------------------------------------
// TFeComponentTransferElement
//------------------------------------------------------------------------------

procedure TFeComponentTransferElement.Apply;
var
  i,j,k, dx1,dx2: integer;
  d: double;
  rangeSize: integer;
  p1: PColor32;
  p2: PARGB;
  childFuncs: array[0..3] of TFeComponentTransferChild;
begin
  if not GetSrcAndDst or (ChildCount = 0) then Exit;
  for i := 0 to 3 do childFuncs[i] := nil;
  for i := 0 to ChildCount -1 do
  begin
    if Child[i] is TFeFuncBElement then
      childFuncs[0] := TFeFuncBElement(Child[i])
    else if Child[i] is TFeFuncGElement then
      childFuncs[1] := TFeFuncGElement(Child[i])
    else if Child[i] is TFeFuncRElement then
      childFuncs[2] := TFeFuncRElement(Child[i])
    else if Child[i] is TFeFuncAElement then
      childFuncs[3] := TFeFuncAElement(Child[i]);
  end;

  // build each childFuncs' bytes array
  for k := 0 to 3 do
    with childFuncs[k] do
    begin
      if not Assigned(childFuncs[k]) then Continue;
      case funcType of
        ftDiscrete:
          begin
            if Length(tableValues) = 0 then Continue;
            SetLength(bytes, 256);
            rangeSize := 256 div Length(tableValues);
            for i:= 0 to High(tableValues) do
              for j:= 0 to rangeSize -1 do
                bytes[i*rangeSize + j] := ClampByte(tableValues[i] * 255);
          end;
        ftTable:
          begin
            if Length(tableValues) < 2 then Continue;
            SetLength(bytes, 256);
            rangeSize := 256 div (Length(tableValues) -1);
            for i:= 0 to High(tableValues) -1 do
            begin
              intercept := tableValues[i];
              slope :=  (tableValues[i+1] - intercept) / rangeSize;
              for j:= 0 to rangeSize -1 do
                bytes[i*rangeSize + j] := ClampByte((j * slope + intercept) * 255);
            end;
          end;
        ftLinear:
          begin
            SetLength(bytes, 256);
            d := intercept * 255;
            for i:= 0 to 255 do
              bytes[i] := ClampByte(i * slope + d);
          end;
      end;
    end;

  for k := 0 to 3 do
    if Assigned(childFuncs[k]) and not Assigned(childFuncs[k].bytes) then
      childFuncs[k] := nil;

  dx1 := srcImg.Width - RectWidth(srcRec);
  dx2 := dstImg.Width - RectWidth(dstRec);
  p1 := @srcImg.Pixels[srcRec.Top * srcImg.Width + srcRec.Left];
  p2 := @dstImg.Pixels[dstRec.Top * dstImg.Width + dstRec.Left];
  for i := srcRec.Top to srcRec.Bottom -1 do
  begin
    for j := srcRec.Left to srcRec.Right -1 do
    begin
      p2.Color := p1^;
      if Assigned(childFuncs[0]) then p2.B := childFuncs[0].bytes[p2.B];
      if Assigned(childFuncs[1]) then p2.G := childFuncs[1].bytes[p2.G];
      if Assigned(childFuncs[2]) then p2.R := childFuncs[2].bytes[p2.R];
      if Assigned(childFuncs[3]) then p2.A := childFuncs[3].bytes[p2.A];
      inc(p1); inc(p2);
    end;
    inc(p1, dx1); inc(p2, dx2);
  end;
end;

//------------------------------------------------------------------------------
// TFeDefuseLightElement
//------------------------------------------------------------------------------

procedure TFeDefuseLightElement.Apply;
begin
  //not implemented
  if not GetSrcAndDst then Exit;
  if srcImg <> dstImg then
    dstImg.Copy(srcImg, srcRec, dstRec);
end;

//------------------------------------------------------------------------------
// TFeDropShadowElement
//------------------------------------------------------------------------------

constructor TFeDropShadowElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  stdDev := InvalidD;
  floodColor := clInvalid;
  offset.X.SetValue(0);
  offset.Y.SetValue(0);
end;
//------------------------------------------------------------------------------

procedure TFeDropShadowElement.Apply;
var
  alpha: Byte;
  off: TPointD;
  dstOffRec: TRect;
  pfe: TFilterElement;
  dropShadImg: TImage32;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  dropShadImg := pfe.GetNamedImage(tmpFilterImg, false);
  dropShadImg.Copy(srcImg, srcRec, dropShadImg.Bounds);

  off := offset.GetPoint(RectD(pfe.fObjectBounds), GetRelFracLimit);
  off := ScalePoint(off, pfe.fScale);
  dstOffRec := dstRec;
  with Point(off) do TranslateRect(dstOffRec, X, Y);
  dstImg.Copy(srcImg, srcRec, dstOffRec);
  dstImg.SetRGB(floodColor);
  alpha := GetAlpha(floodColor);
  if (alpha > 0) and (alpha < 255) then
    dstImg.ReduceOpacity(alpha);
  if stdDev > 0 then
    FastGaussianBlur(dstImg, dstRec,
      Ceil(stdDev *0.75 * ParentFilterEl.fScale) , 1);
  dstImg.CopyBlend(dropShadImg, dropShadImg.Bounds, dstRec, BlendToAlphaLine);
end;

//------------------------------------------------------------------------------
// TFeFloodElement
//------------------------------------------------------------------------------

constructor TFeFloodElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  floodColor := clInvalid;
end;
//------------------------------------------------------------------------------

procedure TFeFloodElement.Apply;
var
  rec: TRect;
begin
  if not GetSrcAndDst then Exit;
  if elRectWH.IsValid then
    rec := Rect(elRectWH.GetRectD(RectD(srcRec), GetRelFracLimit)) else
    rec := dstRec;
  dstImg.FillRect(rec, floodColor);
end;

//------------------------------------------------------------------------------
// TFeGaussElement
//------------------------------------------------------------------------------

constructor TFeGaussElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  stdDev := InvalidD;
end;
//------------------------------------------------------------------------------

procedure TFeGaussElement.Apply;
begin
  if (stdDev = InvalidD) or not GetSrcAndDst then Exit;

  if srcImg <> dstImg then
    dstImg.Copy(srcImg, srcRec, dstRec);
  //GaussianBlur(dstImg, dstRec, Round(stdDev * ParentFilterEl.fScale));
  // FastGaussianBlur is a very good approximation and also much faster.
  // However, empirically stdDev/2 more closely emulates other renderers.
  FastGaussianBlur(dstImg, dstRec, Ceil(stdDev/2 * ParentFilterEl.fScale));
end;

//------------------------------------------------------------------------------
// TFeMergeElement
//------------------------------------------------------------------------------

procedure TFeMergeElement.Apply;
var
  i: integer;
  tmpImg: TImage32;
  pfe: TFilterElement;
begin
  tmpImg := nil;
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;

  for i := 0 to fChilds.Count -1 do
    if TBaseElement(fChilds[i]) is TFeMergeNodeElement then
      with TFeMergeNodeElement(fChilds[i]) do
      begin
        if not GetSrcAndDst then Continue;
        if Assigned(tmpImg) then
          tmpImg.CopyBlend(srcImg, srcRec, tmpImg.Bounds, BlendToAlphaLine)
        else if srcImg = pfe.fSrcImg then
          tmpImg := pfe.GetNamedImage(SourceImage, false)
        else
          tmpImg := srcImg;
      end;

  dstImg.Copy(tmpImg, tmpImg.Bounds, dstRec);
  pfe.fLastImg := dstImg;
end;

//------------------------------------------------------------------------------
// TFeMergeNodeElement
//------------------------------------------------------------------------------

procedure TFeMergeNodeElement.Apply;
begin
  //should never get here ;)
end;

//------------------------------------------------------------------------------
// TFeOffsetElement
//------------------------------------------------------------------------------

procedure TFeOffsetElement.Apply;
var
  off: TPointD;
  dstOffRec: TRect;
  tmpImg: TImage32;
  pfe: TFilterElement;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  off := offset.GetPoint(RectD(pfe.fObjectBounds), GetRelFracLimit);
  off := ScalePoint(off, pfe.fScale);
  dstOffRec := dstRec;
  with Point(off) do TranslateRect(dstOffRec, X, Y);

  if srcImg = dstImg then
  begin
    tmpImg := pfe.GetNamedImage(tmpFilterImg, false);
    tmpImg.Copy(srcImg, srcRec, tmpImg.Bounds);
    dstImg.Clear(dstRec);
    dstImg.Copy(tmpImg, tmpImg.Bounds, dstOffRec);
  end else
  begin
    dstImg.Clear(dstRec);
    dstImg.Copy(srcImg, srcRec, dstOffRec);
  end;
end;

//------------------------------------------------------------------------------
// TFeSpecLightElement
//------------------------------------------------------------------------------

procedure TFeSpecLightElement.Apply;
begin
  //not implemented
  if not GetSrcAndDst then Exit;
  if srcImg <> dstImg then
    dstImg.Copy(srcImg, srcRec, dstRec);
end;

//------------------------------------------------------------------------------
// TClipPathElement
//------------------------------------------------------------------------------

constructor TClipPathElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  fDrawData.visible := false;
end;
//------------------------------------------------------------------------------

procedure TClipPathElement.GetPaths(const drawDat: TDrawData);
var
  i: integer;
begin
  if pathsLoaded then Exit;
  pathsLoaded := true;
  for i := 0 to fChilds.Count -1 do
    if TBaseElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        GetPaths(drawDat);
        AppendPath(self.drawPathsO, drawPathsO);
        AppendPath(self.drawPathsC, drawPathsC);
        // apply child's matrix ...
        MatrixApply(DrawData.matrix, self.drawPathsC);
        MatrixApply(DrawData.matrix, self.drawPathsO);
      end;
  // apply <clippath>'s matrix ...
  MatrixApply(DrawData.matrix, drawPathsC);
  MatrixApply(DrawData.matrix, drawPathsO);
end;

//------------------------------------------------------------------------------
// TShapeElement
//------------------------------------------------------------------------------

constructor TShapeElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  elRectWH.Init;
  hasPaths := true;
  fDrawData.visible := true;
  if fXmlEl.name = '' then Exit;
end;
//------------------------------------------------------------------------------

function TShapeElement.GetBounds: TRectD;
var
  i: integer;
begin
  Result := UnionRect(GetBoundsD(drawPathsC), GetBoundsD(drawPathsO));
  if Result.IsEmpty then
  begin
    Result := NullRectD;
    for i := 0 to fChilds.Count -1 do
      if TBaseElement(Child[i]) is TShapeElement then
        Result := UnionRect(Result, TShapeElement(Child[i]).GetBounds);
  end;
end;
//------------------------------------------------------------------------------

function TShapeElement.HasMarkers: Boolean;
begin
  Result := IsStroked(fDrawData) and ((fDrawData.markerStart <> '') or
    (fDrawData.markerMiddle <> '') or (fDrawData.markerEnd <> ''));
end;
//------------------------------------------------------------------------------

procedure TShapeElement.Draw(image: TImage32; drawDat: TDrawData);
var
  d           : double;
  img         : TImage32;
  stroked     : Boolean;
  filled      : Boolean;
  tmpRec      : TRectD;
  clipRec     : TRectD;
  clipRec2    : TRect;
  clipPathEl  : TBaseElement;
  filterEl    : TBaseElement;
  maskEl      : TBaseElement;
  clipPaths   : TPathsD;
  fillPaths   : TPathsD;
  di          : TDrawData;
  useTmpImage : Boolean;
begin
  UpdateDrawInfo(drawDat, self);

  filled := IsFilled(drawDat);
  stroked := IsStroked(drawDat);
  GetPaths(drawDat);

  if not (filled or stroked) or not hasPaths then
  begin
    inherited;
    Exit;
  end;
  tmpRec := GetBounds;
  if not tmpRec.IsEmpty then
    drawDat.bounds := tmpRec;

  img := image;
  clipRec2 := NullRect;

  maskEl := FindRefElement(drawDat.maskElRef);
  clipPathEl := FindRefElement(drawDat.clipElRef);
  filterEl := FindRefElement(drawDat.filterElRef);

  useTmpImage :=
    Assigned(clipPathEl) or Assigned(filterEl) or Assigned(maskEl);

  if useTmpImage then
  begin
    img := fSvgReader.TempImage;

    //get special effects bounds
    if Assigned(clipPathEl) then
    begin
      drawDat.clipElRef := '';
      di := drawDat;
      with TClipPathElement(clipPathEl) do
      begin
        GetPaths(di);
        clipPaths := drawPathsC;
        AppendPath(clipPaths, drawPathsO);
        clipPaths := MatrixApply(clipPaths, di.matrix);
        clipRec := GetBoundsD(clipPaths);
      end;
    end
    else if Assigned(maskEl) then
    begin
      drawDat.maskElRef := '';
      with TMaskElement(maskEl) do
      begin
        GetPaths(drawDat);
        clipRec := RectD(maskRec);
      end;
    end else
    begin
      clipRec := drawDat.bounds;
      if clipRec.IsEmpty and (drawDat.fontInfo.textLength > 0) and
        (self is TTextPathElement) then
      begin
        clipRec.Left := fParent.elRectWH.left.rawVal;
        clipRec.Bottom := fParent.elRectWH.top.rawVal;
        clipRec.Right := clipRec.Left + drawDat.fontInfo.textLength;
        clipRec.Top := clipRec.Bottom - drawDat.fontInfo.size;
      end;
      if stroked and drawDat.strokeWidth.IsValid then
      begin
        with drawDat.strokeWidth do
          if HasFontUnits then
            d := GetValue(drawDat.fontInfo.size, GetRelFracLimit) else
            d := GetValueXY(clipRec, GetRelFracLimit);
        Img32.Vector.InflateRect(clipRec, d * 0.5, d * 0.5);
      end;
      if Assigned(filterEl) then
      begin
        drawDat.filterElRef := '';
        with TFilterElement(filterEl) do
        begin
          MatrixExtractScale(DrawData.matrix, fScale);
          clipRec := GetAdjustedBounds(clipRec);
        end;
      end;
      MatrixApply(drawDat.matrix, clipRec);
    end;
    clipRec2 := Rect(clipRec);
    Types.IntersectRect(clipRec2, clipRec2, img.Bounds);
    if IsEmptyRect(clipRec2) then Exit;
    if image <> fSvgReader.TempImage then
      img.Clear(clipRec2);
  end;

  if not IsValidMatrix(drawDat.matrix) then
    raise Exception.Create('Invalid matrix found when drawing element');

  if Assigned(drawPathsC) or Assigned(drawPathsO) then
  begin
    if filled then
    begin
      // it's slightly more efficient to apply the matrix here
      // rather than inside DrawFilled().
      fillPaths := drawPathsC;
      if Assigned(drawPathsO) then
        AppendPath(fillPaths, drawPathsO);
      fillPaths := MatrixApply(fillPaths, drawDat.matrix);
      DrawFilled(img, fillPaths, drawDat);
    end;

    if stroked then
    begin
      // it's slightly more efficient to apply the matrix
      // inside DrawStroke() rather than here.
      if Assigned(drawPathsC) then
        DrawStroke(img, drawPathsC, drawDat, true);
      if stroked and Assigned(drawPathsO) then
        DrawStroke(img, drawPathsO, drawDat, false);
    end;
  end;

  if Assigned(filterEl) then
    with TFilterElement(filterEl) do
      Apply(img, clipRec2, drawDat.matrix);

  if Assigned(maskEl) then
    TMaskElement(maskEl).ApplyMask(img, drawDat)
  else if Assigned(clipPathEl) then
    with TClipPathElement(clipPathEl) do
    begin
      if fDrawData.fillRule = frNegative then
        EraseOutsidePaths(img, clipPaths, frNonZero, clipRec2,
          fSvgReader.fCustomRendererCache) else
        EraseOutsidePaths(img, clipPaths, fDrawData.fillRule, clipRec2,
          fSvgReader.fCustomRendererCache);
    end;

  if useTmpImage and (img <> image) then
    image.CopyBlend(img, clipRec2, clipRec2, BlendToAlphaLine);

  //todo: enable "paint-order" to change filled/stroked/marker paint order
  if HasMarkers then DrawMarkers(img, drawDat);

  inherited; // DrawChildren
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawMarkers(img: TImage32; drawDat: TDrawData);
var
  i,j: integer;
  scale, sw: double;
  markerEl: TBaseElement;
  markerPaths: TPathsD;
  pt1, pt2: TPointD;
  di: TDrawData;
begin
  markerPaths := GetSimplePath(drawDat);
  markerPaths := StripNearDuplicates(markerPaths, 0.01, false);

  if not Assigned(markerPaths) then Exit;
  MatrixApply(drawDat.matrix, markerPaths);

  di := emptyDrawInfo;

  //prepare to scale the markers by the stroke width
  with fDrawData.strokeWidth do
    if not IsValid then sw := 1
    else if HasFontUnits then
      sw := GetValue(drawDat.fontInfo.size, GetRelFracLimit)
    else sw := GetValueXY(drawDat.bounds, GetRelFracLimit);

  MatrixExtractScale(drawDat.matrix, scale);
  MatrixScale(di.matrix, sw * scale);
  if (fDrawData.markerStart <> '') then
  begin
    markerEl := FindRefElement(fDrawData.markerStart);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        for i := 0 to High(markerPaths) do
        begin
          if Length(markerPaths[i]) < 2 then Continue;
          pt1 := markerPaths[i][0];
          pt2 := markerPaths[i][1];
          if autoStartReverse then
            SetEndPoint(pt1, GetAngle(pt2, pt1)) else
            SetEndPoint(pt1, GetAngle(pt1, pt2));
          Draw(img, di);
        end;
      end;
  end;

  if (fDrawData.markerMiddle <> '') then
  begin
    markerEl := FindRefElement(fDrawData.markerMiddle);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
        for i := 0 to High(markerPaths) do
          if SetMiddlePoints(markerPaths[i]) then
            Draw(img, di);
  end;

  if (fDrawData.markerEnd <> '') then
  begin
    markerEl := FindRefElement(fDrawData.markerEnd);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        for i := 0 to High(markerPaths) do
        begin
          j := High(markerPaths[i]);
          if j < 1 then Continue;
          pt1 := markerPaths[i][j];
          pt2 := markerPaths[i][j-1];
          SetEndPoint(pt1, GetAngle(pt2, pt1));
          Draw(img, di);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.GetPaths(const drawDat: TDrawData);
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    if TBaseElement(fChilds[i]) is TShapeElement then
      TShapeElement(fChilds[i]).GetPaths(drawDat);
end;
//------------------------------------------------------------------------------

function  TShapeElement.GetSimplePath(const drawDat: TDrawData): TPathsD;
begin
  Result := nil;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawFilled(img: TImage32;
  const paths: TPathsD; drawDat: TDrawData);
var
  refEl: TBaseElement;
  rec: TRect;
  opacity: Byte;
begin
  if not assigned(paths) then Exit;
  if drawDat.fillColor = clCurrent then
    drawDat.fillColor := fSvgReader.currentColor;
  if drawDat.fillRule = frNegative then
    drawDat.fillRule := frNonZero;

  if not IsValid(drawDat.fillOpacity) then
    opacity := 255 else
    opacity := ClampByte(drawDat.fillOpacity * 255);

  if (drawDat.fillEl <> '') then
  begin
    refEl := FindRefElement(drawDat.fillEl);
    if Assigned(refEl) and (refEl is TFillElement) then
    begin
      if refEl is TRadGradElement then
      begin
        with TRadGradElement(refEl) do
        begin
          fSvgReader.RadGradRenderer.Opacity := opacity;
          if PrepareRenderer(fSvgReader.RadGradRenderer, drawDat) then
            DrawPolygon(img, paths, drawDat.fillRule, fSvgReader.RadGradRenderer);
        end;
      end
      else if refEl is TLinGradElement then
      begin
        with TLinGradElement(refEl) do
        begin
          fSvgReader.LinGradRenderer.Opacity := opacity;
          if PrepareRenderer(fSvgReader.LinGradRenderer, drawDat) then
            DrawPolygon(img, paths, drawDat.fillRule, fSvgReader.LinGradRenderer);
        end;
      end
      else if refEl is TPatternElement then
      begin
        with TPatternElement(refEl) do
          if PrepareRenderer(ImgRenderer, drawDat) then
          begin
            rec := img32.Vector.GetBounds(paths);
            ImgRenderer.Offset := rec.TopLeft;
            DrawPolygon(img, paths, drawDat.fillRule, ImgRenderer);
          end;
      end;
    end;
  end
  else if drawDat.fillColor = clInvalid then
  begin
    DrawPolygon(img, paths, drawDat.fillRule,
      MergeColorAndOpacity(clBlack32, drawDat.fillOpacity),
      fSvgReader.fCustomRendererCache);
  end
  else
    with drawDat do
    begin
      DrawPolygon(img, paths, fillRule,
        MergeColorAndOpacity(fillColor, fillOpacity),
        fSvgReader.fCustomRendererCache);
    end;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawStroke(img: TImage32;
  const paths: TPathsD; drawDat: TDrawData; isClosed: Boolean);
var
  i: integer;
  dashOffset, sw: double;
  dashArray: TArrayOfDouble;
  miterLim, scale: Double;
  strokeClr: TColor32;
  strokePaths: TPathsD;
  refEl: TBaseElement;
  endStyle: TEndStyle;
  joinStyle: TJoinStyle;
  bounds: TRectD;
  paths2: TPathsD;
  opacity: Byte;
begin
  if not Assigned(paths) then Exit;
  MatrixExtractScale(drawDat.matrix, scale);
  joinStyle := fDrawData.strokeJoin;

  bounds := fSvgReader.userSpaceBounds;
  with drawDat.strokeWidth do
  begin
    if not IsValid then
      sw := 1
    else if HasFontUnits then
      sw := GetValue(drawDat.fontInfo.size, GetRelFracLimit)
    else
      sw := GetValueXY(bounds, 0);
  end;

  miterLim := drawDat.strokeMitLim;
  if drawDat.strokeColor = clCurrent then
    drawDat.strokeColor := fSvgReader.currentColor;

  if Length(drawDat.dashArray) > 0 then
    dashArray := ScaleDashArray(drawDat.dashArray, scale) else
    dashArray := nil;
  dashOffset := drawDat.dashOffset;

  with drawDat do
    strokeClr := MergeColorAndOpacity(strokeColor, strokeOpacity);

  if not IsValid(drawDat.strokeOpacity) then
    opacity := 255 else
    opacity := ClampByte(drawDat.strokeOpacity * 255);

  if isClosed then
  begin
    if Assigned(dashArray) then
    begin
      if joinStyle = jsRound then
        endStyle := esRound else
        endStyle := esButt;
      dashArray := ScaleDashArray(drawDat.dashArray, 1);  // ie. don't scale yet!
      strokePaths := nil;
      for i := 0 to High(paths) do
      begin
        paths2 := GetDashedPath(paths[i], true, dashArray, @dashOffset);
        AppendPath(strokePaths, paths2);
      end;
      strokePaths :=
        RoughOutline(strokePaths, sw, joinStyle, endStyle, miterLim, scale);
    end else
    begin
      endStyle := esPolygon;
      strokePaths :=
        RoughOutline(paths, sw, joinStyle, endStyle, miterLim, scale);
    end;
  end else
  begin
    if fDrawData.strokeCap = esPolygon then
      endStyle := esButt else
      endStyle := fDrawData.strokeCap;
    if Assigned(dashArray) then
    begin
      strokePaths := MatrixApply(paths, drawDat.matrix);
      DrawDashedLine(img, strokePaths, dashArray,
        @dashOffset, sw * scale, strokeClr, endStyle, jsAuto,
        fSvgReader.fCustomRendererCache);
      Exit;
    end;
    strokePaths :=
      RoughOutline(paths, sw, joinStyle, endStyle, miterLim, scale);
  end;
  strokePaths := MatrixApply(strokePaths, drawDat.matrix);

  if (drawDat.strokeEl <> '') then
  begin
    refEl := FindRefElement(drawDat.strokeEl);
    if not Assigned(refEl) then Exit;

    if refEl is TRadGradElement then
    begin
      with TRadGradElement(refEl) do
      begin
        fSvgReader.RadGradRenderer.Opacity := opacity;
        PrepareRenderer(fSvgReader.RadGradRenderer, drawDat);
      end;
      DrawPolygon(img, strokePaths, frNonZero, fSvgReader.RadGradRenderer);
    end
    else if refEl is TLinGradElement then
    begin
      with TLinGradElement(refEl) do
      begin
        fSvgReader.LinGradRenderer.Opacity := opacity;
        PrepareRenderer(fSvgReader.LinGradRenderer, drawDat);
      end;
      DrawPolygon(img, strokePaths, frNonZero, fSvgReader.LinGradRenderer);
    end
    else if refEl is TPatternElement then
      with TPatternElement(refEl) do
      begin
        imgRenderer.Opacity := opacity;
        PrepareRenderer(imgRenderer, drawDat);
        DrawLine(img, strokePaths,  1, imgRenderer, esPolygon, joinStyle, scale);
        DrawPolygon(img, strokePaths, frNonZero, imgRenderer);
      end;
  end else
  begin
    DrawPolygon(img, strokePaths,
      frNonZero, strokeClr, fSvgReader.fCustomRendererCache);
  end;
end;

//------------------------------------------------------------------------------
// TPathElement
//------------------------------------------------------------------------------

constructor TPathElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  fSvgPaths := TSvgPath.Create;
end;
//------------------------------------------------------------------------------

destructor TPathElement.Destroy;
begin
  fSvgPaths.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TPathElement.GetBounds: TRectD;
var
  i: integer;
begin
  Result := NullRectD;
  for i := 0 to fSvgPaths.Count -1 do
    Result := UnionRect(Result, fSvgPaths[i].GetBounds);
end;
//------------------------------------------------------------------------------

procedure TPathElement.ParseDAttrib(const value: UTF8String);
begin
  fSvgPaths.Parse(value);
end;
//------------------------------------------------------------------------------

procedure TPathElement.Flatten(index: integer; scalePending: double;
  out path: TPathD; out isClosed: Boolean);
begin
  isClosed := fSvgPaths[index].isClosed;
  path := fSvgPaths[index].GetFlattenedPath(scalePending);
end;
//------------------------------------------------------------------------------

procedure TPathElement.GetPaths(const drawDat: TDrawData);
var
  i: integer;
  scalePending: double;
  isClosed: Boolean;
  path: TPathD;
begin
  if pathsLoaded then Exit;
  pathsLoaded := true;
  MatrixExtractScale(drawDat.matrix, scalePending);
  for i := 0 to fSvgPaths.Count -1 do
  begin
    Flatten(i, scalePending, path, isClosed);
    if not Assigned(path) then Continue;

    if isClosed then
      AppendPath(drawPathsC, path) else
      AppendPath(drawPathsO, path);
  end;
end;
//------------------------------------------------------------------------------

function TPathElement.GetSimplePath(const drawDat: TDrawData): TPathsD;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, fSvgPaths.Count);
  for i := 0 to fSvgPaths.Count -1 do
    Result[i] := fSvgPaths[i].GetSimplePath;
end;

//------------------------------------------------------------------------------
// TPolyElement
//------------------------------------------------------------------------------

function TPolyElement.GetBounds: TRectD;
begin
  Result := GetBoundsD(path);
end;
//------------------------------------------------------------------------------

procedure TPolyElement.GetPaths(const drawDat: TDrawData);
begin
  if pathsLoaded or not Assigned(path) then Exit;
  pathsLoaded := true;
  if (fXmlEl.hash = hPolygon) then
  begin
    AppendPath(drawPathsC, path);                    //hPolygon
  end else
  begin
    AppendPath(drawPathsO, path);                   //hPolyline
  end;
end;
//------------------------------------------------------------------------------

function TPolyElement.GetSimplePath(const drawDat: TDrawData): TPathsD;
begin
  Result := nil;
  AppendPath(Result, path);
end;
//------------------------------------------------------------------------------

procedure TPolyElement.ParsePoints(const value: UTF8String);
var
  currCnt, currCap: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if currCnt = currCap then
    begin
      currCap := currCap + buffSize;
      SetLength(path, currCap);
    end;
    path[currCnt] := pt;
    inc(currCnt);
  end;

var
  pt: TPointD;
  c, endC: PUTF8Char;
begin
  currCnt     := 0;
  currCap     := buffSize;
  c := PUTF8Char(value);
  endC := c + Length(value);
  SetLength(path, currCap);
  while IsNumPending(c, endC, true) and
    ParseNextNum(c, endC, true, pt.X) and
    ParseNextNum(c, endC, true, pt.Y) do
      AddPoint(pt);
  SetLength(path, currCnt);
end;

//------------------------------------------------------------------------------
// TLineElement
//------------------------------------------------------------------------------

constructor TLineElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  NewPointDArray(path, 2, True);
  path[0] := NullPointD; path[1] := NullPointD;
end;
//------------------------------------------------------------------------------

function TLineElement.GetBounds: TRectD;
begin
  Result := GetBoundsD(path);
end;
//------------------------------------------------------------------------------

procedure TLineElement.GetPaths(const drawDat: TDrawData);
begin
  if pathsLoaded then Exit;
  pathsLoaded := true;
  AppendPath(drawPathsO, path);
end;
//------------------------------------------------------------------------------

function TLineElement.GetSimplePath(const drawDat: TDrawData): TPathsD;
begin
  Result := nil;
  AppendPath(Result, path);
end;

//------------------------------------------------------------------------------
// TCircleElement
//------------------------------------------------------------------------------

constructor TCircleElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  bounds := NullRectD;
  centerPt.Init;
  radius.Init;
end;
//------------------------------------------------------------------------------

function TCircleElement.GetBounds: TRectD;
begin
  Result := bounds;
end;
//------------------------------------------------------------------------------

procedure TCircleElement.GetPaths(const drawDat: TDrawData);
var
  scalePending : double;
  pt    : TPointD;
  path  : TPathD;
  r: double;
begin
  if pathsLoaded or not radius.IsValid then Exit;
  pathsLoaded := true;
  r := radius.GetValueXY(drawDat.bounds, GetRelFracLimit);
  pt := centerPt.GetPoint(drawDat.bounds, GetRelFracLimit);
  MatrixExtractScale(drawDat.matrix, scalePending);
  bounds := RectD(pt.X -r, pt.Y -r, pt.X +r, pt.Y +r);
  path := Ellipse(bounds, scalePending);
  AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TEllipseElement
//------------------------------------------------------------------------------

constructor TEllipseElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  centerPt.Init;
  radius.Init;
end;
//------------------------------------------------------------------------------

function TEllipseElement.GetBounds: TRectD;
begin
  Result := bounds;
end;
//------------------------------------------------------------------------------

procedure TEllipseElement.GetPaths(const drawDat: TDrawData);
var
  scalePending  : double;
  path      : TPathD;
  rad       : TPointD;
  centPt    : TPointD;
begin
  if pathsLoaded then Exit;
  pathsLoaded := true;
  rad := radius.GetPoint(drawDat.bounds, GetRelFracLimit);
  centPt := centerPt.GetPoint(drawDat.bounds, GetRelFracLimit);
  with centPt do
    bounds := RectD(X -rad.X, Y -rad.Y, X +rad.X, Y +rad.Y);
  MatrixExtractScale(drawDat.matrix, scalePending);
  path := Ellipse(bounds, scalePending);
  AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TRectElement
//------------------------------------------------------------------------------

constructor TRectElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  radius.Init;
  elRectWH.width.SetValue(100, utPercent);
  elRectWH.height.SetValue(100, utPercent);
end;
//------------------------------------------------------------------------------

function  TRectElement.GetBounds: TRectD;
begin
  Result := elRectWH.GetRectD(NullRectD, GetRelFracLimit);
end;
//------------------------------------------------------------------------------

procedure TRectElement.GetPaths(const drawDat: TDrawData);
var
  radXY : TPointD;
  bounds: TRectD;
  path  : TPathD;
begin
  if pathsLoaded then Exit;
  if elRectWH.width.HasFontUnits then
    bounds := elRectWH.GetRectD(drawDat.fontInfo.size, GetRelFracLimit) else
    bounds := elRectWH.GetRectD(drawDat.bounds, GetRelFracLimit);
  if bounds.IsEmpty then Exit;
  pathsLoaded := true;

  radXY := radius.GetPoint(bounds, GetRelFracLimit);
  if (radXY.X > 0) or (radXY.Y > 0) then
  begin
    if (radXY.X <= 0) then radXY.X := radXY.Y
    else if (radXY.Y <= 0) then radXY.Y := radXY.X;
    path := RoundRect(bounds, radXY);
  end else
    path := Rectangle(bounds);
  AppendPath(drawPathsC, path);
end;
//------------------------------------------------------------------------------

function TRectElement.GetSimplePath(const drawDat: TDrawData): TPathsD;
var
  rec: TRectD;
begin
  Result := nil;
  rec := elRectWH.GetRectD(drawDat.bounds, GetRelFracLimit);
  if not rec.IsEmpty then
    AppendPath(Result, Rectangle(rec));
end;

//------------------------------------------------------------------------------
// TTextElement
//------------------------------------------------------------------------------

constructor TTextElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  offset.Init;
  hasPaths := false;
end;
//------------------------------------------------------------------------------

procedure TTextElement.Draw(img: TImage32; drawDat: TDrawData);
begin
  UpdateDrawInfo(drawDat, self);
  UpdateFontInfo(drawDat, self);

  fSvgReader.GetBestFont(drawDat.FontInfo);
  if not Assigned(fSvgReader.fFontCache) then Exit;
  if drawDat.fontInfo.size = 0 then drawDat.fontInfo.size := 16;

  if offset.X.IsValid then
    currentPt.X := offset.X.rawVal
  else if elRectWH.left.IsValid then
    currentPt.X := elRectWH.left.rawVal
  else
    currentPt.X := 0;

  if offset.Y.IsValid then
    currentPt.Y := offset.Y.rawVal
  else if elRectWH.top.IsValid then
    currentPt.Y := elRectWH.top.rawVal
  else
    currentPt.Y := 0;

  lastChrSpc := false;
  textDx := 0;
  currSpanEl := nil;

  //get child paths (which also updates currentPt)
  GetPaths(drawDat);

  DrawChildren(img, drawDat);
end;

//------------------------------------------------------------------------------
// TTextSubElement
//------------------------------------------------------------------------------

function TTextSubElement.GetTextEl: TTextElement;
var
  el: TBaseElement;
begin
  if not Assigned(textEl) then
  begin
    el := fParent;
    while Assigned(el) and not (el is TTextElement) do
      el := el.fParent;
    if Assigned(el) then
      textEl := TTextElement(el);
  end;
  Result := textEl;
end;

//------------------------------------------------------------------------------
// TTSpanElement
//------------------------------------------------------------------------------

procedure TTSpanElement.GetPaths(const drawDat: TDrawData);
var
  tmpX, startX, fontScale, fontSize, bs: double;
  i,j, len  : integer;
  di        : TDrawData;
  s         : UnicodeString;
  mat       : TMatrixD;
  tmpPaths  : TPathsD;
  codepoints: TArrayOfCardinal;
  angles    : TArrayOfDouble;
  glyphInfo : PGlyphInfo;
  glyphRec  : TRectD;
begin
  // 1. We only want to process this method once even though it's called twice,
  //    first indirectly by TTextElement.Draw, and then by TTSpanElement.Draw.
  // 2. This method isn't called when <tspan> is a sub-element of <textpath>.

  if pathsLoaded then Exit;
  pathsLoaded := true;

  di := drawDat;
  if ChildCount > 0 then
  begin
    UpdateDrawInfo(di, self);
    UpdateFontInfo(di, self);
  end;

  if drawDat.FontInfo.size = 0 then
    fontSize := 16.0 else
    fontSize := drawDat.FontInfo.size;
  fSvgReader.GetBestFont(di.FontInfo);
  if not Assigned(fSvgReader.fFontCache) then Exit;

  GetTextEl;
  if not Assigned(textEl) or
    (textEl.currentPt.X = InvalidD) or
    (textEl.currentPt.Y = InvalidD) then Exit;

  //by not changing the fontCache.FontHeight, the quality of
  //small font render improves very significantly (though of course
  //this requires additional glyph scaling and offsetting).
  fontScale := fontSize / fSvgReader.fFontCache.FontHeight;

  if elRectWH.left.IsValid then
    textEl.currentPt.X := elRectWH.left.rawVal;
  if elRectWH.top.IsValid then
    textEl.currentPt.Y := elRectWH.top.rawVal;

  if offset.X.IsValid then
    textEl.currentPt.X := textEl.currentPt.X + offset.X.GetValue(0, 0);
  if offset.Y.IsValid then
    textEl.currentPt.Y := textEl.currentPt.Y + offset.Y.GetValue(0, 0);

  // only 'virtual' (dummy) <tspan> elements are self-closing, and
  // mostly their parents are 'real' <tspan> elements. However,
  // virtual <tspan> elements can also have <text> element parents.
  if not fXmlEl.selfClosed then
  begin
    textEl.currSpanEl := self;
    angles := nil;
  end
  else if (fParent is TTSpanElement) then
  begin
    if Assigned(TTSpanElement(fParent).angle) then
      angles := TTSpanElement(fParent).angle else
      angles := textEl.angle;
  end else
  begin
    angles := textEl.angle;
    textEl.currSpanEl := nil;
  end;

  chunkDx := 0;
  if (Length(fXmlEl.text) > 0) and (fontSize > 1) then
  begin
    // this should be a virtual (dummy) <tspan> element
    //assert(fXmlEl.selfClosed);

    s := DecodeUtf8ToUnicode(HtmlDecode(fXmlEl.text));
    // don't allow a dup. spaces or a space at the beginning of a text
    s := FixSpaces(s, textEl.lastChrSpc or
      ((fParent = textEl) and (self = textEl.Child[0])));

    if IsBlankText(s) then
    begin
      drawPathsC := nil;
      // don't allow duplicate spaces or a space at the beginning of text
      if textEl.lastChrSpc or (self = textEl.Child[0]) then Exit;
      tmpX := fSvgReader.fFontCache.GetSpaceWidth;
      textEl.lastChrSpc := true;
    end
    else if Assigned(angles) then
    begin
      drawPathsC := nil;
      tmpPaths := nil;
      tmpX := 0;
      codepoints := fSvgReader.fFontCache.GetTextCodePoints(s);
      // make sure 'angles' is at least the length of codepoints
      len := Length(codepoints);
      if len > Length(angles) then
      begin
        j := High(angles);
        SetLength(angles, len); // extend angles
        for i := j +1 to len -1 do angles[i] := angles[j];
      end;

      textEl.lastChrSpc := (codepoints[len -1] = 32);
      // now get each rotated glyph and append to drawPathsC ...
      for i := 0 to len -1 do
      begin
        glyphInfo := fSvgReader.fFontCache.GetGlyphInfo(codepoints[i]);
        if Assigned(glyphInfo.paths) then
        begin
          glyphRec := GetBoundsD(glyphInfo.paths);
          tmpPaths := RotatePath(glyphInfo.paths, glyphRec.MidPoint, angles[i]);
          if i > 0 then
            tmpPaths := TranslatePath(tmpPaths, tmpX, 0);
          AppendPath(drawPathsC, tmpPaths);
        end;
        tmpX := tmpX + glyphInfo.hmtx.advanceWidth * fSvgReader.fFontCache.Scale;
      end;
    end else
    begin
      drawPathsC := fSvgReader.fFontCache.GetTextOutline(0, 0, s, tmpX);
      textEl.lastChrSpc := s[length(s)] = space;
    end;

    chunkDx := tmpX * fontScale;
    if Assigned(textEl.currSpanEl) then
      with textEl.currSpanEl do
        chunkDx := chunkDx + self.chunkDx;
    textEl.textDx := textEl.textDx + chunkDx;

    with textEl.currentPt do
    begin
      startX := X;
      X := X + chunkDx;
    end;

    if Assigned(drawPathsC) then // eg. unassigned if a space char
    begin
      with drawDat.fontInfo do
        if not baseShift.IsValid then
          bs := 0 else
          bs := baseShift.GetValue(size, GetRelFracLimit);
      mat := IdentityMatrix;
      MatrixScale(mat, fontScale);
      MatrixTranslate(mat, startX, textEl.currentPt.Y - bs);
      MatrixApply(mat, drawPathsC);
    end;
  end;

  // nested <tspan> elements are always possible,
  // except when self is a pseudo 'selfClosed' <tspan> element
  inherited GetPaths(di); // gets any children paths
end;
//------------------------------------------------------------------------------

procedure TTSpanElement.Draw(image: TImage32; drawDat: TDrawData);
var
  stroked     : Boolean;
  filled      : Boolean;
  tmpRec      : TRect;
  fillPaths   : TPathsD;
begin
  if ChildCount = 0 then
    fDrawData := fParent.fDrawData
  else
  begin
    UpdateDrawInfo(drawDat, self);
    UpdateFontInfo(drawDat, self);
  end;

  if not fXmlEl.selfClosed then
  begin
    // DrawChildren and exit ...
    inherited;
    Exit;
  end;

  filled := IsFilled(drawDat);
  stroked := IsStroked(drawDat);
  if Assigned(drawPathsC) and Assigned(textEl) then
  begin
    // a <tspan> element that contains text (and a path) must be virtual.
    // But its parent may be another <tspan>, or a <text> or a <textarea>.

    tmpRec := Rect(GetBounds);
    if not IsEmptyRect(tmpRec) then
      drawDat.bounds := RectD(tmpRec);

    if (fParent is TTSpanElement) and fParent.elRectWH.left.IsValid then
    begin
      case drawDat.FontInfo.align of
        staCenter: drawPathsC := TranslatePath(drawPathsC, -chunkDx * 0.5, 0);
        staRight: drawPathsC := TranslatePath(drawPathsC, -chunkDx, 0);
      end;
    end
    else if textEl.textDx <> 0 then
    begin
      case drawDat.FontInfo.align of
        staCenter: drawPathsC := TranslatePath(drawPathsC, -textEl.textDx * 0.5, 0);
        staRight: drawPathsC := TranslatePath(drawPathsC, -textEl.textDx, 0);
      end;
    end;

    // todo - 1. implement paint-order - fill stroke vs stroke fill
    //        2. wavy and colored underlines

    if stroked then
    begin
      // it's slightly more efficient to apply the matrix
      // inside DrawStroke() rather than here.
      DrawStroke(image, drawPathsC, drawDat, true);
    end;

    if filled then
    begin
      // it's slightly more efficient to apply the matrix here
      // rather than inside DrawFilled().
      fillPaths := MatrixApply(drawPathsC, drawDat.matrix);
      DrawFilled(image, fillPaths, drawDat);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TTextPathElement
//------------------------------------------------------------------------------

function GetPathDistance(const path: TPathD): double;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to High(path) do
    Result := Result + Distance(path[i-1], path[i]);
end;
//------------------------------------------------------------------------------

procedure TTextPathElement.GetPathsInternal(el: TBaseElement;
  const drawDat: TDrawData);
var
  i, len        : integer;
  charsThatFit  : integer;
  spacing       : double;
  textWidth     : double;
  outX          : double;
  spanEl        : TTSpanElement;
  dd            : TDrawData;
  unicodeText   : UnicodeString;
  pathDist      : double;
  mat           : TMatrixD;
  tmpPath       : TPathD;
  tmpPaths      : TPathsD;
  isClosed      : Boolean;
begin
  if not (el is TTSpanElement) then Exit;
  spanEl := TTSpanElement(el);
  if Assigned(spanEl.drawPathsC) then Exit;
  spanEl.pathsLoaded := true;
  spanEl.GetTextEl;

  dd := drawDat;
  UpdateDrawInfo(dd, el);
  UpdateFontInfo(dd, el);

  if spanEl.offset.X.IsValid then
    textEl.currentPt.X := Max(0, textEl.currentPt.X +
      Round(spanEl.offset.X.rawVal / scale));
  if spanEl.offset.Y.IsValid then
    textEl.currentPt.Y := textEl.currentPt.Y +
      Round(spanEl.offset.Y.rawVal / scale);

  if spanEl.fXmlEl.text = '' then
  begin
    // nb: recursive
    for i := 0 to spanEl.ChildCount -1 do
      GetPathsInternal(spanEl.Child[i], dd);
    Exit;
  end;

  // nb: <tspan> elements that own text will always be pseudo <tspan> elements.
  // Pseudo <tspan> elements have been created inside real <tspan> elements to
  // provide a reliable way to manage text mixed with nested <tspan> elements.

  //trim CRLFs and multiple spaces
  unicodeText := DecodeUtf8ToUnicode(HtmlDecode(spanEl.fXmlEl.text));
  if dd.fontInfo.spacesInText <> sitPreserve then
    unicodeText := TrimMultiSpacesUnicode(unicodeText) else
    unicodeText := StripNewlines(unicodeText);

  //adjust glyph spacing when fFontInfo.textLength is assigned.
  spacing := dd.FontInfo.spacing /scale;
  len := Length(unicodeText);
  if (len < 2) then spacing := 0
  else if (dd.FontInfo.align = staJustify) and
    (pathEl.fsvgPaths.count = 1) then
    with TPathElement(pathEl) do
    begin
      Flatten(0, scale, tmpPath, isClosed);
      pathDist := GetPathDistance(tmpPath);
      textWidth := fSvgReader.fFontCache.GetTextWidth(unicodeText);
      spacing := (pathDist/scale) - textWidth;
      spacing := spacing / (len -1);
    end
  else if (dd.FontInfo.textLength > 0) then
  begin
    textWidth := fSvgReader.fFontCache.GetTextWidth(unicodeText);
    spacing := (dd.FontInfo.textLength/scale) - textWidth;
    spacing := spacing / (len -1);
  end;

  with pathEl do
  begin
    mat := fDrawData.matrix;
    MatrixScale(mat, 1/scale);
    for i := 0 to fSvgPaths.Count -1 do
    begin
      Flatten(i, scale, tmpPath, isClosed);
      //'path' is temporarily scaled to accommodate fReader.fFontCache's
      //static fontheight. The returned glyphs will be de-scaled later.
      MatrixApply(mat, tmpPath);
      tmpPaths := GetTextOutlineOnPath(unicodeText, tmpPath,
        fSvgReader.fFontCache, taLeft, textEl.currentPt.X,
        textEl.currentPt.Y, spacing, charsThatFit, outX);
      AppendPath(spanEl.drawPathsC, tmpPaths);
      textEl.currentPt.X := outX;
      if charsThatFit = Length(unicodeText) then Break;
      Delete(unicodeText, 1, charsThatFit);
      textEl.currentPt := NullPointD;
    end;
  end;
  spanEl.drawPathsC := ScalePath(spanEl.drawPathsC, scale);

  for i := 0 to spanEl.ChildCount -1 do
    GetPathsInternal(spanEl.Child[i], dd);
end;
//------------------------------------------------------------------------------

procedure TTextPathElement.GetPaths(const drawDat: TDrawData);
var
  i: integer;
  dd: TDrawData;
  el: TBaseElement;
begin
  if pathsLoaded or not Assigned(fSvgReader.fFontCache) then Exit;
  pathsLoaded := true;
  GetTextEl;
  if not Assigned(textEl) then Exit;

  dd := drawDat;
  UpdateDrawInfo(dd, self);
  UpdateFontInfo(dd, self);

  el := FindRefElement(pathName);
  if not (el is TPathElement) then Exit;
  pathEl := TPathElement(el);
  fSvgReader.GetBestFont(dd.FontInfo);
  scale := dd.FontInfo.size/fSvgReader.fFontCache.FontHeight;

  if offset.X.IsValid then
    textEl.currentPt.X := Max(0,
      textEl.currentPt.X +
      Round(offset.X.GetValue(dd.bounds.Width, 1) / scale));
  if offset.Y.IsValid then
    textEl.currentPt.Y :=
      textEl.currentPt.Y + Round(offset.Y.rawVal / scale);

  // nb: recursive
  for i := 0 to ChildCount -1 do
    GetPathsInternal(Child[i], drawDat);
end;
//------------------------------------------------------------------------------

function  TTextPathElement.GetBounds: TRectD;
var
  textEl: TTextElement;
begin
  textEl := TTextElement(fParent);
{$IFDEF UNICODE}
  if IsBlankText(UTF8ToUnicodeString(fXmlEl.text)) then
{$ELSE}
  if IsBlankText(Utf8Decode(fXmlEl.text)) then
{$ENDIF}
    Result := textEl.fDrawData.bounds else
    Result := inherited GetBounds;
end;
//------------------------------------------------------------------------------

procedure TTextPathElement.Draw(image: TImage32; drawDat: TDrawData);
begin
  UpdateFontInfo(drawDat, self);
  inherited;
end;

//------------------------------------------------------------------------------
// TTextAreaElement
//------------------------------------------------------------------------------

procedure TTextAreaElement.GetPaths(const drawDat: TDrawData);
var
  scale     : double;
  fontSize  : double;
  lnHeight  : double;
  di        : TDrawData;
  mat       : TMatrixD;
  text      : Utf8String;
  s         : UnicodeString;
  textRec   : TRectD;
const
  margin = 1;
begin
  if pathsLoaded then Exit;
  text := fXmlEl.text;
  if not elRectWH.width.IsValid or not elRectWH.height.IsValid or
    (text = '') then Exit;
  pathsLoaded := true;

  di := drawDat;
  UpdateDrawInfo(di, self);

  if drawDat.FontInfo.size = 0 then
    fontSize := 16.0 else
    fontSize := drawDat.FontInfo.size;
  fSvgReader.GetBestFont(di.FontInfo);
  if not Assigned(fSvgReader.fFontCache) then Exit;
  scale := fontSize / fSvgReader.fFontCache.FontHeight;

  s := DecodeUtf8ToUnicode(HtmlDecode(text));
  s := FixSpaces(s, false);
  s := StringReplace(s, '<tbreak/>', #10, [rfReplaceAll, rfIgnoreCase]);

  lnHeight := fSvgReader.fFontCache.LineHeight;

  textRec := elRectWH.GetRectD(di.bounds.Width, di.bounds.Height, 1);
  textRec := ScaleRect(textRec, 1/scale);
  InflateRect(textRec, -margin, -margin);

  with TChunkedText.Create(s, fSvgReader.fFontCache) do
  try
    // and compress the lineheight a little
    drawPathsC := GetTextGlyphs(Rect(textRec), taLeft, tvaTop, 0, lnHeight * 0.8);
  finally
    Free;
  end;

  mat := IdentityMatrix;
  MatrixScale(mat, scale);
  MatrixApply(mat, drawPathsC);
end;

//------------------------------------------------------------------------------
// TMarkerElement
//------------------------------------------------------------------------------

constructor TMarkerElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  fDrawData.visible := false;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.Draw(img: TImage32; drawDat: TDrawData);
var
  i, len: integer;
  l,t,w,h,scale, a, a2: double;
  mat: TMatrixD;
  angles: TArrayOfDouble;
begin
  UpdateDrawInfo(drawDat, self);
  mat := drawDat.matrix;

  if elRectWH.width.IsValid and elRectWH.height.IsValid and
    not markerBoxWH.IsEmpty then
  begin
    w := elRectWH.width.rawVal;
    h := elRectWH.height.rawVal;
    //currently assume preserve aspect ratio
    scale := Min(w/markerBoxWH.Width, h/markerBoxWH.Height);
    MatrixScale(mat, scale, scale);
  end;

  if refPt.X.IsValid and refPt.Y.IsValid then
  begin
    l := refPt.X.rawVal;
    t := refPt.Y.rawVal;
    MatrixExtractScale(mat, scale);
    MatrixTranslate(mat, -l * scale, -t * scale);
  end;

  len := Length(fPoints);
  if len = 0 then Exit;
  SetLength(angles, len);
  angles[0] := angle;
  a := angle;
  for i := 0 to len -2 do
  begin
    a2 := GetAngle(fPoints[i], fPoints[i+1]);
    angles[i] := Average(a, a2);
    a := a2;
  end;
  if len > 1 then
    angles[len -1] := Average(a, angle2);

  //for each 'point' draw the marker
  for i := 0 to len -1 do
  begin
    drawDat.matrix := mat;
    MatrixRotate(drawDat.matrix, NullPointD, angles[i]);
    MatrixTranslate(drawDat.matrix, fPoints[i].X, fPoints[i].Y);
    DrawChildren(img, drawDat);
  end;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.SetEndPoint(const pt: TPointD; angle: double);
begin
  NewPointDArray(fPoints, 1, True);
  fPoints[0] := pt;
  self.angle := angle;
end;
//------------------------------------------------------------------------------

function TMarkerElement.SetMiddlePoints(const points: TPathD): Boolean;
var
  len: integer;
begin
  len := Length(points);
  Result := len > 2;
  if Result then
  begin
    angle := GetAngle(Points[0],Points[1]);
    angle2 := GetAngle(Points[len-2],Points[len-1]);
    Self.fPoints := Copy(points, 1, len -2);
  end;
end;

//------------------------------------------------------------------------------
// TFillElement
//------------------------------------------------------------------------------

function TFillElement.GetRelFracLimit: double;
begin
  //always assume fractional values below 1 are relative
  Result := 1.0;
end;

//------------------------------------------------------------------------------
// TPatternElement
//------------------------------------------------------------------------------

constructor TPatternElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
  inherited;
  imgRenderer := TImageRenderer.Create;
  elRectWH.Init;
  pattBoxWH.Width   := InvalidD;
  pattBoxWH.Height  := InvalidD;
  fDrawData.visible := false;
end;
//------------------------------------------------------------------------------

destructor TPatternElement.Destroy;
begin
  imgRenderer.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TPatternElement.PrepareRenderer(renderer: TImageRenderer;
  drawDat: TDrawData): Boolean;
var
  i     : integer;
  recWH : TRectWH;
  el    : TBaseElement;
  rec   : TRectD;
  mat   : TMatrixD;
  sx,sy : double;
  scale : TPointD;
begin
  Result := false;

  MatrixExtractScale(drawDat.matrix, scale.X, scale.Y);

  if units = hUserSpaceOnUse then
    rec := fSvgReader.userSpaceBounds else
    rec := drawDat.bounds;

  //todo: implement patternUnits & patternContentUnits too

  sx := 1; sy := 1;
  if elRectWH.Width.IsValid and elRectWH.Height.IsValid then
  begin
    recWH := elRectWH.GetRectWH(rec, GetRelFracLimit);
    if recWH.IsEmpty then Exit;

    //also scale if necessary
    if not pattBoxWH.IsEmpty then
    begin
      sx := recWH.Width/pattBoxWH.Width;
      sy := recWH.Height/pattBoxWH.Height;
    end;

  end
  else if not pattBoxWH.IsEmpty then
  begin
    recWH.Width   := pattBoxWH.Width;
    recWH.Height  := pattBoxWH.Width;
  end else
    Exit;

  renderer.Image.SetSize(
    Round(recWH.Width * scale.X),
    Round(recWH.Height * scale.Y));

  Result := true;

  mat := IdentityMatrix;
  MatrixScale(mat, scale.X * sx, scale.Y * sy);

  if (refEl <> '') then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TShapeElement) then
      with TShapeElement(el) do
      begin
        drawDat := fDrawData;
        drawDat.matrix := mat;
        drawDat.bounds := recWH.RectD;
        Draw(renderer.Image, drawDat);
      end;
  end;

  for i := 0 to fChilds.Count -1 do
    if TBaseElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        drawDat := fDrawData;
        drawDat.matrix := mat;
        drawDat.bounds := rec;
        Draw(renderer.Image, drawDat);
      end
    else if TBaseElement(fChilds[i]) is TImageElement then
      with TImageElement(fChilds[i]) do
      begin
        drawDat := fDrawData;
        drawDat.matrix := mat;
        drawDat.bounds := rec;
        Draw(renderer.Image, drawDat);
      end;
end;

//------------------------------------------------------------------------------
// TSvgElement
//------------------------------------------------------------------------------

procedure TSvgElement.Draw(image: TImage32; drawDat: TDrawData);
var
  sx, sy: double;
  dd: TDrawData;
begin
  dd := drawDat;
  if (fSvgReader.RootElement <> self) then
  begin
    if (elRectWH.left.rawVal <> InvalidD) or
      (elRectWH.top.rawVal <> InvalidD) then
    begin
      MatrixExtractScale(dd.matrix, sx, sy);
      MatrixTranslate(dd.matrix,
        elRectWH.left.rawVal * sx,
        elRectWH.top.rawVal *sy);
    end;
    if not viewboxWH.IsEmpty then
    begin
      sx := fSvgReader.BackgndImage.Width / viewboxWH.Width;
      sy := fSvgReader.BackgndImage.Height / viewboxWH.Height;
      MatrixScale(dd.matrix, sx, sy);
    end;
  end;
  DrawChildren(image, dd);
end;
//------------------------------------------------------------------------------

function TSvgElement.Width: TValue;
begin
  Result := elRectWH.Width;
end;
//------------------------------------------------------------------------------

function TSvgElement.Height: TValue;
begin
  Result := elRectWH.Height;
end;

//------------------------------------------------------------------------------
// TBaseElement
//------------------------------------------------------------------------------

constructor TBaseElement.Create(parent: TBaseElement; svgEl: TSvgXmlEl);
begin
{$IFDEF XPLAT_GENERICS}
  fChilds         := TList<TBaseElement>.create;
{$ELSE}
  fChilds         := TList.Create;
{$ENDIF}
  fXmlEl       := svgEl;
  self.fParent    := parent;
  elRectWH.Init;
  fDrawData       := emptyDrawInfo;
  if Assigned(parent) then
  begin
    fDrawData.strokeCap := parent.fDrawData.strokeCap;
    fDrawData.strokeJoin := parent.fDrawData.strokeJoin;
    fSvgReader := parent.fSvgReader;
  end;
end;
//------------------------------------------------------------------------------

destructor TBaseElement.Destroy;
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    TBaseElement(fChilds[i]).Free;
  fChilds.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function  TBaseElement.IsFirstChild: Boolean;
begin
  Result := not Assigned(fParent) or (self = fParent.fChilds[0]);
end;
//------------------------------------------------------------------------------

procedure TBaseElement.Draw(image: TImage32; drawDat: TDrawData);
begin
  DrawChildren(image, drawDat);
end;
//------------------------------------------------------------------------------

procedure TBaseElement.DrawChildren(image: TImage32; const drawDat: TDrawData);
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    with TBaseElement(fChilds[i]) do
      if fDrawData.visible then Draw(image, drawDat);
end;
//------------------------------------------------------------------------------

function TBaseElement.GetChildCount: integer;
begin
  Result := fChilds.Count;
end;
//------------------------------------------------------------------------------

function TBaseElement.FindChild(const idName: UTF8String): TBaseElement;
var
  i: integer;
begin
  if Match(self.fId, idName) then
  begin
    Result := self;
    Exit;
  end;

  Result := nil;
  for i := 0 to ChildCount -1 do
  begin
    Result := Child[i].FindChild(idName);
    if Assigned(Result) then Break;
  end;
end;
//------------------------------------------------------------------------------

function TBaseElement.GetChild(index: integer): TBaseElement;
begin
  if (index < 0) or (index >= fChilds.count) then
    Result := nil else
    Result := TBaseElement(fChilds[index]);
end;
//------------------------------------------------------------------------------

function TBaseElement.FindRefElement(const refname: UTF8String): TBaseElement;
var
  len: integer;
  c, endC: PUTF8Char;
  ref: UTF8String;
begin
  result := nil;
  len := Length(refname);
  if len = 0 then Exit;
  c := PUTF8Char(Pointer(refname));
  endC := c + len;
  if Match(c, 'url(') then
  begin
    inc(c, 4);
    dec(endC); //removes trailing ')'
  end;
  if c^ = '#' then inc(c);
  if c = PUTF8Char(Pointer(refname)) then
    Result := fSvgReader.fIdList.FindElement(refname)
  else
  begin
    ToUTF8String(c, endC, ref);
    Result := fSvgReader.fIdList.FindElement(ref);
  end;
end;

//------------------------------------------------------------------------------
// dozens of function to process various element attributes
//------------------------------------------------------------------------------

procedure Id_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  aOwnerEl.fId := value;
  aOwnerEl.fSvgReader.fIdList.AddOrIgnore(value, aOwnerEl);
end;
//------------------------------------------------------------------------------

procedure In_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if aOwnerEl is TFeBaseElement then
    TFeBaseElement(aOwnerEl).in1 := value;
end;
//------------------------------------------------------------------------------

procedure In2_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if aOwnerEl is TFeBaseElement then
    TFeBaseElement(aOwnerEl).in2 := value;
end;
//------------------------------------------------------------------------------

procedure Intercept_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  c, endC: PUTF8Char;
  val: double;
begin
  if (value = '') or not (aOwnerEl is TFeComponentTransferChild) then Exit;
  c := PUTF8Char(value);
  endC := c + Length(value);
  with TFeComponentTransferChild(aOwnerEl) do
    if ParseNextNum(c, endC, false, val) then
      intercept := val else
      intercept := 1;
end;
//------------------------------------------------------------------------------

procedure Slope_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  c, endC: PUTF8Char;
  val: double;
begin
  if (value = '') or not (aOwnerEl is TFeComponentTransferChild) then Exit;
  c := PUTF8Char(value);
  endC := c + Length(value);
  with TFeComponentTransferChild(aOwnerEl) do
    if ParseNextNum(c, endC, false, val) then
      slope := val else
      slope := 1;
end;
//------------------------------------------------------------------------------

procedure TableValues_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  c, endC: PUTF8Char;
  val: double;
  len: integer;
begin
  if (value = '') or not (aOwnerEl is TFeComponentTransferChild) then
    Exit;
  with TFeComponentTransferChild(aOwnerEl) do
  begin
    len := 0;
    tableValues := nil;
    c := PUTF8Char(value);
    endC := c + Length(value);
    while ParseNextNum(c, endC, true, val) do
    begin
      SetLength(tableValues, len +1);
      tableValues[len] := val;
      inc(len);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure Type_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if (value = '') or not (aOwnerEl is TFeComponentTransferChild) then Exit;
  with TFeComponentTransferChild(aOwnerEl) do
  begin
    case value[1] of
      'D','d': funcType := ftDiscrete;
      'G','g': funcType := ftGamma;
      'L','l': funcType := ftLinear;
      'T','t': funcType := ftTable;
      else funcType := ftIdentity;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure LetterSpacing_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  with aOwnerEl do
    UTF8StringToFloat(value, fDrawData.FontInfo.spacing);
end;
//------------------------------------------------------------------------------

procedure Href_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  case aOwnerEl.fXmlEl.Hash of
    hFeImage:
      TFeImageElement(aOwnerEl).refEl := ExtractRef(value);
    hImage:
      TImageElement(aOwnerEl).fRefEl := ExtractRef(value);
    hUse:
      TUseElement(aOwnerEl).fRefEl := ExtractRef(value);
    hTextPath:
      TTextPathElement(aOwnerEl).pathName := ExtractRef(value);
    else if aOwnerEl is TFillElement then
      TFillElement(aOwnerEl).refEl := ExtractRef(value);
  end;
end;
//------------------------------------------------------------------------------

procedure Space_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  case aOwnerEl.fXmlEl.Hash of
    hText: if value = 'preserve' then
      TTextPathElement(aOwnerEl).fDrawData.fontInfo.spacesInText := sitPreserve;
  end;
end;
//------------------------------------------------------------------------------


procedure BaselineShift_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
  hash: cardinal;
  c, endC: PUTF8Char;
begin
  c := PUTF8Char(value);
  endC := c + Length(value);
  hash := ParseNextWordHash(c, endC);
  with aOwnerEl.fDrawData.FontInfo do
    case hash of
      hSuper: baseShift.SetValue(50, utPercent);
      hSub: baseShift.SetValue(-50, utPercent);
      hBaseline: baseShift.SetValue(0, utPixel);
      else
      begin
        UTF8StringToFloatEx(value, val, mu);
        baseShift.SetValue(val, mu);
      end;
    end;
end;
//------------------------------------------------------------------------------

procedure Color_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  color: TColor32;
begin
  color := clInvalid;
  UTF8StringToColor32(value, color);
  //for setting currentcolor when drawing (eg drawing shapes)
  aOwnerEl.fDrawData.currentColor := color;
  //for setting currentcolor during element creation (eg gradient colors)
  aOwnerEl.fSvgReader.currentColor := color;
end;
//------------------------------------------------------------------------------

procedure LightingColor_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  color: TColor32;
begin
  color := clInvalid;
  UTF8StringToColor32(value, color);
  if (aOwnerEl is TFeSpecLightElement) then
    TFeSpecLightElement(aOwnerEl).color := color
  else if (aOwnerEl is TFeDefuseLightElement) then
    TFeDefuseLightElement(aOwnerEl).color := color
end;
//------------------------------------------------------------------------------

procedure ClipPath_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  aOwnerEl.fDrawData.clipElRef := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure D_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if aOwnerEl is TPathElement then
    TPathElement(aOwnerEl).ParseDAttrib(value);
end;
//------------------------------------------------------------------------------

procedure Fill_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  case aOwnerEl.fXmlEl.Hash of
    hfeDropShadow:
      UTF8StringToColor32(value, TFeDropShadowElement(aOwnerEl).floodColor);
    hfeFlood:
      UTF8StringToColor32(value, TFeFloodElement(aOwnerEl).floodColor);
    else
    begin
      if Match(PUTF8Char(value), 'url(') then
        aOwnerEl.fDrawData.fillEl := ExtractRef(value)
      else
        UTF8StringToColor32(value, aOwnerEl.fDrawData.fillColor);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure FillOpacity_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  opacity: double;
begin
  case aOwnerEl.fXmlEl.Hash of
    hfeDropShadow:
      UTF8StringToOpacity(value, TFeDropShadowElement(aOwnerEl).floodColor);
    hfeFlood:
      UTF8StringToOpacity(value, TFeFloodElement(aOwnerEl).floodColor);
    else
    begin
      UTF8StringToFloat(value, opacity);
      with aOwnerEl.fDrawData do
      begin
        if IsValid(fillOpacity) then
          fillOpacity := fillOpacity * opacity else
          fillOpacity := opacity;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure DashArray_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  c, endC: PUTF8Char;
  val: double;
  len: integer;
begin
  c := PUTF8Char(value);
  endC := c + Length(value);
  with aOwnerEl.fDrawData do
  begin
    len := Length(dashArray);
    while ParseNextNum(c, endC, true, val) do
    begin
      SetLength(dashArray, len +1);
      dashArray[len] := val;
      inc(len);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure DashOffset_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  c, endC: PUTF8Char;
begin
  c := PUTF8Char(value);
  endC := c + Length(value);
  with aOwnerEl.fDrawData do
    ParseNextNum(c, endC, true, dashOffset);
end;
//------------------------------------------------------------------------------

procedure Display_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if GetHash(value) = hNone then
    aOwnerEl.fDrawData.visible := false;
end;
//------------------------------------------------------------------------------

procedure Font_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  GetSvgFontInfo(value, aOwnerEl.fDrawData.FontInfo);
end;
//------------------------------------------------------------------------------

procedure FontFamily_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  hash: cardinal;
  c, endC: PUTF8Char;
begin
  with aOwnerEl.fDrawData.FontInfo do
  begin
    family := tfUnknown;
    familyNames := GetCommaSeparatedArray(value);
    // get comma separated family names

    c := PUTF8Char(value);
    endC := c + Length(value);
    while ParseNextWordExHash(c, endC, hash) do
    begin
      case hash of
        hSans_045_Serif, hArial  : family := tfSansSerif;
        hSerif, hTimes: family := tfSerif;
        hMonospace: family := tfMonospace;
        else Continue;
      end;
      break;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure FontSize_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  num: double;
  c, endC: PUTF8Char;
begin
  c := PUTF8Char(value); endC := c + Length(value);
  if not ParseNextNum(c, endC, false, num) then Exit;
  aOwnerEl.fDrawData.FontInfo.size := num;
end;
//------------------------------------------------------------------------------

procedure FontStyle_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  with aOwnerEl.fDrawData.FontInfo do
    if GetHash(value) = hItalic then
      italic := sfsItalic else
      italic := sfsNone;
end;
//------------------------------------------------------------------------------

procedure FontWeight_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  num: double;
  hash: cardinal;
  c, endC: PUTF8Char;
begin
  c := PUTF8Char(value);
  endC := c + Length(value);
  with aOwnerEl.fDrawData.FontInfo do
  begin
    if IsNumPending(c, endC, false) and
      ParseNextNum(c, endC, false, num) then
        weight := Round(num)
    else if ParseNextWordHash(c, endC, hash) then
      case hash of
        hBold   : weight := 600;
        hNormal : weight := 400;
        hBolder : if weight >= 0 then weight := Min(900, weight + 200)
                  else weight := 600;
        hLighter: if weight >= 0 then weight := Max(0, weight - 200)
                  else weight := 200;

      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Fx_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if (aOwnerEl is TRadGradElement) then
    with TRadGradElement(aOwnerEl) do
    begin
      UTF8StringToFloatEx(value, F.X.rawVal, F.X.unitType);
    end;
end;
//------------------------------------------------------------------------------

procedure Fy_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if (aOwnerEl is TRadGradElement) then
    with TRadGradElement(aOwnerEl) do
    begin
      UTF8StringToFloatEx(value, F.Y.rawVal, F.Y.unitType);
    end;
end;
//------------------------------------------------------------------------------

procedure TextAlign_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  with aOwnerEl.fDrawData.FontInfo do
    case GetHash(value) of
      hMiddle   : align := staCenter;
      hEnd      : align := staRight;
      hJustify  : align := staJustify;
      else align := staLeft;
    end;
end;
//------------------------------------------------------------------------------

procedure TextDecoration_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  with aOwnerEl.fDrawData.FontInfo do
    if PosEx('underline', value) > 0 then
      decoration := fdUnderline
    else if PosEx('line-through', value) > 0 then
      decoration := fdStrikeThrough
    else
      decoration := fdNone;
end;
//------------------------------------------------------------------------------

procedure TextLength_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  UTF8StringToFloat(value, aOwnerEl.fDrawData.FontInfo.textLength);
end;
//------------------------------------------------------------------------------


procedure MarkerStart_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if not (aOwnerEl is TShapeElement) then Exit;
  aOwnerEl.fDrawData.markerStart := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure MarkerMiddle_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if not (aOwnerEl is TShapeElement) then Exit;
  aOwnerEl.fDrawData.markerMiddle := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure MarkerEnd_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if not (aOwnerEl is TShapeElement) then Exit;
  aOwnerEl.fDrawData.markerEnd := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure Filter_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if (aOwnerEl is TShapeElement) then
    aOwnerEl.fDrawData.filterElRef := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure Mask_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if (aOwnerEl is TShapeElement) then
    aOwnerEl.fDrawData.maskElRef := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure Offset_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  val: TValue;
begin
  if (aOwnerEl is TGradStopElement) then
    with TGradStopElement(aOwnerEl) do
    begin
      val.Init;
      UTF8StringToFloatEx(value, val.rawVal, val.unitType);
      offset := val.GetValue(1, GetRelFracLimit);
    end
end;
//------------------------------------------------------------------------------

procedure Opacity_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  opacity: double;
begin
  if not UTF8StringToFloat(value, opacity) then Exit;
  opacity := ClampRange(opacity, 0,1);
  with aOwnerEl.fDrawData do
  begin
    if IsValid(fillOpacity) then
      fillOpacity := fillOpacity * opacity else
      fillOpacity := opacity;
    if IsValid(strokeOpacity) then
      strokeOpacity := strokeOpacity * opacity else
      strokeOpacity := opacity;
  end;
end;
//------------------------------------------------------------------------------

procedure Operator_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if (aOwnerEl is TFeCompositeElement) then
    with TFeCompositeElement(aOwnerEl) do
      case GetHash(value) of
        hAtop       : compositeOp := coAtop;
        hIn         : compositeOp := coIn;
        hOut        : compositeOp := coOut;
        hOver       : compositeOp := coOver;
        hXor        : compositeOp := coXor;
        hArithmetic : compositeOp := coArithmetic;
      end;
end;
//------------------------------------------------------------------------------

procedure Orient_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if (aOwnerEl is TMarkerElement) and
    (GetHash(value) = hauto_045_start_045_reverse) then
        TMarkerElement(aOwnerEl).autoStartReverse := true;
end;
//------------------------------------------------------------------------------

procedure StartOffset_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  val: double;
begin
  if (aOwnerEl is TTextPathElement) and
    UTF8StringToFloat(value, val) then
    TTextPathElement(aOwnerEl).offset.X.SetValue(val);
end;
//------------------------------------------------------------------------------

procedure StopColor_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  acolor: TColor32;
begin
  if aOwnerEl is TGradStopElement then
  begin
    acolor := clInvalid;
    UTF8StringToColor32(value, acolor);
    with TGradStopElement(aOwnerEl) do
      if acolor = clCurrent then
        color := aOwnerEl.fSvgReader.currentColor else
        color := acolor;
  end;
end;
//------------------------------------------------------------------------------

procedure StopOpacity_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if aOwnerEl is TGradStopElement then
  UTF8StringToOpacity(value, TGradStopElement(aOwnerEl).color);
end;
//------------------------------------------------------------------------------

procedure Points_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if aOwnerEl is TPolyElement then
    TPolyElement(aOwnerEl).ParsePoints(value);
end;
//------------------------------------------------------------------------------

procedure Stroke_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if Match(PUTF8Char(value), 'url(') then
    aOwnerEl.fDrawData.strokeEl := ExtractRef(value)
  else if Match(PUTF8Char(value), 'currentcolor') then
    aOwnerEl.fDrawData.strokeColor := clCurrent
  else
    UTF8StringToColor32(value, aOwnerEl.fDrawData.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeLineCap_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  hash: cardinal;
  c, endC: PUTF8Char;
begin
  c := PUTF8Char(value);
  endC := c + Length(value);
  hash := ParseNextWordHash(c, endC);
  with aOwnerEl.fDrawData do
    case hash of
      hButt   : strokeCap := esButt;
      hRound  : strokeCap := esRound;
      hSquare : strokeCap := esSquare;
    end;
end;
//------------------------------------------------------------------------------

procedure StrokeLineJoin_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  hash: cardinal;
  c, endC: PUTF8Char;
begin
  c := PUTF8Char(value);
  endC := c + Length(value);
  hash := ParseNextWordHash(c, endC);
  with aOwnerEl.fDrawData do
    case hash of
      hMiter  : strokeJoin := jsMiter;
      hRound  : strokeJoin := jsRound;
      hBevel  : strokeJoin := jsSquare;
    end;
end;
//------------------------------------------------------------------------------

procedure StrokeMiterLimit_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  UTF8StringToFloat(value, aOwnerEl.fDrawData.strokeMitLim);
end;
//------------------------------------------------------------------------------

procedure StrokeOpacity_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  opacity: double;
begin
  UTF8StringToFloat(value, opacity);
  opacity := ClampRange(opacity, 0,1);
  with aOwnerEl.fDrawData do
  begin
    if IsValid(strokeOpacity) then
      strokeOpacity := strokeOpacity * opacity else
      strokeOpacity := opacity;
  end;
end;
//------------------------------------------------------------------------------

procedure StrokeWidth_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  with aOwnerEl do
  begin
    UTF8StringToFloatEx(value, fDrawData.strokewidth.rawVal,
      fDrawData.strokewidth.unitType);
  end;
end;
//------------------------------------------------------------------------------

procedure FillRule_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if LowerCaseTable[value[1]] = 'e' then
    aOwnerEl.fDrawData.fillRule := frEvenOdd else
    aOwnerEl.fDrawData.fillRule := frNonZero;
end;
//------------------------------------------------------------------------------

procedure Rotate_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  i, cnt: integer;
  angle: TArrayOfDouble;
  c, endC: PUTF8Char;
begin
  SetLength(angle, Length(value));
  c := PUTF8Char(value);
  endC := c + Length(value);
  cnt := 0;
  while ParseNextNum(c, endC, true, angle[cnt]) do inc(cnt);
  SetLength(angle, cnt);

  for i := 0 to cnt -1 do
    angle[i] := DegToRad(angle[i]);

  if aOwnerEl is TTextElement then
    TTextElement(aOwnerEl).angle := angle
  else if aOwnerEl is TTSpanElement then
    TTSpanElement(aOwnerEl).angle := angle;
end;
//------------------------------------------------------------------------------

procedure Transform_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  with aOwnerEl.fDrawData do
    MatrixMultiply2(ParseTransform(value), matrix);
end;
//------------------------------------------------------------------------------

procedure Values_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  cnt: integer;
  c, endC: PUTF8Char;
begin
  if aOwnerEl is TFeColorMatrixElement then
    with TFeColorMatrixElement(aOwnerEl) do
    begin
      SetLength(values, 20);
      c := PUTF8Char(value);
      endC := c + Length(value);
      cnt := 0;
      while (cnt < 20) and ParseNextNum(c, endC, true, values[cnt]) do
        inc(cnt);
      if cnt < 20 then values := nil;
    end;
end;
//------------------------------------------------------------------------------

procedure GradientTransform_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mat: TMatrixD;
begin
  if not (aOwnerEl is TGradientElement) then Exit;
  mat := ParseTransform(value);
  with aOwnerEl.fDrawData do
    MatrixMultiply2(mat, matrix);
end;
//------------------------------------------------------------------------------

procedure GradientUnits_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if aOwnerEl is TFillElement then
    with TFillElement(aOwnerEl) do
      units := GetHash(value);
end;
//------------------------------------------------------------------------------

procedure Viewbox_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);

  function LoadViewbox: TRectWH;
  var
    c, endC: PUTF8Char;
  begin
    c := PUTF8Char(value);
    endC := c + Length(value);
    with Result do
      if not ParseNextNum(c, endC, false, Left) or
        not ParseNextNum(c, endC, true, Top) or
        not ParseNextNum(c, endC, true, Width) or
        not ParseNextNum(c, endC, true, Height) then
          Result := RectWH(0,0,0,0);
  end;

begin
  case aOwnerEl.fXmlEl.Hash of
    hSvg    : TSvgElement(aOwnerEl).viewboxWH := LoadViewbox;
    hMarker : TMarkerElement(aOwnerEl).markerBoxWH := LoadViewbox;
    hSymbol : TSymbolElement(aOwnerEl).viewboxWH := LoadViewbox;
    else if aOwnerEl is TPatternElement then
      TPatternElement(aOwnerEl).pattBoxWH := LoadViewbox;
  end;
end;
//------------------------------------------------------------------------------

procedure Visibility_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  case GetHash(value) of
    hCollapse: aOwnerEl.fDrawData.visible := false;
    hHidden: aOwnerEl.fDrawData.visible := false;
    hVisible: aOwnerEl.fDrawData.visible := true;
  end;
end;
//------------------------------------------------------------------------------


procedure Height_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  with aOwnerEl do
  begin
    elRectWH.height.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Width_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  with aOwnerEl do
  begin
    elRectWH.width.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Cx_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hCircle:
      with TCircleElement(aOwnerEl) do centerPt.X.SetValue(val, mu);
    hEllipse:
      with TEllipseElement(aOwnerEl) do centerPt.X.SetValue(val, mu);
    hRadialGradient:
      with TRadGradElement(aOwnerEl) do
      begin
        C.X.SetValue(val, mu);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Cy_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hCircle:
      with TCircleElement(aOwnerEl) do centerPt.Y.SetValue(val, mu);
    hEllipse:
      with TEllipseElement(aOwnerEl) do centerPt.Y.SetValue(val, mu);
    hRadialGradient:
      with TRadGradElement(aOwnerEl) do
      begin
        C.Y.SetValue(val, mu);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Dx_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hfeDropShadow:
      TFeDropShadowElement(aOwnerEl).offset.X.SetValue(val, mu);
    hfeOffset:
      TFeOffsetElement(aOwnerEl).offset.X.SetValue(val, mu);
    hText:
      TTextElement(aOwnerEl).offset.X.SetValue(val, mu);
    hTSpan:
      TTSpanElement(aOwnerEl).offset.X.SetValue(val, mu);
    hTextPath:
      TTextPathElement(aOwnerEl).offset.X.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Dy_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hfeDropShadow:
      TFeDropShadowElement(aOwnerEl).offset.Y.SetValue(val, mu);
    hfeOffset:
      TFeOffsetElement(aOwnerEl).offset.Y.SetValue(val, mu);
    hText:
      TTextElement(aOwnerEl).offset.Y.SetValue(val, mu);
    hTSpan:
      TTSpanElement(aOwnerEl).offset.Y.SetValue(val, mu);
    hTextPath:
      TTextPathElement(aOwnerEl).offset.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Result_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
begin
  if (aOwnerEl is TFeBaseElement) then
    TFeBaseElement(aOwnerEl).res := ExtractRef(value);
end;
//------------------------------------------------------------------------------

procedure Rx_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hRect:
      with TRectElement(aOwnerEl) do
      begin
        radius.X.SetValue(val, mu);
      end;
    hCircle:
      with TCircleElement(aOwnerEl) do
      begin
        radius.SetValue(val, mu);
      end;
    hEllipse:
      with TEllipseElement(aOwnerEl) do
      begin
        radius.X.SetValue(val, mu);
      end;
    hRadialGradient:
      with TRadGradElement(aOwnerEl) do
      begin
        radius.X. SetValue(val, mu);
        radius.Y. SetValue(val, mu);
      end;
    hMarker:
      with TMarkerElement(aOwnerEl) do
        refPt.X.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Ry_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hRect:
      with TRectElement(aOwnerEl) do
      begin
        radius.Y.SetValue(val, mu);
      end;
    hEllipse:
      with TEllipseElement(aOwnerEl) do
      begin
        radius.Y.SetValue(val, mu);
      end;
    hMarker:
      with TMarkerElement(aOwnerEl) do refPt.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure SpreadMethod_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  hash: cardinal;
  c, endC: PUTF8Char;
begin
  if not (aOwnerEl is TGradientElement) then Exit;
  c := PUTF8Char(value);
  endC := c + Length(value);
  hash := ParseNextWordHash(c, endC);
  with TGradientElement(aOwnerEl) do
    case hash of
      hPad      : spreadMethod := gfsClamp;
      hReflect  : spreadMethod := gfsMirror;
      hRepeat   : spreadMethod := gfsRepeat;
    end;
end;
//------------------------------------------------------------------------------

procedure SpectacularExponent(aOwnerEl: TBaseElement; const value: UTF8String);
var
  se: double;
begin
  if not (aOwnerEl is TFeSpecLightElement) then Exit;
  UTF8StringToFloat(value, se);
  if (se > 0) and (se < 100) then
    TFeSpecLightElement(aOwnerEl).exponent := se;
end;
//------------------------------------------------------------------------------

procedure StdDev_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  sd: double;
begin
  UTF8StringToFloat(value, sd);
  if (sd < 0) and (sd > 100) then Exit;
  case aOwnerEl.fXmlEl.Hash of
    hfeGaussianBlur:
      TFeGaussElement(aOwnerEl).stdDev := sd;
    hfeDropShadow:
      TFeDropShadowElement(aOwnerEl).stdDev := sd;
  end;
end;
//------------------------------------------------------------------------------

procedure K1_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  val: double;
begin
  UTF8StringToFloat(value, val);
  if aOwnerEl is TFeCompositeElement then
    TFeCompositeElement(aOwnerEl).fourKs[0] := val;
end;
//------------------------------------------------------------------------------

procedure K2_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  val: double;
begin
  UTF8StringToFloat(value, val);
  if aOwnerEl is TFeCompositeElement then
    TFeCompositeElement(aOwnerEl).fourKs[1] := val;
end;
//------------------------------------------------------------------------------

procedure K3_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  val: double;
begin
  UTF8StringToFloat(value, val);
  if aOwnerEl is TFeCompositeElement then
    TFeCompositeElement(aOwnerEl).fourKs[2] := val;
end;
//------------------------------------------------------------------------------

procedure K4_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  val: double;
begin
  UTF8StringToFloat(value, val);
  if aOwnerEl is TFeCompositeElement then
    TFeCompositeElement(aOwnerEl).fourKs[3] := val;
end;
//------------------------------------------------------------------------------

procedure X1_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hLine:
      TLineElement(aOwnerEl).path[0].X := val;
    hLinearGradient:
      with TLinGradElement(aOwnerEl) do
      begin
        startPt.X.SetValue(val, mu);
      end;
    hFilter:
      with aOwnerEl do
      begin
        elRectWH.left.SetValue(val, mu);
      end;
    else
      aOwnerEl.elRectWH.left.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure X2_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hLine:
      TLineElement(aOwnerEl).path[1].X := val;
    hLinearGradient:
      with TLinGradElement(aOwnerEl) do
      begin
        endPt.X.SetValue(val, mu);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Y1_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hLine:
      TLineElement(aOwnerEl).path[0].Y := val;
    hLinearGradient:
      with TLinGradElement(aOwnerEl) do
      begin
        startPt.Y.SetValue(val, mu);
      end;
    hFilter:
      with aOwnerEl do
      begin
        elRectWH.top.SetValue(val, mu);
      end;
    else
      aOwnerEl.elRectWH.top.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Y2_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  mu: TUnitType;
  val: double;
begin
  UTF8StringToFloatEx(value, val, mu);
  case aOwnerEl.fXmlEl.Hash of
    hLine:
      TLineElement(aOwnerEl).path[1].Y := val;
    hLinearGradient:
      with TLinGradElement(aOwnerEl) do
      begin
        endPt.Y.SetValue(val, mu);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Z_Attrib(aOwnerEl: TBaseElement; const value: UTF8String);
var
  val: double;
begin
  UTF8StringToFloat(value, val);
  if aOwnerEl is TFePointLightElement then
    TFePointLightElement(aOwnerEl).z := val;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TBaseElement.LoadAttribute(attrib: PSvgAttrib);
begin
  with attrib^ do
    case hash of
      hbaseline_045_shift:    BaselineShift_Attrib(self, value);
      hColor:                 Color_Attrib(self, value);
      hClip_045_path:         ClipPath_Attrib(self, value);
      hCx:                    Cx_Attrib(self, value);
      hCy:                    Cy_Attrib(self, value);
      hD:                     D_Attrib(self, value);
      hDisplay:               Display_Attrib(self, value);
      hDx:                    Dx_Attrib(self, value);
      hDy:                    Dy_Attrib(self, value);
      hStroke_045_DashArray:  DashArray_Attrib(self, value);
      hStroke_045_DashOffset: DashOffset_Attrib(self, value);
      hFill:                  Fill_Attrib(self, value);
      hFill_045_Opacity:      FillOpacity_Attrib(self, value);
      hFill_045_Rule:         FillRule_Attrib(self, value);
      hFilter:                Filter_Attrib(self, value);
      hflood_045_color:       Fill_Attrib(self, value);
      hflood_045_opacity:     FillOpacity_Attrib(self, value);
      hFont:                  Font_Attrib(self, value);
      hFont_045_Family:       FontFamily_Attrib(self, value);
      hFont_045_Size:         FontSize_Attrib(self, value);
      hFont_045_Style:        FontStyle_Attrib(self, value);
      hFont_045_Weight:       FontWeight_Attrib(self, value);
      hFx:                    Fx_Attrib(self, value);
      hFy:                    Fy_Attrib(self, value);
      hGradientTransform:     GradientTransform_Attrib(self, value);
      hGradientUnits:         GradientUnits_Attrib(self, value);
      hHeight:                Height_Attrib(self, value);
      hHref:                  Href_Attrib(self, value);
      hId:                    Id_Attrib(self, value);
      hIn:                    In_Attrib(self, value);
      hIn2:                   In2_Attrib(self, value);
      hIntercept:             Intercept_Attrib(self, value);
      hk1:                    K1_Attrib(self, value);
      hk2:                    K2_Attrib(self, value);
      hk3:                    K3_Attrib(self, value);
      hk4:                    K4_Attrib(self, value);
      hletter_045_spacing:    LetterSpacing_Attrib(self, value);
      //hlighting_045_color:    LightingColor_Attrib(self, value);
      hMarker_045_End:        MarkerEnd_Attrib(self, value);
      hMarkerHeight:          Height_Attrib(self, value);
      hMarker_045_Mid:        MarkerMiddle_Attrib(self, value);
      hMarker_045_Start:      MarkerStart_Attrib(self, value);
      hMarkerWidth:           Width_Attrib(self, value);
      hMask:                  Mask_Attrib(self, value);
      hOffset:                Offset_Attrib(self, value);
      hOpacity:               Opacity_Attrib(self, value);
      hOperator:              Operator_Attrib(self, value);
      hOrient:                Orient_Attrib(self, value);
      hPatternUnits:          GradientUnits_Attrib(self, value);
      hPatternTransform:      Transform_Attrib(self, value);
      hPoints:                Points_Attrib(self, value);
      hR:                     Rx_Attrib(self, value);
      hRefX:                  Rx_Attrib(self, value);
      hRefY:                  Ry_Attrib(self, value);
      hResult:                Result_Attrib(self, value);
      hRotate:                Rotate_Attrib(self, value);
      hRx:                    Rx_Attrib(self, value);
      hRy:                    Ry_Attrib(self, value);
      hspecularExponent:      SpectacularExponent(self, value);
      hSlope:                 Slope_Attrib(self, value);
      hSpace:                 Space_Attrib(self, value);
      hSpreadMethod:          SpreadMethod_Attrib(self, value);
      hstdDeviation:          StdDev_Attrib(self, value);
      hStartOffset:           StartOffset_Attrib(self, value);
      hStop_045_Color:        StopColor_Attrib(self, value);
      hStop_045_Opacity:      StopOpacity_Attrib(self, value);
      hStroke:                Stroke_Attrib(self, value);
      hstroke_045_linecap:    StrokeLineCap_Attrib(self, value);
      hstroke_045_linejoin:   StrokeLineJoin_Attrib(self, value);
      hstroke_045_miterlimit: StrokeMiterLimit_Attrib(self, value);
      hStroke_045_Opacity:    StrokeOpacity_Attrib(self, value);
      hStroke_045_Width:      StrokeWidth_Attrib(self, value);
      hTableValues:           TableValues_Attrib(self, value);
      hText_045_Anchor:       TextAlign_Attrib(self, value);
      hText_045_Decoration:   TextDecoration_Attrib(self, value);
      hTextLength:            TextLength_Attrib(self, value);
      hTransform:             Transform_Attrib(self, value);
      hType:                  Type_Attrib(self, value);
      hValues:                Values_Attrib(self, value);
      hViewbox:               Viewbox_Attrib(self, value);
      hVisibility:            Visibility_Attrib(self, value);
      hWidth:                 Width_Attrib(self, value);
      hX:                     X1_Attrib(self, value);
      hX1:                    X1_Attrib(self, value);
      hX2:                    X2_Attrib(self, value);
      hXlink_058_Href:        Href_Attrib(self, value);
      hY:                     Y1_Attrib(self, value);
      hY1:                    Y1_Attrib(self, value);
      hY2:                    Y2_Attrib(self, value);
      hZ:                     Z_Attrib(self, value);
    end;
end;
//------------------------------------------------------------------------------

procedure TBaseElement.LoadAttributes;
var
  i: integer;
begin
  for i := 0 to fXmlEl.AttribCount -1 do
    LoadAttribute(PSvgAttrib(fXmlEl.attrib[i]));
end;
//------------------------------------------------------------------------------

function PreferRelativeFraction(val: TValue): TTriState;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  if (val.rawVal = InvalidD) or (val.unitType = utUnknown) then
    Result := tsUnknown
  else if val.unitType = utPercent then Result := tsYes
  else if val.unitType <> utNumber then Result := tsNo
  else if (Abs(val.rawVal) < 1) then Result := tsYes
  else Result := tsNo;
end;
//------------------------------------------------------------------------------

function TBaseElement.GetRelFracLimit: double;
begin
  //the default behaviour here is to assume untyped fractional values
  //below 1.0 are values relative (to the bounding size) BUT ONLY WHEN
  //the parent element's width or height are relative (ie percentages).
  if Assigned(fParent) and (fParent.fXmlEl.hash <> hSvg) then
  begin
    case PreferRelativeFraction(fParent.elRectWH.width) of
      tsYes: begin Result := 1.0; Exit; end;
      tsNo : begin Result := 0; Exit; end;
    end;
    case PreferRelativeFraction(fParent.elRectWH.height) of
      tsYes: begin Result := 1.0; Exit; end;
      tsNo : begin Result := 0; Exit; end;
    end;
  end;
  Result := 0;
end;
//------------------------------------------------------------------------------

function TBaseElement.LoadContent: Boolean;
var
  i       : integer;
  svgEl   : TSvgXmlEl;
  elClass : TElementClass;
  el      : TBaseElement;
begin
  Result := false;
  for i := 0 to fXmlEl.childs.Count -1 do
  begin
    svgEl := TSvgXmlEl(fXmlEl.childs[i]);
    if svgEl.hash = 0 then Continue;
    elClass := HashToElementClass(svgEl.hash);
    el := elClass.Create(self, svgEl);
    Self.fChilds.Add(el);
    el.LoadAttributes;
    if el.fXmlEl.childs.Count = 0 then Continue
    else if not el.LoadContent then Exit; //error
  end;
  Result := true;
end;

//------------------------------------------------------------------------------
// TSvgReader
//------------------------------------------------------------------------------

constructor TSvgReader.Create;
begin
  fSvgParser           := TSvgParser.Create;
  fLinGradRenderer     := TLinearGradientRenderer.Create;
  fRadGradRenderer     := TSvgRadialGradientRenderer.Create;
  fCustomRendererCache := TCustomRendererCache.Create;
  fIdList              := TSvgIdNameHashMap.Create;
  fSimpleDrawList      := TList.Create;

  fBlurQuality        := 1; //0: draft (faster); 1: good; 2: excellent (slow)
  currentColor        := clBlack32;
  fUsePropScale       := true;
end;
//------------------------------------------------------------------------------

destructor TSvgReader.Destroy;
begin
  Clear;
  fSvgParser.Free;
  fIdList.Free;

  fLinGradRenderer.Free;
  fRadGradRenderer.Free;
  fCustomRendererCache.Free;
  FreeAndNil(fFontCache);
  fSimpleDrawList.Free;

  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.Clear;
var
  i: integer;
begin
  FreeAndNil(fRootElement);
  fSvgParser.Clear;
  fIdList.Clear;
  fLinGradRenderer.Clear;
  fRadGradRenderer.Clear;
  currentColor := clBlack32;
  userSpaceBounds := NullRectD;
  for i := 0 to fSimpleDrawList.Count -1 do
    Dispose(PSimpleDrawData(fSimpleDrawList[i]));
  fSimpleDrawList.Clear;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.DrawImage(img: TImage32; scaleToImage: Boolean);
var
  scale, scaleH: double;
  di: TDrawData;
begin
  if not Assigned(fRootElement) or not assigned(img) then Exit;

  with fRootElement do
  begin
    if viewboxWH.IsEmpty then
    begin
      viewboxWH.Width := elRectWH.width.GetValue(defaultSvgWidth, 0);
      viewboxWH.height := elRectWH.height.GetValue(defaultSvgHeight, 0);
      if viewboxWH.IsEmpty then Exit;  // this should never happen
    end;

    fBackgndImage := img;
    di := fDrawData;
    if di.currentColor = clInvalid then
      di.currentColor := currentColor;

    MatrixTranslate(di.matrix, -viewboxWH.Left, -viewboxWH.Top);

    //the width and height attributes generally indicate the size of the
    //rendered image unless they are percentage values. Nevertheless, these
    //values can be still overridden by the scaleToImage parameter above

    di.bounds := viewboxWH.RectD;
    userSpaceBounds  := fDrawData.bounds;

    if scaleToImage and not img.IsEmpty then
    begin
      //nb: the calculated vbox.width and vbox.height are ignored here since
      //we're scaling the SVG image to the display image. However we still
      //need to call GetViewbox (above) to make sure that viewboxWH is filled.
      if fUsePropScale then
      begin
        scale := GetScaleForBestFit(
          viewboxWH.Width, viewboxWH.Height, img.Width, img.Height);
        scaleH := scale;
      end else
      begin
        scale := GetScale(viewboxWH.Width, img.Width);
        scaleH := GetScale(viewboxWH.Height, img.Height);
      end;
      MatrixScale(di.matrix, scale, scaleH);
      img.SetSize(
        Round(viewboxWH.Width * scale),
        Round(viewboxWH.Height * scaleH));
    end else
      img.SetSize(Round(viewboxWH.Width), Round(viewboxWH.Height));

  end;

  if fBkgndColor <> clNone32 then
    img.Clear(fBkgndColor);

//  // Delay the creation of the TempImage until it is actually needed.
//  // Not all SVGs need it.
//  fTempImageWidth := img.Width;
//  fTempImageHeight := img.Height;

  img.BeginUpdate;
  try
    fRootElement.Draw(img, di);
  finally
//    fTempImageWidth := 0;
//    fTempImageHeight := 0;
    FreeAndNil(fTempImage);
    img.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.GetTempImage: TImage32;
var
  Pixels: TArrayOfColor32;
begin
  // Use an additional method to execute the dyn-array management
  // only if we create the TempImage.
  if fTempImage = nil then
  begin
    // Create an uninitialized image. It is cleared by the caller before it is used.
    NewColor32Array(Pixels, BackgndImage.Width * BackgndImage.Height, True);
    fTempImage := TImage32.Create(Pixels, BackgndImage.Width, BackgndImage.Height);
    fTempImage.BlockNotify;
  end;
  Result := fTempImage;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadInternal: Boolean;
begin
  Result := false;
  if not Assigned(fSvgParser.svgTree) or
    (fSvgParser.svgTree.hash <> hSvg) then Exit;
  fRootElement := TSvgElement.Create(nil, fSvgParser.svgTree);
  fRootElement.fSvgReader := self;
  fRootElement.LoadAttributes;
  Result := fRootElement.LoadContent;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromFile(const filename: string): Boolean;
begin
  Clear;
  Result := fSvgParser.LoadFromFile(filename) and LoadInternal;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromStream(stream: TStream): Boolean;
begin
  Clear;
  Result := fSvgParser.LoadFromStream(stream) and LoadInternal;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromString(const str: string): Boolean;
begin
  Clear;
  Result := fSvgParser.LoadFromString(str) and LoadInternal;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.SetOverrideFillColor(color: TColor32);
var
  dd: TDrawData;
begin
  if not Assigned(RootElement) or (color = clNone32) then Exit;
  dd := RootElement.DrawData;
  dd.fillColor := color;
  RootElement.DrawData := dd;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.SetOverrideStrokeColor(color: TColor32);
var
  dd: TDrawData;
begin
  if not Assigned(RootElement) or (color = clNone32) then Exit;
  dd := RootElement.DrawData;
  if dd.strokeColor = clInvalid then Exit;
  dd.strokeColor := color;
  RootElement.DrawData := dd;
end;
//------------------------------------------------------------------------------

function TSvgReader.FindElement(const idName: UTF8String): TBaseElement;
begin
  if Assigned(RootElement) then
    Result := RootElement.FindChild(idName) else
    Result := nil;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.GetBestFont(const svgFontInfo: TSVGFontInfo);
var
  i, len: integer;
  bestFontReader: TFontReader;
  fi: TFontInfo;
begin
  if svgFontInfo.family = tfUnknown then
    fi.family := tfSerif else
    fi.family := svgFontInfo.family;
  fi.faceName := ''; //just match to a family here, not to a specific facename
  fi.macStyles := [];
  len := Length(svgFontInfo.familyNames);
  SetLength(fi.familyNames, len);
  for i := 0 to len -1 do
    fi.familyNames[i] := svgFontInfo.familyNames[i];

  if svgFontInfo.italic = sfsItalic then
    Include(fi.macStyles, msItalic);
  if svgFontInfo.weight >= 600 then
    Include(fi.macStyles, msBold);

  bestFontReader := FontManager.GetBestMatchFont(fi);
  if not Assigned(bestFontReader) then Exit;

  if Assigned(fFontCache) then
    fFontCache.FontReader := bestFontReader else
    fFontCache := TFontCache.Create(bestFontReader, defaultFontHeight);

  fFontCache.Underlined := False;
  fFontCache.StrikeOut := False;
  case svgFontInfo.decoration of
    fdUnderline     : fFontCache.Underlined := true;
    fdStrikeThrough : fFontCache.StrikeOut := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.SetBlurQuality(quality: integer);
begin
  fBlurQuality := Max(0, Min(2, quality));
end;
//------------------------------------------------------------------------------

function TSvgReader.GetIsEmpty: Boolean;
begin
  Result := not Assigned(fRootElement);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
