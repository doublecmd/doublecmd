Unit uGifViewer;

(*==============================================================================
 DESCRIPTION   : Visual component for displaying an animated image in the
                 GIF (Graphic Interchange Format) format
 DATE          : 17/06/2018
 UPDATE        : 01/07/2025
 VERSION       : 1.0
 AUTHOR        : J.Delauney (BeanzMaster)
 CONTRIBUTORS  : Jipete, Jurassik Pork, bpranoto, Alexander Koblov
 LICENSE       : MPL 2.0
================================================================================
*)

{$mode objfpc}{$H+}

Interface

Uses
  Types, Classes, SysUtils, Graphics, Math, Contnrs, Dialogs,
  Controls, ExtCtrls,
  Lresources, GifViewerStrConsts,
  uFastBitmap;

{%region=====[ Définitions des types et constantes utiles pour le format GIF ]===================================}
Const
  GIF_MaxColors  : Integer    = 256;    // Nombre de couleurs maximum supportées. NE PAS TOUCHER A CETTE VALEUR
  GIF_DelayFactor  : Integer  = 10;     // Facteur de multiplication pour les délais en ms entre chaque image de l'animation
  GIF_DefaultDelay  : Integer = 100;    // 10*10

Type
  TGIFVersion    = (gvUnknown, gv87a, gv89a);
  TGIFVersionRec = Array[0..2] Of AnsiChar;

Const
  GIFVersions  : Array[gv87a..gv89a] Of TGIFVersionRec = ('87a', '89a');

Type
  { En-tête }
  TGIFFileHeader = Packed Record
    Signature: Array[0..2] Of AnsiChar; // 'GIF'
    Version:   TGIFVersionRec;          // '87a' ou '89a' }
  End;

  { Description globale de l'image }
  TGIFLogicalScreenDescriptorRec = Packed Record
    ScreenWidth:  Word;             // Largeur de l'image en pixels // Width
    ScreenHeight: Word;             // Hauteur de l'image en pixels // Height
    PackedFields: Byte;             // champs compactés // Compacted field
    BackgroundColorIndex: Byte;     // Index globale de la couleur de fond // Index of background color
    AspectRatio:  Byte;             // Ratio d'échelle = (AspectRatio + 15) / 64
  End;

  { Description d'une image }
  TGIFImageDescriptorRec = Packed Record
    //Separator: byte;    // On lis toujours un byte avant  // we always read it before
    Left:   Word;         // Colonne en pixels par rapport au bord gauche de l'écran // Column in pixels from the left edge of the screen
    Top:    Word;         // Rangée en pixels par rapport au haut de l'écran // Row in pixels from the top edge of the screen
    Width:  Word;         // Largeur de l'image en cours en pixels // image width
    Height: Word;         // Hauteur de l'image en cours pixels // Image height
    PackedFields: Byte;   // Champs compactés // Compacted field
  End;

  { Graphic Control Extension bloc a.k.a GCE }
  TGIFGraphicControlExtensionRec = Packed Record
    // BlockSize: byte;           // Normalement toujours 4 octets // Always 4 bytes
    PackedFields: Byte;           // Champs compacté // Compacted field
    DelayTime:    Word;           // Délai entre chaque image en centième de secondes // Delay between each image in hundredths of a second
    TransparentColorIndex: Byte;  // Index dans la palette si plus petit ou égale  // Delay between each image in hundredths of a second
    // Terminator: Byte;          // Normalement toujours ZERO // Normally always ZERO
  End;

  TGIFDisposalFlag = (dmNone, dmKeep, dmErase, dmRestore); // Methodes pour l'affichage des images lors de l'animation

  { Plain Text Extension }
  TGIFPlainTextExtensionRec = Packed Record
    // BlockSize: byte;              // Normalement égal à 12 octets // Normally equal to 12 bytes
    Left, Top, Width, Height: Word;  // Positions et dimensions du texte  // position and dimension of text
    CellWidth, CellHeight:    Byte;  // Dimensions d'une cellule dans l'image // Size of cell
    TextFGColorIndex,                // Index de la couleur de fond dans la palette // Index of the background color
    TextBGColorIndex:         Byte;  // Index de la couleur du texte dans la palette // Index of the text color
  End;

  { Application Extension }
  TGIFApplicationExtensionRec = Packed Record
    AppID: Array [0..7] Of AnsiChar;                  // Identification de l'application majoritairement 'NETSCAPE' ou ''
    AppAuthenticationCode: Array [0..2] Of AnsiChar;  // Code d'authentification ou numero de version
  End;

  { Informations de "l'application extension" si disponible }
  TGIFNSLoopExtensionRec = Packed Record
    Loops:      Word;   // Nombre de boucle de l'animation 0 = infinie  // nb loop
    BufferSize: DWord;  // Taille du tampon. Usage ?????
  End;

Const
  // Description des masques pour la description globale de l'image
  GIF_GLOBALCOLORTABLE = $80;        // Défini si la table de couleurs globale suit la description globale
  GIF_COLORRESOLUTION = $70;         // Résolution de la couleur (BitsPerPixel) - 3 bits
  GIF_GLOBALCOLORTABLESORTED = $08;  // Définit si la palette globale est triée - 1 bit
  GIF_COLORTABLESIZE  = $07;         // Taille de la palette - 3 bits
  GIF_RESERVED        = $0C;         // Réservé - doit être défini avec $00 - Taille des données = 2^value+1 - 3 bits

  // Descption des masques pour les images
  GIF_LOCALCOLORTABLE = $80;        // Défini si la table de couleurs locale suit la description de l'image
  GIF_INTERLACED      = $40;        // Défini si l'image est entrelacée
  GIF_LOCALCOLORTABLESORTED = $20;  // Définit si la palette locale est triée

  // Identification des blocs
  GIF_PLAINTEXT       = $01;
  GIF_GRAPHICCONTROLEXTENSION = $F9;
  GIF_COMMENTEXTENSION = $FE;
  GIF_APPLICATIONEXTENSION = $FF;
  GIF_IMAGEDESCRIPTOR = $2C;       // ','
  GIF_EXTENSIONINTRODUCER = $21;   // '!'
  GIF_TRAILER         = $3B;               // ';'

  // Graphic Control Extension - Définition des masques pour les paramètres
  GIF_NO_DISPOSAL     = $00;           // 0
  GIF_DO_NOT_DISPOSE  = $04;           // 1
  GIF_RESTORE_BACKGROUND_COLOR = $08;  // 2
  GIF_RESTORE_PREVIOUS = $12;          // 3
  GIF_DISPOSAL_ALL    = $1C;           // bits 2-4 ($1C)
  GIF_USER_INPUT_FLAG = $02;
  GIF_TRANSPARENT_FLAG = $01;
  GIF_RESERVED_FLAG   = $E0;

  // Identification des sous-blocs pour "Application Extension"
  GIF_LOOPEXTENSION   = 1;
  GIF_BUFFEREXTENSION = 2;

Const
  GifGCEDisposalModeStr  : Array[TGIFDisposalFlag] Of String = ('None', 'Keep', 'Erase', 'Restore');

Type
  { Informations sur une image de l'animation }
  TGIFFrameInformations = Record
    Left, Top,                          // Position de l'image
    Width, Height:   Integer;           // Dimension de l'image
    HasLocalPalette: Boolean;           // Palette locale disponible
    IsTransparent:   Boolean;           // Image transparente
    UserInput:       Boolean;           // Données personnelle
    BackgroundColorIndex: Byte;         // Normalement seulement valide si une palette globale existe
    TransparentColorIndex: Byte;        // Index de la couleur transparente
    DelayTime:       Word;              // Délai d'animation
    Disposal:        TGIFDisposalFlag;  // Methode d'affichage
    Interlaced:      Boolean;           // Image entrelacée
  End;
  PGifFrameInformations = ^TGifFrameInformations;

  {%endregion%}

  { TGIFFastMemoryStream }
  { Classe d'aide à la lecture des données dans un flux en mémoire }
  TGIFFastMemoryStream = Class
  Private
    FBuffer:   PByte;
    FPosition: Int64;
    FBytesRead, FBytesLeft, FSize: Int64;
  Public
    Constructor Create(AStream : TStream);
    Destructor Destroy; Override;

    { Lit un Byte dans le tampon / Read a byte in buffer }
    Function ReadByte: Byte;
    { Lit un Word dans le tampon / Read a word in buffer}
    Function ReadWord: Word;
    { Lit un DWord dans le tampon / Read a DWord in buffer }
    Function ReadDWord: DWord;
    { Lit et retourne un tampon "Buffer" de taille "Count" octets / Read a buffer of size "count" }
    Function Read(Var Buffer; Count : Int64): Int64;
    { Déplacement dans le flux de "Offset" depuis  "Origin"
      TSeekOrigin =
        - soBeginning : Depuis le début du flux
        - soCurrent   : a partir de la position courante
        - soEnd       : A partir de la fin du flux
    }
    Function Seek(Const Offset : Int64; Origin : TSeekOrigin): Int64;
    { Déplacement dans le flux vers l'avant de "Cnt" octet depuis la position courrante }
    Procedure SeekForward(Cnt : Integer);
    { Indique si la fin du flux est atteinte (EOS = End Of Stream) }
    Function EOS: Boolean;

    { Retourne la taille du flux en octet // Size in byte of the buffer}
    Property Size: Int64 read FSize;
    { Retourne la position courrante de lecture dans le tampon // Current position in buffer }
    Property Position: Int64 read FPosition;
  End;

  { TGIFLoadErrorEvent : Fonction d'évènement levée en cas d'erreur(s) dans le chargement // Event raise on error }
  TGIFLoadErrorEvent = Procedure(Sender : TObject; Const ErrorCount : Integer; Const ErrorList : TStringList) Of Object;

  { TGIFImageListItem }
  { Définition d'une image contenue dans le fichier GIF }
  TGIFImageListItem = Class
  Private
    FBitmap:      TFastBitmap;
    FDrawMode:    TGIFDisposalFlag;
    FLeft, FTop:  Integer;
    FComment:     TStringList;
    FDelay:       Integer;
    FTransparent: Boolean;
    FIsCorrupted : Boolean;
  Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;

    { Objet contenant l'image }
    Property Bitmap: TFastBitmap read FBitmap write FBitmap;
    { Mode de rendu de l'image // Render Mode}
    Property DrawMode: TGIFDisposalFlag read FDrawMode write FDrawMode;
    { Position gauche de l'image }
    Property Left: Integer read FLeft write FLeft;
    { Position Haut de l'image }
    Property Top: Integer read FTop write FTop;
    { Temps d'attente entre deux image de l'animation }
    Property Delay: Integer read FDelay write FDelay;
    { Commentaire sur l'image }
    Property Comment: TStringList read FComment write FComment;
    { Retourne TRUE si l'image utilise la transparence }
    Property IsTransparent: Boolean read FTransparent write FTransparent;
    { Indique si l'image est corrompue }
    property IsCorrupted : Boolean read FIsCorrupted write FIsCorrupted;
  End;

  { TGIFImageList }
  { Classe d'aide à la gestion des images contenues dans le fichier GIF }
  { Helper class for manage image in GIF }
  TGIFImageList = Class(TObjectList)
  Private
  Protected
    Function GetItems(Index : Integer): TGIFImageListItem;
    Procedure SetItems(Index : Integer; AGifImage : TGIFImageListItem);
  Public
    { Efface la liste  }
    Procedure Clear; Override;
    { Ajoute une nouvelle image vide à la liste }
    Function AddNewImage: TGIFImageListItem;
    { Ajout d'une image dans la liste }
    Function Add(AGifImage : TGIFImageListItem): Integer;
    { Extraction d'une image de la liste }
    Function Extract(Item : TGIFImageListItem): TGIFImageListItem;
    { Effacement d'une image dans la liste }
    Function Remove(AGifImage : TGIFImageListItem): Integer;
    { Retourne l'index de l'image recherchée (retourne -1 si non trouvé) }
    Function IndexOf(AGifImage : TGIFImageListItem): Integer;
    { Retourne la première image }
    Function First: TGIFImageListItem;
    { Retourne la dernière image }
    Function Last: TGIFImageListItem;
    { Insertion d'une image à la position "Index" }
    Procedure Insert(Index : Integer; AGifImage : TGIFImageListItem);

    { Liste des images }
    Property Items[Index: Integer]: TGIFImageListItem read GetItems write SetItems; Default;
  End;

  { TGIFImageLoader }
  { Classe spécialisée pour la lecture d'une image au format GIF }
  { Special class for read a GIF }
  TGIFImageLoader = Class
  Private
    FCurrentLayerIndex: Integer;
    FGIFFIleHeader: TGIFFileHeader;
    FLogicalScreenChunk: TGIFLogicalScreenDescriptorRec;
    FHasGlobalPalette: Boolean;
    FTransparent: Boolean;
    FGlobalPalette: TColor32List;
    FVersion: String;

    FWidth, FHeight:  Integer;
    FBackgroundColor: TColor32;

    FFrames: TGIFImageList;

    FErrorList:   TStringList;
    FErrorCount:  Integer;
    FOnLoadError: TGIFLoadErrorEvent;
    Procedure SetCurrentLayerIndex(AValue : Integer);

  Protected
    Memory: TGIFFastMemoryStream;

    CurrentFrameInfos: TGifFrameInformations;

    Function GetFrameCount: Integer;
    Procedure LoadFromMemory();
    Function CheckFormat(): Boolean;
    Function ReadImageProperties: Boolean;
    Procedure AddError(Msg : String);
    Procedure NotifyError;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    { LoadFromStream : Charge les données depuis un flux }
    Procedure LoadFromStream(aStream : TStream); Virtual;
    { LoadFromFile : Charge les données depuis un fichier physique }
    Procedure LoadFromFile(Const FileName : String); Virtual;
    { Chargement depuis une Resource Lazarus }
    Procedure LoadFromResource(Const ResName : String);
    { Retourne la version du fichier GIF }
    Property Version: String read FVersion;
    { Retourne la largeur de l'image GIF }
    Property Width: Integer read FWidth;
    { Retourne la hauteur de l'image GIF }
    Property Height: Integer read FHeight;
    { Retourne la couleur de l'image GIF si elle existe,. Sinon retourne une couleur transparente (clrTransparent) }
    Property BackgroundColor: TColor32 read FBackgroundColor write FBackgroundColor;
    { Prise en charge de la transparence dans l'image GIF  // Take transparency in account}
    Property Transparent: Boolean read FTransparent write FTransparent;
    { Retourne l'index courrant de l'image de l'animation traité  // Return the current index frame}
    Property CurrentFrameIndex: Integer read FCurrentLayerIndex write SetCurrentLayerIndex;
    { Liste des images de l'animation // List of frame}
    Property Frames: TGIFImageList read FFrames;
    { Nombre d'image de l'animation // Nb frames }
    Property FrameCount: Integer read GetFrameCount;
    { Nombre d'erreur produite loars d'un cahrgement ou d'un enregistrement // Nb error }
    Property ErrorCount: Integer read FErrorCount;
    { Liste des erreurs // List of error }
    Property Errors: TStringList read FErrorList;

    { Evenement pour intercepter les erreurs notifiées lors du chargement des données // Error Event }
    Property OnLoadError: TGIFLoadErrorEvent read FOnLoadError write FOnLoadError;
  End;

  { TGIFRenderCacheListItem }
  { Définition d'une image cache de l'animation }
  { Image cache class }
  TGIFRenderCacheListItem = Class
  Private
    FBitmap: Graphics.TBitmap;
    FDelay:  Integer;
    FIsCorrupted : Boolean;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    { Image cache prérendu de l'animation }
    Property Bitmap: Graphics.TBitmap read FBitmap write FBitmap;
    { Temps d'attente en ms avec l'image suivante }
    Property Delay: Integer read FDelay write FDelay;
    { Indique si l'image est corrompue }
    property IsCorrupted : Boolean read FIsCorrupted write FIsCorrupted;
  End;

  { TGIFRenderCacheList }
  { Classe d'aide à la gestion des images rendues de l'animation }
  { Helper class for manage list of image cache }
  TGIFRenderCacheList = Class(TObjectList)
  Private
  Protected
    Function GetItems(Index : Integer): TGIFRenderCacheListItem;
    Procedure SetItems(Index : Integer; AGIFRenderCache : TGIFRenderCacheListItem);
  Public
    { Efface la liste }
    Procedure Clear; Override;
    { Ajoute un nouvel objet cache vide }
    Function AddNewCache: TGIFRenderCacheListItem;
    { Ajoute un nouveau cache }
    Function Add(AGIFRenderCache : TGIFRenderCacheListItem): Integer;
    { Extrait un cache de la liste }
    Function Extract(Item : TGIFRenderCacheListItem): TGIFRenderCacheListItem;
    { Supprime un cache de la liste }
    Function Remove(AGIFRenderCache : TGIFRenderCacheListItem): Integer;
    { Retourne l'index du cache recherchée (retourne -1 si non trouvé) }
    Function IndexOf(AGIFRenderCache : TGIFRenderCacheListItem): Integer;
    { Retourne le premier élément de la liste }
    Function First: TGIFRenderCacheListItem;
    { Retourne le dernier élément de la liste }
    Function Last: TGIFRenderCacheListItem;
    { Insertion d'un cache à la position "Index" }
    Procedure Insert(Index : Integer; AGIFRenderCache : TGIFRenderCacheListItem);
    { Vérifie si "anIndex" ne dépasse pas la nombre d'élément dans la liste. Retroune FALSE si l'index est hors limite }
    { Check if 'anIndex' does not exceed the number of items in the list. Retrieve FALSE if the index is out of range }
    function IsIndexOk(anIndex : Integer) : Boolean;
    { Supprime les éléments dont le drapeau "IsCorrupted" est vrai  }
    { Remove items wich "IsCorrupted" flag is on True }
    procedure Pack;
    { Liste des caches }
    Property Items[Index: Integer]: TGIFRenderCacheListItem read GetItems write SetItems; Default;
  End;

  { TGIFAutoStretchMode
    Mode de redimensionnement automatique}
  TGIFAutoStretchMode = (smManual, smStretchAll, smStretchOnlyBigger, smStretchOnlySmaller );
  TOnStretchChanged = procedure (Sender:TObject; IsStretched : Boolean) of object;
  { TGIFViewer }
  { Composant visuel pour afficher une image GIF animée }
  { Visual component for display the animated GIF }
  TGIFViewer = Class(TGraphicControl)
  Private
    FAutoStretchMode: TGIFAutoStretchMode;
    FGIFLoader: TGIFImageLoader;
    FLastDrawMode : TGIFDisposalFlag;
    FFileName:  String;

    FRestoreBitmap, FVirtualView: TFastBitmap;

    FRenderCache:       TGIFRenderCacheList;
    FCurrentFrameIndex: Integer;
    FGIFWidth, FGIFHeight: Integer;
    FCurrentView:       Graphics.TBitmap;

    FAnimateTimer:     TTimer;
    FAnimateSpeed:     Integer;
    FAnimated, FPause: Boolean;
    FAutoPlay:         Boolean;
    FCache:            Boolean;

    FDisplayInvalidFrames : Boolean;
    FAutoRemoveInvalidFrame : Boolean;

    FPainting:         Boolean;

    FBorderShow:       Boolean;
    FBorderColor:      TColor;
    FBorderWidth:      Byte;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:       TBevelWidth;
    FBevelColor, FColor: TColor;

    FCenter, FStretch, FTransparent: Boolean;

    FOnStart, FOnStop, FOnPause, FOnFrameChange: TNotifyEvent;
    FOnLoadError : TGIFLoadErrorEvent;
    FOnStretchChanged : TOnStretchChanged;

    Function GetCanvas: TCanvas;
    Function GetFrameCount: Integer;
    Function GetGIFVersion: String;
    Function GetRawFrameItem(Index : Integer): TGIFImageListItem;
    Procedure SetAutoStretchMode(AValue: TGIFAutoStretchMode);
    Procedure SetCenter(Const Value : Boolean);
    Procedure SetStretch(Const Value : Boolean);
    Procedure SetPause(Const Value : Boolean);
    Procedure SetFileName(Const Value : String);
    Function GetFrame(Const Index : Integer): Graphics.TBitmap;
    Procedure SetTransparent(Const Value : Boolean);
    Procedure SetBevelInner(Const Value : TPanelBevel);
    Procedure SetBevelOuter(Const Value : TPanelBevel);
    Procedure SetBevelWidth(Const Value : TBevelWidth);

    procedure ResetCurrentView;
  Protected
    Procedure DoInternalOnLoadError(Sender : TObject; Const ErrorCount : Integer; Const ErrorList : TStringList);
    Procedure DoTimerAnimate(Sender : TObject);

    { Rendu d'une image de l'animation }
    procedure RenderFrame(Index : Integer); Virtual;
    { Creation des image cache pour l'animation }
    Procedure ComputeCache; Virtual;
    { Calcul de la postion et de la dimension pour l'afficchage sur le "Canvas" }
    Function DestRect: TRect; Virtual;

    { Fonctions hérités }
    Procedure CalculatePreferredSize(Var PreferredWidth, PreferredHeight : Integer; {%H-}WithThemeSpace : Boolean); Override;
    Class Function GetControlClassDefaultSize: TSize; Override;
    Procedure Paint; Override;
    procedure Loaded; override;
    procedure BeforeLoad;
    procedure AfterLoad;
  Public
    { Création du composant }
    Constructor Create(AOwner : TComponent); Override;
    { Destruction du composant }
    Destructor Destroy; Override;

    { Mise à jour de la surface de dessin (Canvas) du composant }
    Procedure Invalidate; Override;
    { LoadFromStream : Charge les données depuis un flux }
    Procedure LoadFromStream(aStream : TStream);
    { Chargement depuis un fichier }
    Procedure LoadFromFile(Const aFileName : String);
    { Chargement depuis une Resource Lazarus }
    Procedure LoadFromResource(Const ResName : String);
    { Joue l'animation }
    Procedure Start;
    { Arrête l'animation }
    Procedure Stop;
    { Met en pause l'animation }
    Procedure Pause;
    Procedure NextFrame;
    Procedure PriorFrame;
    { Retourne l'image brute du GIF à la position Index }
    Function GetRawFrame(Index : Integer): TBitmap;
    { Affiche l'image de l'animation mise en cache à la position Index }
    Procedure DisplayFrame(Index : Integer);
    { Affiche l'image brute de l'animation à la position Index }
    Procedure DisplayRawFrame(Index : Integer);
    { Extrait l'image de l'animation mise en cache à la position Index vers un TBitmap }
    procedure ExtractFrame(Index : Integer; Var bmp:TBitmap) ;
    { Extrait l'image brute de l'animation à la position Index vers un TBitmap}
    procedure ExtractRawFrame(Index : Integer; Var bmp:TBitmap);
    { Retourne le Canvas du composant }
    Property Canvas: TCanvas read GetCanvas;
    { Retourne TRUE si l'animation est en pause }
    Property Paused: Boolean read FPause;
    { Retourne TRUE si l'animation est en cours }
    Property Playing: Boolean read FAnimated;
    { Retourne l'index actuel de l'image affichée // Current Index of displayed frame }
    Property CurrentFrameIndex: Integer read FCurrentFrameIndex;
    { Liste des images de l'animation // List of frame}
    Property Frames[Index: Integer]: TBitmap read GetFrame;
    { Retourne le nombre d'image de l'animation // Number of frames }
    Property FrameCount: Integer read GetFrameCount;
    { Retourne la version du fichier GIF chargé // version of the gif }
    Property Version: String read GetGIFVersion;
    { Image courante de l'animation affichée // Current displayed image }
    Property CurrentView: Graphics.TBitmap read FCurrentView;

    property RawFrames[Index : Integer] : TGIFImageListItem read GetRawFrameItem;
  Published
    Property Color: TColor read FColor write FColor;
    { Bordure visible autour du composant // Border visible around component }
    Property Border: Boolean read FBorderShow write FBorderShow;
    { Couleur de la bordure // Color of border }
    Property BorderColor: TColor read FBorderColor write FBorderColor;
    { Epaisseur de la bordure // Width of border }
    Property BorderWidth: Byte read FBorderWidth write FBorderWidth;

    Property BevelColor: TColor read FBevelColor write FBevelColor;
    Property BevelInner: TPanelBevel read FBevelInner write SetBevelInner Default bvNone;
    Property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter Default bvRaised;
    Property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth Default 1;

    Property Cache: Boolean read FCache write FCache;
    { Joue l'animation automatiquement lors du chargement d'une image GIF animée }
    { Play animation automatically when loading an animated GIF image }
    Property AutoPlay: Boolean read FAutoPlay write FAutoPlay;
    { Affichage du GIF avec prise en charge de la transparence }
    { GIF view with transparency support }
    Property Transparent: Boolean read FTransparent write SetTransparent;
    { Centrer l'affichage // Center display }
    Property Center: Boolean read FCenter write SetCenter;
    { Mode du redimensionnement // Automatic stretch mode
      smManual             : Adpatation Manuelle via la propriété stretch
      smStretchAll         : Adapte toute les images
      smStretchOnlyBigger  : Adapte seulement les images plus grande
      smStretchOnlySmaller : Adapte seulement les images plus petite
     }
    property AutoStretchMode : TGIFAutoStretchMode read FAutoStretchMode write SetAutoStretchMode;
    { Redimensionner l'affichage proportionnellement // Resize the display proportionally }
    Property Stretch: Boolean read FStretch write SetStretch;
    { Nom du fichier à charger // Name of file to load }
    Property FileName: String read FFileName write SetFileName;
    { Définis si les images corrompues doivent être affichées. Si le GIF contient que une seule image ce paramètre n'est pas appliqué. Par defaut FALSE }
    property DisplayInvalidFrames : Boolean read FDisplayInvalidFrames write FDisplayInvalidFrames;
    { Définis si les images corrompues doivent être effacées de la liste de l'animation automatiquement. Par defaut TRUE }
    property AutoRemoveInvalidFrame : Boolean Read FAutoRemoveInvalidFrame write FAutoRemoveInvalidFrame;

    { Evènement déclenché lorsque l'animation débute }
    { Event triggered when the animation starts }
    Property OnStart: TNotifyEvent read FOnStart write FOnStart;
    { Evènement déclenché lorsque l'animation s'arrête }
    { Event triggered when the animation stops }
    Property OnStop: TNotifyEvent read FOnStop write FOnStop;
    { Evènement déclenché lorsque l'animation est mise en pause }
    { Event triggered when the animation is paused }
    Property OnPause: TNotifyEvent read FOnPause write FOnPause;
    { Evènement déclenché lorsque une nouvelle image est affiché lors de l'animation }
    { Event triggered when a new image is displayed during the animation }
    Property OnFrameChange: TNotifyEvent read FOnFrameChange write FOnFrameChange;
    { Evenement pour intercepter les erreurs notifiées lors du chargement des données }
    Property OnLoadError: TGIFLoadErrorEvent read FOnLoadError write FOnLoadError;
    { Evenement pour intercepter le changement du mode stretch. Uniquement si AutoStretchMode <> smManual }
    { Event to intercept the change of the stretch mode. Only if AutoStretchMode <> smManual }
    property OnStretchChanged : TOnStretchChanged read FOnStretchChanged write FOnStretchChanged;

    { Propriétés héritées }
    Property Align;
    Property Anchors;
    Property AutoSize;
    Property Constraints;
    Property BorderSpacing;
    Property Visible;
    Property ParentShowHint;
    Property ShowHint;
    { Evènements héritées }
    Property OnClick;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnMouseWheel;
    Property OnMouseWheelDown;
    Property OnMouseWheelUp;

  End;

  TGIFView = Class(TGIFViewer);

Procedure Register;

Implementation

Uses
  GraphType;

{$R ../gifview.res}

{%region=====[ Constantes et types internes ]===================================}


Type
  // Statut de décodage / encodage LZW
  TLZWDecoderStatus = (
    dsOK,                     // Tout va bien
    dsNotEnoughInput,         // Tampon d'entrée trop petit
    dsOutputBufferTooSmall,   // Tampon de sortie trop petit
    dsInvalidInput,           // Donnée corrompue
    dsBufferOverflow,         // débordement de tampon
    dsInvalidBufferSize,      // Taille d'un des tampons invalide
    dsInvalidInputBufferSize, // Taille du tampon d'entrée invalide
    dsInvalidOutputBufferSize,// Taille du tampon de sortie invalide
    dsInternalError           // Erreur interne signifiant qu'il y a un défaut dans le code
    );

{%endregion%}

{%region=====[ Fonctions utiles ]===============================================}

Function FixPathDelimiter(S : String): String;
Var
  I: Integer;
Begin
  Result := S;
  For I  := Length(Result) Downto 1 Do
  Begin
    If (Result[I] = '/') Or (Result[I] = '\') Then Result[I] := PathDelim;
  End;
End;

Function CreateFileStream(Const fileName : String; mode : Word = fmOpenRead + fmShareDenyNone): TStream;
Var
  fn: String;
Begin
  fn := filename;
  FixPathDelimiter(fn);
  If ((mode And fmCreate) = fmCreate) Or FileExists(fn) Then Result := TFileStream.Create(fn, mode)
  Else
    Raise Exception.Create('Fichier non trouvé : "' + fn + '"');

End;

{%endregion%}

{%region=====[ TGIFFastMemoryStream ]==============================================}

Constructor TGIFFastMemoryStream.Create(AStream : TStream);
Var
  ms: TMemoryStream;
Begin
  ms := TMemoryStream.Create;
  With ms Do
  Begin
    CopyFrom(aStream, 0);
    Position := 0;
  End;
  FSize      := ms.Size;
  FPosition  := 0;
  FBytesLeft := FSize;
  FBytesRead := 0;
  FBuffer    := nil;
  ReAllocMem(FBuffer, FSize);
  Move(PByte(ms.Memory)^, FBuffer^, FSize);
  FreeAndNil(ms);
End;

Destructor TGIFFastMemoryStream.Destroy;
Begin
  If FBuffer <> nil Then
  Begin
    FreeMem(FBuffer);
    FBuffer := nil;
  End;
  Inherited Destroy;
End;

Function TGIFFastMemoryStream.ReadByte: Byte;
Begin
  Result := 0;
  If FBytesLeft > 0 Then
  Begin
    Result := PByte(FBuffer + FPosition)^;
    Inc(FPosition);
    Inc(FBytesRead);
    Dec(FBytesLeft);
  End;
End;

Function TGIFFastMemoryStream.ReadWord: Word;
Begin
  Result := 0;
  If (FBytesLeft >= 2) Then
  Begin
    Result := PWord(FBuffer + FPosition)^;
    Inc(FPosition, 2);
    Inc(FBytesRead, 2);
    Dec(FBytesLeft, 2);
  End;
End;

Function TGIFFastMemoryStream.ReadDWord: DWord;
Begin
  Result := 0;
  If (FBytesLeft >= 4) Then
  Begin
    Result := PDWord(FBuffer + FPosition)^;
    Inc(FPosition, 4);
    Inc(FBytesRead, 4);
    Dec(FBytesLeft, 4);
  End;
End;

Function TGIFFastMemoryStream.Read(Var Buffer; Count : Int64): Int64;
Var
  NumOfBytesToCopy, NumOfBytesLeft: Longint;
  CachePtr, BufferPtr: PByte;
Begin
  Result := 0;

  If (Count > FBytesLeft) Then NumOfBytesLeft := FBytesLeft
  Else
    NumOfBytesLeft := Count;

  BufferPtr := @Buffer;

  While NumOfBytesLeft > 0 Do
  Begin
    // On copie les données
    NumOfBytesToCopy := Min(FSize - FPosition, NumOfBytesLeft);
    CachePtr         := FBuffer;
    Inc(CachePtr, FPosition);

    Move(CachePtr^, BufferPtr^, NumOfBytesToCopy);
    Inc(Result, NumOfBytesToCopy);
    Inc(FPosition, NumOfBytesToCopy);
    Inc(BufferPtr, NumOfBytesToCopy);
    // On met à jour les marqueur de notre tampon
    Inc(FBytesRead, NumOfBytesToCopy);
    Dec(FBytesLeft, NumOfBytesToCopy);
    Dec(NumOfBytesLeft, NumOfBytesToCopy);

  End;
End;

Function TGIFFastMemoryStream.Seek(Const Offset : Int64; Origin : TSeekOrigin): Int64;
Var
  NewPos: Integer;
Begin
  // Calcul de la nouvelle position
  Case Origin Of
    soBeginning: NewPos := Offset;
    soCurrent: NewPos   := FPosition + Offset;
    soEnd: NewPos       := pred(FSize) - Offset;
    Else
      Raise Exception.Create('TFastStream.Seek: Origine Invalide');
  End;
  Result := NewPos;
  If Offset = 0 Then exit;

  FPosition  := NewPos;
  FBytesLeft := FSize - FPosition;
  Result     := NewPos;
End;

Procedure TGIFFastMemoryStream.SeekForward(Cnt : Integer);
Begin
  Seek(Cnt, soCurrent);
End;

Function TGIFFastMemoryStream.EOS: Boolean;
Begin
  Result := ((FBytesLeft <= 0) Or (FPosition >= Pred(FSize)));
End;

{%endregion%}

{%region=====[ TGIFImageListItem ]==============================================}

Constructor TGIFImageListItem.Create;
Begin
  FBitmap   := TFastBitmap.Create;
  FLeft     := 0;
  FTop      := 0;
  FDelay    := 0;
  FDrawMode := dmNone;
  FComment  := TStringList.Create;
  FComment.Clear;
  FIsCorrupted := False;
End;

Destructor TGIFImageListItem.Destroy;
Begin
  FreeAndNil(FComment);
  FreeAndNil(FBitmap);
  Inherited Destroy;
End;

{%endregion%}

{%region=====[ TGIFImageList ]==================================================}

Function TGIFImageList.GetItems(Index : Integer): TGIFImageListItem;
Begin
  Result := TGIFImageListItem(Inherited Items[Index]);
End;

Procedure TGIFImageList.SetItems(Index : Integer; AGifImage : TGIFImageListItem);
Begin
  Put(Index, AGifImage);
End;

Procedure TGIFImageList.Clear;
Var
  anItem: TGIFImageListItem;
  i:      Integer;
Begin
  If Count > 0 Then
  Begin
    For i := Count - 1 Downto 0 do
    Begin
      AnItem := Items[i];
      If anItem <> nil Then anItem.Free;
    End;
  End;
  Inherited Clear;
End;

Function TGIFImageList.AddNewImage: TGIFImageListItem;
Var
  anItem: TGIFImageListItem;
Begin
  anitem := TGIFImageListItem.Create;
  Add(anItem);
  Result := Items[Self.Count - 1];
End;

Function TGIFImageList.Add(AGifImage : TGIFImageListItem): Integer;
Begin
  Result := Inherited Add(AGifImage);
End;

Function TGIFImageList.Extract(Item : TGIFImageListItem): TGIFImageListItem;
Begin
  Result := TGIFImageListItem(Inherited Extract(Item));
End;

Function TGIFImageList.Remove(AGifImage : TGIFImageListItem): Integer;
Begin
  Result := Inherited Remove(AGifImage);
End;

Function TGIFImageList.IndexOf(AGifImage : TGIFImageListItem): Integer;
Begin
  Result := Inherited IndexOf(AGifImage);
End;

Function TGIFImageList.First: TGIFImageListItem;
Begin
  Result := TGIFImageListItem(Inherited First);
End;

Function TGIFImageList.Last: TGIFImageListItem;
Begin
  Result := TGIFImageListItem(Inherited Last);
End;

Procedure TGIFImageList.Insert(Index : Integer; AGifImage : TGIFImageListItem);
Begin
  Inherited Insert(Index, AGifImage);
End;

{%endregion%}

{%region=====[ TGIFImageLoader ]================================================}

Constructor TGIFImageLoader.Create;
Begin
  Inherited Create;
  FFrames        := TGIFImageList.Create(False);
  FErrorList     := TStringList.Create;
  FErrorCount    := 0;
  FGlobalPalette := nil;
  FTransparent   := True;
  FBackgroundColor := clrTransparent;
End;

Destructor TGIFImageLoader.Destroy;
Begin
  FreeAndNil(FFrames);
  FreeAndNil(FErrorList);
  Inherited Destroy;
End;

Function TGIFImageLoader.CheckFormat(): Boolean;
Begin
  Result := False;
  // Chargement de l'en-tête
  Memory.Read(FGIFFileHeader, SizeOf(TGIFFileHeader));
  // Vérification de quelques paramètres
  Result := uppercase(String(FGIFFileHeader.Signature)) = 'GIF';
  If Result Then
  Begin
    // Le fichier est valide
    // On sauvegarde la version du GIF
    FVersion := String(FGIFFileHeader.Version);
    If (FVersion = GIFVersions[gv87a]) Or (FVersion = GIFVersions[gv89a]) Then Result := ReadImageProperties // On lit les propriétés
    Else
      Raise Exception.Create(rsUnknownVersion);
  End
  Else
  Begin
    // Signature du fichier GIF Invalide. On lève une exception
    Raise Exception.Create(Format(rsBadSignature,[uppercase(String(FGIFFileHeader.Signature))]));
  End;
End;

Function TGIFImageLoader.ReadImageProperties: Boolean;
Begin
  Result := False;

  Memory.Read(FLogicalScreenChunk, SizeOf(TGIFLogicalScreenDescriptorRec));

  // On sauvegarde en local les dimensions de l'image, pour plus tard
  FWidth  := FLogicalScreenChunk.ScreenWidth;
  FHeight := FLogicalScreenChunk.ScreenHeight;

  If (FWidth < 1) Or (FHeight < 1) Then
  Begin
    // Dimensions incorrectes on lève une exception
    Raise Exception.Create(Format(rsBadScreenSize,[FWidth,FHeight]));
    exit;
  End;
  FHasGlobalPalette := (FLogicalScreenChunk.PackedFields And GIF_GLOBALCOLORTABLE) <> 0;

  Result := True;
End;

Procedure TGIFImageLoader.AddError(Msg : String);
Begin
  FErrorList.Add(Msg);
End;

Procedure TGIFImageLoader.NotifyError;
Begin
  If FErrorList.Count > 0 Then
  Begin
    If Assigned(FOnLoadError) Then FOnLoadError(Self, FErrorList.Count, FErrorList);
  End;
End;

Procedure TGIFImageLoader.LoadFromStream(aStream : TStream);
Begin
  If Memory <> nil Then FreeAndNil(Memory);
  Memory := TGIFFastMemoryStream.Create(aStream);
  If CheckFormat Then LoadFromMemory;
  FreeAndNil(Memory);
End;

Procedure TGIFImageLoader.LoadFromFile(Const FileName : String);
Var
  Stream: TStream;
Begin
  FErrorList.Clear;
  FErrorCOunt := 0;
  Stream      := CreateFileStream(FileName);
  Try
    LoadFromStream(Stream);
  Finally
    FreeAndNil(Stream);
  End;
End;

Procedure TGIFImageLoader.LoadFromResource(Const ResName : String);
Var
  Stream: TLazarusResourceStream;
Begin
  FErrorList.Clear;
  FErrorCOunt := 0;
  Stream      := TLazarusResourceStream.Create(ResName, nil);
  Try
    LoadFromStream(Stream);
  Finally
    FreeAndNil(Stream);
  End;
End;

Function TGIFImageLoader.GetFrameCount: Integer;
Begin
  Result := FFrames.Count;
End;

Procedure TGIFImageLoader.SetCurrentLayerIndex(AValue : Integer);
Begin
  If FCurrentLayerIndex = AValue Then Exit;
  FCurrentLayerIndex := AValue;
End;

Procedure TGIFImageLoader.LoadFromMemory();
Var
  aRGBColor: TColorRGB24;
  aColor: TColor32;
  PaletteCount: Integer;
  Done: Boolean;
  BlockID: Byte;
  BlockSize: Byte;
  Terminator{%H-}: Byte;
  CurrentLayer: TGIFImageListItem;

  ImageDescriptor: TGIFImageDescriptorRec;
  GraphicControlExtensionChunk: TGIFGraphicControlExtensionRec;
  ApplicationExtensionChunk: TGIFApplicationExtensionRec;
  NSLoopExtensionChunk: TGIFNSLoopExtensionRec;
  PlainTextChunk: TGIFPlainTextExtensionRec;

  LocalPalette: TColor32List;
  ColorCount: Integer;
  DMode: Byte;
  ret: TLZWDecoderStatus;

  { Chargement palette globale }
  Procedure LoadGlobalPalette;
  Var
    J: Byte;
  Begin
    If FHasGlobalPalette Then
    Begin
      // Remise à zero de la palette globale si elle existe sinon création de celle-ci
      If FGlobalPalette = nil Then FGlobalPalette := TColor32List.Create
      Else
        FGlobalPalette.Clear;

      PaletteCount := 2 Shl (FLogicalScreenChunk.PackedFields And GIF_COLORTABLESIZE);
      // Le cas ou le nombre de couleurs serait plus grand que 256. On prend en charge.
      If (PaletteCount < 2) Then //or (PaletteCount>256) then
        Raise Exception.Create(rsScreenBadColorSize + ' : ' + IntToStr(PaletteCount));

      // On charge la palette
      For J := 0 To PaletteCount - 1 Do
      Begin
        Memory.Read(aRGBColor, SizeOF(TColorRGB24));
        aColor.Create(aRGBColor);
        FGlobalPalette.AddColor(aColor);
      End;
    End;
  End;

  { Chargement palette locale }
  Procedure LoadLocalPalette;
  Var
    J: Byte;
  Begin
    // Aucune palette locale n'a été assignée. On en créer une nouvelle. Sinon on efface simplement son contenu.
    If LocalPalette = nil Then LocalPalette := TColor32List.Create
    Else
      LocalPalette.Clear;

    // On verifie que le nombre de couleur dans la palette est correcte
    ColorCount := (2 Shl (ImageDescriptor.PackedFields And GIF_COLORTABLESIZE));
    // Le cas ou le nombre de couleurs serait plus grand que 256. On prend en charge qudn même et on charge la palette.
    If (ColorCount < 2) Then //or (ColorCount>256) then
      Raise Exception.Create(rsImageBadColorSize + ' : ' + IntToStr(ColorCount));

    // On charge la palette
    For J := 0 To ColorCount - 1 Do
    Begin
      Memory.Read(aRGBColor, SizeOF(TColorRGB24));
      aColor.Create(aRGBColor);
      LocalPalette.AddColor(aColor);
    End;
  End;

  { Lecture des extensions }
  Procedure ReadExtension;
  Var
    ExtensionID, BlockType: Byte;
    BufStr: Array[0..255] Of Char;
    Loops:  Word;
    CurrentExtension : String;
  Begin
    // On lit les extension jusqu'a ce qu'un bloc de description d'une image soit détecter ou que jusqu'a la fin du fichier
    Repeat
      //showmessage('Read extension at '+ Memory.Position.ToString);
      ExtensionID := Memory.ReadByte;
      CurrentExtension :='';
      // Si c'est un  nouveau marqueur d'introduction d'extension. On lit le nouvel ID
      If (ExtensionID = GIF_EXTENSIONINTRODUCER) Then ExtensionID := Memory.ReadByte;
      If (ExtensionID = 0) Then
      Begin
        // On Saute les ID Nul
        Repeat
          ExtensionID := Memory.ReadByte;
        Until (ExtensionID <> 0);
      End;
      Case ExtensionID Of
        GIF_PLAINTEXT:
        Begin
          BlockSize   := Memory.ReadByte;
          Memory.Read(PlainTextChunk, SizeOf(TGIFPlainTextExtensionRec));
          Repeat
            // On lit la taille du bloc. Si Zero alors fin des données de l'extension
            BlockSize := Memory.ReadByte;
            // On lit la chaine de caractères
            If (BlockSize > 0) Then
            Begin
              fillchar({%H-}BufStr, 256, 0);
              Memory.Read(BufStr, BlockSize);
              BufStr[BlockSize] := #0;
              // On place le texte dans les commentaires
              CurrentLayer.Comment.Add(String(BufStr));
            End;
          Until (BlockSize = 0);
          // On ajoute une ligne vide de séparation
          CurrentLayer.Comment.Add('');
        End;
        GIF_COMMENTEXTENSION:
        Begin
          Repeat
            // On lit la taille du commentaire. Si Zero alors fin des données de l'extension
            BlockSize := Memory.ReadByte;
            // On lit la chaine de caractères
            If (BlockSize > 0) Then
            Begin
              Memory.Read(BufStr, BlockSize);
              BufStr[BlockSize] := #0;
              // On place le texte dans les commentaires
              CurrentLayer.Comment.Add(String(BufStr));
            End;
          Until (BlockSize <= 0);
          // On ajoute une ligne vide de séparation
          CurrentLayer.Comment.Add('');
        End;
        GIF_APPLICATIONEXTENSION:
        Begin

          BlockSize := Memory.ReadByte;
          // Certains vieux filtres d'exportation Adobe, ou d'autres logiciels utilisent par erreur une valeur de 10, ou plus petite ou trop grande
          If (BlockSize <> 11) Then
          Begin
            FillChar(ApplicationExtensionChunk, SizeOf(TGIFApplicationExtensionRec), 0);
          End;
          //else if (BlockSize<11) then
          //   Raise Exception.Create('Bad extension size' + ' : ' + inttostr(BlockSize) +' octets. ( Taille valide = 11 octets )');
          Memory.Read(ApplicationExtensionChunk, SizeOf(TGIFApplicationExtensionRec));
          CurrentExtension := ApplicationExtensionChunk.AppAuthenticationCode;
          Repeat
            // On lit la taille du  bloc. Zero si il n'y a pas de données supplémentaires
            BlockSize := Memory.ReadByte;
            If (BlockSize > 0) Then
            Begin
              if UpperCase(CurrentExtension) = 'NETSCAPE' then
              begin
                BlockType := Memory.ReadByte;
                Dec(BlockSize);
                Case (BlockType And $07) Of
                  GIF_LOOPEXTENSION:
                  Begin
                    // Lecture du nombre de boucle, Si Zero alors boucle infinie
                    Loops := Memory.ReadWord;
                    If Loops > 0 Then Inc(NSLoopExtensionChunk.Loops);
                    Dec(BlockSize, SizeOf(Loops));
                  End;
                  GIF_BUFFEREXTENSION:
                  Begin
                    // Lecture de la taille du tampon. Utilisé pour ??????
                    NSLoopExtensionChunk.BufferSize := Memory.ReadDWord;
                    Dec(BlockSize, SizeOF(NSLoopExtensionChunk.BufferSize));
                  End;
                  else // Extension NETSCAPE inconnue
                    begin
                      Memory.SeekForward(BlockSize);
                      //BlockSize := 0;
                    end;
                End;
              end
              else
              // On saute et on ignore les donnée non lues
              If (BlockSize > 0) Then
              Begin
                Memory.SeekForward(BlockSize);
                //BlockSize := 0;
              End;
            End;
          Until (BlockSize = 0);
        End;
        GIF_GRAPHICCONTROLEXTENSION:
        Begin
          // On lit la taille de l'extension. Normalement 4 Octets. Cette valeur peut-être erronée. On en tient pas compte ici et on lit les données.
          BlockSize := Memory.ReadByte;
          //if BlockSize = 4 then
          //begin
          Memory.Read(GraphicControlExtensionChunk, SizeOf(TGIFGraphicControlExtensionRec));
          // On renseigne notre tampon d'informations pour les prochaines images décodées
          DMode     := ((GraphicControlExtensionChunk.PackedFields And GIF_DISPOSAL_ALL) Shr 2);
          With CurrentFrameInfos Do
          Begin
            // Ces valeurs peuvent être utilisées pour plusieurs image. Elles restent valides jusqu'a la lecture du prochain "GCE" trouvé.
            Disposal      := TGIFDisposalFlag(DMode);
            IsTransparent := (GraphicControlExtensionChunk.PackedFields And GIF_TRANSPARENT_FLAG) <> 0;
            UserInput     := (GraphicControlExtensionChunk.PackedFields And GIF_USER_INPUT_FLAG) <> 0;
            TransparentColorIndex := GraphicControlExtensionChunk.TransparentColorIndex;
            BackgroundColorIndex := FLogicalScreenChunk.BackgroundColorIndex;
            DelayTime     := GraphicControlExtensionChunk.DelayTime;
          End;
          // Lecture de l'octet de fin de l'extension
          Terminator := Memory.ReadByte;
        End;
      End;
    Until (ExtensionID = GIF_IMAGEDESCRIPTOR) Or Memory.EOS;

    // Si l'ID pour la description de l'image est détecter on revient en arrière pour la prise en charge par le traitement des données
    If (ExtensionID = GIF_IMAGEDESCRIPTOR) Then Memory.Seek(-1, soCurrent);
  End;

  { Chargement d'une image }
  Procedure LoadImage;
  Var
    DecoderStatus{%H-}: TLZWDecoderStatus;
    BufferSize, TargetBufferSize, BytesRead: Int64;
    InitCodeSize: Byte;
    OldPosition: Int64;
    Buffer, BufferPtr: PByte;
    TargetBuffer, TargetBufferPtr: PByte;
    LinePtr: PColor32;
    Pass, Increment: Byte;
    x:      Integer;
    TargetColor: TColor32;
    ColIdx: Byte;
    CurrentLine: Integer;
    OutBmp: TFastBitmap;

    // Decodeur GIF LZW. Basé sour le code source de la bibliothèque GraphicEX pour Delphi
    Function DecodeLZW(Var Source, Dest : Pointer; PackedSize, UnpackedSize : Integer): TLZWDecoderStatus;
    Const
      { Constantes pour la décompression LZW }
      _LZWGIFCodeBits  = 12;    // Nombre maximal de bits par code d'un jeton (12 bits = 4095)
      _LZWGIFCodeMax   = 4096; // Nombre maximum de jeton
      _LZWGIFStackSize = (2 Shl _LZWGIFCodeBits);   // Taille de la pile de décompression
      _LZWGIFTableSize = (1 Shl _LZWGIFCodeBits);   // Taille de la table de décompression

    Var
      J:         Integer;
      Data,             // Données actuelle
      Bits,             // Compteur de bit
      Code:      Cardinal;   // Valeur courrante du Code
      SourcePtr: PByte;
      InCode:    Cardinal; // Tampon pour passé le Code

      CodeSize:  Cardinal;
      CodeMask:  Cardinal;
      FreeCode:  Cardinal;
      OldCode:   Cardinal;
      Prefix:    Array[0.._LZWGIFTableSize] Of Cardinal; // LZW prefix
      Suffix,                                         // LZW suffix
      Stack:     Array [0.._LZWGIFStackSize] Of Byte;
      StackPointer: PByte;
      MaxStackPointer: PBYte;
      Target:    PByte;
      FirstChar: Byte;  // Tampon de décodage d'un octet
      ClearCode, EOICode: Word;
      MaxCode:   Boolean;

    Begin
      Result        := dsOk;
      DecoderStatus := dsOk;
      If (PackedSize <= 0) Or (UnpackedSize <= 0) Then
      Begin
        // Taille des tampons invalides
        If (PackedSize <= 0) And (UnpackedSize <= 0) Then Result := dsInvalidBufferSize
        Else If PackedSize <= 0 Then Result   := dsInvalidInputBufferSize
        Else If UnpackedSize <= 0 Then Result := dsInvalidOutputBufferSize;
        Exit;
      End;

      // Initialisation  des paramètres pour la décompression
      CodeSize  := InitCodeSize + 1;
      ClearCode := 1 Shl InitCodeSize;
      EOICode   := ClearCode + 1;
      FreeCode  := ClearCode + 2;
      OldCode   := _LZWGIFCodeMax - 1;
      CodeMask  := (1 Shl CodeSize) - 1;
      MaxCode   := False;
      Code      := 0;
      Target    := PByte(Dest);
      SourcePtr := PByte(Source);

      // Initialisation des tables de Code
      For J := 0 To _LZWGIFTableSize Do
      Begin
        Prefix[J] := _LZWGIFCodeMax;
        Suffix[J] := J;
      End;

      // Initalisation de la pile
      StackPointer    := @Stack;
      MaxStackPointer := @Stack[_LZWGIFStackSize];
      FirstChar       := 0;

      Data := 0;
      Bits := 0;
      While (UnpackedSize > 0) And (PackedSize > 0) Do
      Begin
        // On lit le "Code" dans le tampon d'entrée
        Inc(Data, SourcePtr^ Shl Bits);
        Inc(Bits, 8);
        While (Bits > CodeSize) And (UnpackedSize > 0) Do
        Begin
          // Code actuel
          Code := Data And CodeMask;
          // Préparation pour la donnée suivante
          Data := Data Shr CodeSize;
          Dec(Bits, CodeSize);

          // Décompression finie ?
          If Code = EOICode Then
          Begin
            // Si nous arrivons ici, il y a probablement quelque chose de suspect avec l'image GIF
            // Car normalement on stoppe dès que le tampon de sortie est plein.
            // Cela signifie que nous ne lirons jamais l'EOICode de fermeture dans les images normales.
            // Comme l'état du buffer est déjà vérifié après la boucle principale, nous ne le ferons pas ici.
            Break;
          End;

          // On vérifie s'il s'agit d'un code valide déjà enregistré
          If Code > FreeCode Then
          Begin
            // Code ne peux à être supérieur à FreeCode. Nous avons donc une image cassée.
            // On notifie l'erreur à l'utilisateur. Et on considère qu'il n'ya pas d'erreur.
            DecoderStatus := dsInvalidInput;
            AddError(Format(rsLZWInvalidInput,[CurrentFrameIndex]));
            //NotifyUser('Le décodeur a rencontré une entrée invalide (données corrompues)');
            Code :=  ClearCode;
            //Break; //Ici, on continue le chargement du reste de l'image au lieu de le stopper
          End;

          // RAZ
          If Code = ClearCode Then
          Begin
            // réinitialisation de toutes les variables
            CodeSize := InitCodeSize + 1;
            CodeMask := (1 Shl CodeSize) - 1; //CodeMasks[CodeSize];
            FreeCode := ClearCode + 2;
            OldCode  := _LZWGIFCodeMax;
            MaxCode  := False;
          End
          Else If OldCode = _LZWGIFCodeMax Then
          Begin
            // Gestion du premier Code LZW : On le définit dans le tampon de sortie et on le conserve
            FirstChar := Suffix[Code];
            Target^   := FirstChar;
            Inc(Target);
            Dec(UnpackedSize);
            OldCode   := Code;
          End
          Else
          Begin
            //On conserve le Code LZW actuel
            InCode := Code;

            // On place le nouveau code LZW sur la pile sauf quand nous avons déjà utilisé tous les codes disponibles
            If (Code = FreeCode) And Not MaxCode Then
            Begin
              StackPointer^ := FirstChar;
              Inc(StackPointer);
              Code := OldCode;
            End;

            // boucle pour placer les octets décodés sur la pile
            While Code > ClearCode Do
            Begin
              StackPointer^ := Suffix[Code];
              If StackPointer >= MaxStackPointer Then
              Begin
                // Ne doit jamais arriver, c'est juste une précaution au cas ou.
                Result := dsBufferOverflow;
                break;
              End;
              Inc(StackPointer);
              Code := Prefix[Code];
            End;
            If Result <> dsOK Then break; // Si il ya eu des erreurs on ne va pas plus loin

            // Place le nouveau Code dans la table
            FirstChar     := Suffix[Code];
            StackPointer^ := FirstChar;
            Inc(StackPointer);

            //Transfert des données décodées vers notre tampon de sortie
            Repeat
              If UnpackedSize <= 0 Then
              Begin
                // Le tampon de sortie est trop petit. On ne va pas plus loin
                // On notifie l'erreur à l'utilisateur. Et on considère qu'il n'ya pas d'erreur.
                // Afin de pouvoir afficher le GIF et continuer le chargement des images suivantes
                Result := dsOutputBufferTooSmall;
                AddError(Format(rsLZWOutputBufferTooSmall,[CurrentFrameIndex]));
                break;
              End;
              Dec(StackPointer);
              Target^ := StackPointer^;
              Inc(Target);
              Dec(UnpackedSize);
            Until StackPointer = @Stack;
            If Result <> dsOK Then break;

            If Not MaxCode Then
            Begin
              If FreeCode <= _LZWGIFCodeMax Then
              Begin
                Prefix[FreeCode] := OldCode;
                Suffix[FreeCode] := FirstChar;
              End
              Else If FreeCode > _LZWGIFCodeMax Then
              Begin
                // On a intercepter une donnée corrompue. On continue quand la même décompression sans en tenir compte.
                // On notifie juste l'erreur à l'utilisateur
                DecoderStatus := dsInvalidInput;
                AddError(Format(rsLZWInvalidInput,[CurrentFrameIndex]));
                FreeCode :=  _LZWGIFCodeMax;
                Prefix[FreeCode] := OldCode;
                Suffix[FreeCode] := FirstChar;
                //MaxCode       := True;
              End;

              // On augmente la taille du Code si nécessaire
              If (FreeCode = CodeMask) And Not (MaxCode) Then
              Begin
                If (CodeSize < _LZWGIFCodeBits) Then
                Begin
                  Inc(CodeSize);
                  CodeMask := (1 Shl CodeSize) - 1;//CodeMasks[CodeSize];
                End
                Else //On a atteind la limite maximum
                  MaxCode  := True;
              End;

              If FreeCode < _LZWGIFTableSize Then Inc(FreeCode);
            End;
            OldCode := InCode;
          End;
        End;
        Inc(SourcePtr);
        Dec(PackedSize);
        If (Result <> dsOK) Or (Code = EOICode) Then Break;
      End;

      If Result = dsOK Then
      Begin
        // On vérifie seulement si il n'ya pas eu d'erreur. Si ce n'est pas le cas, nous savons déjà que quelque chose ne va pas.
        // Notez qu'il est normal que PackedSize soit un peu> 0 parce que nous pouvons
        // pas lire l'EOICode mais arrêter dès que notre tampon de sortie est plein et
        // qui devrait normalement être le code juste avant l'EOICode.
        If PackedSize < 0 Then
        Begin
          Result := dsInternalError;
          // C'est une erreur sérieuse : nous avons eu un dépassement de tampon d'entrée que nous aurions dû intercepter. Nous devons arrêter maintenant.
          Raise Exception.Create(rsLZWInternalErrorInputBufferOverflow);
          Exit;
        End;
        If UnpackedSize <> 0 Then
        Begin
          //if UnpackedSize > 0 then
          //begin
          //  //  Image corrompue
          //  DecoderStatus := dsNotEnoughInput;
          //  AddError('Image #'+CurrentFrameIndex)+' : Le décodeur n''a pas pu décoder toutes les données car le tampon d''entrée est trop petit');
          //  //NotifyUser('Le décodeur  n''a pas pu décoder toutes les données car le tampon d''entrée est trop petit');
          //End
          //else
          If UnpackedSize < 0 Then
          Begin
            Result := dsInternalError;
            // C'est une erreur sérieuse : nous avons eu un dépassement de tampon de sortie que nous aurions dû intercepter. Nous devons arrêter maintenant.
            Raise Exception.Create(rsLZWInternalErrorOutputBufferOverFlow);
          End;
        End;
      End;
    End;

  Begin
    BufferSize       := 0;
    TargetBufferSize := 0;

    // On lit la description de l'image
    Memory.Read(ImageDescriptor, SizeOf(TGIFImageDescriptorRec));

    // On vérifie que les dimensions sont correctes.
    // Si on trouve des dimensions à zero, il se peut qu'il faudra traiter
    // une extension PlainText et dessiner ce texte en fonction des paramètres
    If (ImageDescriptor.Height = 0) Or (ImageDescriptor.Width = 0) Then
    Begin
      // On assigne les dimensions par défaut du GIF
      ImageDescriptor.Width  := FLogicalScreenChunk.ScreenWidth;
      ImageDescriptor.Height := FLogicalScreenChunk.ScreenHeight;
      // On notifie à l'utilisateur que les dimensions de l'image sont erronée. Mais on tente le chargement quand même
      // ShowMessage
    End;

    // Dans le cas ou les dimensions de l'image sont incorrectes dans "l'image descriptor". Ou que la taille des données compressées soit erronée.
    If (ImageDescriptor.Width > FLogicalScreenChunk.ScreenWidth) Or (ImageDescriptor.Height > FLogicalScreenChunk.ScreenHeight) Then
    Begin
      // On assigne les dimensions par défaut du GIF
      If (ImageDescriptor.Width > FLogicalScreenChunk.ScreenWidth) Then ImageDescriptor.Width := FLogicalScreenChunk.ScreenWidth;
      If (ImageDescriptor.Height > FLogicalScreenChunk.ScreenHeight) Then ImageDescriptor.Height := FLogicalScreenChunk.ScreenHeight;
      // On notifie à l'utilisateur que les dimensions de l'image sont erronée. Mais on tente le chargement quand même
      // ShowMessage
    End;

    // On renseigne notre tampon d'informations
    With CurrentFrameInfos Do
    Begin
      Left       := ImageDescriptor.Left;
      Top        := ImageDescriptor.Top;
      Width      := ImageDescriptor.Width;
      Height     := ImageDescriptor.Height;
      Interlaced := (ImageDescriptor.PackedFields And GIF_INTERLACED) = GIF_INTERLACED;
      HasLocalPalette := (ImageDescriptor.PackedFields And GIF_LOCALCOLORTABLE) = GIF_LOCALCOLORTABLE;
    End;

    // L'image possède-t-elle sa propre palette de couleur ? Si oui on la charge.
    If CurrentFrameInfos.HasLocalPalette Then LoadLocalPalette;

    // Decompression de l'image

    // On ajoute une nouvelle image si besoin
    If (FCurrentLayerIndex > 0) And (FCurrentLayerIndex > FFrames.Count - 1) Then CurrentLayer := FFrames.AddNewImage;
    // On assigne la nouvelle image au Bitmap de travail
    OutBmp := FFrames.Items[CurrentFrameIndex].Bitmap;

    // On met à jour les informations
    With FFrames.Items[FCurrentLayerIndex] Do
    Begin
      Drawmode := CurrentFrameInfos.Disposal;
     // Showmessage('#'+inttostr(FCurrentLayerIndex) + 'DrawMode : '+ GifGCEDisposalModeStr[Drawmode]);
      Left     := CurrentFrameInfos.Left;
      Top      := CurrentFrameInfos.Top;
      IsTransparent := CurrentFrameInfos.IsTransparent;
      If CurrentFrameInfos.DelayTime = 0 Then Delay := GIF_DefaultDelay
      Else
        Delay := CurrentFrameInfos.DelayTime * GIF_DelayFactor;
    End;

    // On lit le code d'initalisation de la compression LZW
    InitCodeSize := Memory.ReadByte;
    If InitCodeSize < 2 Then InitCodeSize := 2;
    If InitCodeSize > 8 Then InitCodeSize := 8;

    // On sauve la position actuelle dans le flux
    OldPosition := Memory.position;

    BufferSize := 0;

    // 1) On comptabilise la taille totale des données compresser. Afin de les décompresser en une seule fois.
    // On lit la taille du premier bloc
    BlockSize := Memory.ReadByte;
    While (BlockSize > 0) And Not (Memory.EOS) Do
    Begin
      Inc(BufferSize, BlockSize);
      // On saute les données
      Memory.SeekForward(BlockSize);
      If Not (Memory.EOS) Then BlockSize := Memory.ReadByte
      Else
        blocksize := 0;
    End;

    // 2) On initalise notre bitmap avec les bonnes dimensions
    OutBmp.SetSize(CurrentFrameInfos.Width, CurrentFrameInfos.Height);

    BufferPtr := nil;
    Buffer    := nil;
    // 3) On alloue notre tampon pour les données compressées
    If (BufferSize > 0) Then Reallocmem(Buffer, BufferSize);

    // 4) On charge toutes les données dans notre tampon
    // On se replace au début des données
    Memory.Seek(OldPosition, soBeginning);
    // On travail toujours sur une copie du "pointer"
    BufferPtr := Buffer;
    // On lit la taille du premier bloque
    BlockSize := Memory.ReadByte;
    While (BlockSize > 0) And Not (Memory.EOS) Do
    Begin
      // On charge les données dans le tampon. On previent des erreurs en cas de dépassements
      BytesRead := Memory.Read(BufferPtr^, BlockSize);
      Inc(BufferPtr, BytesRead);
      If Not (Memory.EOS) Then BlockSize := Memory.ReadByte
      Else
        blocksize := 0;
    End;
    // On se replace au debut du tampon
    BufferPtr := Buffer;
    // 5) On decompresse les données
    //  On initialise notre buffer ou seront décompressées les données
    TargetBufferSize := Int64(CurrentFrameInfos.Width) * Int64(CurrentFrameInfos.Height);
    TargetBufferPtr  := nil;
    TargetBuffer     := nil;
    // Si la taille est plus grande que zero, on alloue l'espace nécessaire à notre tampon
    If (TargetBufferSize > 0) Then Reallocmem(TargetBuffer, TargetBufferSize);

    // Décodage des données compressées
    Ret := DecodeLZW(Buffer, TargetBuffer, BufferSize, TargetBufferSize);

    // 6) On transfert les données de l'image vers notre bitmap. Si il n'y a pas eu d'erreurs
    If (Ret = dsOk) Then
    Begin
      TargetBufferPtr := TargetBuffer;
      OutBmp.Clear(clrTransparent);

      // Image non entrelacée
      If Not (CurrentFrameInfos.Interlaced) Then
      Begin
        CurrentLine := 0;
        While (CurrentLine <= CurrentFrameInfos.Height - 1) Do
        Begin
          LinePtr  := OutBmp.GetScanLine(CurrentLine);// FFrames.Items[CurrentFrameIndex].Bitmap.GetScanLine(CurrentLine);
          For x    := 0 To (CurrentFrameInfos.Width - 1) Do
          Begin
            // Lecture de l'index de la couleur dans la palette
            ColIdx := TargetBufferPtr^;
            // On utilise la palette de couleur locale
            If CurrentFrameInfos.HasLocalPalette Then
            Begin
              If LocalPalette <> nil Then // La palette est-elle chargée ?
              Begin
                //if (ColIdx> ColorCount-1) then ColIdx := ColorCount -1;
                If (ColIdx < ColorCount) Then TargetColor := LocalPalette.Colors[ColIdx].Value
                Else
                  TargetColor := clrTransparent;
              End
              Else If FGlobalPalette <> nil Then // Non, alors on utilise la palette globale si elle est présente
              Begin
                //if (ColIdx> PaletteCount-1) then ColIdx := PaletteCount -1;
                If (ColIdx < PaletteCount) Then TargetColor := FGlobalPalette.Colors[ColIdx].Value
                Else
                  TargetColor := clrTransparent;
              End
              Else
              Begin
                AddError(rsEmptyColorMap);
                Exit;
              End;
            End
            Else // On utilise la palette de couleur globale
            Begin
              If FGlobalPalette <> nil Then
              Begin
                //if (ColIdx> PaletteCount-1) then ColIdx := PaletteCount -1;
                If (ColIdx < PaletteCount) Then TargetColor := FGlobalPalette.Colors[ColIdx].Value
                Else
                  TargetColor := clrTransparent;
              End
              Else If LocalPalette <> nil Then
              Begin
                //if (ColIdx> ColorCount-1) then ColIdx := ColorCount -1;
                If (ColIdx > ColorCount - 1) Then //ColIdx := ColorCount -1;
                  TargetColor := LocalPalette.Colors[ColIdx].Value
                Else
                  TargetColor := clrTransparent;
              End
              Else
              Begin
                AddError(rsEmptyColorMap);
                Exit;
              End;
            End;

            If CurrentFrameInfos.IsTransparent Then
            Begin
              If FHasGlobalPalette Then If ColIdx < FGlobalPalette.Count Then OutBmp.TransparentColor := FGlobalPalette.Colors[ColIdx].Value.ToColor
                Else If ColIdx < LocalPalette.Count Then OutBmp.TransparentColor := LocalPalette.Colors[ColIdx].Value.ToColor;

              If (Self.FTransparent) Then
              Begin
                If (ColIdx = CurrentFrameInfos.TransparentColorIndex) Then
                begin
                  TargetColor.Alpha := 0; // clrTransparent;
                end;
                If (CurrentFrameInfos.TransparentColorIndex = CurrentFrameInfos.BackgroundColorIndex) Then FbackgroundColor.Alpha := 0; //clrTransparent;
              End;
            End;
            LinePtr^ := TargetColor;
            // On avance de 1 élément dans nos "pointer"
            Inc(TargetBufferPtr);
            Inc(LinePtr);
          End;
          Inc(CurrentLine);
        End;
      End
      Else // Image entrelacée
      Begin
      CurrentLine := 0;
      For pass    := 0 To 3 Do
      Begin
        Case Pass Of
          0:
          Begin
            CurrentLine := 0;
            Increment   := 8;
          End;
          1:
          Begin
            CurrentLine := 4;
            Increment   := 8;
          End;
          2:
          Begin
            CurrentLine := 2;
            Increment   := 4;
          End;
          Else
          Begin
            CurrentLine := 1;
            Increment   := 2;
          End;
        End;
        While (CurrentLine < CurrentFrameInfos.Height) Do
        Begin
          LinePtr  :=OutBmp.GetScanLine(CurrentLine); // FFrames.Items[CurrentFrameIndex].Bitmap
          For x    := 0 To (FFrames.Items[CurrentFrameIndex].Bitmap.Width - 1) Do
          Begin
            // Lecture de l'index de la couleur dans la palette
            ColIdx := TargetBufferPtr^;
            // On utilise la palette de couleur locale
            If CurrentFrameInfos.HasLocalPalette Then
            Begin
              If LocalPalette <> nil Then // La palette est-elle chargée ?
              Begin
                If (ColIdx < ColorCount) Then // Dans le cas contraire il s'agit d'un index pour la transparence
                  TargetColor := LocalPalette.Colors[ColIdx].Value;
              End
              Else If FGlobalPalette <> nil Then // Non, alors on utilise la palette globale si elle est présente
              Begin
                If (ColIdx < PaletteCount) Then //if (ColIdx< PaletteCount-1) then ColIdx := PaletteCount -1;
                  TargetColor := FGlobalPalette.Colors[ColIdx].Value;
              End
              Else
              Begin
                AddError(rsEmptyColorMap);
                Exit;
              End;
            End
            Else // On utilise la palette de couleur globale
            Begin
              If FGlobalPalette <> nil Then
              Begin
                If (ColIdx > PaletteCount - 1) Then ColIdx := PaletteCount - 1;
                TargetColor := FGlobalPalette.Colors[ColIdx].Value;
              End
              Else If LocalPalette <> nil Then
              Begin
                If (ColIdx > ColorCount - 1) Then ColIdx := ColorCount - 1;
                TargetColor := LocalPalette.Colors[ColIdx].Value;
              End
              Else
              Begin
                AddError(rsEmptyColorMap);
                Exit;
              End;
            End;

            If CurrentFrameInfos.IsTransparent Then
            Begin
              If FHasGlobalPalette Then If ColIdx < FGlobalPalette.Count Then OutBmp.TransparentColor := FGlobalPalette.Colors[ColIdx].Value.ToColor
                Else If ColIdx < LocalPalette.Count Then OutBmp.TransparentColor := LocalPalette.Colors[ColIdx].Value.ToColor;
              If (FTransparent) Then
              Begin
                If CurrentFrameInfos.TransparentColorIndex = colIdx Then
                begin
                   TargetColor.Alpha := 0; // := clrTransparent;
                End;
                If (CurrentFrameInfos.TransparentColorIndex = CurrentFrameInfos.BackgroundColorIndex) Then FBackgroundColor.Alpha := 0;
              End;
            End;

            LinePtr^ := TargetColor;
            Inc(TargetBufferPtr);
            If (CurrentLine < CurrentFrameInfos.Height - 1) Then Inc(LinePtr);
          End;
          Inc(CurrentLine, Increment);
        End;
      End;
    End;
      if DecoderStatus <> dsOk then
      begin
        //outBmp.Clear(ClrTransparent);
        FFrames.Items[FCurrentLayerIndex].IsCorrupted := True;
        FFrames.Items[FCurrentLayerIndex].Delay:= 1;
      End;
      Inc(FCurrentLayerIndex);   // Index pour la prochaine image
    End
    Else
    Begin
      Case Ret Of
        dsInvalidBufferSize: AddError(Format(rsInvalidBufferSize,[CurrentFrameIndex]));
        dsInvalidInputBufferSize: AddError(Format(rsInvalidInputBufferSize,[CurrentFrameIndex]));
        dsInvalidOutputBufferSize: AddError(Format(rsInvalidOutputBufferSize,[CurrentFrameIndex]));
        dsBufferOverflow: AddError(Format(rsBufferOverFlow,[CurrentFrameIndex]));
        dsOutputBufferTooSmall :
         (* begin
            // On supprime l'image. Le tampon de sortie étant trop petit, cela va générer des erreurs lors du transfert des données décompressées vers l'image
            //FFrames.Delete(CurrentFrameIndex);

          end;*)
          dec(FCurrentLayerIndex);

      End;
      if Ret<>dsOutputBufferTooSmall then
      begin
        FFrames.Items[FCurrentLayerIndex].IsCorrupted := True;
        FFrames.Items[FCurrentLayerIndex].Delay:= 1;
      end;
    End;

    // On libére la mémoire allouée pour nos tampons
    If (TargetBufferSize > 0) And (targetBuffer <> nil) Then FreeMem(TargetBuffer);
    If (BufferSize > 0) And (Buffer <> nil) Then FreeMem(Buffer);
  End;

Begin
  PaletteCount := 0;
  ColorCount   := 0;
  LocalPalette := nil;
  FFrames.Clear;

  // Par defaut, on considère que la couleur de fond est totalement transparente
  FBackgroundColor := clrTransparent;
  // Si une palette globale existe, alors on charge
  LoadGlobalPalette;
  If FHasGlobalPalette Then
  Begin
    If FLogicalScreenChunk.BackgroundColorIndex < PaletteCount - 1 Then FBackgroundColor := FGlobalPalette.Colors[FLogicalScreenChunk.BackgroundColorIndex].Value
    Else
    Begin
      FBackgroundColor := clrTransparent; //FGlobalPalette.Colors[FLogicalScreenChunk.BackgroundColorIndex].Value;
    End;
  End;

  // Les valeurs suivante seront renseignées lors du chargement d'une image
  // On réinitialise juste les valeurs par défaut des informations de l'image en cours au cas ou il n'y aurait pas de GCE
  With CurrentFrameInfos Do
  Begin
    Left       := 0;
    Top        := 0;
    Width      := FLogicalScreenChunk.ScreenWidth;
    Height     := FLogicalScreenChunk.ScreenHeight;
    Interlaced := False;
    HasLocalPalette := False;
    IsTransparent := False;
  End;
  // On ajoute l'image de départ afin de pouvoir assigner les valeurs des premières extensions (Extensions déclarées avant l'image)
  CurrentLayer := FFrames.AddNewImage;
  // On efface l'image avec la couleur de fond
  //CurrentLayer.Bitmap.Clear(FBackgroundColor);
  FCurrentLayerIndex := 0;
  // On lit le 1er octet
  Done := False;
  While Not (Done) Do
  Begin
    // On verifie l'existence d'extensions avant les données de l'image (Application, Graphic Control, PlainText, Comment)
    If Not (Memory.EOS) Then BlockID := Memory.ReadByte
    Else
      BlockID := GIF_Trailer;
    If (BlockID = GIF_Trailer) Then
    Begin
      Done := True;
    End;
    If (BlockID = 0) Then
    Begin
      // On Saute les ID Nul
      While (BlockId = 0) Do BlockId := Memory.ReadByte;
    End
    Else If (BlockID = GIF_IMAGEDESCRIPTOR) Then  // C'est une image
    Begin
      // On charge l'image
      LoadImage;
    End
    Else If (BlockID = GIF_EXTENSIONINTRODUCER) Then // c'est une extension
    Begin
      ReadExtension; // On charge toutes les extensions qui sont à la suite
    End
    Else
    Begin
      // Extension inconnue on saute jusqu'a trouver un ZERO.
      // A Verifier avec le flag UseInput dans le "Graphic Control Extension"
      // Ici on ignore simplement les données
      While BlockID <> 0 Do
      Begin
        BlockID := Memory.ReadByte;
      End;
    End;
  End;
  // Si il y a des erreurs elles seront notifier à l'utilisateur
  NotifyError;

  // Il n'y a aucune images on notifie l'erreur
  If FFrames.Count = 0 Then Raise Exception.Create(rsEmptyImage);

  // On libere la mémoire, prise par nos palettes de couleurs si besoin
  If (LocalPalette <> nil) Then
  Begin
    FreeAndNil(LocalPalette);
  End;
  If (FGlobalPalette <> nil) Then
  Begin
    FreeAndNil(FGlobalPalette);
  End;
End;

{%endregion%}

{%region=====[ TGIFRenderCacheListItem ]========================================}

Constructor TGIFRenderCacheListItem.Create;
Begin
  Inherited Create;
  FBitmap := Graphics.TBitmap.Create;
  FDelay  := 0;
End;

Destructor TGIFRenderCacheListItem.Destroy;
Begin
  FreeAndNil(FBitmap);
  Inherited Destroy;
End;

{%endregion%}

{%region=====[ TGIFRenderCacheList ]============================================}

Function TGIFRenderCacheList.GetItems(Index : Integer): TGIFRenderCacheListItem;
Begin
  Result := TGIFRenderCacheListItem(Inherited Items[Index]);
End;

Procedure TGIFRenderCacheList.SetItems(Index : Integer; AGIFRenderCache : TGIFRenderCacheListItem);
Begin
  Put(Index, AGIFRenderCache);
End;

Procedure TGIFRenderCacheList.Clear;
Var
  anItem: TGIFRenderCacheListItem;
  i:      Integer;
Begin
  If Count > 0 Then
  Begin
    For i := Count - 1 Downto 0 do
    Begin
      AnItem := Items[i];
      If anItem <> nil Then anItem.Free;
    End;
  End;
  Inherited Clear;
End;

Function TGIFRenderCacheList.AddNewCache: TGIFRenderCacheListItem;
Var
  anItem: TGIFRenderCacheListItem;
Begin
  anitem := TGIFRenderCacheListItem.Create;
  Add(anItem);
  Result := Items[Self.Count - 1];
End;

Function TGIFRenderCacheList.Add(AGIFRenderCache : TGIFRenderCacheListItem): Integer;
Begin
  Result := Inherited Add(AGIFRenderCache);
End;

Function TGIFRenderCacheList.Extract(Item : TGIFRenderCacheListItem): TGIFRenderCacheListItem;
Begin
  Result := TGIFRenderCacheListItem(Inherited Extract(Item));
End;

Function TGIFRenderCacheList.Remove(AGIFRenderCache : TGIFRenderCacheListItem): Integer;
Begin
  Result := Inherited Remove(AGIFRenderCache);
End;

Function TGIFRenderCacheList.IndexOf(AGIFRenderCache : TGIFRenderCacheListItem): Integer;
Begin
  Result := Inherited IndexOf(AGIFRenderCache);
End;

Function TGIFRenderCacheList.First: TGIFRenderCacheListItem;
Begin
  Result := TGIFRenderCacheListItem(Inherited First);
End;

Function TGIFRenderCacheList.Last: TGIFRenderCacheListItem;
Begin
  Result := TGIFRenderCacheListItem(Inherited Last);
End;

Procedure TGIFRenderCacheList.Insert(Index : Integer; AGIFRenderCache : TGIFRenderCacheListItem);
Begin
  Inherited Insert(Index, AGIFRenderCache);
End;

Function TGIFRenderCacheList.IsIndexOk(anIndex: Integer): Boolean;
Begin
  Result := True;
  If (anIndex < 0) or (anIndex > Count-1) then result := False;
End;

Procedure TGIFRenderCacheList.Pack;
Var
  i: Integer;
Begin
  if Count>1 then
  begin
    I := 0;
    While I<Count do
    begin
      if Items[I].IsCorrupted then
      begin
        Remove(Items[I]);
        break;
      End;
      inc(I);
    End;
    if I<Count then Pack;
  End;
End;

{%endregion%}

{%region=====[ TGIFViewer ]=====================================================}

Constructor TGIFViewer.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize     := False;
  FCenter      := False;
  FStretch     := False;
  FTransparent := True;
  With GetControlClassDefaultSize Do SetInitialBounds(0, 0, CX, CY);
  FRestoreBitmap := nil;
  FRenderCache   := TGIFRenderCacheList.Create(False);
  FGIFLoader     := TGIFImageLoader.Create;
  FGIFLoader.OnLoadError := @DoInternalOnLoadError;
  FVirtualView   := TFastBitmap.Create;
  FCurrentView   := nil;
  FCurrentView   := Graphics.TBitmap.Create;
  FRestoreBitmap := nil;
  FAutoPlay      := False;
  FBorderShow    := False;
  FBorderColor   := clBlack;
  FBorderWidth   := 1;
  FBevelInner    := bvNone;
  FBevelOuter    := bvNone;
  FBevelWidth    := 1;
  FColor         := clNone;
  FDisplayInvalidFrames := False;
  FAutoRemoveInvalidFrame := True;
  FLastDrawMode  := dmNone;

  FAnimateTimer := TTimer.Create(nil);
  With FAnimateTimer Do
  Begin
    Enabled  := False;
    Interval := 1000;
    OnTimer  := @DoTimerAnimate;
  End;
  FAnimateSpeed := 1;
  FCurrentFrameIndex := 0;
  FGIFWidth  := 90;
  FGIFHeight := 90;
  FAutoStretchMode := smManual;
End;

Destructor TGIFViewer.Destroy;
Begin
  FAnimateTimer.Enabled := False;

  FreeAndNil(FAnimateTimer);
  If FCurrentView <> nil Then FreeAndNil(FCurrentView);
  If FRestoreBitmap <> nil Then FreeAndNil(FRestoreBitmap);
  FreeAndNil(FVirtualView);
  FRenderCache.Clear;
  FreeAndNil(FRenderCache);
  FreeAndNil(FGIFLoader);

  Inherited Destroy;
End;

Procedure TGIFViewer.SetCenter(Const Value: Boolean);
Begin
  If Value = FCenter Then exit;
  FCenter := Value;
  Invalidate;
End;

Function TGIFViewer.GetCanvas: TCanvas;
Begin
  Result := Inherited Canvas;// FCurrentView.Canvas
End;

Function TGIFViewer.GetFrameCount: Integer;
Begin
  If FCache Then
    Result := FRenderCache.Count
  Else Begin
    Result := FGifLoader.FrameCount;
  End;
End;

Function TGIFViewer.GetGIFVersion: String;
Begin
  Result := FGIFLoader.Version;
End;

Function TGIFViewer.GetRawFrameItem(Index : Integer): TGIFImageListItem;
Begin
  Result := nil;
  If (Index >= 0) And (Index < FGIFLoader.FrameCount) Then Result := FGIFLoader.Frames[Index];
end;

Procedure TGIFViewer.SetAutoStretchMode(AValue: TGIFAutoStretchMode);
Begin
  If FAutoStretchMode = AValue Then Exit;
  FAutoStretchMode := AValue;
  Invalidate;
End;

Procedure TGIFViewer.SetStretch(Const Value: Boolean);
Begin
  If Value = FStretch Then exit;
  FStretch := Value;
  Invalidate;
End;

Procedure TGIFViewer.SetPause(Const Value: Boolean);
Begin
  If Value = FPause Then exit;
  FPause := Value;
  If FPause Then FAnimateTimer.Enabled := False;
  If Assigned(FOnPause) Then FOnPause(Self);
End;

Procedure TGIFViewer.SetFileName(Const Value: String);
Begin
  If Value = FFileName Then exit;
  FFileName := Value;
  LoadFromFile(FFileName);
End;

Function TGIFViewer.GetFrame(Const Index: Integer): Graphics.TBitmap;
Begin
  Result := nil;
  If (Index >= 0) And (Index < FrameCount) Then Result := FRenderCache.Items[Index].Bitmap;
End;

Procedure TGIFViewer.SetTransparent(Const Value: Boolean);
Begin
  If FTransparent = Value Then exit;
  FTransparent := Value;
  FGIFLoader.Transparent := Value;
  If FFileName <> '' Then LoadFromFile(FFileName);
End;

Procedure TGIFViewer.SetBevelWidth(Const Value: TBevelWidth);
Begin
  If FBevelWidth <> Value Then
  Begin
    FBevelWidth := Value;
    Invalidate;
  End;
End;

Procedure TGIFViewer.ResetCurrentView;
Var
  I: Integer;
  Corrupted : Boolean;
begin
  if FRenderCache.Count>1 then
  begin
    if not(FDisplayInvalidFrames) then
    begin
      Corrupted := false;
      i := 0;
      Repeat
        Corrupted := FRenderCache.Items[i].IsCorrupted;
        inc(i);
      until (i>FRenderCache.Count-1) or (Corrupted = false);
      if (i>FRenderCache.Count-1) and (Corrupted = true) then
      begin
        Raise Exception.Create(rsAllFrameCorrupted);
        exit;
      end
      else
      begin
        Dec(i);
        FCurrentframeIndex     := i;
        FAnimateTimer.Interval := FRenderCache.Items[i].Delay;
        FCurrentView.Assign(FRenderCache.Items[i].Bitmap);
      end;
    end
    else
    begin
      FAnimateTimer.Interval := FRenderCache.Items[0].Delay;
      FCurrentView.Assign(FRenderCache.Items[0].Bitmap);
    end;
  end
  else
  begin
    FCurrentView.Assign(FRenderCache.Items[0].Bitmap);
  end;
  FLastDrawMode := dmNone;
End;

Procedure TGIFViewer.SetBevelInner(Const Value: TPanelBevel);
Begin
  If BevelInner <> Value Then
  Begin
    FBevelInner := Value;
    Invalidate;
  End;
End;

Procedure TGIFViewer.SetBevelOuter(Const Value: TPanelBevel);
Begin
  If BevelOuter <> Value Then
  Begin
    FBevelOuter := Value;
    Invalidate;
  End;
End;

Procedure TGIFViewer.DoInternalOnLoadError(Sender: TObject; Const ErrorCount: Integer; Const ErrorList: TStringList);
Begin
  If Assigned(FOnLoadError) Then FOnloadError(Self, ErrorCount, ErrorList);
End;

Procedure TGIFViewer.DoTimerAnimate(Sender: TObject);
Begin
  Inc(FCurrentFrameIndex);
  If FCurrentFrameIndex > (FGIFLoader.FrameCount - 1) Then FCurrentFrameIndex := 0;

  If (not FCache) and (FCurrentFrameIndex >= FRenderCache.Count) Then
  Begin
    RenderFrame(FCurrentFrameIndex);
  End;

  If Assigned(FOnFrameChange) Then FOnFrameChange(Self);
  if not(FDisplayInvalidFrames) then
  begin
    if not(FRenderCache.Items[FCurrentFrameIndex].IsCorrupted) then
    begin
      FAnimateTimer.Interval := FRenderCache.Items[FCurrentFrameIndex].Delay;
      FCurrentView.Assign(FRenderCache.Items[FCurrentFrameIndex].Bitmap);
    End
    else FAnimateTimer.Interval := FRenderCache.Items[FCurrentFrameIndex].Delay;
  end
  else
  begin
    FAnimateTimer.Interval := FRenderCache.Items[FCurrentFrameIndex].Delay;
    FCurrentView.Assign(FRenderCache.Items[FCurrentFrameIndex].Bitmap);
  end;
  Invalidate;
End;

Procedure TGIFViewer.RenderFrame(Index: Integer);
Var
  Src:         TFastBitmap;
  pTop, pLeft: Integer;
  iDrawMode:   TFastBitmapDrawMode;
  TmpBmp : Graphics.TBitmap;
Begin

  Src   := FGIFLoader.Frames.Items[Index].Bitmap;
  pLeft := FGIFLoader.Frames.Items[Index].Left;
  pTop  := FGIFLoader.Frames.Items[Index].Top;

  FRenderCache.AddNewCache;
  FRenderCache.Items[Index].Delay  := FGIFLoader.Frames[Index].Delay * FAnimateSpeed;
  FRenderCache.Items[Index].IsCorrupted  := FGIFLoader.Frames[Index].IsCorrupted;

  If (FTransparent) Then
  Begin
    iDrawMode := dmAlphaCheck;
  End
  Else
  Begin
    iDrawMode := dmSet;
  End;

  If Index = 0 Then
  Begin
    If (FTransparent) Then
    Begin
      FVirtualView.Clear(clrTransparent);
    End
    Else
    Begin
      FVirtualView.Clear(FGIFLoader.BackgroundColor);
    End;
    FVirtualView.PutImage(Src, 0, 0, Src.Width, Src.Height, pLeft, pTop, dmSet);
    if FGIFLoader.Frames.Items[0].DrawMode = dmKeep then begin
      if Assigned( FRestoreBitmap) then begin
        FRestoreBitmap.Free;
      end;
      FRestoreBitmap := FVirtualView.Clone;
    end;
  End
  Else
  Begin

    With FGIFLoader.Frames.Items[Index] Do
    Begin
      Case DrawMode Of
        dmNone:
        Begin
          FVirtualView.PutImage(Src, 0, 0, Src.Width, Src.Height, pLeft, pTop, iDrawMode);
        End;
        dmKeep:
        Begin
          if FLastDrawMode = dmErase then
          begin
            If (FGIFLoader.Frames.Items[Index].IsTransparent And FTransparent) Then FVirtualView.Clear(clrTransparent)
            Else
              FVirtualView.Clear(FGIFLoader.BackgroundColor);
          end;
          FVirtualView.PutImage(Src, 0, 0, Src.Width, Src.Height, pLeft, pTop, iDrawMode);
          If Assigned(FRestoreBitmap) Then FreeAndNil(FRestoreBitmap);
          FRestoreBitmap := FVirtualView.Clone;
        End;
        dmErase:
        Begin
          If (FGIFLoader.Frames.Items[Index].IsTransparent And FTransparent) Then FVirtualView.Clear(clrTransparent)
          Else
            FVirtualView.Clear(FGIFLoader.BackgroundColor);
          FVirtualView.PutImage(Src, 0, 0, Src.Width, Src.Height, pLeft, pTop, iDrawMode);
        End;
        dmRestore:
        Begin
          if FLastDrawMode = dmErase then
          begin
            If (FGIFLoader.Frames.Items[Index].IsTransparent And FTransparent) Then FVirtualView.Clear(clrTransparent)
            Else
              FVirtualView.Clear(FGIFLoader.BackgroundColor);
          End;

          If Assigned(FRestoreBitmap) Then FVirtualView.PutImage(FRestoreBitmap, 0, 0, FRestoreBitmap.Width, FRestoreBitmap.Height, 0, 0, dmSet)
          else
          begin
            If (FGIFLoader.Frames.Items[Index].IsTransparent And FTransparent) Then FVirtualView.Clear(clrTransparent)
            Else
              FVirtualView.Clear(FGIFLoader.BackgroundColor);
          end;

          FVirtualView.PutImage(Src, 0, 0, Src.Width, Src.Height, pLeft, pTop, iDrawMode);
        End;
        Else
          FVirtualView.PutImage(Src, 0, 0, Src.Width, Src.Height, pLeft, pTop, dmSet);
      End;
      FLastDrawMode := DrawMode;
    End;
  End;
  // Note : Sous MacOS on ne peux pas assigner FRenderCache.Items[Index].Bitmap directement avec
  // FVirtualView.GetBitmap; On est obligé de créer le bitmap de destination et utiliser Assign.
  // Dans le cas contraire seulment la première image sera affichée.
  //TmpBmp := Graphics.TBitmap.Create; <== MEMORY LEAK
  TmpBmp := FVirtualView.GetBitmap;
  FRenderCache.Items[Index].Bitmap.Assign(TmpBmp);
  FreeAndNil(TmpBmp);
End;

Procedure TGIFViewer.ComputeCache;
Var
  I: Integer;
Begin
  FCurrentFrameIndex := 0;
  FRenderCache.Clear;
  If FGIFLoader.FrameCount > 0 Then
  Begin
    For I := 0 To Pred(FGIFLoader.FrameCount) Do
    Begin
      RenderFrame(I);
    End;
  end;
  if AutoRemoveInvalidFrame then FRenderCache.Pack;
  ResetCurrentView;
End;

Procedure TGIFViewer.CalculatePreferredSize(Var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
Var
  extraWidth: Integer;
Begin
  extraWidth := - 2;
  if FBorderShow then extraWidth      := (FBorderWidth * 2) + (FBevelWidth * 2);
  PreferredWidth  := FGIFWidth + extraWidth + 2;
  PreferredHeight := FGIFHeight + extraWidth + 2;
End;

Class Function TGIFViewer.GetControlClassDefaultSize: TSize;
Begin
  Result.CX := 90; // = ClientWidth
  Result.CY := 90; // = ClientHeight
End;

Function TGIFViewer.DestRect: TRect;
Var
  PicWidth, PicHeight: Integer;
  ImgWidth, ImgHeight: Integer;
  n: Integer;

  procedure KeepAspectRatio( Var aWidth, aHeight : Integer; MaxWidth, MaxHeight : Integer);
  var
     w, h : Integer;
  begin
      w :=  MaxWidth;
      h := (aHeight * w) Div aWidth;
      If h > MaxHeight Then
      Begin
        h := MaxHeight;
        w := (aWidth * h) Div aHeight;
      End;
      aWidth := w;
      aHeight := h;
  End;

Begin
  PicWidth  := FCurrentView.Width;
  PicHeight := FCurrentView.Height;
  ImgWidth  := ClientWidth;
  ImgHeight := ClientHeight;
  If (PicWidth = 0) Or (PicHeight = 0) Then Exit(Rect(0, 0, 0, 0));

  if FAutoStretchMode <> smManual then
  begin
    Case FAutoStretchMode of
     smStretchAll : FStretch := True;
     smStretchOnlyBigger : if (PicWidth > ImgWidth) or (PicHeight > ImgHeight) then FStretch := True else FStretch := False;
     smStretchOnlySmaller : if (PicWidth < ImgWidth) and (PicHeight < ImgHeight) then FStretch := True else FStretch := False;
    end;
    if Assigned(FOnStretchChanged) then FOnStretchChanged(Self,FStretch);
  End;

  If FStretch Then
  Begin
   KeepAspectRatio(PicWidth, PicHeight,ImgWidth, ImgHeight);
  End;

  n  := FBorderWidth + FBevelWidth;
  If FBorderShow Then
  Begin
    Result := Rect(n, n, n + PicWidth, n + PicHeight);
  End
  Else
    Result := Rect(0, 0, PicWidth, PicHeight);

  If FCenter Then
  Begin
    If FBorderShow Then
    Begin
      Result.Left   := n + ((ClientWidth -(n+n))  - PicWidth)  shr 1;
      Result.Top    := n + ((ClientHeight-(n+n))  - PicHeight) shr 1;
    end
    else
    begin
      Result.Left   := ((ClientWidth  - PicWidth) shr 1);
      Result.Top    := ((ClientHeight - PicHeight) shr 1);
    end;
    Result.Right  := Result.Left + PicWidth;
    Result.Bottom := Result.Top + PicHeight;
  End;
End;

Procedure TGIFViewer.Paint;

  Procedure DrawFrame;
  Begin
    With Inherited Canvas Do
    Begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(Self.Width - 1, 0);
      LineTo(Self.Width - 1, Self.Height - 1);
      LineTo(0, Self.Height - 1);
      LineTo(0, 0);
    End;
  End;

Var
  R:     TRect;
  C:     TCanvas;
  ARect: TRect;
  w:     Integer;
Begin

  If csDesigning In ComponentState Then DrawFrame;

  C         := Inherited Canvas;
  FPainting := True;
  R         := DestRect;
  Try
    C.Lock;
    // Fond
    If (FColor <> clNone) Then //and Not(FTransparent)
    Begin
      With C Do
      Begin
        Brush.Style := bsSolid;
        Brush.Color := FColor;
        FillRect(0, 0, ClientWidth, ClientHeight);
      End;
    End;

    // Bitmap
    FCurrentView.Transparent := FTransparent;
    C.StretchDraw(R, FCurrentView);

    // Bordures
    If FBorderShow Then
    Begin
      ARect := rect(0, 0, ClientWidth, ClientHeight);
      w     := FBevelWidth;
      If (FBevelInner <> bvNone) And (w > 0) Then C.Frame3d(ARect, w, BevelInner); // Note: Frame3D inflates ARect
      InflateRect(ARect, -(FBorderWidth + 1), -(FBorderWidth + 1));
      If (FBevelOuter <> bvNone) And (w > 0) Then C.Frame3d(ARect, w, BevelOuter);

      If FBorderWidth > 0 Then With C Do
        Begin
          Pen.Style   := psSolid;
          Pen.Width   := FBorderWidth;
          Pen.Color   := FBorderColor;
          Brush.Style := bsClear;
          Rectangle(0, 0, ClientWidth, ClientHeight);
        End;
    End;

    C.UnLock;
  Finally
    FPainting := False;
  End;

  Inherited Paint;
End;

Procedure TGIFViewer.Loaded;
begin
  if FFileName<>'' then LoadFromFile(FFileName);
  inherited Loaded;
end;

procedure TGIFViewer.BeforeLoad;
begin
  FAnimateTimer.Enabled := False;
  FPause     := False;
  FAnimated  := False;
  FCurrentFrameIndex := 0;
end;

procedure TGIFViewer.AfterLoad;
begin
  FGIFWidth  := FGIFLoader.Width;
  FGIFHeight := FGIFLoader.Height;
  FVirtualView.SetSize(FGIFWidth, FGIFHeight);

  if FCache then
    ComputeCache
  else begin
    FRenderCache.Clear;
    FCurrentFrameIndex := 0;
    RenderFrame(0);
    ResetCurrentView;
  end;

  If AutoSize Then
  Begin
    InvalidatePreferredSize;
    AdjustSize;
  End;
  Invalidate;
  If FAutoPlay Then Start;
end;

Procedure TGIFViewer.Invalidate;
Begin
  If FPainting Then exit;
  Inherited Invalidate;
End;

Procedure TGIFViewer.LoadFromStream(aStream : TStream);
Begin
  BeforeLoad;
  FGIFLoader.FErrorList.Clear;
  FGIFLoader.FErrorCOunt := 0;
  FGIFLoader.LoadFromStream(aStream);
  AfterLoad;
End;

Procedure TGIFViewer.LoadFromFile(Const aFileName: String);
Begin
  BeforeLoad;
  if Not(FileExists(aFileName)) then
  begin
    MessageDlg(Format(rsFileNotFound,[aFileName]), mtError, [mbOK],0);
    Exit;
  end;
  FGIFLoader.LoadFromFile(aFileName);
  FFileName  := aFileName;
  AfterLoad;
End;

Procedure TGIFViewer.LoadFromResource(Const ResName: String);
Var
  Resource: TLResource;
Begin
  BeforeLoad;
  Resource  := LazarusResources.Find(ResName);
  If Resource = nil Then Raise Exception.Create(Format(rsResourceNotFound,[ResName]))
  Else If CompareText(LazarusResources.Find(ResName).ValueType, 'gif') = 0 Then
  Begin
    FGIFLoader.LoadFromResource(ResName);
    AfterLoad;
  End;
End;

Procedure TGIFViewer.Start;
Begin
  If Not (FPause) Then FCurrentFrameIndex := 0;
  FPause    := False;
  FAnimated := True;
  FAnimateTimer.Enabled := True;
  If Assigned(FOnStart) Then FOnStart(Self);
End;

Procedure TGIFViewer.Stop;
Begin
  FAnimateTimer.Enabled := False;
  FAnimated := False;
  FPause    := False;
  If Assigned(FOnStop) Then FOnStop(Self);
  FCurrentframeIndex     := 0;
  ResetCurrentView;
  Invalidate;
End;

Procedure TGIFViewer.Pause;
Begin
  FAnimateTimer.Enabled := False;
  FPause := True;
End;

Procedure TGIFViewer.NextFrame;
begin
  if FCurrentFrameIndex < FGifLoader.FrameCount - 1 then
  begin
    Inc(FCurrentFrameIndex);

    repeat

      If (not FCache) and (FCurrentFrameIndex >= FRenderCache.Count) Then
      begin
        RenderFrame(FCurrentFrameIndex);
      end;

      If Assigned(FOnFrameChange) Then FOnFrameChange(Self);

      if not(FDisplayInvalidFrames) then
      begin
        if not(FRenderCache.Items[FCurrentFrameIndex].IsCorrupted) then
        begin
          FCurrentView.Assign(FRenderCache.Items[FCurrentFrameIndex].Bitmap);
        End
        Else If FCurrentFrameIndex > 0 Then
        Begin
          Inc(FCurrentFrameIndex);
          Continue;
        End;
      end
      else
      begin
        FCurrentView.Assign(FRenderCache.Items[FCurrentFrameIndex].Bitmap);
      end;
      Break;
    until False;

    FAnimateTimer.Interval := FRenderCache.Items[FCurrentFrameIndex].Delay;
  end;
  Invalidate;
end;

Procedure TGIFViewer.PriorFrame;
begin
  if FCurrentFrameIndex > 0 then
  begin
    Dec(FCurrentFrameIndex);

    repeat
      If Assigned(FOnFrameChange) Then FOnFrameChange(Self);

      if not(FDisplayInvalidFrames) then
      begin
        if not(FRenderCache.Items[FCurrentFrameIndex].IsCorrupted) then
        begin
          FCurrentView.Assign(FRenderCache.Items[FCurrentFrameIndex].Bitmap);
        End
        Else If FCurrentFrameIndex > 0 Then
        Begin
          Dec(FCurrentFrameIndex);
          Continue;
        End;
      end
      else
      begin
        FCurrentView.Assign(FRenderCache.Items[FCurrentFrameIndex].Bitmap);
      end;
      Break;
    until False;

    FAnimateTimer.Interval := FRenderCache.Items[FCurrentFrameIndex].Delay;
  end;
  Invalidate;
end;

Function TGIFViewer.GetRawFrame(Index: Integer): TBitmap;
Begin
  Result := FGIFLoader.Frames[Index].Bitmap.GetBitmap;
End;

Procedure TGIFViewer.DisplayFrame(Index: Integer);
Begin
  If not(FRenderCache.IsIndexOk(Index)) then exit;
  if Not(DisplayInvalidFrames) then
  begin
    if FRenderCache.Items[Index].IsCorrupted then
    begin
      inc(Index);
      DisplayFrame(Index);
    End
    else
    begin
      FCurrentView.Assign(FRenderCache.Items[Index].Bitmap);
    End;
  end
  else
  begin
    FCurrentView.Assign(FRenderCache.Items[Index].Bitmap);
  End;
  Invalidate;
End;

Procedure TGIFViewer.DisplayRawFrame(Index: Integer);
Var
  Tmp: Graphics.TBitmap;
Begin
  If not(FRenderCache.IsIndexOk(Index)) Then exit;
  Tmp := GetRawFrame(Index);
  FCurrentView.Assign(Tmp);
  FreeAndNil(Tmp);
  Invalidate;
End;

Procedure TGIFViewer.ExtractFrame(Index: Integer; Var bmp: TBitmap);
Begin
  If not(FRenderCache.IsIndexOk(Index)) then exit;
  Bmp.Assign(FRenderCache.Items[Index].Bitmap);
End;

Procedure TGIFViewer.ExtractRawFrame(Index: Integer; Var bmp: TBitmap);
Var
  Tmp: Graphics.TBitmap;
Begin
  If not(FRenderCache.IsIndexOk(Index)) Then exit;
  Tmp := GetRawFrame(Index);
  Bmp.Assign(Tmp);
  FreeAndNil(Tmp);
End;

{%endregion}

Procedure Register;
Begin
  RegisterComponents('Misc', [TGIFView]);
End;

End.
