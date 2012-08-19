unit VTAccessibilityFactory;

// class to create IAccessibles for the tree passed into it.
// If not already assigned, creates IAccessibles for the tree itself
// and the focused item
// the tree accessible is returned when the tree receives an WM_GETOBJECT message
// the AccessibleItem is returned when the Accessible is being asked for the first child
// To create your own IAccessibles, use the VTStandardAccessible unit as a reference,
// and assign your Accessibles to the variables in tthe unit's initialization.
// You only need to add the unit to your project, and voilá, you have an accessible string tree!
//
// Written by Marco Zehe. (c) 2007

interface

uses
  Classes, oleacc, VirtualTrees;

type
  IVTAccessibleProvider = interface
    function CreateIAccessible(ATree: TBaseVirtualTree): IAccessible;
  end;

  TVTAccessibilityFactory = class(TObject)
  private
    FAccessibleProviders: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateIAccessible(ATree: TBaseVirtualTree): IAccessible;
    procedure RegisterAccessibleProvider(AProvider: IVTAccessibleProvider);
    procedure UnRegisterAccessibleProvider(AProvider: IVTAccessibleProvider);
  end;

var
  VTAccessibleFactory: TVTAccessibilityFactory;

implementation

{ TVTAccessibilityFactory }

constructor TVTAccessibilityFactory.Create;
begin
  inherited;
  FAccessibleProviders := TInterfaceList.Create;
  FAccessibleProviders.Clear;
end;

function TVTAccessibilityFactory.CreateIAccessible(
  ATree: TBaseVirtualTree): IAccessible;
var
  I: Integer;
  TmpIAccessible: IAccessible;
// returns an IAccessible.
// 1. If the Accessible property of the passed-in tree is nil,
// the first registered element will be returned.
// Usually, this is the IAccessible that provides information about the tree itself.
// If it is not nil, we'll check whether the AccessibleItem is nil.
// If it is, we'll look in the registered IAccessibles for the appropriate one.
// Each IAccessibleProvider will check the tree for properties to determine whether it is responsible.
// We'll work top to bottom, from the most complicated to the most simple.
// The index for these should all be greater than 0, e g the IAccessible for the tree itself should always be registered first, then any IAccessible items.
begin
  result := nil;
  if ATree <> nil then
  begin
    if ATree.Accessible = nil then
    begin
      if FAccessibleProviders.Count > 0 then
      begin
        result := IVTAccessibleProvider(FAccessibleProviders.Items[0]).CreateIAccessible(ATree);
        exit;
      end;
    end;
    if ATree.AccessibleItem = nil then
    begin
      if FAccessibleProviders.Count > 0 then
      begin
        for I := FAccessibleProviders.Count - 1 downto 1 do
        begin
          TmpIAccessible := IVTAccessibleProvider(FAccessibleProviders.Items[I]).CreateIAccessible(ATree);
          if TmpIAccessible <> nil then
          begin
            result := TmpIAccessible;
            break;
          end;
        end;
        if TmpIAccessible = nil then
        begin
          result := IVTAccessibleProvider(FAccessibleProviders.Items[0]).CreateIAccessible(ATree);
        end;
      end;
    end
    else begin
      result := ATree.AccessibleItem;
    end;
  end;
end;

destructor TVTAccessibilityFactory.Destroy;
begin
  FAccessibleProviders.Free;
  FAccessibleProviders := nil;
  inherited;
end;

procedure TVTAccessibilityFactory.RegisterAccessibleProvider(
  AProvider: IVTAccessibleProvider);
// Ads a provider if it is not already registered
begin
  if FAccessibleProviders.IndexOf(AProvider) < 0 then
    FAccessibleProviders.Add(AProvider)
end;

procedure TVTAccessibilityFactory.UnRegisterAccessibleProvider(
  AProvider: IVTAccessibleProvider);
// Unregisters/removes an IAccessible provider if it is present
begin
  if FAccessibleProviders.IndexOf(AProvider) >= 0 then
    FAccessibleProviders.Remove(AProvider);
end;

end.
