unit mProgressManager;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs, sysutils,
  mProgressClasses;

type

  { TmProgressManager }

  TmProgressManager = class
  strict private
    FProgressList : TObjectList;
    procedure OnRefresh (Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterProgress (aProgress : TmAbstractProgress);
    procedure UnregisterProgress (aProgress : TmAbstractProgress);
  end;


function GetProgressManager : TmProgressManager;

implementation

var
  internalProgressManager : TmProgressManager;

function GetProgressManager: TmProgressManager;
begin
  if not Assigned(internalProgressManager) then
    internalProgressManager := TmProgressManager.Create;
  Result := internalProgressManager;
end;

{ TmProgressManager }

procedure TmProgressManager.OnRefresh(Sender: TObject);
begin
  GetProgressGUIFactory.GetCurrentProgressGUI.RefreshProgress(Sender as TmAbstractProgress);
end;


constructor TmProgressManager.Create;
begin
  FProgressList := TObjectList.Create(false);
end;

destructor TmProgressManager.Destroy;
begin
  FProgressList.Free;
  inherited Destroy;
end;

procedure TmProgressManager.RegisterProgress(aProgress: TmAbstractProgress);
begin
  FProgressList.Add(aProgress);
  LinkEvents(aProgress, Self.OnRefresh);
  GetProgressGUIFactory.GetCurrentProgressGUI.AddProgress(aProgress);
end;

procedure TmProgressManager.UnregisterProgress(aProgress: TmAbstractProgress);
begin
  GetProgressGUIFactory.GetCurrentProgressGUI.RemoveProgress(aProgress);
  FProgressList.Remove(aProgress);
end;


finalization

  if Assigned(internalProgressManager) then
    FreeAndNil(internalProgressManager);

end.
