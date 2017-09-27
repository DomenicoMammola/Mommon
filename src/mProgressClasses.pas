unit mProgressClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, contnrs, math, sysutils,
  mProgress;

type

  TmAbstractProgress = class;

  { TmThreadWithProgress }

  TmThreadWithProgress = class (TThread)
  protected
    FProgress : TmAbstractProgress;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure RefreshProgress;
  end;

  { TmAbstractProgress }

  TmAbstractProgress = class (ImProgress)
  strict private
    FCaption : String;
    FCurrentStep : integer;
    FMaxStep : integer;
    FBouncing : boolean;
    procedure SetMaxStep(AValue: integer);
  private
    FOnRefresh : TNotifyEvent;
    FOwnerThread : TmThreadWithProgress;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetBouncing(AValue: boolean);
    procedure SetCaption(AValue: string);
    procedure SetCurrentStep(AValue: integer);

    property OwnerThread : TmThreadWithProgress read FOwnerThread write FOwnerThread;
    property OnRefresh : TNotifyEvent read FOnRefresh write FOnRefresh;
    property Caption : String read FCaption;
  end;

  TmProgressGUI = class;

  TmProgressGUIClass = class of TmProgressGUI;

  TmProgressGUI = class abstract
  public
    constructor Create; virtual; abstract;
    procedure AddProgress(aProgress : TmAbstractProgress); virtual; abstract;
    procedure RemoveProgress(aProgress : TmAbstractProgress); virtual; abstract;
    procedure RefreshProgress (aProgress : TmAbstractProgress); virtual; abstract;
  end;


  { TmProgressGUIFactory }

  TmProgressGUIFactory = class
  strict private
    FCurrentProgressGUI : TmProgressGUI;
    FProgressGUIClass : TmProgressGUIClass;
    FFakeProgressGUI : TmProgressGUI;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterProgressGUIClass (aClass : TmProgressGUIClass);
    function GetCurrentProgressGUI : TmProgressGUI;
  end;

  procedure LinkEvents (aProgress : TmAbstractProgress; aOnRefresh : TNotifyEvent);

  function GetProgressGUIFactory : TmProgressGUIFactory;

implementation

uses
  mProgressManager;

type

  { TFakeProgressGUI }

  TFakeProgressGUI = class(TmProgressGUI)
  public
    constructor Create; override;
    procedure AddProgress(aProgress : TmAbstractProgress); override;
    procedure RemoveProgress(aProgress : TmAbstractProgress); override;
    procedure RefreshProgress (aProgress : TmAbstractProgress); override;
  end;

var
  internalProgressGUIFactory : TmProgressGUIFactory;

procedure LinkEvents(aProgress: TmAbstractProgress; aOnRefresh: TNotifyEvent);
begin
  aProgress.FOnRefresh:= aOnRefresh;
end;

function GetProgressGUIFactory: TmProgressGUIFactory;
begin
  if not Assigned(internalProgressGUIFactory) then
    internalProgressGUIFactory:= TmProgressGUIFactory.Create;
  Result := internalProgressGUIFactory;
end;

{ TmThreadWithProgress }

destructor TmThreadWithProgress.Destroy;
begin
  FProgress.Free;
  inherited Destroy;
end;

procedure TmThreadWithProgress.AfterConstruction;
begin
  inherited AfterConstruction;
  FProgress := TmAbstractProgress.Create;
  FProgress.OwnerThread := Self;
end;

procedure TmThreadWithProgress.RefreshProgress;
begin
  FProgress.OnRefresh(FProgress);
end;

{ TFakeProgressGUI }

constructor TFakeProgressGUI.Create;
begin
  //
end;

procedure TFakeProgressGUI.AddProgress(aProgress: TmAbstractProgress);
begin
  // do nothing
end;

procedure TFakeProgressGUI.RemoveProgress(aProgress: TmAbstractProgress);
begin
  // do nothing
end;

procedure TFakeProgressGUI.RefreshProgress(aProgress: TmAbstractProgress);
begin
  // do nothing
end;

{ TmProgressGUIFactory }

constructor TmProgressGUIFactory.Create;
begin
  FCurrentProgressGUI := nil;
  FProgressGUIClass:= nil;
  FFakeProgressGUI := TFakeProgressGUI.Create;
end;

destructor TmProgressGUIFactory.Destroy;
begin
  FreeAndNil(FFakeProgressGUI);
  FreeAndNil(FCurrentProgressGUI);
  inherited Destroy;
end;

procedure TmProgressGUIFactory.RegisterProgressGUIClass(aClass: TmProgressGUIClass);
begin
  FProgressGUIClass:= aClass;
end;

function TmProgressGUIFactory.GetCurrentProgressGUI: TmProgressGUI;
begin
  if FProgressGUIClass = nil then
    Result := FFakeProgressGUI
  else
  begin
    if not Assigned(FCurrentProgressGUI) then
      FCurrentProgressGUI := FProgressGUIClass.Create;
    Result := FCurrentProgressGUI;
  end;
end;

{ TmAbstractProgress }

procedure TmAbstractProgress.SetCaption(AValue: string);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  FOwnerThread.Synchronize(FOwnerThread, FOwnerThread.RefreshProgress);
end;

procedure TmAbstractProgress.SetBouncing(AValue: boolean);
begin
  if FBouncing=AValue then Exit;
  FBouncing:=AValue;
  FOwnerThread.Synchronize(FOwnerThread, FOwnerThread.RefreshProgress);
end;

procedure TmAbstractProgress.SetCurrentStep(AValue: integer);
begin
  if FCurrentStep=AValue then Exit;
  FCurrentStep:=AValue;
  FOwnerThread.Synchronize(FOwnerThread, FOwnerThread.RefreshProgress);
end;

procedure TmAbstractProgress.SetMaxStep(AValue: integer);
begin
  if FMaxStep=AValue then Exit;
  FMaxStep:=AValue;
end;

constructor TmAbstractProgress.Create;
begin
  FCaption := '';
  GetProgressManager.RegisterProgress(Self);
end;

destructor TmAbstractProgress.Destroy;
begin
  GetProgressManager.UnregisterProgress(Self);
  inherited Destroy;
end;


finalization
  FreeAndNil(internalProgressGUIFactory);

end.
