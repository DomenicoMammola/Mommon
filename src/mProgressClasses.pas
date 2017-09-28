// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
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
    procedure RemoveProgress;
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
    FOnRemove : TNotifyEvent;
    FOwnerThread : TmThreadWithProgress;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetBouncing(AValue: boolean);
    procedure SetCaption(AValue: string);
    procedure SetCurrentStep(AValue: integer);

    property OwnerThread : TmThreadWithProgress read FOwnerThread write FOwnerThread;
    property OnRefresh : TNotifyEvent read FOnRefresh write FOnRefresh;
    property OnRemove : TNotifyEvent read FOnRemove write FOnRemove;
    property Caption : String read FCaption;
  end;

  TmProgressGUI = class;

  TmProgressGUIClass = class of TmProgressGUI;

  TmProgressGUI = class abstract
  public
    constructor Create; virtual; abstract;
    procedure AddProgress(aProgress : TmAbstractProgress); virtual; abstract;
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

  function GetProgressGUIFactory : TmProgressGUIFactory;

implementation

type

  { TFakeProgressGUI }

  TFakeProgressGUI = class(TmProgressGUI)
  public
    constructor Create; override;
    procedure AddProgress(aProgress : TmAbstractProgress); override;
  end;

var
  internalProgressGUIFactory : TmProgressGUIFactory;

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

procedure TmThreadWithProgress.RemoveProgress;
begin
  FProgress.OnRemove(FProgress);
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
  GetProgressGUIFactory.GetCurrentProgressGUI.AddProgress(Self);
end;

destructor TmAbstractProgress.Destroy;
begin
  FOwnerThread.Synchronize(FOwnerThread, FOwnerThread.RemoveProgress);
  inherited Destroy;
end;


finalization
  FreeAndNil(internalProgressGUIFactory);

end.
