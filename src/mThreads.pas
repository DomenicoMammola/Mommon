// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mThreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, contnrs, syncobjs, SysUtils,
  {$ifdef lcl}Forms, Controls,{$endif}
  mUtility, mProgress;

type
  TDoJobProcedure = procedure (aProgress: ImProgress) of object;
  TOnEndCallback = procedure of object;

  TmJob = class
  strict private
    FDoJobProcedure : TDoJobProcedure;
  private
    FJobId : integer;
  public
    property DoJobProcedure : TDoJobProcedure read FDoJobProcedure write FDoJobProcedure;
    property JobId : integer read FJobId;
  end;

  { TmBatchExecutor }
  TmBatchExecutor = class
  private
    FRunning : boolean;
    FControlThread : TThread;
    FCanEndEvent : TSimpleEvent;
  public
    constructor Create;
    destructor Destroy; override;

    function QueueJob : TmJob;
    procedure Execute ({$ifdef lcl}aParentForm : TForm;{$endif}aCallBack : TOnEndCallback);
  end;

  function BatchExecutor : TmBatchExecutor;

implementation

uses
  mProgressClasses;

type

  { TJobThread }

  TJobThread = class (TmThreadWithProgress)
  strict private
    FCanStartEvent : TSimpleEvent;
    FCanDieEvent : TSimpleEvent;
    FJob : TmJob;
  public
    constructor Create (aJob : TmJob); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;

    property CanDieEvent : TSimpleEvent read FCanDieEvent write FCanDieEvent;
    property CanStartEvent : TSimpleEvent read FCanStartEvent;
    property Job : TmJob read FJob;
  end;

  { TControlThread }

  TControlThread = class (TThread)
  strict private
    FCanStartEvent : TSimpleEvent;
    FCanDieEvent : TSimpleEvent;

    FJobs : TObjectList;
    FThreads : TObjectList;
    FCanDieEvents : TObjectList;
    FCallBack : TOnEndCallback;
    FRunning : boolean;
    {$ifdef lcl}
    FParentForm: TForm;
    {$endif}

    procedure RunEndCallBack;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
    property CanStartEvent : TSimpleEvent read FCanStartEvent;
    property CanDieEvent : TSimpleEvent read FCanDieEvent write FCanDieEvent;
    property Jobs : TObjectList read FJobs;

    property CallBack : TOnEndCallback read FCallBack write FCallBack;
    property Running : boolean read FRunning;
    {$ifdef lcl}
    property ParentForm: TForm read FParentForm write FParentForm;
    {$endif}
  end;

var
  internalBatchExecutor : TmBatchExecutor;

function GetControlThread (aExecutor : TmBatchExecutor) : TControlThread;
begin
  Result := aExecutor.FControlThread as TControlThread;
end;

function BatchExecutor: TmBatchExecutor;
begin
  if not Assigned(internalBatchExecutor) then
    internalBatchExecutor := TmBatchExecutor.Create;
  Result := internalBatchExecutor;
end;

{ TControlThread }

procedure TControlThread.RunEndCallBack;
begin
  {$ifdef lcl}
  if Assigned(FParentForm) then
  begin
    FParentForm.Cursor:= crArrow;
    FParentForm.Enabled := true;
  end;
  {$endif}
  FCallBack();
end;

constructor TControlThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate:= false;
  FCanStartEvent := TSimpleEvent.Create;
  FJobs := TObjectList.Create (true);
  FThreads := TObjectList.Create(true);
  FCanDieEvents := TObjectList.Create(true);
end;

destructor TControlThread.Destroy;
var
  i : integer;
begin
  for i := 0 to FThreads.Count - 1 do
  begin
    (FThreads.Items[i] as TThread).Terminate;
    (FCanDieEvents.Items[i] as TSimpleEvent).WaitFor(3000);
  end;
  FreeAndNil(FThreads);
  FreeAndNil(FCanDieEvents);
  FreeAndNil(FJobs);
  FCanStartEvent.Free;
  FCanDieEvent.SetEvent;
  inherited Destroy;
end;

procedure TControlThread.Execute;
var
  i : integer;
  runningJobs : integer;
  tmpThread : TJobThread;
  tmpCanDiedEvent : TSimpleEvent;
begin
  while not Terminated do
  begin
    FCanStartEvent.WaitFor(INFINITE);

    FRunning := true;

    if not Terminated then
    begin
      runningJobs := FJobs.Count;
      for i := 0 to FJobs.Count -1 do
      begin
        tmpThread := TJobThread.Create(FJobs.Items[i] as TmJob);
        tmpCanDiedEvent := TSimpleEvent.Create;
        tmpThread.CanDieEvent := tmpCanDiedEvent;
        FThreads.Add(tmpThread);
        FCanDieEvents.Add(tmpCanDiedEvent);
        tmpThread.CanStartEvent.SetEvent;
      end;
    end;

    while (not Terminated) and (runningJobs > 0) do
    begin
      for i := 0 to FJobs.Count - 1 do
      begin
        if (FCanDieEvents.Items[i] as TSimpleEvent).WaitFor(10) <> wrTimeout then
          dec(runningJobs);
        if Terminated or (runningJobs = 0) then
          break;
      end;
    end;

    if not Terminated then
    begin
      FCanStartEvent.ResetEvent;
      Synchronize(@RunEndCallBack);
      FThreads.Clear;
      FCanDieEvents.Clear;
      FJobs.Clear;
    end;
    FRunning := false;
  end;
  FCanDieEvent.SetEvent;
end;

{ TJobThread }

constructor TJobThread.Create(aJob : TmJob);
begin
  inherited Create(false);
  FreeOnTerminate:= false;
  FCanDieEvent := nil;
  FCanStartEvent := TSimpleEvent.Create;
  FJob := aJob;
  Self.Priority:= tpNormal;
end;

destructor TJobThread.Destroy;
begin
  FCanStartEvent.Free;
  inherited Destroy;
end;

procedure TJobThread.Execute;
begin
  FCanStartEvent.WaitFor(INFINITE);

  if (not Terminated) then
  begin
    try
      FJob.DoJobProcedure (FProgress);
    except
      on e:Exception do
      begin
        Application.ShowException(e);
      end;
    end;
  end;
  FCanDieEvent.SetEvent;
end;


{ TmBatchExecutor }

constructor TmBatchExecutor.Create;
begin
  FRunning:= false;
  FCanEndEvent := TSimpleEvent.Create;
  FControlThread := TControlThread.Create;
  GetControlThread(Self).CanDieEvent := FCanEndEvent;
end;

destructor TmBatchExecutor.Destroy;
begin
  if Assigned(FControlThread) then
  begin
    FControlThread.Terminate;
    (FControlThread as TControlThread).CanStartEvent.SetEvent;
    FCanEndEvent.WaitFor(3000);
    FreeAndNil(FControlThread);
  end;
  FreeAndNil(FCanEndEvent);
  inherited Destroy;
end;

function TmBatchExecutor.QueueJob : TmJob;
begin
  if GetControlThread(Self).Running then
    exit;
  Result := TmJob.Create;
  GetControlThread(Self).Jobs.Add(Result);
  Result.FJobId:= GetControlThread(Self).Jobs.Count;
end;

procedure TmBatchExecutor.Execute({$ifdef lcl}aParentForm : TForm;{$endif}aCallBack : TOnEndCallback);
begin
  if GetControlThread(Self).Running then
    exit;

  if GetControlThread(Self).Jobs.Count = 0 then
    exit;

  {$ifdef lcl}
  GetControlThread(Self).ParentForm := aParentForm;
  aParentForm.Cursor := crHourGlass;
  aParentForm.Enabled:= false;
  {$endif}
  GetControlThread(Self).CallBack:= aCallBack;
  FRunning := true;

  GetControlThread(Self).CanStartEvent.SetEvent;
end;

finalization

FreeAndNil(internalBatchExecutor);

end.
