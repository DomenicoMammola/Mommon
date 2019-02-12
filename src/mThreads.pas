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
  {$ifdef lcl}{$ifndef console}Forms, Controls, {$endif}{$endif}
  mUtility, mProgress, mThreadsBaseClasses;

type

  { TJobResult }

  TJobResult = class
  strict private
    FReturnCode: integer;
    FReturnMessage : String;
    FReturnData : TObject;
    FOwnsData : boolean;
  private
    FGotException: boolean;
    FExceptionMessage : string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property ReturnCode : integer read FReturnCode write FReturnCode;
    property ReturnMessage : String read FReturnMessage write FReturnMessage;
    property ReturnData : TObject read FReturnData write FReturnData;
    property OwnsData : boolean read FOwnsData write FOwnsData;
    property GotException : boolean read FGotException;
    property ExceptionMessage : string read FExceptionMessage;
  end;

  { TJobResults }

  TJobResults = class
  strict private
    FList : TObjectList;
  private
    procedure Add (aJobResult : TJobResult);
  public
    constructor Create;
    destructor Destroy; override;
    function Get(aIndex : integer) : TJobResult;
    function Count : integer;
  end;

  TDoJobProcedure = procedure (aProgress: ImProgress; aJobResult : TJobResult) of object;
  TOnEndJobCallback = procedure (const aJobsResult : TJobResults) of object;

  TmJob = class
  strict private
    FDoJobProcedure : TDoJobProcedure;
    FDescription : String;
    FTrapExceptions : boolean;
  private
    FJobId : integer;
  public
    property DoJobProcedure : TDoJobProcedure read FDoJobProcedure write FDoJobProcedure;
    property JobId : integer read FJobId;
    property Description : String read FDescription write FDescription;
    property TrapExceptions : boolean read FTrapExceptions write FTrapExceptions;
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
    procedure Execute ({$ifdef lcl}aParentForm : TForm;{$endif}aCallBack : TOnEndJobCallback);
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
    FJobResult : TJobResult;
    FLastException : Exception;
    procedure RaiseApplicationOnException;
  public
    constructor Create (aJob : TmJob; aJobResult : TJobResult); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
    function GetDebugInfo: string; override;

    property CanDieEvent : TSimpleEvent read FCanDieEvent write FCanDieEvent;
    property CanStartEvent : TSimpleEvent read FCanStartEvent;
    property Job : TmJob read FJob;
  end;

  { TControlThread }

  TControlThread = class (TmThread)
  strict private
    FCanStartEvent : TSimpleEvent;
    FCanDieEvent : TSimpleEvent;

    FJobs : TObjectList;
    FThreads : TObjectList;
    FCanDieEvents : TObjectList;
    FCallBack : TOnEndJobCallback;
    FRunning : boolean;
    FCurrentJobResults : TJobResults;
    {$ifdef lcl}
    FParentForm: TForm;
    {$endif}

    procedure RunEndCallBack;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
    function GetDebugInfo: string; override;

    property CanStartEvent : TSimpleEvent read FCanStartEvent;
    property CanDieEvent : TSimpleEvent read FCanDieEvent write FCanDieEvent;
    property Jobs : TObjectList read FJobs;

    property CallBack : TOnEndJobCallback read FCallBack write FCallBack;
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

{ TJobResults }

procedure TJobResults.Add(aJobResult: TJobResult);
begin
  FList.Add(aJobResult);
end;

constructor TJobResults.Create;
begin
  FList := TObjectList.Create(true);
end;

destructor TJobResults.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TJobResults.Get(aIndex: integer): TJobResult;
begin
  Result := FList.Items[aIndex] as TJobResult;
end;

function TJobResults.Count: integer;
begin
  Result := FList.Count;
end;

{ TJobResult }

constructor TJobResult.Create();
begin
  FReturnCode:= 0;
  FReturnMessage:= '';
  FReturnData := nil;
  FOwnsData:= false;
  FGotException:= false;
  FExceptionMessage:= '';
end;

destructor TJobResult.Destroy;
begin
  if FOwnsData and (Assigned(FReturnData)) then
    FReturnData.Free;
  inherited Destroy;
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
  FCallBack(FCurrentJobResults);
  FreeAndNil(FCurrentJobResults);
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
  FreeAndNil(FCurrentJobResults);
  FCanDieEvent.SetEvent;
  inherited Destroy;
end;

procedure TControlThread.Execute;
var
  i : integer;
  runningJobs : integer;
  tmpThread : TJobThread;
  tmpJobResult : TJobResult;
  tmpCanDiedEvent : TSimpleEvent;
begin
  runningJobs := 0;

  while not Terminated do
  begin
    FCanStartEvent.WaitFor(INFINITE);

    if FJobs.Count > 0 then
    begin
      FRunning := true;

      if not Terminated then
      begin
        FCurrentJobResults := TJobResults.Create;

        runningJobs := FJobs.Count;
        for i := 0 to FJobs.Count -1 do
        begin
          tmpJobResult := TJobResult.Create;
          FCurrentJobResults.Add(tmpJobResult);
          tmpThread := TJobThread.Create(FJobs.Items[i] as TmJob, tmpJobResult);
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
  end;
  FCanDieEvent.SetEvent;
end;

function TControlThread.GetDebugInfo: string;
begin
  Result:= 'Control thread';
end;


{ TJobThread }

procedure TJobThread.RaiseApplicationOnException;
begin
  if Assigned(Application.OnException) then
    Application.OnException(Self, FLastException)
  else
    Application.ShowException(FLastException);
end;

constructor TJobThread.Create(aJob : TmJob; aJobResult : TJobResult);
begin
  inherited Create(false);
  FreeOnTerminate:= false;
  FCanDieEvent := nil;
  FCanStartEvent := TSimpleEvent.Create;
  FJob := aJob;
  FJobResult := aJobResult;
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
      FJob.DoJobProcedure (FProgress, FJobResult);
    except
      on e:Exception do
      begin
        FJobResult.FExceptionMessage:= e.Message;
        FJobResult.FGotException:= true;

        if not FJob.TrapExceptions then
        begin
          if Assigned(Application.OnException) then
          begin
            FLastException := e;
            Synchronize(@RaiseApplicationOnException);
          end
          else
            Application.ShowException(e);
        end;
      end;
    end;
  end;
  FCanDieEvent.SetEvent;
end;

function TJobThread.GetDebugInfo: string;
begin
  Result:= '[TJobThread] ' + FJob.Description;
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
  Result := nil;
  if GetControlThread(Self).Running then
    exit;
  Result := TmJob.Create;
  GetControlThread(Self).Jobs.Add(Result);
  Result.FJobId:= GetControlThread(Self).Jobs.Count;
end;

procedure TmBatchExecutor.Execute({$ifdef lcl}aParentForm : TForm;{$endif}aCallBack : TOnEndJobCallback);
begin
  if GetControlThread(Self).Running then
    exit;

  if GetControlThread(Self).Jobs.Count = 0 then
    exit;

  GetControlThread(Self).ParentForm := nil;

  {$ifdef lcl}
  if Assigned(aParentForm) then
  begin
    GetControlThread(Self).ParentForm := aParentForm;
    aParentForm.Cursor := crHourGlass;
    aParentForm.Enabled:= false;
  end;
  {$endif}
  GetControlThread(Self).CallBack:= aCallBack;
  FRunning := true;

  GetControlThread(Self).CanStartEvent.SetEvent;
end;

finalization

FreeAndNil(internalBatchExecutor);

end.
