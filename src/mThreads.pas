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

//{$mode objfpc}{$H+}
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}


interface

{$I mDefines.inc}

uses
  Classes, contnrs, syncobjs, SysUtils, CustApp,
  {$IFDEF GUI}Forms, Controls, {$ENDIF}
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

  TDoJobProcedure = procedure (aProgress: ImProgress; aData: TObject; aJobResult : TJobResult) of object;
  TOnEndJobCallback = procedure (const aJobsResult : TJobResults) of object;

  { TmJob }

  TmJob = class
  strict private
    FDoJobProcedure : TDoJobProcedure;
    FDescription : String;
    FTrapExceptions : boolean;
    FData : TObject;
    FApplication : TCustomApplication;
  private
    FJobId : integer;
  public
    constructor Create;

    property DoJobProcedure : TDoJobProcedure read FDoJobProcedure write FDoJobProcedure;
    property JobId : integer read FJobId;
    property Description : String read FDescription write FDescription;
    property TrapExceptions : boolean read FTrapExceptions write FTrapExceptions;
    property Data : TObject read FData write FData;
    property Application : TCustomApplication read FApplication write FApplication;
  end;

  { TmBatchExecutor }
  TmBatchExecutor = class
  private
    FRunning : boolean;
    FControlThread : TThread;
    FCanEndEvent : TSimpleEvent;
    FMaxConcurrentThreads : integer;
    procedure SetMaxConcurrentThreads(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;

    function QueueJob : TmJob;
    procedure Execute ({$ifdef gui}aParentForm : TForm;{$endif}aCallBack : TOnEndJobCallback);
    property MaxConcurrentThreads : integer read FMaxConcurrentThreads write SetMaxConcurrentThreads;
  end;

  function BatchExecutor : TmBatchExecutor;

implementation

uses
  mProgressClasses, mIntList
  {$IFDEF DEBUG}, mLog{$ENDIF}
  ;

type

  { TJobThread }

  TJobThread = class (TmThreadWithProgress)
  strict private
    FCanStartEvent : TSimpleEvent;
    FCanDieEvent : TSimpleEvent;
    FJob : TmJob;
    FJobResult : TJobResult;
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
    FJobsRunning : TIntegerList;
    FMaxConcurrentThreads : integer;
    {$ifdef gui}
    FParentForm: TForm;
    {$endif}

    procedure RunEndCallBack;
    procedure SetMaxConcurrentThreads(AValue: integer);
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function GetDebugInfo: string; override;

    property CanStartEvent : TSimpleEvent read FCanStartEvent;
    property CanDieEvent : TSimpleEvent read FCanDieEvent write FCanDieEvent;
    property Jobs : TObjectList read FJobs;

    property CallBack : TOnEndJobCallback read FCallBack write FCallBack;
    property Running : boolean read FRunning;
    property MaxConcurrentThreads : integer read FMaxConcurrentThreads write SetMaxConcurrentThreads;
    {$ifdef gui}
    property ParentForm: TForm read FParentForm write FParentForm;
    {$endif}
  end;

var
  internalBatchExecutor : TmBatchExecutor;
  {$IFDEF DEBUG}
  logger : TmLog;
  {$ENDIF}

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

{ TmJob }

constructor TmJob.Create;
begin
  FData := nil;
  FApplication := nil;
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
  {$ifdef gui}
  if Assigned(FParentForm) then
  begin
    FParentForm.Cursor:= crArrow;
    FParentForm.Enabled := true;
  end;
  {$endif}
  FCallBack(FCurrentJobResults);
  FreeAndNil(FCurrentJobResults);
end;

procedure TControlThread.SetMaxConcurrentThreads(AValue: integer);
begin
  if (FMaxConcurrentThreads=AValue) or (AValue <= 0) then Exit;
  FMaxConcurrentThreads:=AValue;
end;

constructor TControlThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate:= false;
  FCanStartEvent := TSimpleEvent.Create;
  FJobs := TObjectList.Create (true);
  FThreads := TObjectList.Create(true);
  FCanDieEvents := TObjectList.Create(true);
  FJobsRunning := TIntegerList.Create;
  FMaxConcurrentThreads:= 99;
end;

destructor TControlThread.Destroy;
var
  i : integer;
begin
  for i := 0 to FThreads.Count - 1 do
  begin
    (FThreads.Items[i] as TThread).Terminate;
    (FCanDieEvents.Items[i] as TSimpleEvent).WaitFor(INFINITE);
  end;
  FreeAndNil(FThreads);
  FreeAndNil(FCanDieEvents);
  FreeAndNil(FJobs);
  FCanStartEvent.Free;
  FreeAndNil(FCurrentJobResults);
  FreeAndNil(FJobsRunning);
  FCanDieEvent.SetEvent;
  inherited Destroy;
end;

procedure TControlThread.Execute;
var
  i : integer;
  tmpThread : TJobThread;
  tmpJobResult : TJobResult;
  tmpCanDiedEvent : TSimpleEvent;

  lastRunJobIdx : integer;
begin
  try
    while not Terminated do
    begin
      FCanStartEvent.WaitFor(INFINITE);

      lastRunJobIdx := -1;
      FJobsRunning.Clear;

      {$IFDEF DEBUG}
      if not Terminated then
      begin
        logger.Debug('[TControlThread] Scheduled jobs:' + IntToStr(FJobs.Count));
        logger.Debug('[TControlThread] Max parallel jobs:' + IntToStr(FMaxConcurrentThreads));
      end;
      {$ENDIF}

      if (FJobs.Count > 0) then
      begin
        FRunning := true;

        if not Terminated then
        begin
          FCurrentJobResults := TJobResults.Create;

          while (lastRunJobIdx < FJobs.Count - 1) and (FJobsRunning.Count < FMaxConcurrentThreads) do
          begin
            inc (lastRunJobIdx);
            FJobsRunning.Add(lastRunJobIdx);
            {$IFDEF DEBUG}
            logger.Debug('[TControlThread] Starting job ' + IntToStr(lastRunJobIdx) + '...');
            {$ENDIF}

            tmpJobResult := TJobResult.Create;
            FCurrentJobResults.Add(tmpJobResult);
            tmpThread := TJobThread.Create(FJobs.Items[lastRunJobIdx] as TmJob, tmpJobResult);
            tmpCanDiedEvent := TSimpleEvent.Create;
            tmpThread.CanDieEvent := tmpCanDiedEvent;
            FThreads.Add(tmpThread);
            FCanDieEvents.Add(tmpCanDiedEvent);
            tmpThread.CanStartEvent.SetEvent;
          end;
        end;

        while (not Terminated) and (FJobsRunning.Count > 0) do
        begin
          for i:= FJobsRunning.Count - 1 downto 0 do
          begin
            if (FCanDieEvents.Items[FJobsRunning.Items[i]] as TSimpleEvent).WaitFor(10) <> wrTimeout then
            begin
              {$IFDEF DEBUG}
              logger.Debug('[TControlThread] Job ' + IntToStr(FJobsRunning.Items[i]) + ' is terminated');
              {$ENDIF}
              FJobsRunning.Delete(i);
            end;
            if Terminated then
              break;
          end;

          if Terminated then
            break;

          while (not Terminated) and (lastRunJobIdx < FJobs.Count - 1) and (FJobsRunning.Count < FMaxConcurrentThreads) do
          begin
            inc (lastRunJobIdx);
            FJobsRunning.Add(lastRunJobIdx);
            {$IFDEF DEBUG}
            logger.Debug('[TControlThread] Starting job ' + IntToStr(lastRunJobIdx) + '...');
            {$ENDIF}

            tmpJobResult := TJobResult.Create;
            FCurrentJobResults.Add(tmpJobResult);
            tmpThread := TJobThread.Create(FJobs.Items[lastRunJobIdx] as TmJob, tmpJobResult);
            tmpCanDiedEvent := TSimpleEvent.Create;
            tmpThread.CanDieEvent := tmpCanDiedEvent;
            FThreads.Add(tmpThread);
            FCanDieEvents.Add(tmpCanDiedEvent);
            tmpThread.CanStartEvent.SetEvent;
          end;
        end;

        if not Terminated then
        begin
          FCanStartEvent.ResetEvent;
          {$IFDEF DEBUG}
          logger.Debug('[TControlThread] Running callback...');
          {$ENDIF}
          {$IFDEF GUI}
          Synchronize(RunEndCallBack);
          {$ELSE}
          RunEndCallBack;
          {$ENDIF}
          FThreads.Clear;
          FCanDieEvents.Clear;
          FJobs.Clear;
        end;
        FRunning := false;
      end;
    end;
    FCanDieEvent.SetEvent;
  except
    on e:Exception do
    begin
      DumpExceptionBackTrace;
      FLastException := e;
      {$IFDEF GUI}
      Synchronize(ReRaiseLastException);
      {$ELSE}
      ReRaiseLastException;
      {$ENDIF}
    end;
  end;
end;

function TControlThread.GetDebugInfo: string;
begin
  Result:= 'Control thread';
end;


{ TJobThread }

constructor TJobThread.Create(aJob : TmJob; aJobResult : TJobResult);
begin
  inherited Create(false);
  FreeOnTerminate:= false;
  FCanDieEvent := nil;
  FCanStartEvent := TSimpleEvent.Create;
  FJob := aJob;
  FApplication := aJob.Application;
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
      FJob.DoJobProcedure (FProgress, FJob.Data, FJobResult);
      Self.FProgress.Close;
    except
      on e:Exception do
      begin
        DumpExceptionBackTrace;

        FJobResult.FExceptionMessage:= e.Message;
        FJobResult.FGotException:= true;

        if not FJob.TrapExceptions then
        begin
          FLastException := e;
          {$IFDEF NOGUI}
          ReRaiseLastException;
          {$ELSE}
          Synchronize(ReRaiseLastException);
          {$ENDIF}
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

procedure TmBatchExecutor.SetMaxConcurrentThreads(AValue: integer);
begin
  if (FMaxConcurrentThreads=AValue) or (AValue <= 0) then Exit;
  FMaxConcurrentThreads:=AValue;
end;

constructor TmBatchExecutor.Create;
begin
  FRunning:= false;
  MaxConcurrentThreads:= GetCPUCores * 2;
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
    FCanEndEvent.WaitFor(INFINITE);
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

procedure TmBatchExecutor.Execute({$ifdef gui}aParentForm : TForm;{$endif}aCallBack : TOnEndJobCallback);
begin
  {$IFDEF DEBUG}
  logger.Debug('Start execution..');
  {$ENDIF}

  if GetControlThread(Self).Running then
  begin
    {$IFDEF DEBUG}
    logger.Debug('Control thread is not running: aborting...');
    {$ENDIF}
    exit;
  end;

  if GetControlThread(Self).Jobs.Count = 0 then
  begin
    {$IFDEF DEBUG}
    logger.Debug('No scheduled jobs: aborting..,');
    {$ENDIF}
    exit;
  end;


  {$ifdef gui}
  GetControlThread(Self).ParentForm := nil;
  if Assigned(aParentForm) then
  begin
    GetControlThread(Self).ParentForm := aParentForm;
    aParentForm.Cursor := crHourGlass;
    aParentForm.Enabled:= false;
  end;
  {$endif}
  GetControlThread(Self).CallBack:= aCallBack;
  GetControlThread(Self).MaxConcurrentThreads:= FMaxConcurrentThreads;
  FRunning := true;

  GetControlThread(Self).CanStartEvent.SetEvent;
end;


{$IFDEF DEBUG}
initialization
  Logger := logManager.AddLog('mThreads');
{$ENDIF DEBUG}

finalization
  FreeAndNil(internalBatchExecutor);

end.
