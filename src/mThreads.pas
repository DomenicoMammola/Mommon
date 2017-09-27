unit mThreads;

{$mode objfpc}{$H+}

interface

uses
  Classes, contnrs, syncobjs, Forms, SysUtils,
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

//    procedure SetMaxNumberOfThreads(AValue: integer);
//    procedure RunEndCallBack;
  public
    constructor Create;
    destructor Destroy; override;

    function QueueJob : TmJob;
    procedure Execute (aCallBack : TOnEndCallback);

//    property MaxNumberOfThreads : integer read GetMaxNumberOfThreads write SetMaxNumberOfThreads;
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

    property CanDieEvent : TSimpleEvent read FCanDieEvent;
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
    //FMaxNumberOfThreads : integer;
    FCallBack : TOnEndCallback;
    FRunning : boolean;

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
  end;

var
  internalBatchExecutor : TmBatchExecutor;

function GetControlThread (aExecutor : TmBatchExecutor) : TControlThread;
begin
  Result := aExecutor.FControlThread as TControlThread;
end;

function GetThread (aList : TObjectList; aIndex: integer) : TJobThread;
begin
  Result := aList.Items[aIndex] as TJobThread;
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
  FCallBack();
end;

constructor TControlThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate:= false;
  FCanStartEvent := TSimpleEvent.Create;
  FJobs := TObjectList.Create (true);
  FThreads := TObjectList.Create(true);
end;

destructor TControlThread.Destroy;
var
  i : integer;
begin
  for i := 0 to FThreads.Count - 1 do
  begin
    GetThread(FThreads, i).Terminate;
    GetThread(FThreads, i).CanDieEvent.WaitFor(3000);
  end;
  FreeAndNil(FThreads);
  FreeAndNil(FJobs);
  FCanStartEvent.Free;
  FCanDieEvent.SetEvent;
  inherited Destroy;
end;

procedure TControlThread.Execute;
var
  i : integer;
  currentJob, runningJobs : integer;
  tmpThread : TJobThread;
begin
  while not Terminated do
  begin
    FCanStartEvent.WaitFor(INFINITE);

    FRunning := true;
    runningJobs := FJobs.Count;
    for i := 0 to FJobs.Count -1 do
    begin
      tmpThread := TJobThread.Create(FJobs.Items[i] as TmJob);
      FThreads.Add(tmpThread);
      tmpThread.CanStartEvent.SetEvent;
    end;

    while (not Terminated) and (runningJobs > 0) do
    begin
      for i := 0 to FJobs.Count - 1 do
      begin
        if GetThread(FThreads, i).CanDieEvent.WaitFor(10) <> wrTimeout then
          dec(runningJobs);
        if Terminated or (runningJobs = 0) then
          break;
      end;
    end;

    FCanStartEvent.ResetEvent;
    Synchronize(@RunEndCallBack);
    FJobs.Clear;
    FThreads.Clear;
    FRunning := false;
  end;
  FCanDieEvent.SetEvent;
end;

{ TJobThread }

constructor TJobThread.Create(aJob : TmJob);
begin
  inherited Create(false);
  FreeOnTerminate:= false;
  FCanDieEvent := TSimpleEvent.Create;
  FCanStartEvent := TSimpleEvent.Create;
  FJob := aJob;
  Self.Priority:= tpNormal;
end;

destructor TJobThread.Destroy;
begin
  FCanDieEvent.Free;
  FCanStartEvent.Free;
  FProgress.Free;
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

(*
procedure TmBatchExecutor.SetMaxNumberOfThreads(AValue: integer);
begin
  if FRunning then
    exit;
  if FMaxNumberOfThreads=AValue then Exit;
  FMaxNumberOfThreads:=AValue;
end;*)

(*procedure TmBatchExecutor.RunEndCallBack;
begin
  if FRunning then
  begin
    FRunning:=false;
    if Assigned(FCallBack) then
      FCallBack();
  end;
end;*)

constructor TmBatchExecutor.Create;
begin
  FRunning:= false;
//  FMaxNumberOfThreads:= GetCPUCores;
  FCanEndEvent := TSimpleEvent.Create;
  FControlThread := TControlThread.Create;
  GetControlThread(Self).CanDieEvent := FCanEndEvent;
end;

destructor TmBatchExecutor.Destroy;
begin
  if Assigned(FControlThread) then
  begin
    FControlThread.Terminate;
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

procedure TmBatchExecutor.Execute(aCallBack : TOnEndCallback);
begin
  if GetControlThread(Self).Running then
    exit;

  if GetControlThread(Self).Jobs.Count = 0 then
    exit;

  GetControlThread(Self).CallBack:= aCallBack;
  FRunning := true;

  GetControlThread(Self).CanStartEvent.SetEvent;
end;

finalization

FreeAndNil(internalBatchExecutor);

end.
