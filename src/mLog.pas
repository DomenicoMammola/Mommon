// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mLog;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, Contnrs, SyncObjs;

type

  TmLogMessageLevel = (mlDebug, mlInfo, mlWarning, mlError);

  TmLogMessage = class
  strict private
    FMessage : string;
    FContext : string;
    FLevel : TmLogMessageLevel;
    FDateTime : TDateTime;
  private
    procedure SetMessage (aLevel : TmLogMessageLevel; aContext, aMessage : string);
    procedure CopyFrom (aMessage : TmLogMessage);

    property Message : string read FMessage;
    property Context : string read FContext;
    property Level : TmLogMessageLevel read FLevel;
    property DateTime : TDateTime read FDateTime;
  end;

  { TmLogMessageList }

  TmLogMessageList = class
  strict private
    FList : TObjectList;
    FCriticalSection : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PushMessage (aLevel : TmLogMessageLevel; aContext, aMessage : string);
    function PullMessage: TmLogMessage;
    function Empty : boolean;
  end;

  TmLogManager = class;

  TmLog = class
  private
    FContext : string;
    FLogManager : TmLogManager;
  public
    procedure Debug(aMessage : string);
    procedure Info(aMessage : string);
    procedure Warning(aMessage : string);
    procedure Error(aMessage : string);
  end;

  TmLogPublisher = class
  private
    FActive : boolean;
  protected
    function GetFormattedString (aContext, aLevel, aMessage : string; aDate : TDateTime) : string;
    procedure AfterCreate; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function ActInsideMainThread : boolean; virtual; abstract;
    procedure Publish (aContext, aLevel, aMessage : string; aDate : TDateTime); virtual; abstract;
    function Level : TmLogMessageLevel; virtual; abstract;

    property Active : boolean read FActive write FActive;
  end;

  TmLogPublisherClass = class of TmLogPublisher;

  { TmLogManager }

  TmLogManager = class
  strict private
    FThread : TThread;
    FEndThreadEvent : TEvent;

    FVCLThread : TThread;
    FVCLEndThreadEvent : TEvent;
    FLogs : TObjectList;
  private
    function GetTrashMessagesOnQuit: boolean;
    procedure PushMessage (aLevel : TmLogMessageLevel; aContext, aMessage : string);
    procedure SetTrashMessagesOnQuit(AValue: boolean);
  public
    function AddLog (loggerContext : string): TmLog;
    procedure AddPublisher(aPublisher:TmLogPublisher; aOwned : boolean);
    procedure RemovePublisher(aPublisher:TmLogPublisher);

    constructor Create;
    destructor Destroy; override;

    property TrashMessagesOnQuit : boolean read GetTrashMessagesOnQuit write SetTrashMessagesOnQuit;
  end;

  function logManager : TmLogManager;
  function logMessageLevelToStr (aLevel : TmLogMessageLevel) : string;

implementation

uses
  SysUtils;

var
  InternalLogManager : TmLogManager;

type

  { TmLogPublisherThread }

  TmLogPublisherThread = class (TThread)
  strict private
    FStartEvent : TEvent;
    FEndEvent : TEvent;
    FLogManager : TmLogManager;
    FMessages : TmLogMessageList;
    FPublishersCriticalSection : TCriticalSection;
    FPublishers : TObjectList;
    FOwnedPublishers : TList;

    FCurrentPublisher : TmLogPublisher;
    FCurrentMsg : TmLogMessage;
    FTrashMessagesOnQuit : boolean;

    procedure PublishOnMainThread;
    function LevelsAreCompatible(levelOfMessage, levelOfPublisher : TmLogMessageLevel) : boolean;
  protected
    procedure Execute; override;
  public
    constructor Create (aEndEvent : TEvent; aLogManager : TmLogManager);
    destructor Destroy; override;
    procedure AddPublisher (aPublisher : TmLogPublisher; aOwned : boolean);
    procedure RemovePublisher(aPublisher : TmLogPublisher);
    procedure ClearEndEvent;

    property StartEvent : TEvent read FStartEvent;
    property EndEvent : TEvent read FEndEvent;
    property Messages : TmLogMessageList read FMessages;
    property TrashMessagesOnQuit : boolean read FTrashMessagesOnQuit write FTrashMessagesOnQuit;
  end;

  TmDeleteLogPublisherThread = class (TThread)
  private
    FPublisher : TmLogPublisher;
    procedure DeletePublisher;
  protected
    procedure Execute; override;
  public
    constructor Create (aPublisher : TmLogPublisher);
  end;

{ TmLogManager }

function TmLogManager.GetTrashMessagesOnQuit: boolean;
begin
  Result := (FThread as TmLogPublisherThread).TrashMessagesOnQuit;
end;

procedure TmLogManager.PushMessage(aLevel : TmLogMessageLevel; aContext, aMessage : string);
begin
  (FThread as TmLogPublisherThread).Messages.PushMessage(aLevel, aContext, aMessage);
  (FThread as TmLogPublisherThread).StartEvent.SetEvent;

  (FVCLThread as TmLogPublisherThread).Messages.PushMessage(aLevel, aContext, aMessage);
  (FVCLThread as TmLogPublisherThread).StartEvent.SetEvent;
end;

procedure TmLogManager.SetTrashMessagesOnQuit(AValue: boolean);
begin
  (FThread as TmLogPublisherThread).TrashMessagesOnQuit:= AValue;
  (FVCLThread as TmLogPublisherThread).TrashMessagesOnQuit:= AValue;
end;



procedure TmLogManager.RemovePublisher(aPublisher: TmLogPublisher);
begin
  if aPublisher.ActInsideMainThread then
    (FVCLThread as TmLogPublisherThread).RemovePublisher(aPublisher)
  else
    (FThread as TmLogPublisherThread).RemovePublisher(aPublisher);
end;

constructor TmLogManager.Create;
begin
  FEndThreadEvent := TEvent.Create{$IFDEF FPC}(nil, True, False, 'mLogManager_EndThreadEvent'){$ENDIF};
  FThread := TmLogPublisherThread.Create(FEndThreadEvent, Self);

  FVCLEndThreadEvent := TEvent.Create{$IFDEF FPC}(nil, True, False, 'mLogManager_VCLEndThreadEvent'){$ENDIF};
  FVCLThread := TmLogPublisherThread.Create(FVCLEndThreadEvent, Self);

  FLogs := TObjectList.Create(true);
end;

procedure TmLogManager.AddPublisher(aPublisher:TmLogPublisher; aOwned : boolean);
begin
  if aPublisher.ActInsideMainThread then
    (FVCLThread as TmLogPublisherThread).AddPublisher(aPublisher, aOwned)
  else
    (FThread as TmLogPublisherThread).AddPublisher(aPublisher, aOwned);
end;

function TmLogManager.AddLog(loggerContext: string): TmLog;
begin
  Result := TmLog.Create;
  Result.FContext := loggerContext;
  Result.FLogManager := LogManager;
  FLogs.Add(Result);
end;


destructor TmLogManager.Destroy;
begin
  (FThread as TmLogPublisherThread).Terminate;
  (FThread as TmLogPublisherThread).StartEvent.SetEvent;
  FEndThreadEvent.WaitFor(INFINITE);
  (FThread as TmLogPublisherThread).ClearEndEvent; // this is necessary to handle random AV when application terminates
  FEndThreadEvent.Free;
  FThread.Free;

  (FVCLThread as TmLogPublisherThread).Terminate;
  (FVCLThread as TmLogPublisherThread).StartEvent.SetEvent;
  FVCLEndThreadEvent.WaitFor(INFINITE);
  (FVCLThread as TmLogPublisherThread).ClearEndEvent;// this is necessary to handle random AV when application terminates
  FVCLEndThreadEvent.Free;
  FVCLThread.Free;

  FLogs.Free;

  inherited;
end;



{ TmLog }

procedure TmLog.Debug(aMessage: string);
begin
  FLogManager.PushMessage(mlDebug, Self.FContext, aMessage);
end;

procedure TmLog.Error(aMessage: string);
begin
  FLogManager.PushMessage(mlError, Self.FContext, aMessage);
end;

procedure TmLog.Info(aMessage: string);
begin
  FLogManager.PushMessage(mlInfo, Self.FContext, aMessage);
end;

procedure TmLog.Warning(aMessage: string);
begin
  FLogManager.PushMessage(mlWarning, Self.FContext, aMessage);
end;

{ TmLogPublisherThread }

procedure TmLogPublisherThread.AddPublisher(aPublisher: TmLogPublisher; aOwned : boolean);
begin
  FPublishersCriticalSection.Acquire;
  try
    if FPublishers.IndexOf(aPublisher) < 0 then
    begin
      FPublishers.Add(aPublisher);
      if aOwned then
        FOwnedPublishers.Add(aPublisher);
    end;
  finally
    FPublishersCriticalSection.Leave;
  end;
end;

constructor TmLogPublisherThread.Create(aEndEvent : TEvent; aLogManager : TmLogManager);
begin
  inherited Create(false);
  Self.FreeOnTerminate:= false;
  FStartEvent := TEvent.Create{$IFDEF FPC}(nil, True, False, ''){$ENDIF};
  FStartEvent.ResetEvent;
  FEndEvent := aEndEvent;
  FEndEvent.ResetEvent;
  FLogManager := aLogManager;
  FMessages := TmLogMessageList.Create;
  FPublishers := TObjectList.Create(false);
  FOwnedPublishers := TList.Create;
  FPublishersCriticalSection := TCriticalSection.Create;
  FTrashMessagesOnQuit := false;
end;

destructor TmLogPublisherThread.Destroy;
var
  i : integer;
begin
  FreeAndNil(FStartEvent);
  FreeAndNil(FMessages);
  FreeAndNil(FPublishers);
  FreeAndNil(FPublishersCriticalSection);
  if Assigned(FCurrentMsg) then
    FreeAndNil(FCurrentMsg);
  try
    for i:= 0 to FOwnedPublishers.Count-1 do
      TmLogPublisher(FOwnedPublishers.Items[i]).Free;
  except
    //
  end;
  FreeAndNil(FOwnedPublishers);

  inherited;
end;

procedure TmLogPublisherThread.Execute;
var
  i : integer;
begin
  FStartEvent.WaitFor(INFINITE);

  if not Self.Terminated then
  begin
    try
      while (not Self.Terminated) or ((not FTrashMessagesOnQuit) and Assigned(FMessages) and (not FMessages.Empty)) do
      begin
        FreeAndNil(FCurrentMsg);
        FCurrentMsg := FMessages.PullMessage;
        while (FCurrentMsg <> nil) do
        begin
          if (not FTrashMessagesOnQuit) or (not Self.Terminated) then
          begin
            FPublishersCriticalSection.Acquire;
            try
              if Assigned(FPublishers) then
              begin
                for i := 0 to FPublishers.Count - 1 do
                begin
                  FCurrentPublisher := FPublishers.Items[i] as TmLogPublisher;
                  if FCurrentPublisher.Active then
                  begin
                    if LevelsAreCompatible(FCurrentMsg.Level, FCurrentPublisher.Level) then
                    begin
                      if FCurrentPublisher.ActInsideMainThread then
                        Self.Synchronize(PublishOnMainThread)
                      else
                        FCurrentPublisher.Publish(FCurrentMsg.Context, logMessageLevelToStr(FCurrentMsg.Level), FCurrentMsg.Message, FCurrentMsg.DateTime);
                    end;
                  end;
                end;
              end;
            finally
              FPublishersCriticalSection.Leave;
            end;
            FreeAndNil(FCurrentMsg);
            FCurrentMsg := FMessages.PullMessage;
          end
          else
            break;
        end;

        if not Self.Terminated then
        begin
          FStartEvent.ResetEvent;
          FStartEvent.WaitFor(INFINITE);
        end;
      end;
    except
      on e: Exception do
      begin
        FEndEvent.SetEvent;
        raise;
      end;
    end;
  end;

  if Assigned(FEndEvent) then
    FEndEvent.SetEvent;
end;

function TmLogPublisherThread.LevelsAreCompatible(levelOfMessage, levelOfPublisher: TmLogMessageLevel): boolean;
begin
  Result := integer(levelOfMessage) >= integer (levelOfPublisher);
end;

procedure TmLogPublisherThread.PublishOnMainThread;
begin
  if Assigned(FCurrentPublisher) and FCurrentPublisher.Active and Assigned(FCurrentMsg) and (not Self.Terminated) then
    FCurrentPublisher.Publish(FCurrentMsg.Context, logMessageLevelToStr(FCurrentMsg.Level), FCurrentMsg.Message, FCurrentMsg.DateTime);
end;

procedure TmLogPublisherThread.RemovePublisher(aPublisher: TmLogPublisher);
begin
  FPublishersCriticalSection.Acquire;
  try
    FPublishers.Remove(aPublisher);
    FOwnedPublishers.Remove(aPublisher);
  finally
    FPublishersCriticalSection.Leave;
  end;
end;

procedure TmLogPublisherThread.ClearEndEvent;
begin
  FEndEvent := nil;
end;

function logManager : TmLogManager;
begin
 if not Assigned(InternalLogManager) then
   InternalLogManager := TmLogManager.Create;
 Result := InternalLogManager;
end;

{ TmLogPublisher }


constructor TmLogPublisher.Create;
begin
  FActive := false;
  Self.AfterCreate;
end;

destructor TmLogPublisher.Destroy;
begin
  inherited;
end;

function TmLogPublisher.GetFormattedString(aContext, aLevel, aMessage: string; aDate: TDateTime): string;
begin
  Result := '[' + FormatDateTime('dd/mm/yyyy hh:nn:ss.zzz', aDate) + '] [' + aLevel + '] [' + aContext + '] ' + aMessage;
end;

{ TmLogMessage }


procedure TmLogMessage.CopyFrom(aMessage: TmLogMessage);
begin
  Self.FLevel := aMessage.FLevel;
  Self.FContext := aMessage.FContext;
  Self.FDateTime := aMessage.DateTime;
  Self.FMessage := aMessage.FMessage;
end;

procedure TmLogMessage.SetMessage(aLevel : TmLogMessageLevel; aContext, aMessage : string);
begin
  Self.FLevel := aLevel;
  Self.FContext := aContext;
  Self.FMessage := aMessage;
  Self.FDateTime := Now;
end;


function logMessageLevelToStr (aLevel : TmLogMessageLevel) : string;
begin
  case aLevel of
    mlDebug: Result := 'DEBUG';
    mlInfo: Result := 'INFO';
    mlWarning: Result := 'WARNING';
    mlError : Result := 'ERROR'
  else
    Result := 'UNKNOWN';
  end;
end;

{ TmDeleteLogPublisherThread }

constructor TmDeleteLogPublisherThread.Create(aPublisher: TmLogPublisher);
begin
  inherited Create(false);
  FPublisher := aPublisher;
  Self.FreeOnTerminate := true;
end;

procedure TmDeleteLogPublisherThread.DeletePublisher;
begin
end;

procedure TmDeleteLogPublisherThread.Execute;
begin
(*  FPublisher.FCriticalSection.Acquire;
  logManager.FPublishersCriticalSection.Acquire;
  try
    Self.Synchronize(DeletePublisher);
  finally
    logManager.FPublishersCriticalSection.Free;
  end;*)
end;

{ TmLogMessageList }

constructor TmLogMessageList.Create;
begin
  FList := TObjectList.Create(true);
  FCriticalSection := TCriticalSection.Create;
end;

destructor TmLogMessageList.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FCriticalSection);
  inherited;
end;

function TmLogMessageList.PullMessage: TmLogMessage;
var
  msg : TmLogMessage;
begin
  FCriticalSection.Acquire;
  try
    if FList.Count > 0 then
    begin
      msg := FList.Items[0] as TmLogMessage;
      Result := TmLogMessage.Create;
      Result.CopyFrom(msg);
      FList.Delete(0);
    end
    else
      Result := nil;
  finally
    FCriticalSection.Leave;
  end;
end;

function TmLogMessageList.Empty: boolean;
begin
 Result := true;
 if Assigned(FCriticalSection) then
 begin
    FCriticalSection.Acquire;
    try
      if Assigned(FList) then
        Result := (FList.Count = 0);
    finally
      FCriticalSection.Leave;
    end;
 end;
end;

procedure TmLogMessageList.PushMessage(aLevel: TmLogMessageLevel; aContext, aMessage: string);
var
  msg : TmLogMessage;
begin
  FCriticalSection.Acquire;
  try
    msg := TmLogMessage.Create;
    msg.SetMessage(aLevel, aContext, aMessage);
    FList.Add(msg);
  finally
    FCriticalSection.Leave;
  end;
end;

initialization

finalization

if Assigned(InternalLogManager) then
  FreeAndNil(InternalLogManager);

end.
