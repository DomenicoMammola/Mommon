// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mLogPublishers;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$ifdef lcl}Forms, StdCtrls, Controls,{$endif}
  sysutils, Classes,
  mLog;

const
  SHOW_LOG_MEMO_FORM_COMMAND_LINE_PARAMETER = 'showlog';

type
  {$ifdef lcl}
  TmMemoPublisher = class (TmLogPublisher)
  strict private
    FCurrentLevel : TmLogMessageLevel;
    FForm : TForm;
    FMemo : TMemo;
  public
    procedure AfterCreate; override;
    destructor Destroy; override;

    function ActInsideMainThread : boolean; override;
    procedure Publish (aContext, aLevel, aMessage : string; aDate : TDateTime); override;
    function Level : TmLogMessageLevel; override;

    property CurrentLevel : TmLogMessageLevel read FCurrentLevel write FCurrentLevel;
  end;
  {$endif}

  TmFilePublisher = class (TmLogPublisher)
  strict private
    FCurrentLevel : TmLogMessageLevel;
    FFileName : TFileName;
    FFileStream : TFileStream;
    FMaxFileSizeInMb : integer;
  public
    procedure AfterCreate; override;
    destructor Destroy; override;

    function ActInsideMainThread : boolean; override;
    procedure Publish (aContext, aLevel, aMessage : string; aDate : TDateTime); override;
    function Level : TmLogMessageLevel; override;

    property CurrentLevel : TmLogMessageLevel read FCurrentLevel write FCurrentLevel;
    property FileName : TFileName read FFileName write FFileName;
    property MaxFileSizeInMb : integer read FMaxFileSizeInMb write FMaxFileSizeInMb;
  end;

  { TmConsolePublisher }

  TmConsolePublisher = class (TmLogPublisher)
  strict private
    FCurrentLevel: TmLogMessageLevel;
  public
    procedure AfterCreate; override;
    destructor Destroy; override;

    function ActInsideMainThread : boolean; override;
    procedure Publish (aContext, aLevel, aMessage : string; aDate : TDateTime); override;
    function Level : TmLogMessageLevel; override;

    property CurrentLevel : TmLogMessageLevel read FCurrentLevel write FCurrentLevel;
  end;

implementation

uses
  FileUtil,
  mUtility;

{ TmConsolePublisher }

procedure TmConsolePublisher.AfterCreate;
begin

end;

destructor TmConsolePublisher.Destroy;
begin
  inherited Destroy;
end;

function TmConsolePublisher.ActInsideMainThread: boolean;
begin
  Result := false;
end;

procedure TmConsolePublisher.Publish(aContext, aLevel, aMessage: string; aDate: TDateTime);
begin
  WriteLn(Self.GetFormattedString(aContext, aLevel, aMessage, aDate));
end;

function TmConsolePublisher.Level: TmLogMessageLevel;
begin
  Result := FCurrentLevel;
end;

{$ifdef lcl}

function TmMemoPublisher.ActInsideMainThread: boolean;
begin
  Result := true;
end;

procedure TmMemoPublisher.AfterCreate;
begin
  FCurrentLevel := mlDebug;
  FForm := nil;
end;

destructor TmMemoPublisher.Destroy;
begin
  FreeAndNil(FForm);
  inherited;
end;

function TmMemoPublisher.Level: TmLogMessageLevel;
begin
  Result := FCurrentLevel;
end;

procedure TmMemoPublisher.Publish(aContext, aLevel, aMessage : string; aDate : TDateTime);
begin
  if (FForm = nil) and Application.HasOption(SHOW_LOG_MEMO_FORM_COMMAND_LINE_PARAMETER) then
  begin
    FForm := TForm.Create(nil);
    FMemo := TMemo.Create(FForm);
    FMemo.Parent := FForm;
    FMemo.Align := alClient;
    FMemo.ScrollBars:= ssAutoVertical;
    FForm.Show;
  end;
  if (FForm <> nil) and (FMemo <> nil) then
    FMemo.Lines.Append(Self.GetFormattedString(aContext, aLevel, aMessage, aDate));
end;
{$endif}

{ TmFilePublisher }

function TmFilePublisher.ActInsideMainThread: boolean;
begin
  Result := false;
end;

procedure TmFilePublisher.AfterCreate;
begin
  FFileName := '';
  FFileStream := nil;
  FMaxFileSizeInMb:= 0;
end;

destructor TmFilePublisher.Destroy;
begin
  if Assigned(FFileStream) then
    FFileStream.WriteByte(10);

  FreeAndNil(FFileStream);
  inherited;
end;

function TmFilePublisher.Level: TmLogMessageLevel;
begin
  Result := FCurrentLevel;
end;

procedure TmFilePublisher.Publish(aContext, aLevel, aMessage: string; aDate: TDateTime);
var
  s: RawByteString;
begin
  if FFileName <> '' then
  begin
    if not Assigned(FFileStream) then
    begin
      if FileExists(FFileName) then
      begin
        (*
        if (FMaxFileSizeInMb > 0) and ((FileUtil.FileSize(FFileName) div 1024) > FMaxFileSizeInMb) then
        begin
          RenameFile(FFileName, FFileName + '.bak');
        end;
        *)
        FFileStream := TFileStream.Create(FFileName, fmOpenWrite);
        FFileStream.Seek(0, soFromEnd);
      end
      else
      begin
        FFileStream := TFileStream.Create(FFileName, fmCreate);
        AddUTF8BOMToStream(FFileStream);
      end;
    end;
    s := UTF8Encode(Self.GetFormattedString(aContext, aLevel, aMessage, aDate) + sLineBreak);
    FFileStream.Write(s[1], Length(s));
  end;
end;

end.
