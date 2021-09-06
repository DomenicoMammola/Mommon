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

{$I mDefines.inc}

interface

uses
  {$ifdef gui}Forms, StdCtrls, Controls,{$endif}
  sysutils, Classes,
  mLog;

const
  SHOW_LOG_MEMO_FORM_COMMAND_LINE_PARAMETER = 'showlog';

type
  {$ifdef gui}
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

  { TmFilePublisher }

  TmFilePublisher = class (TmLogPublisher)
  strict private
    FCurrentLevel : TmLogMessageLevel;
    FFileName : TFileName;
    FFileFolder : TFileName;
    FFileNameTemplate : TFileName;
    FFileStream : TFileStream;
    FCycleEveryDay : boolean;
    FKeepDays : integer;
    FFileDay : integer;

    function EncodeFileName (const aDay : integer): String;
    function DecodeFileName (const aFileName: String) : integer;
    procedure ClearOldLogFiles;
  public
    procedure AfterCreate; override;
    destructor Destroy; override;

    function ActInsideMainThread : boolean; override;
    procedure Publish (aContext, aLevel, aMessage : string; aDate : TDateTime); override;
    function Level : TmLogMessageLevel; override;

    property CurrentLevel : TmLogMessageLevel read FCurrentLevel write FCurrentLevel;
    property FileName : TFileName read FFileName write FFileName;

    property FileFolder : TFileName read FFileFolder write FFileFolder;
    property FileNameTemplate : TFileName read FFileNameTemplate write FFileNameTemplate;
    property CycleEveryDay : boolean read FCycleEveryDay write FCycleEveryDay;
    property KeepDays : integer read FKeepDays write FKeepDays;
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
{$IFDEF FPC}
  FileUtil, LazFileUtils,
{$ENDIF}
  mUtility, mMathUtility;

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

{$ifdef gui}

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
var
  hasOption : boolean;
  {$IFDEF DELPHI}
  i : integer;
  {$ENDIF}
begin
  if (FForm = nil) then
  begin
    {$IFDEF FPC}
    hasOption := Application.HasOption(SHOW_LOG_MEMO_FORM_COMMAND_LINE_PARAMETER);
    {$ELSE}
    hasOption := false;
    for i := 0 to ParamCount do
    begin
      if paramstr(i) = SHOW_LOG_MEMO_FORM_COMMAND_LINE_PARAMETER then
      begin
        hasOption := true;
        break;
      end;
    end;
    {$ENDIF}
    if hasOption then
    begin
      FForm := TForm.Create(nil);
      FMemo := TMemo.Create(FForm);
      FMemo.Parent := FForm;
      {$IFDEF FPC}FForm.ShowInTaskBar:= stAlways;{$ENDIF}
      FMemo.Align := alClient;
      FMemo.ScrollBars:= {$IFDEF FPC}ssAutoVertical{$ELSE}ssBoth{$ENDIF};
      FForm.Show;
      FForm.BringToFront;
    end;
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

function TmFilePublisher.EncodeFileName(const aDay: integer): String;
begin
  if FFileFolder = '' then
    FFileFolder := ExtractFileDir(FFileName);
  if FFileNameTemplate = '' then
    FFileNameTemplate:= LazFileUtils.ExtractFileNameOnly(FFileName);
  Result := IncludeTrailingPathDelimiter(FFileFolder) + mUtility.GetTimeStampForFileName(aDay, false) + FFileNameTemplate;
end;

function TmFilePublisher.DecodeFileName(const aFileName: String): integer;
var
  tmp : String;
begin
  Result := -1;
  tmp := LazFileUtils.ExtractFileNameOnly(aFileName);
  if Length(tmp) >= 8 then
  begin
    tmp := Copy(tmp, 1, 8);
    if IsNumeric(tmp, false, false) then
      Result := round(DecodeTimeStampForFileName(tmp));
  end;
end;

procedure TmFilePublisher.ClearOldLogFiles;
var
  tmpFiles : TStringList;
  i, curDay : integer;
begin
  if FCycleEveryDay and (FKeepDays > 0) then
  begin
    tmpFiles := TStringList.Create;
    try
      FindAllFiles(tmpFiles, FFileFolder, '*' + FFileNameTemplate);
      for i := 0 to tmpFiles.Count - 1 do
      begin
        curDay := DecodeFileName(tmpFiles.Strings[i]);
        if curDay > 0 then
        begin
          if (round(Date) - curDay) > FKeepDays then
            DeleteFile(tmpFiles.Strings[i]);
        end;
      end;
    finally
      tmpFiles.Free;
    end;
  end;
end;

procedure TmFilePublisher.AfterCreate;
begin
  FFileName := '';
  FFileFolder:= '';
  FFileNameTemplate:= '';
  FFileStream := nil;
  FCycleEveryDay := false;
  FKeepDays := 0;
  FFileDay := 0;
end;

destructor TmFilePublisher.Destroy;
begin
  if Assigned(FFileStream) then
  {$IFDEF FPC}
    FFileStream.WriteByte(10);
  {$ELSE}
    FFileStream.WriteData([10], 1);
  {$ENDIF}

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
  actualFileName : String;
begin
  if FFileName = '' then
    FFileName:= IncludeTrailingPathDelimiter(FFileFolder) + FFileNameTemplate;

  if FFileName <> '' then
  begin
    if FCycleEveryDay and (FFileDay <> trunc(Date)) then
    begin
      if Assigned(FFileStream) then
      begin
        FFileStream.WriteByte(10);
        FreeAndNil(FFileStream);
      end;
      ClearOldLogFiles;
    end;

    if not Assigned(FFileStream) then
    begin
      if FCycleEveryDay then
      begin
        FFileDay:= trunc(Date);
        actualFileName:= EncodeFileName(FFileDay);
        ClearOldLogFiles;
      end
      else
        actualFileName:= FFileName;

      if FileExists(actualFileName) then
      begin
        FFileStream := TFileStream.Create(actualFileName, fmOpenWrite);
        FFileStream.Seek(0, soFromEnd);
      end
      else
      begin
        FFileStream := TFileStream.Create(actualFileName, fmCreate);
        AddUTF8BOMToStream(FFileStream);
      end;
    end;
    s := UTF8Encode(Self.GetFormattedString(aContext, aLevel, aMessage, aDate) + sLineBreak);
    FFileStream.Write(s[1], Length(s));
  end;
end;

end.
