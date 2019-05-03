// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mPerformedOperationResults;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

uses
  Classes;

type
  IPerformedOperationResults = interface
    ['{1AFCFE0A-2F11-4A92-8396-C3F70B5181F2}']
    procedure AddError (const aMessage: String);
    procedure AddWarning (const aMessage: String);
    procedure AddInfo (const aMessage: String);
    procedure AddResult (const aMessage: String);
    procedure GetMessagesAsStrings(aResults : TStringList);
  end;



  { TPerformedOperationResultsAsLog }

  TPerformedOperationResultsAsLog = class (IPerformedOperationResults)
  strict private
    FList : TStringList;
    FWarnings : Word;
    FErrors : Word;
    function GetMessages: TStrings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddError (const aMessage: String);
    procedure AddWarning (const aMessage: String);
    procedure AddInfo (const aMessage: String);
    procedure AddResult (const aMessage: String);
    procedure GetMessagesAsStrings(aMessages : TStringList);

    property Messages : TStrings read GetMessages;
    property Warnings : Word read FWarnings;
    property Errors : Word read FErrors;
  end;

implementation

{ TPerformedOperationResultsAsLog }

function TPerformedOperationResultsAsLog.GetMessages: TStrings;
begin
  Result := FList;
end;

constructor TPerformedOperationResultsAsLog.Create;
begin
  FList := TStringList.Create;
  FWarnings:= 0;
  FErrors:= 0;
end;

destructor TPerformedOperationResultsAsLog.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TPerformedOperationResultsAsLog.AddError(const aMessage: String);
begin
  FList.Append('[ERROR] ' + aMessage);
  inc (FErrors);
end;

procedure TPerformedOperationResultsAsLog.AddWarning(const aMessage: String);
begin
  FList.Append ('[WARNING] ' + aMessage);
  inc (FWarnings);
end;

procedure TPerformedOperationResultsAsLog.AddInfo(const aMessage: String);
begin
  FList.Append ('[INFO] ' + aMessage);
end;

procedure TPerformedOperationResultsAsLog.AddResult(const aMessage: String);
begin
  FList.Append('[RESULT] ' + aMessage);
end;

procedure TPerformedOperationResultsAsLog.GetMessagesAsStrings(aMessages: TStringList);
begin
  aMessages.Clear;
  aMessages.AddStrings(FList);
end;

end.
