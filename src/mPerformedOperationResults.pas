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
  Classes, contnrs;

type
  IPerformedOperationResults = interface
    ['{1AFCFE0A-2F11-4A92-8396-C3F70B5181F2}']
    procedure AddError (const aMessage: String; const aMustBeValidated : boolean = false);
    procedure AddWarning (const aMessage: String; const aMustBeValidated : boolean = false);
    procedure AddInfo (const aMessage: String; const aMustBeValidated : boolean = false);
    procedure AddResult (const aMessage: String; const aMustBeValidated : boolean = false);
    procedure GetMessagesAsStrings(aResults : TStringList);
  end;

  { TPerformedOperation }

  TPerformedOperation = class
  strict private
    FMessage : String;
    FMustBeValidated : boolean;
  public
    constructor Create(const aMessage : String; const aMustBeValidated: boolean); overload;
    constructor Create(); overload;
    destructor Destroy; override;

    property Message : String read FMessage write FMessage;
    property MustBeValidated : boolean read FMustBeValidated write FMustBeValidated;
  end;

  { TPerformedOperations }

  TPerformedOperations = class
  strict private
    FList : TObjectList;
  public
    constructor Create();
    destructor Destroy; override;
    function Count : integer;
    function Get(const aIndex : integer): TPerformedOperation;
    function Add : TPerformedOperation; overload;
    procedure Add(aItem : TPerformedOperation); overload;
    procedure Clear;
  end;

  { TPerformedOperationResultsAsLog }

  TPerformedOperationResultsAsLog = class (IPerformedOperationResults)
  strict private
    FErrors : TPerformedOperations;
    FWarnings : TPerformedOperations;
    FInfos : TPerformedOperations;
    FResults : TPerformedOperations;
    FMessages : TStringList;

    function GetMessages: TStrings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddError (const aMessage: String; const aMustBeValidated : boolean = false);
    procedure AddWarning (const aMessage: String; const aMustBeValidated : boolean = false);
    procedure AddInfo (const aMessage: String; const aMustBeValidated : boolean = false);
    procedure AddResult (const aMessage: String; const aMustBeValidated : boolean = false);
    procedure GetMessagesAsStrings(aMessages : TStringList);

    property Messages : TStrings read GetMessages;
    property Warnings : TPerformedOperations read FWarnings;
    property Errors : TPerformedOperations read FErrors;
    property Infos : TPerformedOperations read FInfos;
    property Results : TPerformedOperations read FResults;
  end;

implementation

{ TPerformedOperations }

constructor TPerformedOperations.Create();
begin
  FList := TObjectList.Create(true);
end;

destructor TPerformedOperations.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TPerformedOperations.Count: integer;
begin
  Result := FList.Count;
end;

function TPerformedOperations.Get(const aIndex: integer): TPerformedOperation;
begin
  Result := FList.Items[aIndex] as TPerformedOperation;
end;

function TPerformedOperations.Add: TPerformedOperation;
begin
  Result := TPerformedOperation.Create();
  Self.Add(Result);
end;

procedure TPerformedOperations.Add(aItem: TPerformedOperation);
begin
  FList.Add(aItem);
end;

procedure TPerformedOperations.Clear;
begin
  FList.Clear;
end;

{ TPerformedOperation }

constructor TPerformedOperation.Create(const aMessage : String; const aMustBeValidated: boolean);
begin
  FMessage:= aMessage;
  FMustBeValidated:= aMustBeValidated;
end;

constructor TPerformedOperation.Create();
begin
  Self.Create('', false);
end;

destructor TPerformedOperation.Destroy;
begin
  inherited Destroy;
end;

{ TPerformedOperationResultsAsLog }

function TPerformedOperationResultsAsLog.GetMessages: TStrings;
begin
  Result := FMessages;
end;

constructor TPerformedOperationResultsAsLog.Create;
begin
  FErrors := TPerformedOperations.Create();
  FWarnings := TPerformedOperations.Create();
  FInfos := TPerformedOperations.Create();
  FResults := TPerformedOperations.Create();
  FMessages := TStringList.Create;
end;

destructor TPerformedOperationResultsAsLog.Destroy;
begin
  FErrors.Free;
  FWarnings.Free;
  FInfos.Free;
  FResults.Free;
  FMessages.Free;
  inherited Destroy;
end;

procedure TPerformedOperationResultsAsLog.Clear;
begin
  FErrors.Clear;
  FWarnings.Clear;
  FInfos.Clear;
  FResults.Clear;
  FMessages.Clear;
end;

procedure TPerformedOperationResultsAsLog.AddError(const aMessage: String; const aMustBeValidated : boolean = false);
begin
  FMessages.Append('[ERROR] ' + aMessage);
  FErrors.Add(TPerformedOperation.Create(aMessage, aMustBeValidated));
end;

procedure TPerformedOperationResultsAsLog.AddWarning(const aMessage: String; const aMustBeValidated : boolean = false);
begin
  FMessages.Append ('[WARNING] ' + aMessage);
  FWarnings.Add(TPerformedOperation.Create(aMessage, aMustBeValidated));
end;

procedure TPerformedOperationResultsAsLog.AddInfo(const aMessage: String; const aMustBeValidated : boolean = false);
begin
  FMessages.Append ('[INFO] ' + aMessage);
  FInfos.Add(TPerformedOperation.Create(aMessage, aMustBeValidated));
end;

procedure TPerformedOperationResultsAsLog.AddResult(const aMessage: String; const aMustBeValidated : boolean = false);
begin
  FMessages.Append('[RESULT] ' + aMessage);
  FResults.Add(TPerformedOperation.Create(aMessage, aMustBeValidated));
end;

procedure TPerformedOperationResultsAsLog.GetMessagesAsStrings(aMessages: TStringList);
begin
  aMessages.Clear;
  aMessages.AddStrings(FMessages);
end;

end.
