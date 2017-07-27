// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDatabaseConnectionClasses;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  DB, Contnrs, SysUtils, Variants,
  mNullables, mXML;

type
  TmDataConnectionException = class (Exception);

  TmDatabaseVendor = (dvUnknown, dvSQLServer);

  TmParameterDataType = (ptUnknown, ptDate, ptDateTime, ptInteger, ptFloat, ptString, ptWideString);

{ TmQueryParameter }

  TmQueryParameter = class
  strict private
    FName : String;
    FValue : Variant;
    FDataType : TmParameterDataType;
    function GetAsDate: TDate;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsWideString: WideString;
    procedure SetAsDate(AValue: TDate);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsFloat(AValue: Double);
    procedure SetAsInteger(AValue: Integer);
    procedure SetAsString(AValue: String);
    procedure SetAsWideString(AValue: WideString);
    procedure SetParameterDataType (value : TmParameterDataType);
    function GetParameterDataType : TmParameterDataType;
    function ValueAsDouble : Double;
  public
    constructor Create;
    procedure ImportFromParam (aSource : TParam);

    procedure SetNull;
    function IsNull : boolean;

    procedure Assign(aValue : TNullableString); overload;
    procedure Assign(aValue : TNullableInteger); overload;
    procedure Assign(aValue : TNullableDateTime); overload;
    procedure Assign(aValue : TNullableDouble); overload;
    procedure Assign(aValue : Variant; const aDataType : TmParameterDataType); overload;

    property Name : String read FName write FName;
    property DataType : TmParameterDataType read GetParameterDataType write SetParameterDataType;
    property AsString : String read GetAsString write SetAsString;
    property AsWideString : WideString read GetAsWideString write SetAsWideString;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsFloat : Double read GetAsFloat write SetAsFloat;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate : TDate read GetAsDate write SetAsDate;
  end;

  { TmQueryParameters }

  TmQueryParameters = class
  strict private
    FList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add (aSource : TParam); overload;
    procedure Add (aParam : TmQueryParameter); overload;
    function FindByName (const aName : String): TmQueryParameter;
    procedure Clear;
    function Count : integer;
    function GetParam (aIndex : integer) : TmQueryParameter;
  end;

  { TmDatabaseConnectionInfo }

  TmDatabaseConnectionInfo = class
  strict private
    FVendorType : TmDatabaseVendor;
    FServer : String;
    FDatabaseName : String;
    FUserName : String;
    FPassword : String;
    FWindowsIntegratedSecurity : Boolean;
    FExtraSettings : String;

    function GetDatabaseName: String;
    function GetExtraSettings: string;
    function GetPassword: String;
    function GetServer: String;
    function GetUserName: String;
    function GetWindowsIntegratedSecurity: Boolean;
    procedure SetDatabaseName(AValue: String);
    procedure SetExtraSettings(AValue: string);
    procedure SetPassword(AValue: String);
    procedure SetServer(AValue: String);
    procedure SetUserName(AValue: String);
    procedure SetWindowsIntegratedSecurity(AValue: Boolean);
  public
    constructor Create;

    procedure SaveToXMLElement (aXMLElement : TmXmlElement);
    procedure LoadFromXMLElement (const aXMLElement : TmXmlElement);

    property VendorType : TmDatabaseVendor read FVendorType write FVendorType;
    property Server : String read GetServer write SetServer;
    property DatabaseName : String read GetDatabaseName write SetDatabaseName;
    property UserName : String read GetUserName write SetUserName;
    property Password : String read GetPassword write SetPassword;
    property WindowsIntegratedSecurity : Boolean read GetWindowsIntegratedSecurity write SetWindowsIntegratedSecurity;
    property ExtraSettings : string read GetExtraSettings write SetExtraSettings;
  end;


function DataTypeToParameterDataType (aValue : TFieldType) : TmParameterDataType;
function ParameterDataTypeToDataType(aValue : TmParameterDataType) : TFieldType;

function DatabaseVendorToString (aValue : TmDatabaseVendor) : string;
function StringToDatabaseVendor (aValue : String) : TmDatabaseVendor;

implementation

function DataTypeToParameterDataType(aValue: TFieldType): TmParameterDataType;
begin
  case aValue of
    ftUnknown:
      Result := ptUnknown;
    ftInteger:
      Result := ptInteger;
    ftFloat:
      Result := ptFloat;
    ftDate:
      Result := ptDate;
    ftDateTime:
      Result := ptDateTime;
    ftWideString:
      Result := ptWideString;
    else
      Result := ptString;
  end;
end;

function ParameterDataTypeToDataType(aValue: TmParameterDataType): TFieldType;
begin
  case aValue of
    ptUnknown:
      Result := ftUnknown;
    ptInteger:
      Result := ftInteger;
    ptFloat:
      Result := ftFloat;
    ptDate:
      Result := ftDate;
    ptDateTime:
      Result := ftDateTime;
    ptWideString:
      Result := ftWideString;
    else
      Result := ftString;
    end;
end;

function DatabaseVendorToString(aValue: TmDatabaseVendor): string;
begin
  if aValue = dvUnknown then
    Result := 'dvUnknown'
  else if aValue = dvSQLServer then
    Result := 'dvSQLServer'
  else
    Result := '';
end;

function StringToDatabaseVendor(aValue: String): TmDatabaseVendor;
begin
  if aValue = 'dvSQLServer' then
    Result := dvSQLServer
  else
    Result := dvUnknown;
end;

{ TmQueryParameters }

constructor TmQueryParameters.Create;
begin
  FList:= TObjectList.Create;
end;

destructor TmQueryParameters.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TmQueryParameters.Add(aSource: TParam);
var
  TempParam : TmQueryParameter;
begin
  TempParam := TmQueryParameter.Create;
  FList.Add(TempParam);
  TempParam.ImportFromParam(aSource);
end;

procedure TmQueryParameters.Add(aParam: TmQueryParameter);
begin
  FList.Add(aParam);
end;

function TmQueryParameters.FindByName(const aName: String): TmQueryParameter;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    if CompareText((FList.Items[i] as TmQueryParameter).Name, aName) = 0 then
    begin
      Result := FList.Items[i] as TmQueryParameter;
      exit;
    end;
  end;
end;

procedure TmQueryParameters.Clear;
begin
  FList.Clear;
end;

function TmQueryParameters.Count: integer;
begin
  Result := FList.Count;
end;

function TmQueryParameters.GetParam(aIndex: integer): TmQueryParameter;
begin
  Result := FList.Items[aIndex] as TmQueryParameter;
end;

{ TmDatabaseConnectionInfo }

constructor TmDatabaseConnectionInfo.Create;
begin
  FVendorType:= dvUnknown;
  FExtraSettings:= '';
end;

procedure TmDatabaseConnectionInfo.SaveToXMLElement(aXMLElement: TmXmlElement);
begin
  aXMLElement.SetAttribute('vendorType', DatabaseVendorToString(Self.VendorType));
  aXMLElement.SetAttribute('server', Self.Server);
  aXMLElement.SetAttribute('databaseName', Self.DatabaseName);
  aXMLElement.SetAttribute('userName', Self.UserName);
  aXMLElement.SetAttribute('password', Self.Password);
  aXMLElement.SetAttribute('extraSettings', Self.ExtraSettings);
  aXMLElement.SetBooleanAttribute('windowsIntegratedSecurity', Self.WindowsIntegratedSecurity);
end;

procedure TmDatabaseConnectionInfo.LoadFromXMLElement(const aXMLElement: TmXmlElement);
begin
  VendorType := StringToDatabaseVendor(aXMLElement.GetAttribute('vendorType');
  Self.Server := aXMLElement.GetAttribute('server');
  Self.DatabaseName := aXMLElement.GetAttribute('databaseName');
  Self.UserName := aXMLElement.GetAttribute('userName');
  Self.Password := aXMLElement.GetAttribute('password');
  Self.ExtraSettings := aXMLElement.GetAttribute('extraSettings');
  Self.WindowsIntegratedSecurity := aXMLElement.GetBooleanAttribute('windowsIntegratedSecurity');
end;

{ TmQueryParameter }

procedure TmQueryParameter.SetParameterDataType(value: TmParameterDataType);
begin
  FDataType:= value;
end;

function TmQueryParameter.GetAsDateTime: TDateTime;
begin
  if (FDataType = ptDate) or (FDataType = ptDateTime) then
  begin
    Result := ValueAsDouble;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not date or dateTime');
  end;
end;

function TmQueryParameter.GetAsDate: TDate;
begin
  if (FDataType = ptDate) or (FDataType = ptDateTime) then
  begin
    Result := trunc(ValueAsDouble);
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not date or dateTime');
  end;
end;

function TmQueryParameter.GetAsFloat: Double;
begin
  if (FDataType = ptFloat) then
  begin
    Result := ValueAsDouble;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not float');
  end;
end;

function TmQueryParameter.GetAsInteger: Integer;
begin
  if (FDataType = ptInteger) then
  begin
    if (FValue = Null) then
      Result := 0
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not integer');
  end;
end;

function TmQueryParameter.GetAsString: String;
begin
  if (FDataType = ptString) then
  begin
    if (FValue = Null) then
      Result := ''
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not string');
  end;
end;

function TmQueryParameter.GetAsWideString: WideString;
begin
  if (FDataType = ptString) or (FDataType = ptWideString) then
  begin
    if (FValue = Null) then
      Result := ''
    else
      Result := FValue;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not wideString or string');
  end;
end;

procedure TmQueryParameter.SetAsDate(AValue: TDate);
begin
  FValue := AValue;
  FDataType:= ptDate;
end;

procedure TmQueryParameter.SetAsDateTime(AValue: TDateTime);
begin
  FValue := AValue;
  FDataType:= ptDateTime;
end;

procedure TmQueryParameter.SetAsFloat(AValue: Double);
begin
  FValue := AValue;
  FDataType:= ptFloat;
end;

procedure TmQueryParameter.SetAsInteger(AValue: Integer);
begin
  FValue := AValue;
  FDataType:= ptInteger;
end;

procedure TmQueryParameter.SetAsString(AValue: String);
begin
  FValue := AValue;
  FDataType := ptString;
end;

procedure TmQueryParameter.SetAsWideString(AValue: WideString);
begin
  FValue := AValue;
  FDataType := ptWideString;
end;

function TmQueryParameter.GetParameterDataType: TmParameterDataType;
begin
  Result := FDataType;
end;

function TmQueryParameter.ValueAsDouble: Double;
begin
  if (FValue = Null) then
    Result := 0
  else
    Result := FValue;
end;

constructor TmQueryParameter.Create;
begin
  FDataType:= ptUnknown;
  FValue:= Null;
end;

procedure TmQueryParameter.ImportFromParam(aSource: TParam);
begin
  Self.Name:= aSource.Name;
  Self.DataType:= DataTypeToParameterDataType(aSource.DataType);
end;

procedure TmQueryParameter.SetNull;
begin
  FValue := Null;
end;

function TmQueryParameter.IsNull: boolean;
begin
  Result := (FValue = Null);
end;

procedure TmQueryParameter.Assign(aValue: TNullableString);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsString:= aValue.Value;
end;

procedure TmQueryParameter.Assign(aValue: TNullableInteger);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsInteger:= aValue.Value;
end;

procedure TmQueryParameter.Assign(aValue: TNullableDateTime);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsDateTime:= aValue.Value;
end;

procedure TmQueryParameter.Assign(aValue: TNullableDouble);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsFloat:= aValue.Value;
end;

procedure TmQueryParameter.Assign(aValue: Variant; const aDataType: TmParameterDataType);
begin
  FDataType:= aDataType;
  if VarIsNull(aValue) then
    Self.SetNull
  else
    FValue := aValue;
end;


function TmDatabaseConnectionInfo.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

function TmDatabaseConnectionInfo.GetExtraSettings: string;
begin
  Result := FExtraSettings;
end;

function TmDatabaseConnectionInfo.GetPassword: String;
begin
  Result := FPassword;
end;

function TmDatabaseConnectionInfo.GetServer: String;
begin
  Result := FServer;
end;

function TmDatabaseConnectionInfo.GetUserName: String;
begin
  Result := FUserName;
end;

function TmDatabaseConnectionInfo.GetWindowsIntegratedSecurity: Boolean;
begin
  Result := FWindowsIntegratedSecurity;
end;

procedure TmDatabaseConnectionInfo.SetDatabaseName(AValue: String);
begin
  FDatabaseName:= AValue;
end;

procedure TmDatabaseConnectionInfo.SetExtraSettings(AValue: string);
begin
  FExtraSettings:= aValue;
end;

procedure TmDatabaseConnectionInfo.SetPassword(AValue: String);
begin
  FPassword:= AValue;
end;

procedure TmDatabaseConnectionInfo.SetServer(AValue: String);
begin
  FServer := AValue;
end;

procedure TmDatabaseConnectionInfo.SetUserName(AValue: String);
begin
  FUserName := AValue;
end;

procedure TmDatabaseConnectionInfo.SetWindowsIntegratedSecurity(AValue: Boolean);
begin
  FWindowsIntegratedSecurity:= AValue;
end;

end.
