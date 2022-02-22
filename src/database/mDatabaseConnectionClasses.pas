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
  DB, Contnrs, SysUtils, Variants, Classes, {$IFDEF FPC}base64,{$ELSE}EncdDecd, {$ENDIF}
  mNullables, mXML, mUtility, mIntList, mDoubleList, mFilter, mFilterOperators, mMaps;

type
  TmDataConnectionException = class (Exception);

  TmDatabaseVendor = (dvUnknown, dvSQLServer, dvMySQL, dvPostgresql, dvMariaDB);

  TmParameterDataType = (ptUnknown, ptDate, ptDateTime, ptTime, ptInteger, ptFloat, ptString, ptWideString, ptBoolean);

  TmBooleanParameterConvention = (bpcUseBooleans, bpcUseIntegers, bpcUseIntegersButStrings, bpcUseIntegersAvoidNull, bpcUseIntegersButStringsAvoidNull);

{ TmQueryParameter }

  TmQueryParameter = class
  strict private
    FName : String;
    FValue : Variant;
    FDataType : TmParameterDataType;
    FOperator : TmFilterOperator;
    function GetAsDate: TDate;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsTime: TDateTime;
    function GetAsWideString: WideString;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsDate(AValue: TDate);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsFloat(AValue: Double);
    procedure SetAsInteger(AValue: Integer);
    procedure SetAsString(AValue: String);
    procedure SetAsTime(AValue: TDateTime);
    procedure SetAsWideString(AValue: WideString);
    procedure SetParameterDataType (value : TmParameterDataType);
    function GetParameterDataType : TmParameterDataType;
    function ValueAsBoolean : Boolean;
  public
    constructor Create;
    procedure ImportFromParam (aSource : TParam);

    procedure SetNull;
    function IsNull : boolean;

    procedure Assign(const aValue : TNullableString); overload;
    procedure Assign(const aValue : TNullableInteger); overload;
    procedure Assign(const aValue : TNullableDateTime); overload;
    procedure Assign(const aValue : TNullableTime); overload;
    procedure Assign(const aValue : TNullableDouble); overload;
    procedure Assign(const aValue : TNullableBoolean; const aConvention: TmBooleanParameterConvention; const aValueForTrue : integer = 1; const aValueForFalse : integer = 0); overload;
    procedure Assign(const aFilter : TmFilter); overload;
    procedure Assign(const aValue : Variant; const aType: TmParameterDataType); overload;
    procedure Assign(const aField : TField); overload;
    procedure AssignStrings(const aList : TStringList);
    procedure AssignIntegers(const aList : TIntegerList);
    procedure AssignDoubles(const aList: TDoubleList);
    procedure AssignDates(const aList : TIntegerList);
    procedure AssignDateTimes(const aList: TDoubleList);

    procedure AsStringList (aList : TStringList);
    procedure AsIntegerList (aList : TIntegerList);
    procedure AsFloatList (aList : TDoubleList);
    procedure AsDateList (aList : TIntegerList);
    procedure AsDateTimeList (aList : TDoubleList);

    property Name : String read FName write FName;
    property DataType : TmParameterDataType read GetParameterDataType write SetParameterDataType;
    property AsString : String read GetAsString write SetAsString;
    property AsWideString : WideString read GetAsWideString write SetAsWideString;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsFloat : Double read GetAsFloat write SetAsFloat;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate : TDate read GetAsDate write SetAsDate;
    property AsTime : TDateTime read GetAsTime write SetAsTime;
    property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
    property Operator : TmFilterOperator read FOperator write FOperator;
  end;

  { TmQueryParameters }

  TmQueryParameters = class
  strict private
    FList : TObjectList;
    FIndex : TmStringDictionary;
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
    FDatabaseVersion: String;
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

    procedure SaveToXMLElement (aXMLElement : TmXmlElement; const aCryptPassword : string);
    procedure LoadFromXMLElement (const aXMLElement : TmXmlElement; const aCryptPassword : string);
    procedure Assign (aSource : TmDatabaseConnectionInfo);
    procedure GetReport (aReport : TStringList);
    function AsString: String;
    function IsEqual(const aOther : TmDatabaseConnectionInfo): boolean;

    property VendorType : TmDatabaseVendor read FVendorType write FVendorType;
    property DatabaseVersion : String read FDatabaseVersion write FDatabaseVersion;
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

var
  DefaultParamCheck : boolean = true;

implementation
uses
  FmtBCD;

function DataTypeToParameterDataType(aValue: TFieldType): TmParameterDataType;
begin
  case aValue of
    ftUnknown:
      Result := ptUnknown;
    ftInteger, ftSmallint, ftLargeint:
      Result := ptInteger;
    ftFloat, ftBcd, ftFMTBcd, ftCurrency:
      Result := ptFloat;
    ftDate:
      Result := ptDate;
    ftDateTime, ftTimeStamp:
      Result := ptDateTime;
    ftTime:
      Result := ptTime;
    ftWideString, ftWideMemo:
      Result := ptWideString;
    ftGuid:
      Result := ptString;
    ftBoolean:
      Result := ptBoolean
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
    ptTime:
      Result := ftTime;
    ptWideString:
      Result := ftWideString;
    ptBoolean:
      Result := ftBoolean
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
  else if aValue = dvMySQL then
    Result := 'dvMySQL'
  else if aValue = dvPostgresql then
    Result := 'dvPostgresql'
  else if aValue = dvMariaDB then
    Result := 'dvMariaDB'
  else
    Result := '';
end;

function StringToDatabaseVendor(aValue: String): TmDatabaseVendor;
begin
  if aValue = 'dvSQLServer' then
    Result := dvSQLServer
  else if aValue = 'dvMySQL' then
    Result := dvMySQL
  else if aValue = 'dvPostgresql' then
    Result := dvPostgresql
  else if aValue = 'dvMariaDB' then
    Result := dvMariaDB
  else
    Result := dvUnknown;
end;

{ TmQueryParameters }

constructor TmQueryParameters.Create;
begin
  FList:= TObjectList.Create;
  FIndex := TmStringDictionary.Create(false);
end;

destructor TmQueryParameters.Destroy;
begin
  FIndex.Free;
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
  FIndex.Clear;
end;

procedure TmQueryParameters.Add(aParam: TmQueryParameter);
begin
  FList.Add(aParam);
  FIndex.Clear;
end;

function TmQueryParameters.FindByName(const aName: String): TmQueryParameter;
var
  i : integer;
begin
  Result := nil;
  if FIndex.Count = 0 then
  begin
    for i := 0 to FList.Count - 1 do
    begin
      if FIndex.Contains(Uppercase((FList.Items[i] as TmQueryParameter).Name)) then
        raise Exception.Create('Duplicate parameter: ' + Uppercase((FList.Items[i] as TmQueryParameter).Name));
      FIndex.Add(Uppercase((FList.Items[i] as TmQueryParameter).Name), FList.Items[i] as TmQueryParameter);
    end;
  end;
  Result := FIndex.Find(Uppercase(aName)) as TmQueryParameter;
end;

procedure TmQueryParameters.Clear;
begin
  FIndex.Clear;
  FList.Clear;
end;

function TmQueryParameters.Count: integer;
begin
  Result := FList.Count;
end;

function TmQueryParameters.GetParam(aIndex: integer): TmQueryParameter;
begin
  FIndex.Clear;
  Result := FList.Items[aIndex] as TmQueryParameter;
end;

{ TmDatabaseConnectionInfo }

constructor TmDatabaseConnectionInfo.Create;
begin
  FVendorType:= dvUnknown;
  FDatabaseVersion:= '';
  FExtraSettings:= '';
end;

procedure TmDatabaseConnectionInfo.SaveToXMLElement(aXMLElement: TmXmlElement; const aCryptPassword : string);
begin
  aXMLElement.SetAttribute('vendorType', DatabaseVendorToString(Self.VendorType));
  aXMLElement.SetAttribute('databaseVersion', Self.DatabaseVersion);
  aXMLElement.SetAttribute('server', Self.Server);
  aXMLElement.SetAttribute('databaseName', Self.DatabaseName);
  aXMLElement.SetAttribute('userName', Self.UserName);
  if Self.Password <> '' then
    aXMLElement.SetAttribute('password', EncodeStringBase64(SillyCryptDecrypt(Self.Password, aCryptPassword)))
  else
    aXMLElement.SetAttribute('password', '');
  aXMLElement.SetAttribute('extraSettings', Self.ExtraSettings);
  aXMLElement.SetBooleanAttribute('windowsIntegratedSecurity', Self.WindowsIntegratedSecurity);
end;

procedure TmDatabaseConnectionInfo.LoadFromXMLElement(const aXMLElement: TmXmlElement; const aCryptPassword : string);
var
  tmpPsw: String;
begin
  VendorType := StringToDatabaseVendor(aXMLElement.GetAttribute('vendorType'));
  DatabaseVersion:= aXMLElement.GetAttribute('databaseVersion', '');
  Self.Server := aXMLElement.GetAttribute('server');
  Self.DatabaseName := aXMLElement.GetAttribute('databaseName');
  Self.UserName := aXMLElement.GetAttribute('userName');
  tmpPsw:= trim(aXMLElement.GetAttribute('password'));
  if tmpPsw <> '' then
    Self.Password := SillyCryptDecrypt(DecodeStringBase64(tmpPsw), aCryptPassword)
  else
    Self.Password:= '';
  Self.ExtraSettings := aXMLElement.GetAttribute('extraSettings');
  Self.WindowsIntegratedSecurity := aXMLElement.GetBooleanAttribute('windowsIntegratedSecurity');
end;

procedure TmDatabaseConnectionInfo.Assign(aSource: TmDatabaseConnectionInfo);
begin
  Self.VendorType := aSource.VendorType;
  Self.DatabaseVersion:= aSource.DatabaseVersion;
  Self.Server := aSource.Server;
  Self.DatabaseName := aSource.DatabaseName;
  Self.UserName := aSource.UserName;
  Self.Password := aSource.Password;
  Self.WindowsIntegratedSecurity := aSource.WindowsIntegratedSecurity;
  Self.ExtraSettings := aSource.ExtraSettings;
end;

procedure TmDatabaseConnectionInfo.GetReport(aReport: TStringList);
var
  tmp : String;
begin
  WriteStr(tmp, VendorType);
  aReport.Add('Vendor: ' + tmp );
  aReport.Add('Database version: ' + DatabaseVersion);
  aReport.Add('Server: ' + Server);
  aReport.Add('Database: ' + DatabaseName);
  aReport.Add('User: ' + UserName);
  aReport.Add('Windows integrated security: ' + BoolToStr(WindowsIntegratedSecurity, true));
  aReport.Add('Extra settings: ' + ExtraSettings);
end;

function TmDatabaseConnectionInfo.AsString: String;
var
  tmp : String;
begin
  WriteStr(tmp, VendorType);
  tmp := tmp + '#' + DatabaseVersion;
  tmp := tmp + '#' + Server;
  tmp := tmp + '#' + DatabaseName;
  tmp := tmp + '#' + UserName;
  tmp := tmp + '#' + BoolToStr(WindowsIntegratedSecurity, true);
  tmp := tmp + '#' + ExtraSettings;
  Result := tmp;
end;

function TmDatabaseConnectionInfo.IsEqual(const aOther: TmDatabaseConnectionInfo): boolean;
begin
  Result := (VendorType = aOther.VendorType) and
    (CompareText(DatabaseVersion, aOther.DatabaseVersion) = 0) and
    (CompareText(Server, aOther.Server) = 0) and
    (CompareText(DatabaseName, aOther.DatabaseName) = 0) and
    (CompareText(UserName, aOther.UserName) = 0) and
    (CompareText(Password, aOther.Password) = 0) and
    (CompareText(ExtraSettings, aOther.ExtraSettings) = 0) and
    (WindowsIntegratedSecurity  = aOther.WindowsIntegratedSecurity);
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
    if VarIsNull(FValue) or VarIsEmpty(FValue) then
      Result := 0
    else
      Result := VarToDateTime(FValue);
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
    if VarIsNull(FValue) or VarIsEmpty(FValue) then
      Result := 0
    else
      Result := trunc(VarToDateTime(FValue));
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
    if VarIsNull(FValue) or VarIsEmpty(FValue) then
      Result := 0
    else
      if VarIsFMTBcd(FValue) then
        Result := BCDToDouble(VarToBCD(FValue))
      else
        Result := FValue;
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
    if VarIsNull(FValue) or VarIsEmpty(FValue) then
      Result := 0
    else
      if VarIsFMTBcd(FValue) then
        Result := BCDToInteger(VarToBCD(FValue))
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
    if VarIsNull(FValue) or VarIsEmpty(FValue) then
      Result := ''
    else
      Result := VarToStr(FValue);
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not string');
  end;
end;

function TmQueryParameter.GetAsTime: TDateTime;
begin
  if (FDataType = ptTime) then
  begin
    if VarIsNull(FValue) or VarIsEmpty(FValue) then
      Result := 0
    else
      Result := VarToDateTime(FValue);
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not time');
  end;
end;

function TmQueryParameter.GetAsWideString: WideString;
begin
  if (FDataType = ptString) or (FDataType = ptWideString) then
  begin
    if VarIsNull(FValue) or VarIsEmpty(FValue) then
      Result := ''
    else
      Result := VarToWideStr(FValue);
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not wideString or string');
  end;
end;

function TmQueryParameter.GetAsBoolean: Boolean;
begin
  if (FDataType = ptBoolean) then
  begin
    Result := ValueAsBoolean;
  end
  else
  begin
    raise TmDataConnectionException.Create('Datatype of parameter is not boolean');
  end;
end;

procedure TmQueryParameter.SetAsBoolean(AValue: Boolean);
begin
  FValue:= aValue;
  FDataType:= ptBoolean;
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

procedure TmQueryParameter.SetAsTime(AValue: TDateTime);
begin
  FValue := AValue;
  FDataType:= ptTime;
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

function TmQueryParameter.ValueAsBoolean: Boolean;
var
  tmpNullable : TNullableBoolean;
begin
  tmpNullable := TNullableBoolean.Create();
  try
    tmpNullable.Assign(FValue);
    Result := tmpNullable.AsBoolean;
  finally
    tmpNullable.Free;
  end;
end;

constructor TmQueryParameter.Create;
begin
  FDataType:= ptUnknown;
  FValue:= Null;
  FOperator:= foEq;
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

(*
function TmQueryParameter.HasMultipleValues: boolean;
begin
  Result := (not Self.IsNull) and VarisArray(FValue);
end;
*)

procedure TmQueryParameter.Assign(const aValue: TNullableString);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsString:= aValue.Value;
end;

procedure TmQueryParameter.Assign(const aValue: TNullableInteger);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsInteger:= aValue.Value;
end;

procedure TmQueryParameter.Assign(const aValue: TNullableDateTime);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsDateTime:= aValue.Value;
end;

procedure TmQueryParameter.Assign(const aValue: TNullableTime);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsTime:= aValue.Value;
end;

procedure TmQueryParameter.Assign(const aValue: TNullableDouble);
begin
  if aValue.IsNull then
    Self.SetNull
  else
    Self.AsFloat:= aValue.Value;
end;

procedure TmQueryParameter.Assign(const aValue: TNullableBoolean; const aConvention: TmBooleanParameterConvention; const aValueForTrue : integer = 1; const aValueForFalse : integer = 0);
begin
  if (aValue.IsNull) and (aConvention <> bpcUseIntegersAvoidNull) and (aConvention <> bpcUseIntegersButStringsAvoidNull) then
    Self.SetNull
  else
  begin
    if (aConvention = bpcUseBooleans) then
      Self.AsBoolean := aValue.AsBoolean
    else
    if (aConvention = bpcUseIntegers) or (aConvention = bpcUseIntegersAvoidNull) then
    begin
      if aValue.AsBoolean then
        Self.AsInteger:= aValueForTrue
      else
        Self.AsInteger:= aValueForFalse;
    end
    else if (aConvention = bpcUseIntegersButStrings) or (aConvention = bpcUseIntegersButStringsAvoidNull) then
    begin
      if aValue.AsBoolean then
        Self.AsString:= IntToStr(aValueForTrue)
      else
        Self.AsString:= IntToStr(aValueForFalse);
    end;
  end;
end;

procedure TmQueryParameter.Assign(const aFilter : TmFilter);
begin
  if aFilter.DataType = fdtDate then
    Self.FDataType:= ptDate
  else if aFilter.DataType = fdtDateTime then
    Self.FDataType:= ptDateTime
  else if aFilter.DataType = fdtTime then
    Self.FDataType:= ptTime
  else if aFilter.DataType = fdtInteger then
    Self.FDataType:=ptInteger
  else if aFilter.DataType = fdtBoolean then
    Self.FDataType:=ptBoolean
  else if aFilter.DataType= fdtFloat then
    Self.FDataType := ptFloat
  else
    Self.FDataType := ptString;

  FOperator:= aFilter.FilterOperator;

  if VarIsNull(aFilter.Value) then
    Self.SetNull
  else
    FValue := aFilter.Value;
end;

procedure TmQueryParameter.Assign(const aValue: Variant; const aType: TmParameterDataType);
var
  tmpbool : Boolean;
begin
  Self.FDataType:= aType;
  if VarIsNull(aValue) then
    Self.SetNull
  else
  begin
    if aType = ptBoolean then
    begin
      if TryToUnderstandBooleanString(VarToStr(aValue), tmpBool) then
        FValue:= tmpbool
      else
        raise Exception.Create('Unable to convert ' + VarToStr(aValue) + ' to boolean');
    end
    else if aType = ptString then
    begin
      FValue := VarToStr(aValue);
    end
    else
      FValue:= aValue;
  end;
end;

procedure TmQueryParameter.Assign(const aField: TField);
begin
  Self.Assign(aField.AsVariant, DataTypeToParameterDataType(aField.DataType));
end;

procedure TmQueryParameter.AssignStrings(const aList: TStringList);
begin
  Self.FDataType:= ptString;
  if Self.Operator = foEq then
    Self.Operator:= foIn;
  if aList.Count = 0 then
    Self.SetNull
  else
    FValue := mUtility.ConvertStringListToVariant(aList);
end;

procedure TmQueryParameter.AssignIntegers(const aList: TIntegerList);
begin
  Self.FDataType:= ptInteger;
  if Self.Operator = foEq then
    Self.Operator:= foIn;
  if aList.Count = 0 then
    Self.SetNull
  else
    FValue := mUtility.ConvertIntegerListToVariant(aList);
end;

procedure TmQueryParameter.AssignDoubles(const aList: TDoubleList);
begin
  Self.FDataType:= ptFloat;
  if Self.Operator = foEq then
    Self.Operator:= foIn;
  if aList.Count = 0 then
    Self.SetNull
  else
    FValue := mUtility.ConvertDoubleListToVariant(aList);
end;

procedure TmQueryParameter.AssignDates(const aList: TIntegerList);
begin
  Self.FDataType:= ptDate;
  if Self.Operator = foEq then
    Self.Operator:= foIn;
  if aList.Count = 0 then
    Self.SetNull
  else
    FValue := mUtility.ConvertIntegerListToVariant(aList);
end;

procedure TmQueryParameter.AssignDateTimes(const aList: TDoubleList);
begin
  Self.FDataType:= ptDateTime;
  if Self.Operator = foEq then
    Self.Operator:= foIn;
  if aList.Count = 0 then
    Self.SetNull
  else
    FValue := mUtility.ConvertDoubleListToVariant(aList);
end;

procedure TmQueryParameter.AsStringList(aList: TStringList);
begin
  if not IsNull then
    ConvertVariantToStringList(FValue, aList);
end;

procedure TmQueryParameter.AsIntegerList(aList: TIntegerList);
begin
  if not IsNull then
    ConvertVariantToIntegerList(FValue, aList);
end;

procedure TmQueryParameter.AsFloatList(aList: TDoubleList);
begin
  if not IsNull then
    ConvertVariantToDoubleList(FValue, aList);
end;

procedure TmQueryParameter.AsDateList(aList: TIntegerList);
begin
  if not IsNull then
    ConvertVariantToDateList(FValue, aList);
end;

procedure TmQueryParameter.AsDateTimeList(aList: TDoubleList);
begin
  if not IsNull then
    ConvertVariantToDateTimeList(FValue, aList);
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
