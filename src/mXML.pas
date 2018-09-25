// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mXML;

interface

uses
  Classes, SysUtils,
  mNullables;

type
  TmXMLString = string;

  EmXmlError = class(Exception);

  TImpl_mXmlElement = class;
  TImpl_mXmlDocument = class;
  TImpl_mXmlElementCursor = class;

  TmXmlElement = class;

  { TmXmlDocument }

  TmXmlDocument = class
  private
    FImpl : TImpl_mXmlDocument;
  public
    constructor Create;
    destructor Destroy; override;

    function RootElement: TmXmlElement;
    function CreateRootElement(Name: string): TmXmlElement;
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure SaveToFileEncrypted (FileName : string; Password : String);
    procedure LoadFromFileEncrypted (FileName : string; Password : String);
  end;

  { TmXmlElement }

  TmXmlElement = class
  private
    FImpl : TImpl_mXmlElement;
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;

    function AddElement(Name: TmXMLString): TmXmlElement;
    function HasAttribute(const Name: TmXMLString): boolean;
    procedure SetAttribute(aName, aValue: TmXmlString); overload;
    procedure SetAttribute(aName: TmXmlString; const aValue: TNullableString); overload;
    function GetAttribute(const aName: TmXmlString): TmXmlString; overload;
    function GetAttribute(const aName: TmXmlString; const aDefaultValue: TmXmlString): TmXmlString; overload;
    procedure GetAttribute(const aName: TmXmlString; const aValue: TNullableString);
    procedure SetDateTimeAttribute(const aName : TmXmlString; const aValue: TDateTime); overload;
    procedure SetDateTimeAttribute(const aName: TmXmlString; const aValue: TNullableDateTime); overload;
    function GetDateTimeAttribute(const aName: TmXmlString): TDateTime; overload;
    function GetDateTimeAttribute(const aName: TmXmlString; const aDefaultValue : TDateTime): TDateTime; overload;
    procedure GetDateTimeAttribute(const aName: TmXmlString; aValue: TNullableDateTime);
    procedure SetDateAttribute(const aName : TmXmlString; const aValue: TDateTime); overload;
    procedure SetDateAttribute(const aName: TmXmlString; const aValue: TNullableDateTime); overload;
    function GetDateAttribute(const aName: TmXmlString): TDateTime; overload;
    function GetDateAttribute(const aName: TmXmlString; const aDefaultValue : TDateTime): TDateTime; overload;
    procedure GetDateAttribute(const aName: TmXmlString; aValue: TNullableDateTime);
    procedure SetFloatAttribute(const aName : TmXmlString; const aValue: Double); overload;
    procedure SetFloatAttribute(const aName: TmXmlString; const aValue: TNullableDouble); overload;
    function GetFloatAttribute(const aName: TmXmlString): Double; overload;
    function GetFloatAttribute(const aName: TmXmlString; const aDefaultValue : Double): Double; overload;
    procedure GetFloatAttribute(const aName: TmXmlString; aValue: TNullableDouble);
    procedure SetIntegerAttribute(const aName: TmXmlString; const aValue: integer); overload;
    procedure SetIntegerAttribute(const aName: TmXmlString; const aValue: TNullableInteger); overload;
    function GetIntegerAttribute(const aName: TmXmlString): integer; overload;
    function GetIntegerAttribute(const aName: TmXmlString; const aDefaultValue : integer): integer; overload;
    procedure GetIntegerAttribute(const aName: TmXmlString; aValue : TNullableInteger);
    procedure SetBooleanAttribute(Name : TmXmlString; Value : Boolean);
    function GetBooleanAttribute(Name: TmXMLString): boolean; overload;
    function GetBooleanAttribute(Name: TmXMLString; DefaultValue : boolean): boolean; overload;
  end;

  TmXmlElementCursor = class
  private
    FImpl : TImpl_mXmlElementCursor;
    function GetElement(I: integer): TmXmlElement;
  public
    constructor Create(Element: TmXmlElement; Name: TmXmlString); reintroduce; overload;
    constructor Create(Element: TmXmlElement); reintroduce; overload;
    destructor Destroy; override;
    function Count : integer;
  public
    property Elements[I: integer]: TmXmlElement read GetElement; default;
  end;

  TImpl_mXmlElement = class
  public
    constructor Create; virtual; abstract;
  public
    function _AddElement(Name: TmXMLString): TmXmlElement; virtual; abstract;
    function _HasAttribute(const Name: TmXMLString): boolean; virtual; abstract;
    procedure _SetAttribute(Name, Value: TmXmlString); virtual; abstract;
    function _GetAttribute(Name: TmXmlString): TmXmlString; overload; virtual; abstract;
    function _GetAttribute(Name: TmXmlString; Default: TmXmlString): TmXmlString; overload; virtual; abstract;
    procedure _SetDateTimeAttribute(Name : TmXmlString; Value : TDateTime); virtual; abstract;
    function _GetDateTimeAttribute(Name: TmXmlString): TDateTime; overload; virtual; abstract;
    function _GetDateTimeAttribute(Name: TmXmlString; Default : TDateTime): TDateTime; overload; virtual; abstract;
    procedure _SetDateAttribute(Name : TmXmlString; Value : TDateTime); virtual; abstract;
    function _GetDateAttribute(Name: TmXmlString): TDateTime; overload; virtual; abstract;
    function _GetDateAttribute(Name: TmXmlString; Default : TDateTime): TDateTime; overload; virtual; abstract;
    procedure _SetFloatAttribute(Name : TmXmlString; Value : double); virtual; abstract;
    function _GetFloatAttribute(Name: TmXmlString): double; overload; virtual; abstract;
    function _GetFloatAttribute(Name: TmXmlString; Default : double): double; overload; virtual; abstract;
    procedure _SetIntegerAttribute(Name : TmXmlString; Value: integer); virtual; abstract;
    function _GetIntegerAttribute(Name: TmXmlString): integer; overload; virtual; abstract;
    function _GetIntegerAttribute(Name: TmXmlString; Default : integer): integer; overload; virtual; abstract;

    procedure _SetOwner(aOwner : TObject); virtual; abstract;
  end;

  TImpl_mXmlDocument = class
  public
    constructor Create; virtual; abstract;
    function _RootElement: TmXmlElement; virtual; abstract;
    function _CreateRootElement(Name: string): TmXmlElement; virtual; abstract;
    procedure _Clear; virtual; abstract;
    procedure _SaveToStream(Stream: TStream); virtual; abstract;
    procedure _LoadFromStream(Stream: TStream); virtual; abstract;
    procedure _SaveToFile(FileName: string); virtual; abstract;
    procedure _LoadFromFile(FileName: string); virtual; abstract;
    procedure _SaveToFileEncrypted (FileName : string; Password : String); virtual; abstract;
    procedure _LoadFromFileEncrypted (FileName : string; Password : String); virtual; abstract;
  end;

  TImpl_mXmlElementCursor = class
  public
    constructor Create; virtual; abstract;

    procedure _SetParent(aParent : TImpl_mXmlElement; aFilter : TmXMLString); virtual; abstract;
    function _GetElement(I : integer) : TmXmlElement; virtual; abstract;
    function _Count : integer; virtual; abstract;
  end;

  TImpl_Factory = class
  public
    function GetTImpl_mXmlElement : TImpl_mXmlElement;
    function GetTImpl_mXmlDocument : TImpl_mXmlDocument;
    function GetTImpl_mXmlElementCursor : TImpl_mXmlElementCursor;
  end;

implementation

uses
  mXML_oxml;

var
  InternalFactory : TImpl_Factory;



{ TmXmlElement }

function TmXmlElement.AddElement(Name: TmXMLString): TmXmlElement;
begin
  Result := FImpl._AddElement(Name);
end;

constructor TmXmlElement.Create(Owner: TObject);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElement;
  FImpl._SetOwner(Owner);
end;

destructor TmXmlElement.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TmXmlElement.GetAttribute(const aName: TmXmlString; const aDefaultValue: TmXmlString): TmXmlString;
begin
  Result := FImpl._GetAttribute(aName, aDefaultValue);
end;

procedure TmXmlElement.GetAttribute(const aName: TmXmlString; const aValue: TNullableString);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetAttribute(aName)
  else
    aValue.IsNull:= true;
end;

function TmXmlElement.GetDateTimeAttribute(const aName: TmXmlString): TDateTime;
begin
  Result := FImpl._GetDateTimeAttribute(aName);
end;

function TmXmlElement.GetDateTimeAttribute(const aName: TmXmlString; const aDefaultValue: TDateTime): TDateTime;
begin
  Result := FImpl._GetDateTimeAttribute(aName, aDefaultValue);
end;

procedure TmXmlElement.GetDateTimeAttribute(const aName: TmXmlString; aValue: TNullableDateTime);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetDateTimeAttribute(aName)
  else
    aValue.IsNull:= true;
end;

procedure TmXmlElement.SetDateAttribute(const aName: TmXmlString; const aValue: TDateTime);
begin
  FImpl._SetDateAttribute(aName, aValue);
end;

procedure TmXmlElement.SetDateAttribute(const aName: TmXmlString; const aValue: TNullableDateTime);
begin
  if aValue.NotNull then
    Self.SetDateAttribute(aName, aValue.Value);
end;

function TmXmlElement.GetDateAttribute(const aName: TmXmlString): TDateTime;
begin
  Result := FImpl._GetDateAttribute(aName);
end;

function TmXmlElement.GetDateAttribute(const aName: TmXmlString; const aDefaultValue: TDateTime): TDateTime;
begin
  Result := FImpl._GetDateAttribute(aName, aDefaultValue);
end;

procedure TmXmlElement.GetDateAttribute(const aName: TmXmlString; aValue: TNullableDateTime);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetDateAttribute(aName)
  else
    aValue.IsNull:= true;
end;

procedure TmXmlElement.SetFloatAttribute(const aName: TmXmlString; const aValue: Double);
begin
  FImpl._SetFloatAttribute(aName, aValue);
end;

procedure TmXmlElement.SetFloatAttribute(const aName: TmXmlString; const aValue: TNullableDouble);
begin
  if aValue.NotNull then
    Self.SetFloatAttribute(aName, aValue.Value);
end;

function TmXmlElement.GetFloatAttribute(const aName: TmXmlString): Double;
begin
  Result := FImpl._GetFloatAttribute(aName);
end;

function TmXmlElement.GetFloatAttribute(const aName: TmXmlString; const aDefaultValue: Double): Double;
begin
  Result := FImpl._GetFloatAttribute(aName, aDefaultValue);
end;

procedure TmXmlElement.GetFloatAttribute(const aName: TmXmlString; aValue: TNullableDouble);
begin
  if Self.HasAttribute(aName) then
    aValue.Value := Self.GetFloatAttribute(aName)
  else
    aValue.IsNull:= true;
end;

function TmXmlElement.GetIntegerAttribute(const aName: TmXmlString): integer;
begin
  Result := FImpl._GetIntegerAttribute(aName);
end;

function TmXmlElement.GetIntegerAttribute(const aName: TmXmlString; const aDefaultValue: integer): integer;
begin
  Result := FImpl._GetIntegerAttribute(aName, aDefaultValue)
end;

procedure TmXmlElement.GetIntegerAttribute(const aName: TmXmlString; aValue: TNullableInteger);
begin
  if Self.HasAttribute(aName) then
    aValue.Value:= Self.GetIntegerAttribute(aName)
  else
    aValue.IsNull:= true;
end;

procedure TmXmlElement.SetBooleanAttribute(Name: TmXmlString; Value: Boolean);
begin
  Self.SetAttribute(Name, BoolToStr(Value, true));
end;

function TmXmlElement.GetBooleanAttribute(Name: TmXMLString): boolean;
begin
  Result := StrToBool(Self.GetAttribute(Name));
end;

function TmXmlElement.GetBooleanAttribute(Name: TmXMLString; DefaultValue: boolean): boolean;
begin
  Result := StrToBool(Self.GetAttribute(Name, BoolToStr(DefaultValue, true)));
end;

function TmXmlElement.GetAttribute(const aName: TmXmlString): TmXmlString;
begin
  Result := FImpl._GetAttribute(aName);
end;

function TmXmlElement.HasAttribute(const Name: TmXMLString): boolean;
begin
  Result := FImpl._HasAttribute(Name);
end;


procedure TmXmlElement.SetAttribute(aName, aValue: TmXmlString);
begin
  FImpl._SetAttribute(aName, aValue);
end;

procedure TmXmlElement.SetAttribute(aName: TmXmlString; const aValue: TNullableString);
begin
  if aValue.NotNull then
    Self.SetAttribute(aName, aValue.Value);
end;

procedure TmXmlElement.SetDateTimeAttribute(const aName: TmXmlString; const aValue: TDateTime);
begin
  FImpl._SetDateTimeAttribute(aName, aValue);
end;

procedure TmXmlElement.SetDateTimeAttribute(const aName: TmXmlString; const aValue: TNullableDateTime);
begin
  if aValue.NotNull then
    Self.SetDateTimeAttribute(aName, aValue.Value);
end;

procedure TmXmlElement.SetIntegerAttribute(const aName: TmXmlString; const aValue: integer);
begin
  FImpl._SetIntegerAttribute(aName, aValue);
end;

procedure TmXmlElement.SetIntegerAttribute(const aName: TmXmlString; const aValue: TNullableInteger);
begin
  if aValue.NotNull  then
    Self.SetIntegerAttribute(aName, aValue.Value);
end;

{ TmXmlDocument }

procedure TmXmlDocument.Clear;
begin
  FImpl._Clear;
end;

constructor TmXmlDocument.Create;
begin
  FImpl := InternalFactory.GetTImpl_mXmlDocument;
end;

function TmXmlDocument.CreateRootElement(Name: string): TmXmlElement;
begin
  Result := FImpl._CreateRootElement(Name);
end;

destructor TmXmlDocument.Destroy;
begin
  FImpl.Free;
  inherited;
end;

procedure TmXmlDocument.LoadFromFile(FileName: string);
begin
  FImpl._LoadFromFile(FileName);
end;

procedure TmXmlDocument.SaveToFileEncrypted(FileName: string; Password: String);
begin
  FImpl._SaveToFileEncrypted(FileName, Password);
end;

procedure TmXmlDocument.LoadFromFileEncrypted(FileName: string; Password: String);
begin
  FImpl._LoadFromFileEncrypted(FileName, Password);
end;

procedure TmXmlDocument.LoadFromStream(Stream: TStream);
begin
  FImpl._LoadFromStream(Stream);
end;

function TmXmlDocument.RootElement: TmXmlElement;
begin
  Result := FImpl._RootElement;
end;

procedure TmXmlDocument.SaveToFile(FileName: string);
begin
  FImpl._SaveToFile(FileName);
end;

procedure TmXmlDocument.SaveToStream(Stream: TStream);
begin
  FImpl._SaveToStream(Stream);
end;

{ TImpl_Factory }

function TImpl_Factory.GetTImpl_mXmlDocument: TImpl_mXmlDocument;
begin
  Result := TImpl_oxml_mXmlDocument.Create;
end;

function TImpl_Factory.GetTImpl_mXmlElement: TImpl_mXmlElement;
begin
  Result := TImpl_oxml_mXmlElement.Create;
end;

function TImpl_Factory.GetTImpl_mXmlElementCursor: TImpl_mXmlElementCursor;
begin
  Result := TImpl_oxml_mXmlElementCursor.Create;
end;

{ TmXmlElementCursor }

constructor TmXmlElementCursor.Create(Element: TmXmlElement; Name: TmXmlString);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElementCursor;
  FImpl._SetParent(Element.FImpl, Name);
end;

function TmXmlElementCursor.Count: integer;
begin
  Result := FImpl._Count;
end;

constructor TmXmlElementCursor.Create(Element: TmXmlElement);
begin
  FImpl := InternalFactory.GetTImpl_mXmlElementCursor;
  FImpl._SetParent(Element.FImpl, '');
end;

destructor TmXmlElementCursor.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TmXmlElementCursor.GetElement(I: integer): TmXmlElement;
begin
  Result := FImpl._GetElement(I);
end;

initialization
  InternalFactory := TImpl_Factory.Create;

finalization
  InternalFactory.Free;
end.
