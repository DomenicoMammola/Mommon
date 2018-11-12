// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// This is the implementation of the mXML unit on the OXml library
// which can be found at:
// http://www.kluug.net/oxml.php

unit mXML_oxml;

interface

uses
  Classes, Contnrs, BlowFish, sysutils,
  mXML, OXmlPDOM, mMathUtility, mUtility;

resourcestring
 mXML_oxml_AttributeNotFound = 'XML attribute %s not found.';
 mXML_oxml_WrongDateTimeAttribute = 'XML attribute %s has value %s which is not in ISO date-time format.';
 mXML_oxml_WrongDateAttribute = 'XML attribute %s has value $s which is not in ISO date format.';
 mXML_oxml_WrongFloatAttribute = 'XML attribute %s has value %s that is not a valid float number.';
 mXML_oxml_WrongIntegerAttribute = 'XML attribute %s has value %s that is not a valid integer number.';

 mXML_oxml_ValueNotFound = 'XML element %s has no value.';
 mXML_oxml_WrongDateTimeValue = 'XML element %s has value %s which is not in ISO date-time format.';
 mXML_oxml_WrongDateValue = 'XML element %s has value $s which is not in ISO date format.';
 mXML_oxml_WrongFloatValue = 'XML element %s has value %s that is not a valid float number.';
 mXML_oxml_WrongIntegerValue = 'XML element %s has value %s that is not a valid integer number.';

type
  Tmxml_oxml_PointerShell = class
  public
    MyPointer : pointer;
    constructor Create(aPointer : pointer);
  end;

  { TImpl_oxml_mXmlElement }

  TImpl_oxml_mXmlElement = class (TImpl_mXmlElement)
  private
    FNode : PXMLNode;
    FGarbage : TObjectList;
    FFormatSettings : TFormatSettings;
    procedure RaiseMissingAttributeException(const Name: TmXMLString);
    procedure RaiseWrongDateTimeAttributeException(const Name, Value: TmXMLString);
    procedure RaiseWrongDateAttributeException(const Name, Value: TmXMLString);
    procedure RaiseWrongFloatAttributeException(const Name, Value: TmXMLString);
    procedure RaiseWrongIntegerAttributeException(const Name, Value: TmXMLString);
    procedure RaiseMissingValueException(const aNodeName : TmXMLString);
    procedure RaiseWrongDateTimeValueException(const aNodeName, Value: TmXMLString);
    procedure RaiseWrongDateValueException(const aNodeName, Value: TmXMLString);
    procedure RaiseWrongFloatValueException(const aNodeName, Value: TmXMLString);
    procedure RaiseWrongIntegerValueException(const aNodeName, Value: TmXMLString);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    function _AddElement(Name: TmXMLString): TmXmlElement; override;
    function _HasAttribute(const Name: TmXMLString): boolean; override;
    procedure _DeleteAttribute(const aName: TmXMLString); override;
    procedure _SetAttribute(Name, Value: TmXmlString); override;

    function _GetAttribute(Name: TmXmlString): TmXmlString; overload; override;
    function _GetAttribute(Name: TmXmlString; Default: TmXmlString): TmXmlString; overload; override;
    procedure _SetDateTimeAttribute(Name : TmXmlString; Value: TDateTime); override;
    function _GetDateTimeAttribute(Name: TmXmlString): TDateTime; overload; override;
    function _GetDateTimeAttribute(Name: TmXmlString; Default : TDateTime): TDateTime; overload; override;
    procedure _SetDateAttribute(Name : TmXmlString; Value: TDateTime); override;
    function _GetDateAttribute(Name: TmXmlString): TDateTime; overload; override;
    function _GetDateAttribute(Name: TmXmlString; Default : TDateTime): TDateTime; overload; override;
    procedure _SetFloatAttribute(Name : TmXmlString; Value : double); override;
    function _GetFloatAttribute(Name: TmXmlString): double; overload; override;
    function _GetFloatAttribute(Name: TmXmlString; Default : double): double; overload; override;
    procedure _SetIntegerAttribute(Name : TmXmlString; Value: integer); override;
    function _GetIntegerAttribute(Name: TmXmlString): integer; overload; override;
    function _GetIntegerAttribute(Name: TmXmlString; Default : integer): integer; overload; override;

    procedure _SetValue(const aValue: TmXMLString); override;
    function _GetValue: TmXMLString; override;
    procedure _SetDateTimeValue(const aValue : TDateTime); override;
    function _GetDateTimeValue: TDateTime; override;
    procedure _SetDateValue(const aValue : TDateTime); override;
    function _GetDateValue: TDateTime; override;
    procedure _SetFloatValue(const aValue : double); override;
    function _GetFloatValue: double; override;
    procedure _SetIntegerValue(const aValue : integer); override;
    function _GetIntegerValue: integer; override;

    procedure _SetOwner(aOwner : TObject); override;
  end;

  { TImpl_oxml_mXmlDocument }

  TImpl_oxml_mXmlDocument = class (TImpl_mXmlDocument)
  private
    FXML: IXMLDocument;
    FRoot : PXMLNode;
    FRootElement : TmXmlElement;
    FGarbage : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;
    function _RootElement: TmXmlElement; override;
    function _CreateRootElement(Name: string): TmXmlElement; override;
    procedure _Clear; override;
    procedure _SaveToStream(Stream: TStream); override;
    procedure _LoadFromStream(Stream: TStream); override;
    procedure _SaveToFile(FileName: string); override;
    procedure _LoadFromFile(FileName: string); override;
    procedure _SaveToFileEncrypted (FileName : string; Password : String); override;
    procedure _LoadFromFileEncrypted (FileName : string; Password : String); override;
  end;

  TImpl_oxml_mXmlElementCursor = class (TImpl_mXmlElementCursor)
  private
    FList : TObjectList;
    FGarbage : TObjectList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure _SetParent(aParent : TImpl_mXmlElement; aFilter : TmXMLString); override;
    function _GetElement(I : integer) : TmXmlElement; override;
    function _Count : integer; override;
  end;

implementation

uses
  mISOTime;

{ TImpl_oxml_mXmlElement }

constructor TImpl_oxml_mXmlElement.Create;
begin
  FNode := nil;
  FGarbage := TObjectList.Create(true);

  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.DecimalSeparator:= '.';
  FFormatSettings.CurrencyString:= '€';
  FFormatSettings.ThousandSeparator:= ',';
end;

destructor TImpl_oxml_mXmlElement.Destroy;
begin
  FGarbage.Free;
  inherited Destroy;
end;

procedure TImpl_oxml_mXmlElement.RaiseMissingAttributeException (const Name :TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_AttributeNotFound, [Name]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateTimeAttributeException (const Name, Value :TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongDateAttribute, [Name, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongDateAttribute, [Name, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongFloatAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongFloatAttribute, [Name, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongIntegerAttributeException(const Name, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongIntegerAttribute, [Name, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseMissingValueException(const aNodeName : TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_ValueNotFound, [aNodeName]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateTimeValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongDateTimeValue, [aNodeName, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongDateValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongDateValue, [aNodeName, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongFloatValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongFloatValue, [aNodeName, Value]));
end;

procedure TImpl_oxml_mXmlElement.RaiseWrongIntegerValueException(const aNodeName, Value: TmXMLString);
begin
  raise EmXmlError.Create(Format(mXML_oxml_WrongIntegerAttribute, [aNodeName, Value]));
end;

function TImpl_oxml_mXmlElement._AddElement(Name: TmXMLString): TmXmlElement;
var
  NewNode : PXMLNode;
  tmp : Tmxml_oxml_PointerShell;
begin
  NewNode := Self.FNode^.AddChild(Name);
  tmp := Tmxml_oxml_PointerShell.Create(NewNode);
  FGarbage.Add(tmp);
  Result := TmXmlElement.Create(tmp);
  FGarbage.Add(Result);
end;

function TImpl_oxml_mXmlElement._GetAttribute(Name: TmXmlString;
  Default: TmXmlString): TmXmlString;
begin
  Result := Self.FNode^.GetAttributeDef(Name, Default);
end;

function TImpl_oxml_mXmlElement._GetDateTimeAttribute(Name: TmXmlString): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeAttributeException(Name, tmp);
  end
  else
    RaiseMissingAttributeException(Name);
end;

function TImpl_oxml_mXmlElement._GetDateTimeAttribute(Name: TmXmlString; Default: TDateTime): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeAttributeException(Name, tmp);
  end
  else
    Result := Default;
end;

procedure TImpl_oxml_mXmlElement._SetDateAttribute(Name: TmXmlString; Value: TDateTime);
begin
  FNode^.SetAttribute(Name, mISOTime.ISODateToStr(Value));
end;

function TImpl_oxml_mXmlElement._GetDateAttribute(Name: TmXmlString): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryISOStrToDate(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateAttributeException(Name, tmp);
  end
  else
    RaiseMissingAttributeException(Name);
end;

function TImpl_oxml_mXmlElement._GetDateAttribute(Name: TmXmlString; Default: TDateTime): TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryISOStrToDate(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateAttributeException(Name, tmp);
  end
  else
    Result := Default;
end;

procedure TImpl_oxml_mXmlElement._SetFloatAttribute(Name: TmXmlString;Value: double);
begin
  FNode^.SetAttribute(Name, FloatToStr(Value, FFormatSettings));
end;

function TImpl_oxml_mXmlElement._GetFloatAttribute(Name: TmXmlString): double;
var
  tmp : string;
  tempValue : double;
begin
  Result := 0;
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryToConvertToDouble(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatAttributeException(Name, tmp);
  end
  else
    RaiseMissingAttributeException(Name);
end;

function TImpl_oxml_mXmlElement._GetFloatAttribute(Name: TmXmlString; Default: double): double;
var
  tmp : string;
  tempValue : double;
begin
  Result := 0;
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryToConvertToDouble(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatAttributeException(Name, tmp);
  end
  else
    Result := Default;
end;

function TImpl_oxml_mXmlElement._GetIntegerAttribute(Name: TmXmlString): integer;
var
  tmp : string;
  tempValue : integer;
begin
  Result := 0;
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryToConvertToInteger(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerAttributeException(Name, tmp);
  end
  else
    RaiseMissingAttributeException(Name);
end;

function TImpl_oxml_mXmlElement._GetIntegerAttribute(Name: TmXmlString; Default: integer): integer;
var
  tmp : string;
  tempValue : integer;
begin
  Result := 0;
  if FNode^.HasAttribute(Name) then
  begin
    tmp := FNode^.GetAttribute(Name);
    if TryToConvertToInteger(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerAttributeException(Name, tmp);
  end
  else
    Result := Default;
end;

procedure TImpl_oxml_mXmlElement._SetValue(const aValue: TmXMLString);
begin
  FNode^.NodeValue:= aValue;
end;

function TImpl_oxml_mXmlElement._GetValue: TmXMLString;
begin
  Result := FNode^.NodeValue;
end;

procedure TImpl_oxml_mXmlElement._SetDateTimeValue(const aValue: TDateTime);
begin
  FNode^.NodeValue := mISOTime.ISODateTimeToStr(aValue);
end;

function TImpl_oxml_mXmlElement._GetDateTimeValue: TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.NodeValue <>'' then
  begin
    tmp := FNode^.NodeValue;
    if TryISOStrToDateTime(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateTimeValueException(FNode^.NodeName, tmp);
  end
  else
    RaiseMissingValueException(FNode^.NodeName);
end;

procedure TImpl_oxml_mXmlElement._SetDateValue(const aValue: TDateTime);
begin
  FNode^.NodeValue := mISOTime.ISODateToStr(aValue);
end;

function TImpl_oxml_mXmlElement._GetDateValue: TDateTime;
var
  tmp : string;
  tempValue : TDateTime;
begin
  Result := 0;
  if FNode^.NodeValue <> '' then
  begin
    tmp := FNode^.NodeValue;
    if TryISOStrToDate(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongDateValueException(FNode^.NodeName, tmp);
  end
  else
    RaiseMissingValueException(FNode^.NodeName);
end;

procedure TImpl_oxml_mXmlElement._SetFloatValue(const aValue: double);
begin
  FNode^.NodeValue := FloatToStr(aValue, FFormatSettings);
end;

function TImpl_oxml_mXmlElement._GetFloatValue: double;
var
  tmp : string;
  tempValue : double;
begin
  Result := 0;
  if FNode^.NodeValue <> '' then
  begin
    tmp := FNode^.NodeValue;
    if TryToConvertToDouble(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongFloatValueException(FNode^.NodeName, tmp);
  end
  else
    RaiseMissingValueException(FNode^.NodeName);
end;

procedure TImpl_oxml_mXmlElement._SetIntegerValue(const aValue: integer);
begin
  FNode^.NodeValue := IntToStr(aValue);
end;

function TImpl_oxml_mXmlElement._GetIntegerValue: integer;
var
  tmp : string;
  tempValue : integer;
begin
  Result := 0;
  if FNode^.NodeValue <> '' then
  begin
    tmp := FNode^.NodeValue;
    if TryToConvertToInteger(tmp, tempValue) then
      Result := tempValue
    else
      RaiseWrongIntegerValueException(FNode^.NodeName, tmp);
  end
  else
    RaiseMissingValueException(FNode^.NodeName);
end;

function TImpl_oxml_mXmlElement._GetAttribute(Name: TmXmlString): TmXmlString;
begin
  if not Self.FNode^.HasAttribute(Name) then
    RaiseMissingAttributeException(Name);
  Result := Self.FNode^.GetAttribute(Name);
end;

function TImpl_oxml_mXmlElement._HasAttribute(const Name: TmXMLString): boolean;
begin
  Result := Self.FNode^.HasAttribute(Name);
end;

procedure TImpl_oxml_mXmlElement._DeleteAttribute(const aName: TmXMLString);
begin
  if FNode^.HasAttribute(aName) then
    FNode^.DeleteAttribute(aName);
end;


procedure TImpl_oxml_mXmlElement._SetAttribute(Name, Value: TmXmlString);
begin
  FNode^.SetAttribute(Name, Value);
end;

procedure TImpl_oxml_mXmlElement._SetDateTimeAttribute(Name: TmXmlString; Value : TDateTime);
begin
  FNode^.SetAttribute(Name, mISOTime.ISODateTimeToStr(Value));
end;

procedure TImpl_oxml_mXmlElement._SetIntegerAttribute(Name: TmXmlString; Value: integer);
begin
  FNode^.AddAttribute(Name, IntToStr(Value));
end;

procedure TImpl_oxml_mXmlElement._SetOwner(aOwner: TObject);
begin
  FNode := PXMLNode(Tmxml_oxml_PointerShell(aOwner).MyPointer);
end;

{ TImpl_oxml_mXmlDocument }

constructor TImpl_oxml_mXmlDocument.Create;
begin
  FXML := CreateXMLDoc('', true);
  FXML.WriterSettings.WriteBOM := False;
  FRoot := nil;
  FRootElement := nil;
  FGarbage := TObjectList.Create(true);
end;

destructor TImpl_oxml_mXmlDocument.Destroy;
begin
  Self._Clear;
  FreeAndNil(FRootElement);
  FGarbage.Free;
  inherited;
end;

procedure TImpl_oxml_mXmlDocument._Clear;
begin
  FXML.Clear(true);
end;

function TImpl_oxml_mXmlDocument._CreateRootElement(Name: string): TmXmlElement;
var
  tmp : Tmxml_oxml_PointerShell;
begin
  assert (not Assigned(FRoot), 'XML RootElement already assigned!');
  FRoot := FXML.AddChild(Name);
  tmp := Tmxml_oxml_PointerShell.Create(FRoot);
  FGarbage.Add(tmp);
  Result := TmXmlElement.Create(tmp);
  FRootElement := Result;
end;

procedure TImpl_oxml_mXmlDocument._LoadFromFile(FileName: string);
begin
  if not FXML.LoadFromFile(FileName) then
    FXML.GetParseError.RaiseException;

  if Assigned(FRootElement) then
    FRootElement.Free;
  FRootElement := nil;
  FRoot := FXML.DocumentElement;
end;

procedure TImpl_oxml_mXmlDocument._SaveToFileEncrypted(FileName: string; Password: String);
var
  stream : TMemoryStream;
  fileStream : TFileStream;
  en: TBlowFishEncryptStream;
  buf : TBytes;
  bytesRead : integer;
begin
  stream := TMemoryStream.Create;
  fileStream := TFileStream.Create(Filename, fmCreate or fmOpenReadWrite);
  en:= TBlowFishEncryptStream.Create(Password, fileStream);
  try
    Self._SaveToStream(stream);
    stream.Position:= 0;

    SetLength(buf, 1000);
    bytesRead := stream.Read(buf[0], 1000);
    while bytesRead > 0 do
    begin
      en.Write(buf[0], bytesRead);
      bytesRead := stream.Read(buf[0], 1000);
    end;
  finally
    en.Free;
    fileStream.Free;
    stream.Free;
  end;
end;

procedure TImpl_oxml_mXmlDocument._LoadFromFileEncrypted(FileName: string; Password: String);
var
  de: TBlowFishDeCryptStream;
  fileStream: TFileStream;
  stream : TMemoryStream;
  buf : TBytes;
  bytesRead : integer;
begin
  stream := TMemoryStream.Create;
  fileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    de := TBlowFishDecryptStream.Create(Password, fileStream);
    try
      stream.SetSize(fileStream.Size);
      SetLength(buf, 1000);
      bytesRead := de.Read(buf[0], 1000);
      while bytesRead > 0 do
      begin
        stream.Write(buf[0], bytesRead);
        bytesRead := de.Read(buf[0], 1000);
      end;
    finally
      de.Free;
    end;
    stream.Position:= 0;
    Self._LoadFromStream(stream);
  finally
    fileStream.Free;
    stream.Free;
  end;

end;

procedure TImpl_oxml_mXmlDocument._LoadFromStream(Stream: TStream);
begin
  FXML.LoadFromStream(Stream);
  if Assigned(FRootElement) then
    FRootElement.Free;
  FRootElement := nil;
  FRoot := FXML.DocumentElement;
end;

function TImpl_oxml_mXmlDocument._RootElement: TmXmlElement;
var
  tmp : Tmxml_oxml_PointerShell;
begin
  if Assigned(FRoot) and (not Assigned(FRootElement)) then
  begin
    tmp := Tmxml_oxml_PointerShell.Create(FRoot);
    FGarbage.Add(tmp);
    FRootElement := TmXmlElement.Create(tmp);
  end;
  Result := FRootElement;
end;

procedure TImpl_oxml_mXmlDocument._SaveToFile(FileName: string);
begin
  FXML.SaveToFile(Filename);
end;

procedure TImpl_oxml_mXmlDocument._SaveToStream(Stream: TStream);
begin
  FXML.SaveToStream(Stream);
end;

{ Tmxml_oxml_PointerShell }

constructor Tmxml_oxml_PointerShell.Create(aPointer: pointer);
begin
  MyPointer := aPointer;
end;

{ TImpl_oxml_mXmlElementCursor }

constructor TImpl_oxml_mXmlElementCursor.Create;
begin
  FList := TObjectList.Create(true);
  FGarbage := TObjectList.Create(true);
end;

destructor TImpl_oxml_mXmlElementCursor.Destroy;
begin
  FList.Free;
  FGarbage.Free;
  inherited;
end;

function TImpl_oxml_mXmlElementCursor._Count: integer;
begin
  Result := FList.Count;
end;

function TImpl_oxml_mXmlElementCursor._GetElement(I: integer): TmXmlElement;
begin
  Result := FList.Items[i] as TmXmlElement;
end;

procedure TImpl_oxml_mXmlElementCursor._SetParent(aParent: TImpl_mXmlElement; aFilter : TmXMLString);
var
  ParentNode : PXMLNode;
  i : integer;
  NewElement : TmXmlElement;
  tmp : Tmxml_oxml_PointerShell;
begin
  ParentNode := (aParent as TImpl_oxml_mXmlElement).FNode;
  if CompareText(aFilter, '') = 0 then
  begin
    for i := 0 to ParentNode^.ChildCount -1 do
    begin
      tmp := Tmxml_oxml_PointerShell.Create(ParentNode^.ChildNodes[i]);
      FGarbage.Add(tmp);
      NewElement := TmXmlElement.Create(tmp);
      FList.Add(NewElement);
    end;
  end
  else
  begin
    for i := 0 to ParentNode^.ChildCount -1 do
    begin
      if CompareText(aFilter, ParentNode^.ChildNodes[i]^.NodeName) = 0 then
      begin
        tmp := Tmxml_oxml_PointerShell.Create(ParentNode^.ChildNodes[i]);
        FGarbage.Add(tmp);
        NewElement := TmXmlElement.Create(tmp);
        FList.Add(NewElement);
      end;
    end;
  end;
end;

end.
