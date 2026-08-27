unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, mKnapsackProblem;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  eqList : TEquipmentList;
  outList : TList;
  i : integer;

begin
  outList := TList.Create;
  eqList := TEquipmentList.Create;
  try
    with eqList.Add do
    begin
      Value:= 2;
      Weight := 1;
      Reference:=  pointer(1);
    end;
    with eqList.Add do
    begin
      Value:= 3;
      Weight := 2;
      Reference:= pointer(2);
    end;
    with eqList.Add do
    begin
      Value:= 6;
      Weight := 5;
      Reference:= pointer(3);
    end;
    with eqList.Add do
    begin
      Value:= 3;
      Weight := 6;
      Reference:= pointer(4);
    end;
    OptimizeKnapsack(5, eqList, outList);

    // full
    for i := 0 to outList.Count - 1 do
    begin
      Memo1.Lines.Add('[FULL] equipment:' +  IntToStr(NativeInt(outList.Items[i])));
    end;

    // martello toth
    OptimizeKnapsackMartelloToth(5, eqList, outList);
    for i := 0 to outList.Count - 1 do
    begin
      Memo1.Lines.Add('[MARTELLO TOTH] equipment:' +  IntToStr(NativeInt(outList.Items[i])));
    end;


  finally
    eqList.Free;
    outList.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  eqList : TEquipmentList;
  outList : TList;
  i : integer;
  eq : string;
  decodeEq : TStringList;
begin
  eq := 'map,9,150,compass,13,35,water,153,200,sandwich,50,160,glucose,15,60,tin,68,45,banana,27,60,apple,39,40,' +
     'cheese,23,30,beer,52,10,suntancreme,11,70,camera,32,30,T-shirt,24,15,trousers,48,40,umbrella,73,40,' +
     'waterprooftrousers,42,70,waterproofoverclothes,43,75,notecase,22,80,sunglasses,7,20,towel,18,12,' +
     'socks,4,50,book,30,10';
  outList := TList.Create;
  eqList := TEquipmentList.Create;
  decodeEq := TStringList.Create;
  try
    decodeEq.Delimiter:= ',';
    decodeEq.DelimitedText:= eq;

    i := 0;

    while (i < decodeEq.Count) do
    begin
      with eqList.Add do
      begin
        Reference:=  pchar(decodeEq.Strings[i]);
        Weight := StrToInt(decodeEq.Strings[i+1]);
        Value:= StrToInt(decodeEq.Strings[i+2]);
      end;
      i := i + 3;
    end;

    OptimizeKnapsack(400, eqList, outList);

    // full
    for i := 0 to outList.Count - 1 do
    begin
      Memo1.Lines.Add('[FULL] equipment:' +  pchar(outList.Items[i]));
    end;

    // martello toth
    OptimizeKnapsackMartelloToth(400, eqList, outList);
    for i := 0 to outList.Count - 1 do
    begin
      Memo1.Lines.Add('[MARTELLO TOTH] equipment:' +  pchar(outList.Items[i]));
    end;


  finally
    eqList.Free;
    outList.Free;
    decodeEq.Free;
  end;

end;

end.

