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
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
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

end.

