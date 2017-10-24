unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, StdCtrls, contnrs, mVirtualDataset, mVirtualDatasetPivoter, db,
  VDataset;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    TempDataset : TmVirtualDataset;
    FProvider : TListVirtualDatasetDataProvider;
    FPivoter : TmVirtualDatasetPivoter;
    procedure CreateProvider;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPivoter := TmVirtualDatasetPivoter.Create;
  with FPivoter.VerticalGroupByDefs.Add do
  begin
    FieldName:= 'ValueString';
    DataType:= ftString;
    OperationKind:= gpoDistinct;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  CreateProvider;
  TempDataset := TmVirtualDataset.Create(Self);
  TempDataset.DatasetDataProvider := FProvider;
  TempDataset.Active := true;
  DataSource1.DataSet := TempDataset;
  FPivoter.VirtualDataset := TempDataset;
  FPivoter.CalculateHierarchy;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProvider);
  FreeAndNil(TempDataset);
  FreeAndNil(FProvider);
  FPivoter.Free;
end;

procedure TForm1.CreateProvider;
var
  i : integer;
  Dummy : TCiccio;
begin
  FProvider := TListVirtualDatasetDataProvider.Create;

  for i := 0 to 1000000 do
  begin
    Dummy := TCiccio.Create;
    Dummy.ValueString := 'CICCIO' + IntToStr(trunc(i / 10));
    Dummy.ValueInteger := i;
    Dummy.ValueFloat := (i + 1) / 2;
    FProvider.Add(Dummy);
  end;
end;

end.

