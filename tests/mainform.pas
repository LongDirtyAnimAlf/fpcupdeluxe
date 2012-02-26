unit MainForm;

{ Dependency checker/topological sort

  Copyright (C) 2010-2012 Reinier Olislagers

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls;

type

  { TfrmMainform }

  TfrmMainform = class(TForm)
    btnLoadSample: TButton;
    btnSave: TButton;
    btnLoadDeps: TButton;
    btnSort: TButton;
    btnCopy: TButton;
    OpenDialog1: TOpenDialog;
    stgDependencies: TStringGrid;
    stgSorted: TStringGrid;
    procedure btnCopyClick(Sender: TObject);
    procedure btnLoadDepsClick(Sender: TObject);
    procedure btnLoadSampleClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure stgSortedClick(Sender: TObject);
  private
    { private declarations }
    LoadFile: WideString;
    Sorted: boolean; //Indicates if the input data is sorted already
    procedure SortedToStringList(var OutputList: TStringList);
    procedure UpdateSorted(IsSorted: boolean);
  public
    { public declarations }
  end;

var
  frmMainform: TfrmMainform;

implementation

uses
  topologicalsort,
  Clipbrd {for copy to clipboard};

{$R *.lfm}

{ TfrmMainform }

procedure TfrmMainform.btnLoadDepsClick(Sender: TObject);
// Load dependency file into grid.
var
  RowCounter: integer;
  HeaderAdjust: integer;
  InputList: TStringList;
begin
  if OpenDialog1.Execute then
  begin
    LoadFile := OpenDialog1.Files[0];
    Self.stgDependencies.Clean;
    Self.stgDependencies.Update; //make sure we present this change to the user immediately
    Self.stgSorted.Clean;
    Self.stgSorted.Update;
    UpdateSorted(False);
    //Just for safety's sake:
    if not FileExists(Loadfile) then
    begin
      ShowMessage('Could not load file ' + LoadFile);
      exit;
    end;
    InputList := TStringList.Create;
    try
      InputList.LoadFromFile(LoadFile);
      begin
        HeaderAdjust :=  self.stgDependencies.FixedRows;
        Self.stgDependencies.RowCount := InputList.Count + HeaderAdjust;
        Assert((Self.stgDependencies.ColCount > 1), 'At least 2 column must be present in dependency grid.');
        for RowCounter := 0 to InputList.Count - 1 do
        begin
          Self.stgDependencies.Cells[1, RowCounter + HeaderAdjust] := InputList[RowCounter];
          Self.stgDependencies.Invalidate; //Indicate to OS we'd like to repaint this control, but we're busy doing things
        end;
        Self.stgDependencies.Update; // Make sure all invalidated data is processed by OS before continuing. Note: is this correct? What about BeginUpdate/EndUpdate
      end;
    finally
      InputList.Free;
    end;
  end;
end;

procedure TfrmMainform.btnLoadSampleClick(Sender: TObject);
var
  Answer: longint;
  HeaderAdjust: integer;
  RowCounter: integer;
  SampleData: TSTringList;
begin
  Answer := MessageDlg(
    'This program finds out in which order you need to do things if some of those items depend on others. ' + LineEnding
    + 'To specify dependencies, load a text file containing lines of dependencies: write each thing, then a space, '
    + 'then all its dependencies separated by spaces. You may enter a thing without a dependency; obviously, then it doesn''t matter '
    + 'in what order you process that thing. ' + LineEnding
    + 'If you want, you can work with some predefined dependencies to show how it works. ' + LineEnding
    + 'Do you want to load demo data?'
    , mtConfirmation, [mbYes, mbNo], 0);
  if Answer = mrYes then
  begin
    Self.UpdateSorted(False);
    Self.stgDependencies.Clean;
    Self.stgDependencies.Update; //refresh gui
    Self.stgSorted.Clean;
    Self.stgSorted.Update; //refresh gui
    UpdateSorted(false);
    HeaderAdjust := self.stgDependencies.FixedRows;
    SampleData := TStringList.Create;
    SampleData.Add ( 'des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee');
    SampleData.Add ( 'dw01             ieee dw01 dware gtech');
    SampleData.Add ( 'dw02             ieee dw02 dware');
    SampleData.Add ( 'dw03             std synopsys dware dw03 dw02 dw01 ieee gtech');
    SampleData.Add ( 'dw04             dw04 ieee dw01 dware gtech');
    SampleData.Add ( 'dw05             dw05 ieee dware');
    SampleData.Add ( 'dw06             dw06 ieee dware');
    SampleData.Add ( 'dw07             ieee dware');
    SampleData.Add ( 'dware            ieee dware');
    SampleData.Add ( 'gtech            ieee gtech');
    SampleData.Add ( 'ramlib           std ieee');
    SampleData.Add ( 'std_cell_lib     ieee std_cell_lib');
    SampleData.Add ( 'synopsys         ');
    Self.stgDependencies.RowCount := SampleData.Count + HeaderAdjust;
    //{$DEFINE DEBUG}
    {$IFDEF DEBUG}
    ShowMessage('Sampledata count: ' + inttostr(SampleData.Count) + ', rowcount: ' + inttostr(stgDependencies.RowCount) + ', headeradjust: ' + inttostr(HeaderAdjust) +
    'Column count: ' + inttostr(stgDependencies.ColCount)
    );
    {$ENDIF}
    Assert((Self.stgDependencies.ColCount > 1), 'At least 2 column must be present in dependency grid.');
    Assert((Self.stgDependencies.RowCount >= SampleData.Count + HeaderAdjust), 'Dependency grid is tall enough to take the sample data strignlist.');
    try
      for RowCounter := 0 to SampleData.Count -1 do
      begin
        Self.stgDependencies.Cells[1, RowCounter + HeaderAdjust] := SampleData[RowCounter];
      end;
    finally
      SampleData.Free;
    end;
    ShowMessage('Demo data added. Please see the Rosetta code site for further explanation: ' + LineEnding
    + 'http://rosettacode.org/wiki/Topological_sort' + LineEnding
    + LineEnding
    + 'Oh, try adding a dependency on dw04 in dw01 and see what happens when sorting...');
  end;
end;


procedure TfrmMainform.btnSaveClick(Sender: TObject);
var
  SaveFile: string;
  OutputList: TStringList;
  Counter: integer;
begin
  OutputList := TStringList.Create;
  Self.SortedToStringList(OutputList);
  // Check if we have valid sorted data:
  if OutputList.Count > 0 then
  begin
    if (Length(LoadFile) = 0) then
    begin
      SaveFile := 'dependencies';
    end
    else
    begin
      SaveFile := LoadFile + '.sorted.txt';
    end;
    // Now save data
    OutputList.SaveToFile(SaveFile);
    ShowMessage('Saved results as file ' + SaveFile);
  end;
end;

procedure TfrmMainform.btnCopyClick(Sender: TObject);
//Copy sort results to clipboard
var
  SortedList: TStringList;
begin
  SortedList := TStringList.Create;
  Self.SortedToStringList(SortedList);
  Clipboard.AsText := SortedList.Text;
end;

procedure StripTop(var List: TStringList; const RemoveCount: integer);
var
  i: integer;
begin
  if List.Count>RemoveCount then
  begin
    for i:=1 to RemoveCount do
    begin
      List.Delete(0); //Delete from top
    end;
  end;
end;

procedure TfrmMainform.btnSortClick(Sender: TObject);
var
  HeaderAdjust: integer;
  InputList: TStringList;
  TopSort: TTopologicalSort;
  SortedList: TStringList;
  Counter: integer;
begin
  HeaderAdjust := Self.stgDependencies.FixedRows;
  if (Self.stgDependencies.RowCount - HeaderAdjust = 0) then
  begin
    ShowMessage('Nothing to sort.');
  end
  else
  begin
    //InputList:=TStringList.Create; //Will be created below
    TopSort := TTopologicalSort.Create;
    SortedList := TStringList.Create;
    try
      // Get all values from the first non-header row
      // in column 1 (column 0 is a caption)
      InputList:=TStringList(Self.stgDependencies.Cols[1]);
      StripTop(InputList, HeaderAdjust);

      //Sort:
      TopSort.Sort(InputList, SortedList);

      // Fill column with output starting from first non-header cell:
      Self.stgSorted.RowCount := SortedList.Count + HeaderAdjust;
      for Counter := 0 + HeaderAdjust to SortedList.Count + HeaderAdjust - 1 do
      begin
        Self.stgSorted.Cells[1, Counter + HeaderAdjust] := SortedList[Counter];
        Self.stgSorted.Invalidate; //Indicate to windows we're working on the control, you don't need to repaint if there's too much going on
      end;
      Self.stgSorted.Update; //Make sure control gets repainted if required.
      Self.UpdateSorted(True);
    finally
      InputList.Free;
      SortedList.Free;
      TopSort.Free;
    end;
  end;

end;

procedure TfrmMainform.FormClick(Sender: TObject);
begin
  //todo. but what? I forgot.
end;

procedure TfrmMainform.FormCreate(Sender: TObject);
begin
  // Set some default properties:
  Self.Sorted := False;
  Self.stgDependencies.RowCount := 0; //no data
  Self.stgDependencies.FixedRows := 0; //required for total data copying
  Self.stgDependencies.ColCount := 2; //counter row and data
  Self.stgDependencies.FixedCols := 1; //counter column
  Self.stgSorted.RowCount := 0; //no data
  Self.stgSorted.FixedRows := 0; //required for total data copying
  Self.stgSorted.ColCount := 2; //counter row and data
  Self.stgSorted.FixedCols := 1; //counter column
end;

procedure TfrmMainform.FormResize(Sender: TObject);
var
  WhatsLeft: integer;
begin
  // Make sure grid entries are wide enough
  WhatsLeft := Self.Width - 20;
  Self.stgDependencies.Width := WhatsLeft;
  Self.stgSorted.Width := WhatsLeft;
  // Formula needs some adjustment: take inner width of grid into account
  WhatsLeft := Self.stgDependencies.Width - Self.stgDependencies.ColWidths[0] - 10;
  Assert(Self.stgDependencies.ColCount >= 2, 'Need at least 2 columns for dependencies grid');
  Self.stgDependencies.ColWidths[1] := WhatsLeft;
  WhatsLeft := Self.stgSorted.Width - Self.stgSorted.ColWidths[0] - 10;
  Assert(Self.stgSorted.ColCount >= 2, 'Need at least 2 columns for output grid');
  Self.stgSorted.ColWidths[1] := WhatsLeft;
  // Let's resize the lower grid to the bottom of the screen, move the copy button
  Self.btnCopy.Top := Self.Height - Self.btnCopy.Height - 10;
  Self.btnSave.Top := Self.Height - Self.btnSave.Height - 10;
  WhatsLeft := Self.btnCopy.Top - Self.stgSorted.Top - 10;
  Self.stgSorted.Height := WhatsLeft;
end;

procedure TfrmMainform.stgSortedClick(Sender: TObject);
begin
  // Select entire column if we're clicking on column header for column 1 (= data column)
  //todo
end;

procedure TfrmMainform.SortedToStringList(var OutputList: TStringList);
// If sorted data exists, put it into stringlist
// If no data exists, clear the stringlist of data
var
  Counter: integer;
begin
  if stgSorted.FixedRows = 0 then
  //Simple way if not usign header/fixed rows in grid
  begin
    OutputList := TStringList(stgSorted.Cols[1]);
  end
  else
  begin
    // Complicated way if we are using header/fixed rows in our output
    if stgSorted.RowCount > 1 then
    begin
      for Counter := 1 to Self.stgSorted.RowCount - 1 do
      begin
        OutputList.Add(Self.stgSorted.Cells[1, Counter]);
      end;
    end
    else
    begin
      OutputList.Clear;
    end;
  end;
end;

procedure TfrmMainform.UpdateSorted(IsSorted: boolean);
begin
  if IsSorted = True then
  begin
    Self.Sorted := True;
    Self.btnSave.Visible := True;
    Self.btnCopy.Visible := True;
  end
  else
  begin
    Self.Sorted := False;
    Self.btnSave.Visible := False;
    Self.btnCopy.Visible := False;
  end;
end;

end.

