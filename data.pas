unit Data;

{$Macro on}
{$R *.lfm}
{$Define ReportFolder := 'Report'}
{$Define FontFolder := 'Fonts'}
{$Define ProfileFolder := 'Profiles'}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, TAGraph, TASeries, LaunchParameters,
  LCLType, ExtCtrls, Report, Graphics, ComCtrls;

type
  { TMainMenu }

  TMainMenu = class(TForm)
    BtnClear: TButton; // reset all user inputs to empty string
    BtnExit: TButton; // terminate program
    BtnGenRep: TButton; // generate a report of flight path
    BtnRun: TButton; // run the simulation
    ChkGoalSeek: TCheckBox; // enable program to solve for correction factor
    ChtPath: TChart;
    ChtPresPath: TChart;
    FActPath: TLineSeries;
    FPredPath: TLineSeries;
    FPresPath: TLineSeries;
    GraphControl: TPageControl;
    LEditActDist: TLabeledEdit; // the distance travelled during live testing
    LEditCorFac: TLabeledEdit; // correction factor of drag coefficient
    LEditDP1: TLabeledEdit; // displacement at the end of phase 1
    LEditDP2: TLabeledEdit; // displacement at the end of phase 2
    LEditTime: TLabeledEdit; // total flight time of the rocket
    LEditTotalD: TLabeledEdit; // total x-distance travelled
    LEditTotalH: TLabeledEdit; // maximum height reached
    LEditVP1: TLabeledEdit; // velocity at the end of phase 1
    LEditVP2: TLabeledEdit; // velocity at the end of phase 2
    PanConst: TGroupBox;
    PanInit: TGroupBox;
    PanMeasure: TGroupBox;
    PanResults: TGroupBox;
    RadProfile: TRadioGroup; // radio box to select user profile
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TxtAdiIndex: TLabeledEdit;
    TxtAirDen: TLabeledEdit;
    TxtAngle: TLabeledEdit;
    TxtAtmPres: TLabeledEdit;
    TxtBotCap: TLabeledEdit;
    TxtDCoeff: TLabeledEdit;
    TxtEuler: TLabeledEdit;
    TxtGrav: TLabeledEdit;
    TxtInitPres: TLabeledEdit;
    TxtNozRad: TLabeledEdit;
    TxtRktDiam: TLabeledEdit;
    TxtRktLen: TLabeledEdit;
    TxtRktMass: TLabeledEdit;
    TxtTubeLen: TLabeledEdit;
    TxtWaterDen: TLabeledEdit;
    TxtWaterVol: TLabeledEdit;

    procedure Clear;
    procedure EnableGoalSeek(Sender: TCheckBox);
    procedure Exit;
    procedure FormCreate;
    procedure GenerateReport;
    procedure Run;
    procedure SetTextBox;
    procedure UpdateTextBox;
    procedure ValidateTextBox(Sender: TLabeledEdit);
  private
    FProDir: string; // save directory for user profiles
    FProName: string; // current profile name
    FRepDir: string; // save directory for the report
    FRepName: string; // name of generated report
    FSimulation: TLaunchData; // a copy of the simulation

    procedure GetPanelData(const Sender: TGroupBox; var Data: TStringList);
    procedure GetSeries(const Values: TStringList; var Series: TLineSeries; const LineColor: array of TColor);
    procedure GetSimulationResults(BackCalc: boolean);
    procedure ReadFromDisk(var Sender: TLabeledEdit);
    procedure SaveGraph(const Sender: TChart);
    procedure SaveToDisk(Sender: TLabeledEdit);
    procedure SetConfigName;
  end;

implementation

{ TMainMenu.FormCreate(Sender: TObject)

  This is called on form creation. It sets up the profile name and loads the
  correct values into the textboxes. Only textboxes that are enabled will have
  values initialized. }

procedure TMainMenu.FormCreate;
begin
  SetCurrentDir(ExtractFileDir(ParamStr(0)));
  FRepDir := ExtractFileDir(ParamStr(0)) + PathDelim + ReportFolder;
  FProDir := ExtractFileDir(ParamStr(0)) + PathDelim + ProfileFolder;

  FActPath := TLineSeries.Create(ChtPath);
  FPredPath := TLineSeries.Create(ChtPath);
  FPresPath := TLineSeries.Create(ChtPresPath);

  FSimulation := TLaunchData.Create;

  if not DirectoryExists(FRepDir) then
    CreateDir(FRepDir);
  if not DirectoryExists(FProDir) then
    CreateDir(FProDir);

  SetConfigName; // update name of profile to be read
  SetTextBox; // read text boxes from profile
end;

{ TMainMenu.UpdateFlightPath(const Values: TStringList)

  This fills the flight path graph with the values from a string list. The list
  must have three values per line each separated by a space delimer. The values
  correspond to [phase #] [x distance] [y distance]. Phase # is necessary to
  paint the graph a different color for each phase. }

procedure TMainMenu.GetSeries(const Values: TStringList; var Series: TLineSeries; const LineColor: array of TColor);
var
  Buffer: string;
  X, Y: double;
  Phase: integer;
begin
  Series.Clear;
  Series.BeginUpdate;
  Series.ColorEach := ceLineBefore;
  for Buffer in Values do
  begin
    SScanf(Buffer, '%d %f %f', [@Phase, @X, @Y]);
    Series.AddXY(X, Y, IntToStr(Phase), LineColor[Phase]);
  end;
  Series.EndUpdate;
end;

{ TMainMenu.SetConfigName

  Constructs the name of the file that will be used to save and load values from
  textboxes. It will also create the file if it is not on disk already }

procedure TMainMenu.SetConfigName;
var
  FilePointer: Text;
begin
  FProName := FProDir + PathDelim + IntToStr(RadProfile.ItemIndex);
  // create blank profile
  if not FileExists(FProName) then
  begin
    AssignFile(FilePointer, FProName);
    Rewrite(FilePointer);
    CloseFile(FilePointer);
  end;
end;

{ TMainMenu.SaveToDisk(FileName: string; TextBox: TEdit)

  Saves the contents of a textbox to a profile on disk. If the entry of a
  textbox already exits it updates the value. }

procedure TMainMenu.SaveToDisk(Sender: TLabeledEdit);
var
  FilePointer: Text;
  DataBlock, Buffer, TxtValue, TxtName: string;
  AppendStatus: boolean = True;
begin
  DataBlock := EmptyStr;
  AssignFile(FilePointer, FProName);
  Reset(FilePointer);
  // load contents of file into tempoary location
  while not EOF(FilePointer) do
  begin
    TxtName := EmptyStr;
    TxtValue := EmptyStr;

    ReadLn(FilePointer, Buffer);
    SScanf(Buffer, '%s %s', [@TxtName, @TxtValue]);
    if CompareStr(TxtName, Sender.Name) = 0 then
    begin
      TxtValue := Sender.Text;
      AppendStatus := False;
    end;
    DataBlock += TxtName + ' ' + TxtValue + LineEnding;
  end;
  // textbox not defined so append new entry to file
  if AppendStatus then
  begin
    DataBlock += Sender.Name + ' ' + Sender.Text;
  end;

  // write data to the file and close it
  Rewrite(FilePointer);
  Write(FilePointer, DataBlock);
  CloseFile(FilePointer);
end;

procedure TMainMenu.ReadFromDisk(var Sender: TLabeledEdit);
var
  FilePointer: TextFile;
  Buffer, TxtValue, TxtName: string;
  EntryFound: boolean = False;
begin
  if not FileExists(FProName) then
  begin
    Sender.Text := EmptyStr;
  end
  else
  begin
    // open existing config file
    AssignFile(FilePointer, FProName);
    Reset(FilePointer);

    // find a definition for a text box object
    while not (EOF(FilePointer) or EntryFound) do
    begin
      TxtName := EmptyStr;
      TxtValue := EmptyStr;

      ReadLn(FilePointer, Buffer);
      SScanf(Buffer, '%s %s', [@TxtName, @TxtValue]);
      EntryFound := CompareStr(TxtName, Sender.Name) = 0;
    end;
    Sender.Text := TxtValue;
    CloseFile(FilePointer);
  end;
end;

procedure TMainMenu.ValidateTextBox(Sender: TLabeledEdit);
var
  IsDouble: boolean = True;
begin
  // convert textbox into double
  try
    StrToFloat(Sender.Text);
  except
    on E: EConvertError do
      IsDouble := False;
  end;
  // allow empty string and doubles only
  if not IsDouble and (Sender.Text <> EmptyStr) then
  begin
    Sender.SetFocus;
    Application.MessageBox('Please Enter A Valid Number', 'Input Error', MB_OK);
    Sender.Clear;
  end
  else
  begin
    SaveToDisk(Sender);
  end;
end;

{ TMainMenu.Exit

  Terminate the program and free all objects still in use }

procedure TMainMenu.Exit;
begin
  if Application.MessageBox('Exit Program?', 'Confirmation', MB_YESNO) = idYes then
    Application.Terminate;
end;

{ TMainMenu.EnableGoalSeek(Sender: TCheckBox)

  Turns the back-calculation function on or off. In the on state, the program
  will alter the drag coefficient until it reaches the actual distance specified
  by the user. }

procedure TMainMenu.EnableGoalSeek(Sender: TCheckBox);
var
  EnableSeek: boolean;
begin
  EnableSeek := Sender.State = cbChecked;
  LEditActDist.Enabled := EnableSeek;
  LEditCorFac.Enabled := EnableSeek;
end;

procedure TMainMenu.GenerateReport;
var
  Report: TReport;
  Body: TStringList;
begin
  Body := TStringList.Create;

  Report := TReport.Create('Rocket Simulation Report');
  with Report do
  begin
    FRepName := FRepDir + PathDelim + FormatDateTime('dddddd (hh-mm-ss)', Now) + '.pdf';
    DrawImage(FRepDir + PathDelim + ChtPath.Name + '.png');

    // write results to pdf
    GetPanelData(PanConst, Body);
    AddText(PanConst.Caption, Body);
    GetPanelData(PanInit, Body);
    AddText(PanInit.Caption, Body);
    GetPanelData(PanMeasure, Body);
    AddText(PanMeasure.Caption, Body);
    GetPanelData(PanResults, Body);
    AddText(PanResults.Caption, Body);

    if FileExists(FRepName) then
      DeleteFile(FRepName);
    Save(FRepName);
    Free;
  end;

  Body.Free;
  Application.MessageBox(PChar(FRepDir), 'Report Saved', MB_OK);
end;

{ TMainMenu.Clear

  Clears all of the user textboxes in the screen. It will ask for confirmation
  beforehand. If the text is cleared, the configuration file is permanently
  altered. }

procedure TMainMenu.Clear;
var
  ComponentID: integer;
  Component: TComponent;
  Reply: integer;
begin
  Reply := Application.MessageBox('Are you sure you want to clear the fields?',
    'Confirmation', MB_YESNO);
  if Reply = idYes then
  begin
    // clear all textboxes
    for ComponentID := 0 to Pred(ComponentCount) do
    begin
      Component := Components[ComponentID];
      if Component is TEdit then
      begin
        TEdit(Component).Text := EmptyStr;
        SaveToDisk(TLabeledEdit(Component));
      end;
    end;
  end;
end;

procedure TMainMenu.UpdateTextBox;
begin
  SetConfigName; // update name of profile to be read
  SetTextBox; // set textboxes to default values
end;

procedure TMainMenu.SaveGraph(const Sender: TChart);
var
  Image: TRasterImage;
begin
  Image := Sender.SaveToImage(TPortableNetworkGraphic);
  Image.SaveToFile(FRepDir + PathDelim + Sender.Name + '.' + Image.GetFileExtensions);
  Image.Free;
end;

procedure TMainMenu.Run;
var
  ComponentID: integer = 0;
  Component: TComponent;
  RequiredFields: boolean = True;
begin
  // check for empty fields
  while RequiredFields and (ComponentID < ComponentCount) do
  begin
    Component := Components[ComponentID];
    if Component is TLabeledEdit and not TLabeledEdit(Component).ReadOnly and
      TLabeledEdit(Component).Enabled then
    begin
      RequiredFields := Length(TLabeledEdit(Component).Text) <> 0;
      if not RequiredFields then
      begin
        Application.MessageBox(PChar(TLabeledEdit(Component).Name +
          ' is a required field'),
          'Can Not Run Simulation',
          MB_OK);
        ActiveControl := TLabeledEdit(Component);
      end;
    end;
    Inc(ComponentID);
  end;

  // run simulation if all fields are filled
  if RequiredFields then
  begin
    GetSimulationResults(ChkGoalSeek.State = cbChecked);
    SaveGraph(ChtPath);
    SaveGraph(ChtPresPath);
  end;
end;

procedure TMainMenu.GetSimulationResults(BackCalc: boolean);
begin
  with FSimulation do
  begin
    BottleDiameter := StrToFloat(TxtRktDiam.Text);
    BottleLength := StrToFloat(TxtRktLen.Text);
    NozzleRadius := StrToFloat(TxtNozRad.Text);
    BottleCapacity := StrToFloat(TxtBotCap.Text);
    BottleMass := StrToFloat(TxtRktMass.Text);

    LaunchAngle := StrToFloat(TxtAngle.Text);
    TubeLength := StrToFloat(TxtTubeLen.Text);
    LaunchPressure := StrToFloat(TxtInitPres.Text);
    LaunchVolume := StrToFloat(TxtWaterVol.Text);

    LocalGravity := StrToFloat(TxtGrav.Text);
    AtmosphericPressure := StrToFloat(TxtAtmPres.Text);
    AdiabaticIndex := StrToFloat(TxtAdiIndex.Text);
    WaterDensity := StrToFloat(TxtWaterDen.Text);
    Euler := StrToFloat(TxtEuler.Text);
    DragCoefficient := StrToFloat(TxtDCoeff.Text);
    AirDensity := StrToFloat(TxtAirDen.Text);

    RunSimulation;
    GetSeries(DisplacementList, FPredPath, [clGreen, clRed, clBlue]);
    ChtPath.AddSeries(FPredPath);

    if BackCalc then
    begin
      RunSolveSimulation(StrToFloat(LEditActDist.Text), 0.0000001);
      GetSeries(DisplacementList, FActPath, [clGreen, clRed, clBlue]);
      ChtPath.AddSeries(FActPath);
      LEditTotalD.Text := FloatToStr(FActPath.MaxXValue);
      LEditTotalH.Text := FloatToStr(FActPath.MaxYValue);
    end
    else
    begin
      FActPath.Clear;
      LEditTotalD.Text := FloatToStr(FPredPath.MaxXValue);
      LEditTotalH.Text := FloatToStr(FPredPath.MaxYValue);
    end;
    GetSeries(PressureList, FPresPath, [clGreen, clRed, clBlue]);
    ChtPresPath.AddSeries(FPresPath);

    LEditTime.Text := FloatToStr(Time);
    LEditDP1.Text := FloatToStr(TubeLength);
    LEditVP1.Text := FloatToStr(TubeVelocity);
    LEditDP2.Text := FloatToStr(PhaseTwoDisplacement);
    LEditVP2.Text := FloatToStr(PhaseTwoVelocity);
    LEditCorFac.Text := FloatToStr(DragCoefficient / StrToFloat(TxtDCoeff.Text));
  end;
  BtnGenRep.Enabled := True;
end;

{ TMainMenu.SetTextBox

  Cycles through every text box in the form and loads the correct value from a
  configuration file. }

procedure TMainMenu.SetTextBox;
var
  Component: TComponent;
  ComponentID: byte;
begin
  for ComponentID := 0 to Pred(ComponentCount) do
  begin
    Component := Components[ComponentID];
    if Component is TLabeledEdit and not TLabeledEdit(Component).ReadOnly then
      ReadFromDisk(TLabeledEdit(Component));
  end;
end;

procedure TMainMenu.GetPanelData(const Sender: TGroupBox; var Data: TStringList);
const
  Spacing = 50;
var
  Component: TComponent;
  ComponentID: byte;
  Header, Empty, Value, Content: UnicodeString;
begin
  Content := UnicodeString(EmptyStr);
  for ComponentID := 0 to Pred(ComponentCount) do
  begin
    Component := Components[ComponentID];
    if Component is TLabeledEdit and Component.GetParentComponent.Equals(Sender) then
    begin
      Header := UnicodeString(TLabeledEdit(Component).EditLabel.Caption);
      Empty := UnicodeString(StringOfChar(' ', Spacing - Length(Header)));
      Value := UnicodeString(TLabeledEdit(Component).Text);
      Content += Header + Empty + Value + LineEnding;
    end;
  end;
  Data.Text := string(Content);
end;

end.
