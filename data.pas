unit Data;

{$R *.lfm}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, TAGraph, TASeries, LaunchParameters,
  LCLType, ExtCtrls, Report, Graphics, ComCtrls;

const
  ReportFolder = 'Report';
  ProfileFolder = 'Profiles';
  ReportFile = 'Flight Report.pdf';

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
    PathTab: TTabSheet;
    PresTab: TTabSheet;
    RadProfile: TRadioGroup; // radio box to select user profile
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
    procedure ValidateTextBox(const Sender: TLabeledEdit);
  private
    FProDir: string; // save directory for user profiles
    FProName: string; // current profile name
    FRepDir: string; // save directory for the report
    FRepName: string; // name of generated report
    FSimulation: TLaunchData; // a copy of the simulation

    procedure GetPanelData(const Sender: TGroupBox; var Data: TStringList);
    procedure GetSeries(const Values: TStringList; var Series: TLineSeries;
      const LineColor: array of TColor);
    procedure GetSimulationResults(BackCalc: boolean);
    procedure ReadFromDisk(var Sender: TLabeledEdit);
    procedure SaveGraph(const Sender: TChart);
    procedure SaveToDisk(Sender: TLabeledEdit);
    procedure SetConfigName;
  end;

implementation

{*******************************************************************************
                               Profile Selection
*******************************************************************************}

{ TMainMenu.SetConfigName

  Constructs the name of the file that will be used to save and load values from
  textboxes. It will also create the file if it is not on disk already. }

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

{*******************************************************************************
                                    Text Box
*******************************************************************************}

{ TMainMenu.SaveToDisk(Sender: TLabeledEdit)

  Saves the contents of a textbox to a profile on disk. If the entry of a
  textbox already exits it updates the value. If the value does not exist then
  a new entry is created. }

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

{ TMainMenu.ReadFromDisk(var Sender: TLabeledEdit)

  Searches for the value of the sender in the config file. If the value is
  defined then the textbox is initialized to it. If not defined, an empty string
  is used for initialization. }

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

{ TMainMenu.ValidateTextBox(const Sender: TLabeledEdit)

  Saves the value of a textbox to the config file. If the value is not a number
  then an error message is shown and the focus is reset to the textbox. }

procedure TMainMenu.ValidateTextBox(const Sender: TLabeledEdit);
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

{ TMainMenu.UpdateTextBox

  Called when the profile is changed. It will update the configuration file name
  and set the textboxes according to the new profile. }

procedure TMainMenu.UpdateTextBox;
begin
  SetConfigName;
  SetTextBox;
end;

{ TMainMenu.SetTextBox

  Cycles through every text box in the form and loads the correct value from a
  configuration file. Values are only loaded if the textbox is not read-only and
  is of class TLabeledEdit. }

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

{*******************************************************************************
                                 Helper Methods
*******************************************************************************}

{ TMainMenu.GetSeries(const Values: TStringList; var Series: TLineSeries;
    const LineColor: array of TColor)

  Initializes a line series with the values stored in a string list. The list
  must be formatted with three values: [PhaseNo][X][Y]. These values must be
  space separated. Depending on the phase number, a different color will be
  assigned to the line. }

procedure TMainMenu.GetSeries(const Values: TStringList; var Series: TLineSeries;
  const LineColor: array of TColor);
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

{*******************************************************************************
                                    Controls
*******************************************************************************}

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

{ TMainMenu.Exit

  Terminate the program and free all objects still in use }

procedure TMainMenu.Exit;
begin
  if Application.MessageBox('Exit Program?', 'Confirmation', MB_YESNO) = idYes then
    Application.Terminate;
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

{ TMainMenu.Run

  Runs the simulation if all the textboxes contain valid values. A textbox is
  valid if all of them contain a positive number. Additionally, the initial
  volume of water must be less than the bottle capacity and the actual distance
  must be greater than the tube length. }

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

{ TMainMenu.GetSimulationResults(BackCalc: boolean)

  Runs the simulation and stores the results into the FSimulationResults field.
  It also handles the construction of the graphs and saving said graphs to the
  reports folder. }

procedure TMainMenu.GetSimulationResults(BackCalc: boolean);
var
  SimulationResult: TSimulationResult;
  SimulationSeries: TLineSeries;
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

    SimulationResult := RunSimulation;
    GetSeries(DisplacementList, FPredPath, [clGreen, clRed, clBlue]);
    ChtPath.AddSeries(FPredPath); // add predicted path to chart

    if BackCalc then
    begin
      SimulationResult := RunSimulation(StrToFloat(LEditActDist.Text));
      GetSeries(DisplacementList, FActPath, [clGreen, clRed, clBlue]);
      ChtPath.AddSeries(FActPath); // add actual path to chart
      SimulationSeries := FActPath;
    end
    else
    begin
      FActPath.Clear;
      SimulationSeries := FPredPath;
    end;

    GetSeries(PressureList, FPresPath, [clGreen, clRed, clBlue]);
    ChtPresPath.AddSeries(FPresPath); // add pressure graph to chart

    LEditTotalD.Text := FloatToStr(SimulationSeries.MaxXValue);
    LEditTotalH.Text := FloatToStr(SimulationSeries.MaxYValue);
    LEditDP1.Text := FloatToStr(SimulationResult[One][Displacement]);
    LEditVP1.Text := FloatToStr(SimulationResult[One][Velocity]);
    LEditDP2.Text := FloatToStr(SimulationResult[Two][Displacement]);
    LEditVP2.Text := FloatToStr(SimulationResult[Two][Velocity]);
    LEditTime.Text := FloatToStr(Time);
    LEditCorFac.Text := FloatToStr(DragCoefficient / StrToFloat(TxtDCoeff.Text));
  end;

  BtnGenRep.Enabled := True;
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

{ TMainMenu.GenerateReport

  Generates a .pdf in the reports folder. It is only a single page and contains
  the flight graph and simulation resuts. Without the Courier Prime fonts
  package, no report can be created. }

procedure TMainMenu.GenerateReport;
var
  Report: TReport;
  Body: TStringList;
begin
  Body := TStringList.Create;

  Report := TReport.Create('Rocket Simulation Report');
  with Report do
  begin
    FRepName := FRepDir + PathDelim + ReportFile;
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

{ TMainMenu.SaveGraph(const Sender: TChart)

  Save sender as a .png file with the senders identifier as the file name. }

procedure TMainMenu.SaveGraph(const Sender: TChart);
var
  Image: TRasterImage;
begin
  Image := Sender.SaveToImage(TPortableNetworkGraphic);
  Image.SaveToFile(FRepDir + PathDelim + Sender.Name + '.' + Image.GetFileExtensions);
  Image.Free;
end;

{ GetPanelData(const Sender: TGroupBox; var Data: TStringList)

  Initialized the data list to contain the label name and textbox value of all
  components inside a specific group box. These values are right-justified to
  50 spaces. }

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
