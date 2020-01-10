{*******************************************************************************
                                  Launch Data



*******************************************************************************}
unit LaunchParameters;

interface

uses
  SysUtils, Classes, Vector, Math;

type
  { TLaunchData }
  TLaunchData = class(TObject)
  private
    FAdiIndex: double; // adiabatic index of air
    FAirDense: double; // density of air in (kg/m³)
    FAirVol: double; // initial volume of air inside the rocket (m³)
    FAngle: double; // initial launch angle (radians)
    FAtmPres: double; // atmospheric pressure (pa)
    FBotCap: double; // total volume of bottle (l)
    FBotDiam: double; // diameter of bottle (m)
    FBotLen: double; // length of bottle (m)
    FBotMass: double; // total mass of empty bottle (kg)
    FDCoeff: double; // drag coefficient ([])
    FDConst: double; // drag constant (kg/m)
    FDisp: TVector; // rocket displacement vector
    FDispList: TStringList; // list of all x, y positions
    FPresList: TStringList; // list of presure with respect to time and x-distance
    FEuler: double; // time interval of simulation (seconds)
    FInitPres: double; // launch pressure (pa)
    FLocGrav: double; // local gravity (m/s²)
    FMaxX: double; // total distance travelled by rocket (m)
    FMaxY: double; // total height reached (m)
    FNozRad: double; // radius of bottle nozzle (m)
    FP2Vel: double; // velocity after phase 2 (m/s)
    FTime: double; // total flight time (seconds)
    FTubeLen: double; // length of mounting tube (m)
    FTubeVel: double; // tube velocity at the end of phase 1 (m/s)
    FVel: TVector; // rocket velocity vector
    FWaterDense: double; // water density in (kg/m³)
    FWaterVol: double; // water volume used for launch (m³)
    FP2Disp: double; // displacement after phase 2

    procedure PhaseOne(BackCalc: boolean);
    procedure PhaseThree(BackCalc: boolean);
    procedure PhaseTwo(BackCalc: boolean);
    procedure SetAdiIndex(const Value: double);
    procedure SetAirDense(const Value: double);
    procedure SetAngle(const Value: double);
    procedure SetAtmPres(const Value: double);
    procedure SetBotCap(const Value: double);
    procedure SetBotDiam(const Value: double);
    procedure SetBotLen(const Value: double);
    procedure SetBotMass(const Value: double);
    procedure SetDCoeff(const Value: double);
    procedure SetEuler(const Value: double);
    procedure SetInitPres(const Value: double);
    procedure SetLocGrav(const Value: double);
    procedure SetNozRad(const Value: double);
    procedure SetTubeLen(const Value: double);
    procedure SetWaterDense(const Value: double);
    procedure SetWaterVol(const Value: double);
  public
    function GetDisplacement(PhaseNo: byte): double;
    procedure RunSimulation;
    procedure RunSolveSimulation(ActualDistance, Error: double);
    procedure AppendToList(var List: TStringList; const Row: array of double);

    property AdiabaticIndex: double read FAdiIndex write SetAdiIndex;
    property AirDensity: double read FAirDense write SetAirDense;
    property AtmosphericPressure: double read FAtmPres write SetAtmPres;
    property BottleCapacity: double read FBotCap write SetBotCap;
    property BottleDiameter: double read FBotDiam write SetBotDiam;
    property BottleLength: double read FBotLen write SetBotLen;
    property BottleMass: double read FBotMass write SetBotMass;
    property DisplacementList: TStringList read FDispList;
    property DragCoefficient: double read FDCoeff write SetDCoeff;
    property DragConstant: double read FDConst;
    property Euler: double read FEuler write SetEuler;
    property InitialAirVolume: double read FAirVol;
    property LaunchAngle: double read FAngle write SetAngle;
    property LaunchPressure: double read FInitPres write SetInitPres;
    property LaunchVolume: double read FWaterVol write SetWaterVol;
    property LocalGravity: double read FLocGrav write SetLocGrav;
    property MaximumHeight: double read FMaxY;
    property NozzleRadius: double read FNozRad write SetNozRad;
    property PhaseTwoDisplacement: double read FP2Disp;
    property PhaseTwoVelocity: double read FP2Vel;
    property PressureList: TStringList read FPresList;
    property Time: double read FTime;
    property TotalDistance: double read FMaxX;
    property TubeLength: double read FTubeLen write SetTubeLen;
    property TubeVelocity: double read FTubeVel;
    property WaterDensity: double read FWaterDense write SetWaterDense;
  end;

implementation

{ TLaunchData.SetLocGrav(const Value: double)

  Assign a value to the local local gravity property. }

procedure TLaunchData.SetLocGrav(const Value: double);
begin
  FLocGrav := Value;
end;

procedure TLaunchData.SetAdiIndex(const Value: double);
begin
  FAdiIndex := Value;
end;

procedure TLaunchData.SetAirDense(const Value: double);
begin
  FAirDense := Value;
end;

procedure TLaunchData.SetEuler(const Value: double);
begin
  FEuler := Value;
end;

procedure TLaunchData.SetDCoeff(const Value: double);
begin
  FDCoeff := Value;
end;

procedure TLaunchData.SetWaterDense(const Value: double);
begin
  FWaterDense := Value;
end;

procedure TLaunchData.SetAngle(const Value: double);
begin
  FAngle := (Value / 180) * Pi;
end;

procedure TLaunchData.SetTubeLen(const Value: double);
begin
  FTubeLen := Value;
end;

procedure TLaunchData.SetInitPres(const Value: double);
begin
  FInitPres := Value * 6894.7572931783;
end;

procedure TLaunchData.SetWaterVol(const Value: double);
begin
  FWaterVol := Value;
end;

procedure TLaunchData.SetBotDiam(const Value: double);
begin
  FBotDiam := Value;
end;

procedure TLaunchData.SetBotLen(const Value: double);
begin
  FBotLen := Value;
end;

procedure TLaunchData.SetNozRad(const Value: double);
begin
  FNozRad := Value;
end;

procedure TLaunchData.SetBotCap(const Value: double);
begin
  FBotCap := Value * Power(10, -6);
end;

procedure TLaunchData.SetBotMass(const Value: double);
begin
  FBotMass := Value;
end;

{ TLaunchData.SetAtmPres(const Value: double)

  Assign a value to the atmospheric pressure property }

procedure TLaunchData.SetAtmPres(const Value: double);
begin
  FAtmPres := Value;
end;


function TLaunchData.GetDisplacement(PhaseNo: byte): double;
var
  X, Y: double;
  Phase, Index: integer;
  PhaseEnd: boolean = False;
  ListEnd: boolean = False;
begin
  Index := 0;
  while not PhaseEnd and not ListEnd do
  begin
    ReadStr(FDispList[Index], Phase, X, Y);
    ListEnd := Index = Pred(FDispList.Count);
    PhaseEnd := Phase > PhaseNo;

    if not (PhaseEnd or ListEnd) then
      Inc(Index);
  end;

  ReadStr(FDispList[Index], Phase, X, Y);
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;

procedure TLaunchData.PhaseOne(BackCalc: boolean);
begin
  // reset global time variable
  FTime := 0;
  // calculate drag constant
  FDConst := 0.5 * FDCoeff * Sqr(FBotDiam / 2) * Pi * FAirDense;
  // calculate tube velocity
  FTubeVel := Sqrt(FInitPres * Sqr(FNozRad) * Pi *
    (1 - Exp(-2 * FDConst * FTubeLen / (FBotMass + FWaterVol * FWaterDense))) / FDConst);

  // initialize displacement and velocity vectors
  FDisp := TVector.CreatePol(FTubeLen, FAngle);
  FVel := TVector.CreatePol(FTubeVel, FAngle);

  if not BackCalc then
    AppendToList(FDispList, [0, FDisp.GetX, FDisp.GetY]);
end;

procedure TLaunchData.PhaseTwo(BackCalc: boolean);

  function CalcPressure(const WaterMass: double; out Pressure: double): double;
  begin
    Pressure := (FInitPres + FAtmPres) * (FAirVol /
      (FAirVol + FWaterVol - WaterMass / FWaterDense)) ** FAdiIndex;
    Result := Pressure;
  end;

var
  Transport: double; // rate of mass transport (kg/s)
  BotPres: double; // remaining pressure in bottle (pa)
  WaterMass: double; // initial mass of water in system (kg)
  TotalMass: double; // combined mass of water and rocket (kg)

  WaterVel: TVector; // velocity of water exiting the system
  VelCopy: TVector; // copy of the previous system velocity
  Drag: TVector; // total drag
  Momentum: TVector; // total momentum added to the system
begin
  WaterMass := FWaterVol * FWaterDense;

  // simulate thrust phase while water remains in the rocket
  while (WaterMass > 0) and (CalcPressure(WaterMass, BotPres) > FAtmPres) do
  begin
    // update pressure data
    if not BackCalc then
      AppendToList(FPresList, [1, FDisp.GetX, BotPres]);

    // initialization
    TotalMass := FBotMass + WaterMass;

    VelCopy := TVector.CreateRec(FVel.GetX, FVel.GetY);

    WaterVel := TVector.CreatePol(Sqrt(2 * (BotPres - FAtmPres) / FWaterDense),
      FVel.GetAngle + Pi);
    Transport := Sqr(FNozRad) * Pi * WaterVel.GetMagnitude * FWaterDense;

    Drag := TVector.CreatePol(FDConst * Sqr(FVel.GetMagnitude) /
      TotalMass, FVel.GetAngle + Pi);

    Momentum := TVector.CreatePol(Transport * WaterVel.GetMagnitude /
      TotalMass, FVel.GetAngle);

    // calculation
    Momentum.Multiply(FEuler);
    Drag.Multiply(FEuler);

    FVel.Add(Drag);
    FVel.Add(Momentum);

    Momentum.Multiply(0.5 * FEuler);
    Drag.Multiply(0.5 * FEuler);
    VelCopy.Multiply(FEuler);

    FDisp.Add(VelCopy);
    FDisp.Add(Drag);
    FDisp.Add(Momentum);

    // finalization
    if not BackCalc then
      AppendToList(FDispList, [1, FDisp.GetX, FDisp.GetY]);

    WaterMass -= Transport * FEuler;
    FTime += FEuler;

    Drag.Free;
    Momentum.Free;
    VelCopy.Free;
    WaterVel.Free;
  end;

  FP2Disp := FDisp.GetMagnitude;
  FP2Vel := FVel.GetMagnitude;
end;

procedure TLaunchData.PhaseThree(BackCalc: boolean);
var
  VelCopy, Acc: TVector;
begin
  while FDisp.GetY > 0 do
  begin
    // initialization
    VelCopy := TVector.CreateRec(FVel.GetX, FVel.GetY);

    Acc := TVector.CreatePol(FDConst * Sqr(FVel.GetMagnitude) /
      FBotMass, FVel.GetAngle + Pi);
    Acc.AddY(-FLocGrav);

    // calculation
    Acc.Multiply(FEuler);
    FVel.Add(Acc);

    Acc.Multiply(0.5 * FEuler);
    VelCopy.Multiply(FEuler);

    FDisp.Add(Acc);
    FDisp.Add(VelCopy);

    // finalization
    FTime += FEuler;
    if not BackCalc then
    begin
      AppendToList(FDispList, [2, FDisp.GetX, FDisp.GetY]);
      AppendToList(FPresList, [2, FDisp.GetX, FAtmPres]);
    end;

    Acc.Free;
    VelCopy.Free;
  end;

  FMaxX := FDisp.GetX;
end;

{ TLaunchData.RunSolveSimulation(ActualDistance: double);

  Run a simulation and try and solve for the correction factor using a brute
  force algorithm. }

procedure TLaunchData.RunSolveSimulation(ActualDistance, Error: double);
var
  StepMultiplier: double = 0.1; // use initially high step value
  StepValue: double = 10;
  IsBelow: boolean = False;
  IsAbove: boolean = False;
begin
  RunSimulation; // run the initial simulation

  while not InRange(FDisp.GetX, ActualDistance - Error, ActualDistance + Error) and
    not IsZero(StepValue) do
  begin
    // passed a threshold so now lower the step value
    if IsAbove and IsBelow then
    begin
      StepMultiplier /= 10;
      StepValue /= 10;
      IsAbove := False;
      IsBelow := False;
    end;

    if FDisp.GetX < ActualDistance then
    begin
      if FDCoeff > StepValue then
      begin
        FDCoeff -= StepValue;
      end
      else
      begin
        FDCoeff *= StepMultiplier;
      end;

      IsBelow := True;
    end
    else
    begin
      FDCoeff += StepValue;
      IsAbove := True;
    end;

    // run an optimised simulation
    PhaseOne(True);
    PhaseTwo(True);
    PhaseThree(True);
  end;

  RunSimulation;
end;

{ TLaunchData.RunSimulation

  Run a simulation of the flight path without performing any solve routines. }

procedure TLaunchData.RunSimulation;
begin
  if Assigned(FDispList) then
    FDispList.Free;
  FDispList := TStringList.Create;
  AppendToList(FDispList, [0, 0, 0]);

  if Assigned(FPresList) then
    FPresList.Free;
  FPresList := TStringList.Create;

  // calculate initial volume of air
  FAirVol := FBotCap - FWaterVol;

  PhaseOne(False);
  PhaseTwo(False);
  PhaseThree(False);

  FDisp.Free;
  FVel.Free;
end;

procedure TLaunchData.AppendToList(var List: TStringList; const Row: array of double);
const
  CellDelimer = ' ';
var
  FormattedRow: string;
  Pos: byte;
begin
  FormattedRow := EmptyStr;
  for Pos := 0 to High(Row) do
  begin
    FormattedRow += FloatToStr(Row[Pos]) + CellDelimer;
  end;
  List.Append(FormattedRow);
end;

end.
