unit LaunchParameters;

interface

uses
  Math, SysUtils, TASeries, Classes, Vector;

const
  SimulationFile = 'Output.txt';

type
  { TLaunchData }
  TLaunchData = class
  public
    // constants
    Gravity, AtmPressure, AdiabaticIndex, AirDensity, Step, DragCoefficient,
    DragConstant, WaterDensity: double;

    // launch
    Angle, TubeLength, TubeVelocity, Pressure, Volume, SystemTime: double;

    // rocket
    Diameter, Length, InletRaduis, Capacity, Mass: double;

    constructor Create();
    procedure RunSimulation();
    procedure UpdateGraph(var Series: TLineSeries);

  private
    SimulationResults: TStringList;
    Displacement, Velocity: TVector;

    procedure PhaseOne();
    procedure PhaseTwo();
    procedure PhaseThree();
  end;

function PsiToPascal(Psi: double): double;
function Area(Radius: double): double;
function FloatFormat(Target: double): string;

implementation

function PsiToPascal(Psi: double): double;
begin
  Result := Psi * 6894.7572931783;
end;

function Area(Radius: double): double;
begin
  Result := Pi * Sqr(Radius);
end;

function FloatFormat(Target: double): string;
const
  FloatDigits = 3;
  FloatPrecision = 4;
begin
  Result := FloatToStrF(Target, FFNumber, FloatPrecision, FloatDigits);
end;

{*******************************************************************************
                                  Launch Data



*******************************************************************************}
constructor TLaunchData.Create();
begin
  SimulationResults := TStringList.Create;
  SystemTime := 0;
end;

procedure TLaunchData.UpdateGraph(var Series: TLineSeries);
var
  FilePointer: TextFile;
  Buffer: string;
  X, Y: double;
begin
  AssignFile(FilePointer, SimulationFile);
  Reset(FilePointer);

  Series.BeginUpdate;
  Series.AddXY(0, 0);
  Series.SeriesColor := 220;
  while not EOF(FilePointer) do
  begin
    ReadLn(FilePointer, Buffer);
    SScanf(Buffer, '%f %f', [@X, @Y]);
    Series.AddXY(X, Y, 'Path', 220);
  end;
  CloseFile(FilePointer);
  Series.EndUpdate;
end;


procedure TLaunchData.PhaseOne();
var
  LaunchVelocity, Exponent: double;
begin
  // the exponent part of the launch velocity equation
  Exponent := (1 - Exp(-2 * DragConstant * TubeLength / (Mass + Volume * WaterDensity)));

  // calculate launch velocity
  LaunchVelocity := Sqrt(Pressure * Area(InletRaduis) * Exponent / DragConstant);

  // initialize displacement and velocity vectors
  Displacement := TVector.CreatePol(TubeLength, Angle);
  Velocity := TVector.CreatePol(LaunchVelocity, Angle);
end;

procedure TLaunchData.PhaseTwo();

  function CalcPressure(const WaterMass, AirVolume: double): double;
  var
    Exponent, N: double;
  begin
    Exponent := (AirVolume / (AirVolume + Volume - WaterMass / WaterDensity)) **
      AdiabaticIndex;
    N := (Pressure + AtmPressure) * Exponent;
    if N < AtmPressure then
    begin
      Result := AtmPressure;
    end
    else
    begin
      Result := N;
    end;
  end;

var
  WaterMass, AbsolutePressure, AirVolume, RateOfTransport, TotalMass: double;

  WaterVelocity, Drag, Momentum, PreviousVelocity: TVector;
begin
  WaterMass := Volume * WaterDensity;
  AirVolume := Capacity * Power(10, -6) - Volume;

  // simulate thrust phase while water remains in the rocket
  while (WaterMass > 0) and (CalcPressure(WaterMass, AirVolume) > AtmPressure) do
  begin
    PreviousVelocity := TVector.CreateRec(Velocity.GetX, Velocity.GetY);

    // the pressure remaining in the  rocket
    AbsolutePressure := CalcPressure(WaterMass, AirVolume);

    // the velocity of water as it exits the nozzle
    WaterVelocity := TVector.CreatePol(
      Sqrt(2 * (AbsolutePressure - AtmPressure) / WaterDensity),
      Velocity.GetAngle + Pi);

    // the rate of mass transport out of the system
    RateOfTransport := Area(InletRaduis) * WaterVelocity.GetMagnitude * WaterDensity;

    // the combined mass of the rocket and the water remaining
    TotalMass := Mass + WaterMass;

    // total drag acting on the system
    Drag := TVector.CreatePol(DragConstant * Sqr(Velocity.GetMagnitude) /
      TotalMass, Velocity.GetAngle + Pi);

    // total momentum added to the system
    Momentum := TVector.CreatePol(RateOfTransport * WaterVelocity.GetMagnitude /
      TotalMass, Velocity.GetAngle);

    // update velocity of the system
    Momentum.Multiply(Step);
    Drag.Multiply(Step);

    Velocity.Add(Drag);
    Velocity.Add(Momentum);

    // update displacement of the system
    Momentum.Multiply(0.5 * Step);
    Drag.Multiply(0.5 * Step);
    PreviousVelocity.Multiply(Step);

    Displacement.Add(PreviousVelocity);
    Displacement.Add(Drag);
    Displacement.Add(Momentum);

    // update the amount of water left in the system
    WaterMass -= RateOfTransport * Step;

    // the total airtime of the rocket
    SystemTime += Step;

    // write simulation data to disk
    SimulationResults.Add(FloatFormat(Displacement.GetX) + ' ' +
      FloatFormat(Displacement.GetY));

    // memory management on unneeded classes
    Drag.Free;
    Momentum.Free;
    PreviousVelocity.Free;
  end;
end;

procedure TLaunchData.PhaseThree();
var
  PreviousVelocity, Acceleration: TVector;
begin
  while Displacement.GetY() > 0 do
  begin
    // store previous velocity
    PreviousVelocity := TVector.CreateRec(Velocity.GetX, Velocity.GetY);

    // construct acceleration
    Acceleration := TVector.CreatePol(DragConstant * Sqr(Velocity.GetMagnitude) /
      Mass, Velocity.GetAngle + Pi);
    Acceleration.AddY(-Gravity);

    // update velocity
    Acceleration.Multiply(Step);
    Velocity.Add(Acceleration);

    // update displacement
    Acceleration.Multiply(0.5 * Step);
    PreviousVelocity.Multiply(Step);
    Displacement.Add(Acceleration);
    Displacement.Add(PreviousVelocity);

    // write simulation data to disk
    SimulationResults.Add(FloatFormat(Displacement.GetX) + ' ' +
      FloatFormat(Displacement.GetY));
  end;
end;

procedure TLaunchData.RunSimulation();
begin
  // remove the previous simulation file
  DeleteFile(SimulationFile);

  // calculate the drag constant
  DragConstant := 0.5 * DragCoefficient * Area(Diameter / 2) * AirDensity;

  PhaseOne;
  PhaseTwo;
  PhaseThree;

  SimulationResults.SaveToFile(SimulationFile);
  SimulationResults.Free;
end;



end.


