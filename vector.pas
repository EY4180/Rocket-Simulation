{*******************************************************************************
                                     Vector



*******************************************************************************}
unit Vector;

interface

type
  TVectorFormat = (Polar, Rectangular);

  TVector = class
  private
    FComponents: array [0..1] of double;
  public
    constructor CreateRec(const X, Y: double);
    constructor CreatePol(const Size, Direction: double);

    function GetAngle: double;
    function GetMagnitude: double;
    function GetX: double;
    function GetY: double;

    procedure Add(const V: TVector);
    procedure Multiply(const Scalar: double);
    procedure AddY(const Amount: double);
    procedure AddX(const Amount: double);
  end;

implementation

{ TVector.CreateRec(X, Y: double)

  Constructor for a vector in rectangular coordinates. Both the x and y
  components must be defined for initialization. }
constructor TVector.CreateRec(const X, Y: double);
begin
  FComponents[0] := X;
  FComponents[1] := Y;
end;

{ TVector.CreatePol(Size, Direction: double)

  Constructor for a vector in polar coordinates. Both the size and angle
  must be defined for initialization. }
constructor TVector.CreatePol(const Size, Direction: double);
begin
  FComponents[0] := Size * Cos(Direction);
  FComponents[1] := Size * Sin(Direction);
end;

{ TVector.AddY(Amount: double)

  Adda scalar to the y-component of a vector. }
procedure TVector.AddY(const Amount: double);
begin
  FComponents[1] += Amount;
end;

{ TVector.AddX(Amount: double)

  Adda scalar to the x-component of a vector. }
procedure TVector.AddX(const Amount: double);
begin
  FComponents[0] += Amount;
end;

{ TVector.GetAngle

  An accessor method that returns the direction that the vector points in polar
  form. }
function TVector.GetAngle: double;
begin
  Result := ArcTan(FComponents[1] / FComponents[0]);
end;

{ TVector.GetMagnitude

  An accessor method that returns the size of a vector in polar form. }
function TVector.GetMagnitude: double;
begin
  Result := Sqrt(Sqr(FComponents[0]) + Sqr(FComponents[1]));
end;

{ TVector.Add(V : TVector)

  Adds the components of another vector to the vector which invokes this method.
  The vector permanently has its components changed. }
procedure TVector.Add(const V: TVector);
begin
  FComponents[0] += V.GetX;
  FComponents[1] += V.GetY;
end;

{ TVector.Multiply(Scalar: double)

  Premanently transforms a vector by a scalar. }
procedure TVector.Multiply(const Scalar: double);
begin
  FComponents[0] *= Scalar;
  FComponents[1] *= Scalar;
end;

{ TVector.GetX

  Returns the x-component of a vector.}
function TVector.GetX: double;
begin
  Result := FComponents[0];
end;

{ TVector.GetY

  Returns the y-component of a vector. }
function TVector.GetY: double;
begin
  Result := FComponents[1];
end;

end.
