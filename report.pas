unit Report;

interface

uses
  Classes, SysUtils, fpPDF;

const
  PDFWidth = 595;
  PDFHeight = 842;
  CourierPrime = 'Courier Prime.ttf';

type
  TReport = class(TObject)
    private
      FDoc: TPDFDocument;
      FSec: TPDFSection;
      FPage: TPDFPage;
      FFont: integer;
      FTitle: string;
      FTitleSize: integer;
      FTextSize: integer;
      FPos: integer; // current position in the file
      FBorder: integer;

      FFontFile: string;
    public
      constructor Create(Title: string);
      procedure AddText(Header: string; Body: TStringList);
      procedure DrawTitle;
      procedure SetBorder(const Value: integer);
      procedure Save(FName: string);
      procedure DrawBoundingBox;
      procedure DrawImage(const FName: string);
      property BorderSize: integer read FBorder write SetBorder;
  end;

implementation

function PointToPixels(Point: integer): integer;
begin
  Result := (Point * 72) div 96;
end;

constructor TReport.Create(Title: string);
begin
  FDoc := TPDFDocument.Create(nil);
  FFontFile := ExtractFileDir(ParamStr(0)) + PathDelim + 'Fonts' + PathDelim + CourierPrime;
  with FDoc do
  begin
    Options := [poPageOriginAtTop];
    StartDocument;

    FFont := AddFont(FFontFile, 'Courier Prime');
    FSec := Sections.AddSection;
    FSec.Title := Title;

    FPage := Pages.AddPage;
    FPage.Orientation := ppoPortrait;
    FPage.PaperType := ptA4;
    FPage.UnitOfMeasure := uomPixels;
    FSec.AddPage(FPage);
  end;
  FBorder := 10; // default size of the border
  FTitle := Title;
  FTitleSize := 28;
  FTextSize := 12;
  FPos := FBorder;
end;

procedure TReport.SetBorder(const Value: integer);
begin
  if Value > 0 then
    FBorder := Value;
end;

procedure TReport.DrawTitle;
begin
  FPage.SetFont(FFont, FTitleSize);
  FPage.WriteText(FBorder, FBorder + PointToPixels(FTitleSize), FTitle, 0, False, False);
  FPos := FBorder + PointToPixels(FTitleSize);
end;

procedure TReport.DrawBoundingBox;
var
  BoxWidth, BoxHeight, Y: integer;
begin
  BoxWidth := PDFWidth - 2 * FBorder;
  Y := 2 * FBorder + PointToPixels(FTitleSize);
  BoxHeight := PDFHeight - Y - 2 * FBorder;

  FPage.DrawRect(FPos, Y, BoxWidth, -BoxHeight, 1, False, True, 0);
  FPos := Y;
end;

procedure TReport.AddText(Header: string; Body: TStringList);
const
  SpaceAfter = 20;
var
  WordCount: integer = 0;
  DotLine: integer;
  X: integer;
begin
  DotLine := FDoc.AddLineStyleDef(1, clBlack, ppsDot);
  X := FBorder + 10;

  // draw header
  FPage.SetFont(FFont, FTextSize + 4);
  FPage.WriteText(X, FPos, Header, 0, False, False);
  FPage.DrawLineStyle(X, FPos + 5, PDFWidth - X, FPos + 5, DotLine);
  FPos += PointToPixels(FTitleSize);

  // draw body
  FPage.SetFont(FFont, FTextSize);
  while WordCount < Body.Count do
  begin
    FPage.WriteText(X, FPos, Body[WordCount], 0, False, False);

    FPos += PointToPixels(FTextSize) + 5;
    Inc(WordCount);
  end;
  FPos += SpaceAfter;
end;
procedure TReport.DrawImage(const FName: string);
var
  Image: integer;
  X, Y, Width, Height: integer;
begin
  Image := FDoc.Images.AddFromFile(FName, False);

  Width := PDFWidth -  2 * (FBorder + 5);
  Height := Trunc((Width / FDoc.Images[Image].Width) * FDoc.Images[Image].Height);
  X := FBorder + 5;
  Y := Height + PointToPixels(FTitleSize);

  FPage.DrawImage(X, Y, Width, Height, Image, 0);
  FPos := Y + PointToPixels(FTextSize);
end;

procedure TReport.Save(FName: string);
begin
  FDoc.SaveToFile(FName);
end;

end.

