unit RGBCIEUtils;

interface

uses
  SysUtils, LCLIntf, Graphics, Math;

const
  {// Observer= 2°, Illuminant= D65    - Daylignt
  ref_X =  95.047;
  ref_Z = 108.883;
  // Observer= 10°, Illuminant= D65   - Daylight
  ref_X =  94.811;
  ref_Z = 35.2;

  // Observer= 2°, Illuminant= A      - Incadescent
  ref_X =  109.850;
  ref_Z = 35.585;
  // Observer= 10°, Illuminant= A     - Incadescent
  ref_X =  111.144;
  ref_Z = 35.2;

  // Observer= 2°, Illuminant= C
  ref_X =  98.074;
  ref_Z = 118.232;
  // Observer= 10°, Illuminant= C
  ref_X =  97.285;
  ref_Z = 116.145;
                }
  // Observer= 2°, Illuminant= D50
  ref_X =  96.422;
  ref_Z = 82.521;{
  // Observer= 10°, Illuminant= D50 - Photoshop
  ref_X =  96.72;
  ref_Z = 81.427; }

  {// Observer= 2°, Illuminant= D55
  ref_X =  95.682;
  ref_Z = 92.149;
  // Observer= 10°, Illuminant= D55
  ref_X =  95.799;
  ref_Z = 90.926;

  // Observer= 2°, Illuminant= D75
  ref_X =  94.972;
  ref_Z = 122.638;
  // Observer= 10°, Illuminant= D75
  ref_X =  94.416;
  ref_Z = 12.641;

  // Observer= 2°, Illuminant= F2     - Fluorescent
  ref_X =  99.187;
  ref_Z = 67.395;
  // Observer= 10°, Illuminant= F2    - Fluorescent
  ref_X =  103.28;
  ref_Z = 69.026;

  // Observer= 2°, Illuminant= F7
  ref_X =  95.044;
  ref_Z = 108.755;
  // Observer= 10°, Illuminant= F7
  ref_X =  95.792;
  ref_Z = 107.678;

  // Observer= 2°, Illuminant= F11
  ref_X =  100.966;
  ref_Z = 64.370;
  // Observer= 10°, Illuminant= F11
  ref_X =  103.866;
  ref_Z = 65.627;   }

type
  xyz = record
    x: Double;
    y: Double;
    z: Double;
  end;

function LabToXYZ(l, a, b: double): xyz;
function XYZToRGB(space: xyz): TColor;
function LabToRGB(l, a, b: double): TColor;
function RGBToXYZ(c: TColor): xyz;
procedure RGBToLab(clr: TColor; out l, a, b: double);
procedure XYZToLab(space: xyz; var l, a, b: double);
function CalcDeltaE (l1, a1, b1, l2, a2, b2: double):double;

implementation

function CalcDeltaE (l1, a1, b1, l2, a2, b2: double):double;
var
  i,deltaL,deltaA,deltaB,deltaC,deltaH,sc,sh,deltaLKlsl,deltaCkcsc,deltaHkhsh,c1,c2:double;
begin
  deltaL := l1 - l2;
  deltaA := a1 - a2;
  deltaB := b1 - b2;
  c1 := sqrt(a1 * a1 + b1 * b1);
  c2 := sqrt(a2 * a2 + b2 * b2);
  deltaC := c1 - c2;
  deltaH := deltaA * deltaA + deltaB * deltaB - deltaC * deltaC;
  if deltaH<0 then deltaH := 0 else  deltaH := sqrt(deltaH);
  sc := 1.0 + 0.045 * c1;
  sh := 1.0 + 0.015 * c1;
  deltaLKlsl := deltaL / (1.0);
  deltaCkcsc := deltaC / (sc);
  deltaHkhsh := deltaH / (sh);
  i := deltaLKlsl * deltaLKlsl + deltaCkcsc * deltaCkcsc + deltaHkhsh * deltaHkhsh;
  if i<0 then result := 0 else result := sqrt(i);
end;


procedure Clamp(var AValue: integer; AMin, AMax: integer);
begin
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
end;

procedure Clamp(var AValue: Double; AMin, AMax: Double);
begin
  if AValue < AMin then AValue := AMin;
  if AValue > AMax then AValue := AMax;
end;


function LabToXYZ(l, a, b: double): xyz;
var
  x, y, z: double;
begin
  y := (l + 16)/116;
  x := a/500 + y;
  z := y - b/200;
  if y > 0.2069 then
    y := IntPower(y, 3)
  else
    y := (y - 0.138)/7.787;
  if x > 0.2069 then
    x := IntPower(x, 3)
  else
    x := (x - 0.138)/7.787;
  if z > 0.2069 then
    z := IntPower(z, 3)
  else
    z := (z - 0.138)/7.787;
  Result.x := ref_X * x;
  Result.y := 100 * y;
  Result.z := ref_Z * z;
end;

function XYZToRGB(space: xyz): TColor;
// see:
// https://de.mathworks.com/matlabcentral/fileexchange/28790-colorspace-transformations/content/colorspace/colorspace.html?requestedDomain=www.mathworks.com
var
  r, g, b, x, y, z: double;
begin
  x := space.x/100;
  y := space.y/100;
  z := space.z/100;
  r := x * 3.2406 + y * (-1.5372) + z * (-0.49);
  g := x * (-0.969) + y * 1.8758 + z * 0.0415;
  b := x * 0.0557 + y * (-0.2040) + z * 1.0570;
  if r > 0.00313 then
    r := 1.055 * Power(r, 1/2.4) - 0.055
  else
    r := 12.92 * r;
  if g > 0.00313 then
    g := 1.055 * Power(g, 1/2.4) - 0.055
  else
    g := 12.92 * g;
  if b > 0.00313 then
    b := 1.055 * Power(b, 1/2.4) - 0.055
  else
    b := 12.92 * b;

  Clamp(r, 0, 1);
  Clamp(g, 0, 1);
  Clamp(b, 0, 1);
  Result := RGB(Round(r*255), Round(g*255), Round(b*255));
end;

function LabToRGB(l, a, b: double): TColor;
begin
  Result := XYZToRGB(LabToXYZ(l, a, b));
end;

function RGBToXYZ(c: TColor): xyz;
var
  r, g, b: double;
begin
  r := GetRValue(c)/255;
  g := GetGValue(c)/255;
  b := GetBValue(c)/255;
  if r > 0.04045 then
    r := Power((r + 0.055)/1.055, 2.4)
  else
    r := r/12.92;
  if g > 0.04045 then
    g := Power((g + 0.055)/1.055, 2.4)
  else
    g := g/12.92;
  if b > 0.04045 then
    b := Power((b + 0.055)/1.055, 2.4)
  else
    b := b/12.92;
  r := r * 100;
  g := g * 100;
  b := b * 100;
  // Observer= 2°, Illuminant= D65
  Result.x := r * 0.4124 + g * 0.3576 + b * 0.1805;
  Result.y := r * 0.2126 + g * 0.7152 + b * 0.0722;
  Result.z := r * 0.0193 + g * 0.1192 + b * 0.9505;
end;

procedure XYZToLab(space: xyz; var l, a, b: Double);
var
  x, y, z: double;
begin
  x := space.x/ref_X;
  y := space.y/100;
  z := space.z/ref_Z;
  if x > 0.008856 then
    x := Power(x, 1/3)
  else
    x := (7.787*x) + 0.138;
  if y > 0.008856 then
    y := Power(y, 1/3)
  else
    y := (7.787*y) + 0.138;
  if z > 0.008856 then
    z := Power(z, 1/3)
  else
    z := (7.787*z) + 0.138;
  l := (116*y) - 16;
  a := 500 * (x - y);
  b := 200 * (y - z);
  Clamp(l, 0, 100);
  Clamp(a, -128, 127);
  Clamp(b, -128, 127);
end;

procedure RGBToLab(clr: TColor; out l, a, b: Double);
var
  s: xyz;
begin
  s := RGBToXYZ(clr);
  XYZToLab(s, l, a, b);
end;

end.

