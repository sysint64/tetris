unit uUtils;

interface

Uses
  VectorGeometry, Windows, GLGraphics, glColor, GLMaterial,
  GLVectorFileObjects, VectorTypes, Dialogs, SysUtils, OpenGL1x,
  GLWin32Viewer, Graphics, ExtCtrls;

//

function  TestChanse(Sides: Integer): Boolean;
function  ReplaceString(S,ch1,ch2: AnsiString): AnsiString;

function  PointInRect(pnt: TVector; x1,y1,x2,y2: Integer): Boolean; overload;
function  PointInRect(pnt: TVector; r: TRect): Boolean; overload;

procedure Pixel322RGB(pix: TGlPixel32; var _r,_g,_b,_a: byte);
function  RGB2Pixel32(_r,_g,_b,_a: byte): TGlPixel32;
function  Sign(a: Real): Real;

function  UpString(Source: AnsiString): AnsiString;
function  GetStrINstr(Source: AnsiString; St: Char; En: Char): AnsiString;
function  StrReplace(S,ch1,ch2: AnsiString): AnsiString;
function  Ang(Angle: Single): Single;

procedure ActorAnimation(Actor: TGLActor; Animation: AnsiString);

// Draw's

// Circle

procedure DrawCircleX(Radius: Single; Segments: Integer; Pos: Single);
procedure DrawCircleY(Radius: Single; Segments: Integer; Pos: Single);
procedure DrawCircleZ(Radius: Single; Segments: Integer; Pos: Single);

// Arc

procedure DrawArcX(Radius: Single; Segments: Integer; Pos: Single);
procedure DrawArcY(Radius: Single; Segments: Integer; Pos: Single);
procedure DrawArcZ(Radius: Single; Segments: Integer; Pos: Single);

// Quadric
procedure SetInvertedQuadricOrientation(quadric : PGLUquadricObj);

// Matrix
function  CreateWorldMatrix(Scale, Pos, Axis: TVector; Angle: single): TMatrix;

// Canvas
function  CrossFadeColor(FromColor, ToColor : TColor; Rate : Single) : TColor;
procedure hpixel(x : Single ; y : Integer; Canvas: TCanvas; Color : TColor);
procedure vpixel(x : Integer; y : Single ; Canvas: TCanvas; Color : TColor);

procedure AALine(x1,y1,x2,y2 : Single; Color : TColor; Canvas : TCanvas);

implementation

function TestChanse(Sides: Integer): Boolean;
var
  Side: Integer;
begin
  RandomIze;
  Side:= Random(Sides);

  Result:= False;
  if Side = 0 then Result:= True;
end;

function ReplaceString(S,ch1,ch2: AnsiString): AnsiString;
Var
  S2  : AnsiString;
  i,n : Integer;
begin
  S2:= '';
  n:= 1;

  for i:= 1 to Length(S) do
    if Copy(S,n,Length(ch1)) <> ch1 then
    begin
      S2:= S2+Copy(S,n,1);
      n:= n+1;
    end else
    begin
      S2:= S2+ch2;
      n:= n+Length(ch1);
    end;

  Result:= S2;
end;

function FullSearchStr(Source: AnsiString; St: Char; En: Char; var Temp: Integer): AnsiString;
Var
  i1,i2:Integer;
begin
  i1:= Pos(St, Source);
  i2:= Pos(En, Source);

  if i2 <> 0 then Temp:= i2 + 1
  else Temp:= Length(Source);

  if (i1 <> 0) and (i2 <> 0) then FullSearchStr:= Copy(Source,i1+1,i2-i1-1)
  else FullSearchStr:= '';
end;

function GetStrINstr(Source: AnsiString; St: Char; En: Char): AnsiString;
Var
  A: Integer;
begin
  GetStrinstr:= FullSearchStr(Source,St,En,A);
end;

function StrReplace(S,ch1,ch2: AnsiString): AnsiString;
var
  S2: AnsiString;
  i: Integer;
begin
  S2:= '';

  for i:= 1 to Length(S) do
    if Copy(S,i,1) <> ch1 then
      S2:= S2 + Copy(S,i,1)
    else
      S2:= S2 + ch2;

  Result:= S2;
end;

function UpString(Source: AnsiString): AnsiString;
Var
  Dest: AnsiString;
  i: Integer;
begin
 Dest:= '';

 for i:= 1 to length(Source) do
  Dest:= Dest + UpCase(Source[i]);

 UpString:= Dest;
end;

function Sign(a: Real): Real;
begin
  if a > 0 then result:= 1
  else if a < 0 then result:= -1
  else result:= 0;
end;

procedure Pixel322RGB(pix: TGlPixel32; var _r,_g,_b,_a: byte);
begin
  _r:= pix.r;
  _g:= pix.g;
  _b:= pix.b;
  _a:= pix.a;
end;

function RGB2Pixel32(_r,_g,_b,_a: byte): TGlPixel32;
begin
  result.r:= _r;
  result.g:= _g;
  result.b:= _b;
  result.a:= _a;
end;

function PointInRect(pnt: TVector; x1,y1,x2,y2: Integer): Boolean; overload;
begin
  result:= (pnt[0] >= x1) and (pnt[1] >= y1) and (pnt[0] <= x2) and (pnt[1] <= y2);
end;

function PointInRect (pnt: TVector; r: TRect): Boolean; overload;
begin
  result:= PointInRect(pnt, r.left, r.top, r.right, r.bottom);
end;

function Ang(Angle: Single): Single;
begin
  Result:= ((((Round(Angle) mod 360) + 540) mod 360) - 180);
end;

procedure ActorAnimation(Actor: TGLActor; Animation: AnsiString);
begin
  if Animation <> Actor.CurrentAnimation then
     Actor.SwitchToAnimation(Animation);
end;

//

function CreateWorldMatrix(Scale, Pos, Axis: TVector; Angle: single): TMatrix;
var wm, mt, ms, mr: TMatrix;
begin
  ms:= CreateScaleMatrix(Scale);
  mt:= CreateTranslationMatrix(Pos);
  mr:= CreateRotationMatrix(Axis, Angle);
  wm:= IdentityHmgMatrix;
  wm:= MatrixMultiply(wm, mr);
  wm:= MatrixMultiply(wm, ms);
  wm:= MatrixMultiply(wm, mt);
  TransposeMatrix(wm);
  Result:= wm;
end;

//

function CrossFadeColor(FromColor,ToColor : TColor; Rate : Single) : TColor;
var
  r,g,b : byte;
begin
  r := Round(GetRValue(FromColor)*Rate+GetRValue(ToColor)*(1-Rate));
  g := Round(GetGValue(FromColor)*Rate+GetGValue(ToColor)*(1-Rate));
  b := Round(GetBValue(FromColor)*Rate+GetBValue(ToColor)*(1-Rate));

  Result:= RGB(r,g,b);
end;

procedure hpixel(x : Single; y : Integer; Canvas: TCanvas; Color : TColor);
var
  FadeRate : Single; 
begin
  FadeRate:= x-Trunc(x);

  with Canvas do
  begin
    pixels[trunc(x)+0,y] := CrossFadeColor(Color,Pixels[Trunc(x)+0,y], 1-FadeRate);
    pixels[trunc(x)+1,y] := CrossFadeColor(Color,Pixels[Trunc(x)+1,y],   FadeRate);
  end; 
end;

procedure vpixel(x : integer; y : single; Canvas: TCanvas; Color : TColor);
var
  FadeRate : Single;
begin
  FadeRate:= y-trunc(y);

  with canvas do
  begin
    pixels[x,trunc(y)]   := CrossFadeColor(Color,Pixels[x,Trunc(y)+0], 1-FadeRate);
    pixels[x,trunc(y)+1] := CrossFadeColor(Color,Pixels[x,Trunc(y)+1],   FadeRate);
  end;
end;

procedure AALine(x1,y1,x2,y2 : Single; Color : TColor; Canvas : TCanvas);
var
  i : integer;
  ly,lx,l,skipl     : Single;
  CurrentX,CurrentY : Single;
  DeltaX,DeltaY     : Single;
begin
  if (x1 <> x2) or (y1 <> y2) then
  begin
    CurrentX := x1;
    CurrentY := y1;

    lx := abs(x2-x1);
    ly := abs(y2-y1);

    if lx > ly then
    begin 
      l      := Trunc(lx);
      DeltaY := (y2-y1)/l;

      if x1 > x2 then
      begin 
        DeltaX := -1;
        skipl  := (Currentx-Trunc(CurrentX));
      end else
      begin
        DeltaX := 1;
        skipl  := 1-(CurrentX-Trunc(CurrentX));
      end;
    end else 
    begin 
      l      := Trunc(ly);
      DeltaX := (x2-x1)/l;

      if y1 > y2 then
      begin
        DeltaY := -1;
        skipl  := (Currenty-Trunc(Currenty));
      end else
      begin
        DeltaY := 1;
        skipl  := 1-(Currenty-Trunc(Currenty));
      end; 
    end; 

    CurrentX := CurrentX+DeltaX*skipl;
    CurrentY := CurrentY+DeltaY*skipl;

    for i:= 1 to Trunc(l) do
    begin
      if lx > ly then vpixel(Trunc(CurrentX),CurrentY,Canvas,Color) else hpixel(CurrentX,Trunc(CurrentY),Canvas,Color);

      CurrentX:= Currentx+DeltaX;
      CurrentY:= Currenty+DeltaY;
    end; 
  end; 
end;

// OpenGL Utils

procedure DrawCircleX(Radius: Single; Segments: Integer; Pos: Single);
var
  i: Integer;
  ang: Single;
begin
  glbegin(GL_LINE_STRIP);
    for i:= 0 to Segments do
    begin
      ang := pi * i / (Segments div 2);
      glVertex3f(Pos,Radius*sin(ang),Radius*cos(ang));
    end;
  glEnd;
end;

procedure DrawCircleY(Radius: Single; Segments: Integer; Pos: Single);
var
  i: Integer;
  ang: Single;
begin
  glbegin(GL_LINE_STRIP);
    for i:= 0 to Segments do
    begin
      ang:= pi * i / (Segments div 2);
      glVertex3f(Radius*sin(ang),Pos,Radius*cos(ang));
    end;
  glEnd;
end;

procedure DrawCircleZ(Radius: Single; Segments: Integer; Pos: Single);
var
  i: Integer;
  ang: Single;
begin
  glbegin(GL_LINE_STRIP);
    for i:= 0 to Segments do
    begin
      ang:= pi * i / (Segments div 2);
      glVertex3f(Radius*sin(ang),Radius*cos(ang),Pos);
    end;
  glEnd;
end;

// Arc

procedure DrawArcX(Radius: Single; Segments: Integer; Pos: Single);
var
  i: Integer;
  ang: Single;
begin
  glbegin(GL_LINE_STRIP);
    for i:= 0 to Segments do
    begin
      ang := (pi/2) * i / (Segments div 2);
      glVertex3f(Pos,Radius*sin(ang),Radius*cos(ang));
    end;
  glEnd;
end;

procedure DrawArcY(Radius: Single; Segments: Integer; Pos: Single);
var
  i: Integer;
  ang: Single;
begin
  glbegin(GL_LINE_STRIP);
    for i:= 0 to Segments do
    begin
      ang:= (pi/2) * i / (Segments div 2);
      glVertex3f(Radius*sin(ang),Pos,Radius*cos(ang));
    end;
  glEnd;
end;

procedure DrawArcZ(Radius: Single; Segments: Integer; Pos: Single);
var
  i: Integer;
  ang: Single;
begin
  glbegin(GL_LINE_STRIP);
    for i:= 0 to Segments do
    begin
      ang:= (pi/2) * i / (Segments div 2);
      glVertex3f(Radius*sin(ang),Radius*cos(ang),Pos);
    end;
  glEnd;
end;

// Quadric

procedure SetInvertedQuadricOrientation(quadric : PGLUquadricObj);
//const
   //cNormalDirectionToEnum : array [0..1] of TGLEnum =
   //   (GLU_OUTSIDE, GLU_INSIDE);
begin
   gluQuadricOrientation(quadric, GLU_OUTSIDE);
end;

end.
