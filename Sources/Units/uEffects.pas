unit uEffects;

interface

uses
  GLHUDObjects, GLRenderContextInfo, VectorGeometry;

type
  TExplode = class
  public
    Sprite   : TGLHUDSprite;
    isApply  : Boolean;
    X, Y     : Single;
    aAlpha   : Single;
    Color    : TVector;

    procedure Apply;
    procedure Step   (const deltaTime : Double);
    procedure Render (var rci : TRenderContextInfo);
  end;

implementation

//* TExplode *//

procedure TExplode.Apply;
begin
  isApply := True;
  Sprite.Material.FrontProperties.Diffuse.Alpha := 1;
  aAlpha := 1;
end;

procedure TExplode.Render(var rci: TRenderContextInfo);
begin
  Sprite.Material.FrontProperties.Diffuse.Alpha := aAlpha;
  Sprite.Position.SetPoint(16+X*32,16+Y*32,0);
  Sprite.Render(rci);
end;

procedure TExplode.Step(const deltaTime: Double);
begin
  if isApply then
  with Sprite, Sprite.Material.FrontProperties.Diffuse do
  begin
    if aAlpha > 0 then
    begin
      aAlpha := aAlpha - 3*deltaTime;
    end else isApply := False;
  end;
end;

end.
