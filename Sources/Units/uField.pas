//* uField - работает с классом TField - поле(стакан) тетриса

unit uField;

interface

uses
  uClasses, uConsts, uEffects, VectorGeometry, GLHUDObjects, uAudio,
  GLRenderContextInfo;

type
  //* TFieldBlock - ячейка поля
  //*
  //* Place  - состояние ячейки : 0 - свободна, 1 - занята
  //* Color  - цвет ячейки
  //* Vy     - реальная координата ячейки
  //* Effect - эффект сгорания ячейки при заполнении строки на поле

  TFieldBlock = record
    Place  : BoolInt;
    Color  : TVector;
    Vy     : Single;
    Effect : TExplode;
  end;

  //* TField - поле
  //*
  //* Sprite   - спрайт ячеек поля для рендера
  //* Data     - данные поля
  //* Score    - игровые очки
  //* Lines    - количество сгоревших линий
  //* Level    - уровень
  //*
  //* Mode     - Режим игры
  //* GameOver - Конец игры, если true

  TField = class
  public
    Sprite   : TGLHUDSprite;
    Data     : array[0..FieldWidth-1,0..FieldHeight-1] of TFieldBlock;
    Score    : Integer;
    Lines    : Integer;
    Level    : Integer;
    //
    Mode     : TModes;
    GameOver : Boolean;
    //
    Audio    : TAudioManager;
    Options  : TOptions;

    constructor Create (const aSprite : TGLHUDSprite);
    //
    procedure   ScanLines;
    procedure   Render (var rci : TRenderContextInfo);
    procedure   Step   (const deltaTime : Double);
    //
    procedure   Clear;
  end;

implementation

//* TField *//

//* Create - создание поля, заполняет ячейки нулями, и инициализирует каждую
//* ячейку
//*
//* входной параметр : aSprite - спрайт ячеек

constructor TField.Create (const aSprite : TGLHUDSprite);
var
  i,j : Integer;
begin
  for i := 0 to FieldWidth-1 do
    for j := 0 to FieldHeight-1 do
    with Data[i,j] do
    begin
      Place         := 0;
      vy            := j;
      Effect        := TExplode.Create;
      Effect.Sprite := aSprite;
      Effect.aAlpha := 0;
      Effect.X      := i;
      Effect.Y      := j;
    end;

  Score := 0;
  Lines := 0;
  Level := 1;
end;

//* ScanLines - проверяет, заполнены ли линии на поле, если заполнена, то
//* прибавляем очки (Score) и количество сгоревших линий (Lines), переводим
//* состояние ячеек строки в состояние "свободно", и применяем эффект сгоряния
//* для ечеек. Проверяет самую верхнюю (или самю нижнюю, если перевернуты
//* режим) и если есть хоть один блок, то конец игры

procedure TField.ScanLines;
var
  i,j,m,n,k : Integer;
begin
  for j := 0 to FieldHeight-1 do
  begin
    k := 0;

    for i := 0 to FieldWidth-1 do
      if Data[i,j].Place = 1 then k := k+1;

    if k = FieldWidth then
    begin
      Score := Score+40;
      Lines := Lines+1;

      if Options.Sound then Audio.PlaySource('Line');
      if Lines >= 10*Level then
      begin
        Lines := 0;
        Level := Level+1;
      end;
      
      for i := 0 to FieldWidth-1 do
      begin
        Data[i,j].Place := 0;
        Data[i,j].Effect.Apply;
      end;

      for m := 0 to FieldWidth-1 do
        if Mode = mMirror then
        begin
          for n := j to FieldHeight-2 do
          begin
            Data[m,n].Place := Data[m,n+1].Place;
            Data[m,n].Color := Data[m,n+1].Color;
            Data[m,n].Vy    := Data[m,n+1].Vy;
          end;
        end else
        begin
          for n := j downto 1 do
          begin
            Data[m,n].Place := Data[m,n-1].Place;
            Data[m,n].Color := Data[m,n-1].Color;
            Data[m,n].Vy    := Data[m,n-1].Vy;
          end;
        end;
    end;
  end;

  // Scan Game Over

  k := 0;
  
  if Mode = mMirror then j := FieldHeight-1
  else j := 0;

  for i := 0 to FieldWidth-1 do
    if Data[i,j].Place = 1 then k := k+1;

  if k > 0 then GameOver := True;
end;

//* Render - рисуем поле, проверяем каждую ячейку на состояние и если ячека
//* в состоянии "занята", то рисуем ее, и задаем цвет ячейки
//* рисуем эффект если его состояние isApply = true

procedure TField.Render(var rci: TRenderContextInfo);
var
  i,j : Integer;
begin
  for i := 0 to FieldWidth-1 do
    for j := 0 to FieldHeight-1 do
    begin
      if Data[i,j].Place = 1 then
      begin
        Sprite.Material.FrontProperties.Diffuse.Color := Data[i,j].Color;
        Sprite.Position.SetPoint(16+i*32,16+Data[i,j].Vy*32,0);
        Sprite.Render(rci);
      end;

      Data[i,j].Effect.Render(rci);
    end;
end;

//* Step - если эффект сгорания ячейки isApply, то просчитываем этот эффект
//* и блоки не падают иначе плавно перемещаем блоки на следующую строку

procedure TField.Step(const deltaTime : Double);
var
  i,j : Integer;
begin
  for i := 0 to FieldWidth-1 do
    for j := 0 to FieldHeight-1 do
      Data[i,j].Effect.Step(deltaTime);

  for i := 0 to FieldWidth-1 do
    for j := 0 to FieldHeight-1 do
      if Data[i,j].Effect.isApply then Exit;

  for i := 0 to FieldWidth-1 do
    for j := 0 to FieldHeight-1 do
      if Mode = mMirror then
      begin
        if Data[i,j].Vy > j then Data[i,j].Vy := Data[i,j].Vy-5*deltaTime;
        if Data[i,j].Vy < j then Data[i,j].Vy := j;
      end else
      begin
        if Data[i,j].Vy < j then Data[i,j].Vy := Data[i,j].Vy+5*deltaTime;
        if Data[i,j].Vy > j then Data[i,j].Vy := j;
      end;
end;

procedure TField.Clear;
var
  i,j : Integer;
begin
  for i := 0 to FieldWidth-1 do
    for j := 0 to FieldHeight-1 do
      Data[i,j].Place := 0;
end;

end.
