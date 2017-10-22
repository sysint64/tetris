//* uBlock - работает с классом TBlock - фигура тетриса

unit uBlock;

interface

uses
  uClasses, uConsts, uField, GLHUDObjects, GLRenderContextInfo,
  VectorGeometry, uKeyboard, uAudio;

type
  TBlocksData = array of array of array of BoolInt;

  //* TBlock     - фигура на поле
  //*
  //* Sprite     - Спрайт ячейки фигуры для рендера
  //* X, Y       - Координаты фигуры на сетке поля
  //* Vx, Vy     - Реальны координаты фигуры
  //* BlocksData - Доступные фигуры
  //* Data       - Данные фигуры, сетка, где 0 - пустаяя ячейка 1 - заполненная
  //*              ячейка
  //* keyRotate  - Сосотояние клавиши вращения фигуры
  //* Color      - Цвет фигуры
  //* Field      - Игровое поле
  //* BlockId    - Индекс фигуры в BlockData
  //* PreBlock   - Следующая фигура
  //* Mode       - Режим игры

  TBlock      = class
  private
    procedure ToCorner;
  public
    Sprite     : TGLHUDSprite;
    X,Y        : Integer;
    vx,vy      : Single;
    BlocksData : TBlocksData;
    Data       : array[0..3,0..3] of BoolInt;
    keyRotate  : Boolean;
    Color      : TVector;
    Field      : TField;
    BlockId    : Integer;
    PreBlock   : TBlock;
    //
    Mode       : TModes;
    Audio      : TAudioManager;
    Options    : TOptions;

    procedure LoadFormFile     (const FileName  : ShortString);
    procedure AppendFormFile   (const FileName  : ShortString);
    //
    procedure Rotate;
    procedure ScanCollisionV;
    procedure ScanCollisionH   (const key       : Word);
    procedure SetBlock         (const N         : Integer);
    procedure NextBlock        (const N         : Integer);
    procedure AddBlockInField;
    function  SizeBlockV : Integer;
    //
    procedure Render           (var   rci       : TRenderContextInfo);
    procedure Step             (const DeltaTime : Double);
  end;

implementation

//* TBlock *//

//* ToCorner - Смещает элементы фигуры, если эелементы не на нулевых
//* координатах

procedure TBlock.ToCorner;
var
  i,j : Integer;
  A   : Boolean;
begin
  A := False;

  while not A do
  begin
    for i := 0 to 3 do
      if Data[i,0] = 1 then A := True;

    if not A then
    begin
      for i := 0 to 3 do
        for j := 0 to 2 do
          Data[i,j] := Data[i,j+1];

      for i := 0 to 3 do
        Data[i,3] := 0;
    end;
  end;

  A := False;

  while not A do
  begin
    for j := 0 to 3 do
      if Data[0,j] = 1 then A := True;

    if not A then
    begin
      for j := 0 to 3 do
        for i := 0 to 2 do
          Data[i,j] := Data[i+1,j];

      for j := 0 to 3 do
        Data[3,j] := 0;
    end;
  end;
end;

//* SizeBlockV - Определяет размер фигуры по вертикали
//* Выходной параметр : размер фигуры по вертикали

function TBlock.SizeBlockV : Integer;
var
  k,i,j : Integer;
begin
  k      := 0;
  Result := 0;

  for j := 0 to 3 do
  begin
    for i := 0 to 3 do
    begin
      k := k+Data[i,j];
      if (k <> 0) and (i = 3) then Result := Result+1;
    end;

    k := 0;
  end;
end;

//* AppendFormFile - Добавляет новые фигур в BlockData из файла
//* Входной параметр : FileName - путь файла с фигурами

procedure TBlock.AppendFormFile(const FileName: ShortString);
var
  F   : TextFile;
  v   : Char;
  n   : Integer;
  i,j : Integer;
begin
  AssignFile(F,FileName);
  Reset(F);
  n := Length(BlocksData);

  while not EOF(F) do
  begin
    SetLength (BlocksData,n,4,4);

    for j := 0 to 3 do
      for i := 0 to 3 do
      begin
        if i = 3   then ReadLn (F, v) else Read (F, v);
        if v = '1' then BlocksData[n-1,i,j] := 1;
      end;

    n := n+1;
  end;

  CloseFile(F);
end;

//* LoadFormFile - Перезагрузка фигур из файла
//* Входной параметр : FileName - путь файла с фигурами

procedure TBlock.LoadFormFile(const FileName: ShortString);
var
  F   : TextFile;
  v   : Char;
  n   : Integer;
  i,j : Integer;
begin
  AssignFile(F,FileName);
  Reset(F);
  n := 1;

  while not EOF(F) do
  begin
    SetLength (BlocksData,n,4,4);

    for j := 0 to 3 do
      for i := 0 to 3 do
      begin
        if i = 3   then ReadLn (F, v) else Read (F, v);
        if v = '1' then BlocksData[n-1,i,j] := 1;
      end;

    n := n+1;
  end;

  CloseFile(F);
end;

//* NextBlock - Следующая фигура на поле
//* Входной параметр : N - индекс новой фигуры

procedure TBlock.NextBlock (const N : Integer);
begin
  if Mode = mMirror then Y := FieldHeight-SizeBlockV
  else Y := 0;

  X     := FieldWidth div 2-1;
  Vx    := X; Vy := Y;
  Color := PreBlock.Color;
  PreBlock.SetBlock(Random(Length(BlocksData)));
  PreBlock.Color := VectorMake((51+Random(255-51))/255,(51+Random(255-51))/255,(51+Random(255-51))/255);
  SetBlock (N);
end;

//* Rotate - Вращает фигуру на 90 градусов

procedure TBlock.Rotate;
var
  i,j : Integer;
  tmp : array[0..3,0..3] of BoolInt;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      tmp[3-j,i] := Data[i,j];

  for i := 0 to 3 do
    for j := 0 to 3 do
      Data[i,j] := tmp[i,j];

  ToCorner;
end;

//* Render - Рисует фигуру, считывает значение из Data и проверяет,
//* если значение равно 1 то рисуем спрайт Sprite и задаем ему цвет
//* иначе ничего не рисуем

procedure TBlock.Render(var rci: TRenderContextInfo);
var
  i,j : Integer;
begin
  Sprite.Material.FrontProperties.Diffuse.Color := Color;

  for i := 0 to 3 do
    for j := 0 to 3 do
      if Data[i,j] = 1 then
      begin
        Sprite.Position.SetPoint(16+(vx+i)*32,16+(vy+j)*32,0);
        Sprite.Render(rci);
      end;
end;

//* Step - Плавное перемещениефигуры
//* Входной параметр : DeltaTime - приращение времени

procedure TBlock.Step(const DeltaTime: Double);
begin
  if Vx > X then Vx := Vx-10*deltaTime;
  if Vx < X then Vx := Vx+10*deltaTime;

  if Mode = mMirror then
  begin
    if Vy < Y then Vy := Y;
    if Vy > Y then Vy := Vy-10*deltaTime;
  end else
  begin
    if Vy > Y then Vy := Y;
    if Vy < Y then Vy := Vy+10*deltaTime;
  end;
end;

//* AddBlockInField - Добавляет текущую фигуру на поле, заполняя массив
//* поля новыми значениями из текущего блока и в стакан помещаем еще одну
//* фигуру (точнее в игре всего одна фигура, просто делаем такую не большую
//* имитацию)

procedure TBlock.AddBlockInField;
var
  i, j : Integer;
  c    : TVector;
  b    : BoolInt;
begin
  if Options.Sound then Audio.PlaySource('Put');
  for i := 0 to 3 do
    for j := 0 to 3 do
      if (Data[i,j] = 1) then
        if (X+i < FieldWidth)  and (Y+j < FieldHeight) then
        begin
          Field.Data[X+i,Y+j].Place := 1;
          Field.Data[X+i,Y+j].Color := Color;
        end;

  //

  if Mode = mRight then
    for j := 0 to FieldHeight-1 do
    begin
      b := Field.Data[FieldWidth-1,j].Place;
      c := Field.Data[FieldWidth-1,j].Color;

      for i := FieldWidth-1 downto 1 do
      begin
        Field.Data[i,j].Place := Field.Data[i-1,j].Place;
        Field.Data[i,j].Color := Field.Data[i-1,j].Color;
      end;

      Field.Data[0,j].Place := b;
      Field.Data[0,j].Color := c;
    end;

  //

  if Mode = mLeft then
    for j := 0 to FieldHeight-1 do
    begin
      b := Field.Data[0,j].Place;
      c := Field.Data[0,j].Color;

      for i := 0 to FieldWidth-2 do
      begin
        Field.Data[i,j].Place := Field.Data[i+1,j].Place;
        Field.Data[i,j].Color := Field.Data[i+1,j].Color;
      end;

      Field.Data[FieldWidth-1,j].Place := b;
      Field.Data[FieldWidth-1,j].Color := c;
    end;

  Randomize;
  NextBlock (PreBlock.BlockId);
end;

//* ScanCollisionH - Проверяет на столкновение по горизонтали, проверяет каждый
//* элемент фигуры на столкновение с каждым элементом поля, если нажата клавиша
//* и в месте куда должна переместиться фигура есть занятый блок, то ничего не
//* происходин, иначе двигаем фигуру
//*
//* Входной параметр : key - код клавиши с клавиатуры, в зависимости от нажатой
//* клавиши двигаем фигуру

procedure TBlock.ScanCollisionH(const key: word);
var
  Collision : Boolean;
  i,j,w,k   : Integer;
  m,n       : Integer;
  z         : Integer;

begin
  w := 0;

  for j := 0 to 3 do
  begin
    for i := 0 to 3 do
    begin
      k := k+Data[j,i];
      if (k <> 0) and (i = 3) then w := w+1;
    end;

    k := 0;
  end;
  //w := SizeBlock;

  if (key = keyLeftArrow) and (X > 0) then
  begin
    Collision := False;
  
    for i := 0 to FieldWidth-1 do for j := 0 to FieldHeight-1 do
      if Field.Data[i,j].Place = 1 then
        for m := 0 to 3 do for n := 0 to 3 do
          if (X-1+m = i) and (Y+n = j) then
          if (Data[m,n] = 1) then Collision := True;

    if not Collision then X := X-1;
  end;

  if (key = keyRightArrow) and (X+w < FieldWidth) then
  begin
    Collision := False;

    for i := 0 to FieldWidth-1 do for j := 0 to FieldHeight-1 do
      if Field.Data[i,j].Place = 1 then
        for m := 0 to 3 do for n := 0 to 3 do
          if (X+1+m = i) and (Y+n = j) then
          if (Data[m,n] = 1) then Collision := True;

    if not Collision then X := X+1;
  end;
end;

//* SetBlock - Задает новую фигуру из BlockData
//* Входной параметр : N - индекс новой фигуры

procedure TBlock.SetBlock(const N: Integer);
var
  i,j : Word;
begin
  BlockId := N;
  
  for i := 0 to 3 do
    for j := 0 to 3 do
      Data[i,j] := BlocksData[n,i,j];
end;

//* ScanCollisionV - Проверяет на столкновение по вертикали, проверяет каждый
//* элемент фигуры на столкновение с каждым элементом поля, если ниже на одну
//* клетки поля занята, то добавляем этот блок на поле и переходим к новой
//* фигуре

procedure TBlock.ScanCollisionV;
var
  i,j,w,k   : Integer;
  m,n,t     : Integer;
  Collision : Boolean;
begin
  w := SizeBlockV;

  if Mode = mMirror then t := -1
  else t := 1;

  Collision := False;
  
  for i := 0 to FieldWidth-1 do for j := 0 to FieldHeight-1 do
    if Field.Data[i,j].Place = 1 then
      for m := 0 to 3 do for n := 0 to 3 do
        if (X+m = i) and  ((Y+(1*t))+n = j) then
        if (Data[m,n] = 1) then Collision := True;

  if Mode = mMirror then
  begin
    if ((Y) < 1) or (Collision) then AddBlockInField;
  end else if ((Y+w) > FieldHeight-1) or (Collision) then AddBlockInField;
end;

end.
