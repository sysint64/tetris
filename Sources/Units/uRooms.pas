//* uRooms - работает с комнатами(уровнями) игры

unit uRooms;

interface

uses
  uInterfaces, Dialogs, GLRenderContextInfo;

type
  //* TRoomStep - указатель на функция шага игры
  TRoomStep   = function  (const DeltaTime: Double): Boolean  of Object;

  //* TRoomRender - указатель на процедуру рендера комнаты
  TRoomRender = procedure (var rci: TRenderContextInfo) of Object;

  //* TRoom  - Одна комната событий (можно по другому назвать уровень)
  //*
  //* ID     - индекс комнаты в массиве
  //* Name   - имя комнаты для поиска в масиве
  //* Step   - указатель на функцию шага игры
  //* Render - указатель на функцию рендера комнаты

  TRoom = class
  public
    ID     : Integer;
    Name   : ShortString;
    Step   : TRoomStep;
    Render : TRoomRender;
  end;

  //* TRoomsManager - Менеджер комнат, класс для управление комнатами
  //*
  //* Rooms      - массив с комнатами
  //* GUIManager - менеджер интерфейса
  //* Time       - таймер для переходов по событиям комнате

  TRoomsManager = class
    Rooms         : array of TRoom;
    GUIManager    : TGUIManager;
    //
    Count         : Integer;
    CurrentRoom   : Integer;
    Time          : Double;

    constructor Create;
    //
    function    AddRoom : TRoom;
    procedure   NextRoom;
    procedure   PrevRoom;
    function    Room     (const Name      : ShortString) : TRoom;
    procedure   GoToRoom (const id        : ShortString);
    procedure   Step     (const deltaTime : Double);
    procedure   Render   (var   rci       : TRenderContextInfo);
  end;

implementation

//* TRoomManager *//

//* Create - конструктор класса, обнуляем переменные

constructor TRoomsManager.Create;
begin
  CurrentRoom := 0;
  Count       := 0;
  Time        := 0;
end;

//* AddRoom - добавляет новую комнату в массив
//* выходной параметр - добавленная в массив комната

function TRoomsManager.AddRoom : TRoom;
begin
  SetLength (Rooms,Count + 1);

  Rooms[Count]    := TRoom.Create;
  Rooms[Count].ID := Count;

  Result := Rooms[Count];
  Count  := Count + 1;
end;

//* Переходим в комнату с именем id, очищаем интерфейс и сцену
//* входной парамтр : id - имя комнаты

procedure TRoomsManager.GoToRoom (const id : ShortString);
begin
  GUIManager.Clear(-1);
  //Scene.ClearAll;
  CurrentRoom := Room(id).ID;
  Time        := 0;
end;

//* NextRoom - переход на следующую комнату

procedure TRoomsManager.NextRoom;
begin
  GUIManager.Clear(-1);
  //Scene.ClearAll;
  CurrentRoom:= CurrentRoom + 1;
  Time:= 0;
end;

//* PrevRoom - переходим на предыдущую комнату

procedure TRoomsManager.PrevRoom;
begin
  GUIManager.Clear(-1);
  //Scene.ClearAll;
  CurrentRoom:= CurrentRoom - 1;
  Time:= 0;
end;

//* Room - ищет комнтау с именем Name
//*
//* входной параметр  : Name - имя комнаты
//* выходной параметр : комната с именем Name, если комната не найдена, то
//*                     возвращаем функции nil 

function TRoomsManager.Room (const Name : ShortString): TRoom;
Var
  i: Integer;
begin
  Result:= nil;
  Time:= 0;

  for i:= 0 to Count - 1 do
    if Assigned(Rooms[i]) then
      if Rooms[i].Name = Name then Result:= Rooms[i];
end;

//* Step - игровой цикл комнаты
//* входной параметр : DeltaTime - приращение времени

procedure TRoomsManager.Step (const DeltaTime: Double);
begin
  if Assigned(Rooms[CurrentRoom]) then
    if not Rooms[CurrentRoom].Step(DeltaTime) then
      Time := Time + DeltaTime;
end;

//* Render - игровой рендер (отрисовка) комнаты
//* входной параметр : rci - информация контекста для ренедра

procedure TRoomsManager.Render (var rci: TRenderContextInfo);
begin
  if Assigned(Rooms[CurrentRoom]) and Assigned(Rooms[CurrentRoom].Render) then
    if Time > 0 then Rooms[CurrentRoom].Render(rci);
end;

end.
