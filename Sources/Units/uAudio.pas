//* uAudio - модуль для работы с аудио на библиотеке OpenAL

unit uAudio;

interface

uses
  AL, ALTypes, ALUt, EAX, CLasses, SysUtils;

type

  //* TBuffer - Буффер
  //*
  //* Buffer   - Буффер аудио потока
  //* Name     - Имя буффера
  //* FileName - Путь к аудио файлу
  //* ID       - Порядковый номер в массиве

  TBuffer = class
  public
    Buffer   : TALuint;
    Name     : AnsiString;
    FileName : AnsiString;
    ID       : Integer;
  end;

  //* TSource - Источник
  //*
  //* Private :
  //*   Pos     - Положение в источника в пространстве
  //*   Vel     - Скорость источника
  //*
  //* Public :
  //*   Name    - Имя источника
  //*   Loop    - Использовать зацикливание аудио
  //*   Eax     - Использовать EAX эффекты
  //*   Volume  - Громкость аудио
  //*   Buffer  - Буффер
  //*   Env     - Окружение для EAX эффекта
  //*   ID      - Порядковый номер в массиве
  //*   Playing - Игрет аудио если true
  //*   Source  - Источник аудио

  TSource = class
  private
    Pos : array [0..2] of TALFloat;
    Vel : array [0..2] of TALFloat;
  public
    Name     : AnsiString;
    Loop     : Boolean;
    Eax      : Boolean;
    Volume   : TALFloat;
    Buffer   : TBuffer;
    Env      : TALuint;
    ID       : Integer;
    Playing  : Boolean;
    Source   : TALuint;

    procedure   SetPos(x,y,z: TALFloat);
    procedure   SetVel(x,y,z: TALFloat);
    procedure   Update;
    constructor Create;
  end;

  //* TListener - Слушатель
  //*
  //* Private :
  //*   Pos - Положение слушателя в пространстве
  //*   Vel - Скорость слушателя в пространстве
  //*   Ori - Ориентация слушателя

  TListener = class
  private
    Pos : array [0..2] of TALFloat;
    Vel : array [0..2] of TALFloat;
    Ori : array [0..5] of TALFloat;
  public
    procedure   SetPos(x,y,z : TALFloat);
    procedure   SetVel(x,y,z : TALFloat);
    procedure   SetOri(x1,y1,z1,x2,y2,z2 : TALFloat);
    constructor Create;
  end;

  //* TSoundLibrary - Звуковая библиотека
  //*
  //* Sources - Массив источников
  //* Buffers - Массив бефферов
  //*
  //* PSCount - Длина массива Sources
  //* PBCount : Integer; - Длина массива Buffers

  TSoundLibrary = class
  public
    Sources : array of TSource;
    Buffers : array of TBuffer;

    PSCount : Integer;
    PBCount : Integer;

    //
    constructor Create;
    function AddBuffer (FileName : AnsiString) : TBuffer;
    function AddSource (Buffer   : TBuffer; Loop: Boolean; Volume: TALFloat) : TSource;
    function Buffer    (Name     : AnsiString) : TBuffer;
    function Source    (Name     : AnsiString) : TSource;
  end;

  //* TAudioManager - Менеджер аудио
  //*
  //* SoundLibrary - Звуковая библиотека
  //* Listener     - Слушатель
  //* EAX          - Использовать EAX эффект если true

  TAudioManager = class
  public
    SoundLibrary : TSoundLibrary;
    Listener     : TListener;
    EAX          : Boolean;

    //
    procedure   InitBuffers;
    procedure   InitSources;
    procedure   PlaySource  (Name : AnsiString);
    procedure   PauseSource (Name : AnsiString);
    procedure   StopSource  (Name : AnsiString);
    procedure   Start;
    procedure   ClearAll;
    procedure   ClearBuffers;
    procedure   ClearSources;
    constructor Create;
    destructor  Destroy;
  end;

implementation

//* TSource *//

//* Create - Конструктор класса, задает началые значения переменным

constructor TSource.Create;
begin
  Env := EAX_ENVIRONMENT_GENERIC;
  SetPos(0.0,0.0,0.0);
  SetVel(0.0,0.0,0.0);
end;

//* SetPos - Задает источнику положение в пространстве
//* Входные параметры : x,y,z - координаты

procedure TSource.SetPos(x,y,z: TALfloat);
begin
  Pos[0]:= x; Pos[1]:= y; Pos[2]:= z;
end;

//* SetVel - Задает скорость источнику
//* Входные параметры : x,y,z - координаты

procedure TSource.SetVel(x,y,z: TALfloat);
begin
  Vel[0]:= x; Vel[1]:= y; Vel[2]:= z;
end;

//* Update - Обновляет источник

procedure TSource.Update;
var
  FnName : PALubyte;
begin
  if Eax then
  begin
    alIsExtensionPresent('EAX2.0');
    FnName := 'EAXSet';
    eaxSet := alGetProcAddress(FnName);
    FnName := 'EAXGet';
    eaxGet := alGetProcAddress(FnName);

    eaxSet(DSPROPSETID_EAX20_ListenerProperties,
           DSPROPERTY_EAXLISTENER_ENVIRONMENT or
           DSPROPERTY_EAXLISTENER_DEFERRED,
           0, @Env, sizeof(TALuint));

    eaxSet(DSPROPSETID_EAX20_BufferProperties,
           DSPROPERTY_EAXBUFFER_COMMITDEFERREDSETTINGS,
           Source, nil, 0);

    eaxSet(DSPROPSETID_EAX20_ListenerProperties,
           DSPROPERTY_EAXLISTENER_COMMITDEFERREDSETTINGS, 0, nil, 0);
  end;

  AlSourcei ( Source, AL_BUFFER, Buffer.Buffer);
  AlSourcef ( Source, AL_PITCH, 1.0 );
  AlSourcef ( Source, AL_GAIN, Volume );
  AlSourcefv( Source, AL_POSITION, @Pos);
  AlSourcefv( Source, AL_VELOCITY, @Vel);

  if Loop then
    AlSourcei ( Source, AL_LOOPING, AL_TRUE);
end;

//* TListener *//

//* Create - Конструктор класса, задает началные значения переменным

constructor TListener.Create;
begin
  SetPos(0.0,0.0,0.0);
  SetVel(0.0,0.0,0.0);
  SetOri(0.0,0.0,-1.0,0.0,1.0,0.0);
end;

//* SetOri - Задает ориентацию источника
//* Входные параметры :
//*   x1,y1,z1 - Direction
//*   x2,y2,z2 - Up

procedure TListener.SetOri(x1, y1, z1, x2, y2, z2: TALfloat);
begin
  Ori[0]:= x1; Ori[1]:= y1; Ori[2]:= z1;
  Ori[3]:= x2; Ori[4]:= y2; Ori[3]:= z2;
end;

//* SetPos - Задает слушателю положение в пространстве
//* Входные параметры : x,y,z - координаты

procedure TListener.SetPos(x, y, z: TALfloat);
begin
  Pos[0]:= x; Pos[1]:= y; Pos[2]:= z;
end;

//* SetPos - Задает слушателю скорость
//* Входные параметры : x,y,z - координаты

procedure TListener.SetVel(x, y, z: TALfloat);
begin
  Vel[0]:= x; Vel[1]:= y; Vel[2]:= z;
end;

//* TAudioManager *//

//* Create - Конструктор класса, создает основные объекты

constructor TAudioManager.Create;
var
  argv : array of PChar;
begin
  Listener     := TListener.Create;
  SoundLibrary := TSoundLibrary.Create;
  AlutInit(nil, argv);
end;

//* Destroy - Деструктор класса, освобождает память и прекращает работу
//* с библиотекой OpenAL

destructor TAudioManager.Destroy;
begin
  ClearAll;
  AlutExit();
end;

//* ClearAll - Очищает массив буфферов и источников

procedure TAudioManager.ClearAll;
begin
  ClearBuffers;
  ClearSources;
end;

//* ClearBuffers - Очищает массив буфферов

procedure TAudioManager.ClearBuffers;
var
  i: Integer;
begin
  for i:= 0 to SoundLibrary.PBCount - 1 do
    AlDeleteBuffers(1, @SoundLibrary.Buffers[i].Buffer);

  SoundLibrary.PBCount:= 0;
end;

//* ClearSources - Очищает массив источников

procedure TAudioManager.ClearSources;
var
  i: Integer;
begin
  for i:= 0 to SoundLibrary.PSCount - 1 do
    AlDeleteSources(1, @SoundLibrary.Sources[i].Source);

  SoundLibrary.PSCount:= 0;
end;

//* InitBuffers - инициализирует буфферы - загружает звук в память

procedure TAudioManager.InitBuffers;
var
  i       : Integer;
  Data    : TALVoid;
  FileExt : AnsiString;
  OggExt  : TAlBoolean;
  OggFile : Tmemorystream;

  Format  : TALEnum;
  Size    : TALSizei;
  Freq    : TALSizei;
  LoopInt : TALInt;
begin
  for i:= 0 to SoundLibrary.PBCount - 1 do
  begin
    AlGenBuffers(1, @SoundLibrary.Buffers[i].Buffer);
    FileExt := ExtractFileExt(SoundLibrary.Buffers[i].FileName);

    if CompareText ('.wav',FileExt) = 0 then
    begin
      AlutLoadWavFile (SoundLibrary.Buffers[i].FileName, format, data, size, freq, LoopInt);
      AlBufferData    (SoundLibrary.Buffers[i].Buffer, format, data, size, freq);
      AlutUnloadWav   (format, data, size, freq);
    end else if CompareText ('.ogg',FileExt) = 0 then
    begin
      oggext := alIsExtensionPresent('AL_EXT_vorbis');

      if oggext then
      begin
        oggfile:= TMemoryStream.Create;
        oggfile.LoadFromFile(SoundLibrary.Buffers[i].FileName);
        AlBufferData(SoundLibrary.Buffers[i].Buffer, AL_FORMAT_VORBIS_EXT, oggfile.Memory, oggfile.Size, 44800);
        oggfile.Free;
      end;
    end;
  end;
end;

//* InitSources - Иницаилизирует источники - создает их в памяти и обновляет

procedure TAudioManager.InitSources;
var
  i : Integer;
begin
  for i:= 0 to SoundLibrary.PSCount - 1 do
  begin
    AlGenSources(1, @SoundLibrary.Sources[i].Source);
    SoundLibrary.Sources[i].Update;
  end;
end;

//* PlaySource - Проигрывает источник с именем Name
//* Входной параметр : Name - имя источника

procedure TAudioManager.PlaySource(Name : AnsiString);
var
  i : Integer;
begin
  for i:= 0 to SoundLibrary.PSCount - 1 do
    if SoundLibrary.Sources[i].Name = Name then
    begin
      AlSourcePlay(SoundLibrary.Sources[i].Source);
      SoundLibrary.Sources[i].Playing:= True;
      Exit;
    end;
end;

//* PauseSource - Приостанавливает проигрывание источника с именем Name
//* Входной параметр : Name - имя источника

procedure TAudioManager.PauseSource(Name : AnsiString);
var
  i : Integer;
begin
  for i:= 0 to SoundLibrary.PSCount - 1 do
    if SoundLibrary.Sources[i].Name = Name then
    begin
      AlSourcePause(SoundLibrary.Sources[i].Source);
      SoundLibrary.Sources[i].Playing:= False;
      exit;
    end;
end;

//* PauseSource - Останавливает проигрывание источника с именем Name
//* Входной параметр : Name - имя источника

procedure TAudioManager.StopSource(Name: AnsiString);
var
  i : Integer;
begin
  for i:= 0 to SoundLibrary.PSCount - 1 do
    if SoundLibrary.Sources[i].Name = Name then
    begin
      AlSourceStop(SoundLibrary.Sources[i].Source);
      SoundLibrary.Sources[i].Playing:= False;
      exit;
    end;
end;

//* Start - Иницализирует все источники и буфферы

procedure TAudioManager.Start;
var
  i : Integer;
begin
  InitBuffers;
  InitSources;
end;

//* TSoundLibrary *//

//* Create - Конструктор класса, обнулает счетчики

constructor TSoundLibrary.Create;
begin
  PSCount := 0;
  PBCount := 0;
end;

//* AddBuffer - Добавляет буффер в библиотеку
//*
//* Входной параметр : FileName - Путь к аудио файлу
//* Выходной параметр - Буффер

function TSoundLibrary.AddBuffer(FileName: AnsiString): TBuffer;
begin
  SetLength(Buffers,PBCount+1);

  Buffers[PBCount]          := TBuffer.Create;
  Buffers[PBCount].ID       := PBCount;
  Buffers[PBCount].FileName := FileName;

  Result  := Buffers[PBCount];
  PBCount := PBCount + 1;
end;

//* AddSource - Добавляет источник в библиотеку
//*
//* Входные параметр :
//*   Buffer - Буффер
//*   Loop   - Зацикливать, если true
//*   Volume - Громкость источника
//*
//* Выходной параметр - Источник

function TSoundLibrary.AddSource(Buffer: TBuffer; Loop: Boolean; Volume: TALFloat): TSource;
begin
  SetLength(Sources,PSCount+1);
  
  Sources[PSCount]        := TSource.Create;
  Sources[PSCount].ID     := PSCount;
  Sources[PSCount].Buffer := Buffer;
  Sources[PSCount].Loop   := Loop;
  Sources[PSCount].Volume := Volume;

  Result  := Sources[PSCount];
  PSCount := PSCount + 1;
end;

//* Buffer - Ищет буффер с именем Name
//*
//* Входной параметр : Name - имя буффера
//* Выходной параметр - Буффер

function TSoundLibrary.Buffer(Name : AnsiString): TBuffer;
var
  i : Integer;
begin
  Result := nil;

  for i := 0 to PBCount - 1 do
    if Buffers[i].Name = Name then
    begin
      Result := Buffers[i];
      Exit;
    end;
end;

//* Buffer - Ищет источник с именем Name
//*
//* Входной параметр : Name - имя источника
//* Выходной параметр - Источник

function TSoundLibrary.Source(Name: AnsiString): TSource;
var
  i : Integer;
begin
  Result := nil;

  for i := 0 to PSCount - 1 do
    if Sources[i].Name = Name then
    begin
      Result := Sources[i];
      Exit;
    end;
end;

end.
