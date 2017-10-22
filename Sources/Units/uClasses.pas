//* uClasses - в нем хранятся дополнительные классы для работы с игрой

unit uClasses;

interface

uses
  GLScene, GLMaterial, GLTexture, GLGraphics, GLHUDObjects, uAudio;

type
  BoolInt = 0..1;

  //* TMode - Режим игры
  //*
  //* mClassic - Классический режим
  //* mMirror  - Перевернутый тетрис
  //* mLeft    - Тетрис со сдвигом влево
  //* mRight   - Тетрис со сдвигом вправо

  TModes   = (mClassic, mMirror, mLeft, mRight);

  //* TQuility - Качество графики
  //*
  //* qtLow    - Низкое
  //* qtMedium - Среднее
  //* qtHigh   - Высокое

  TQuility = (qtLow, qtMedium, qtHigh);

  //* TOptions - Опции игры
  //*
  //* Music     - Музыка
  //* Sound     - Звук
  //* NewBlocks - Дополнительные блоки
  //* Quility   - Качество

  TOptions = class
  public
    Music     : Boolean;
    Sound     : Boolean;
    NewBlocks : Boolean;
    Quility   : TQuility;
    Sprite    : TGLHUDSprite;

    constructor Create;
    //
    procedure Save  (const FileName : ShortString);
    procedure Load  (const FileName : ShortString);
    procedure Apply (const Scene    : TGLScene; const Audio : TAudioManager);
  end;

implementation

//* TOptions *//

//* Create - Конструктор класса, задает стандартные настройки

constructor TOptions.Create;
begin
  Music     := True;
  Sound     := True;
  NewBlocks := False;
  Quility   := qtMedium;
end;

//* Save - Сохраняет настройки игры в файл
//* Входной параметр : FileName - путь к файлу настроек

procedure TOptions.Save(const FileName: ShortString);
var
  F : TextFile;
begin
  AssignFile (F, FileName);
  Rewrite    (F);

  if Music     then WriteLn (F, 'true') else WriteLn (F, 'false');
  if Sound     then WriteLn (F, 'true') else WriteLn (F, 'false');
  if NewBlocks then WriteLn (F, 'true') else WriteLn (F, 'false');

  case Quility of
    qtLow    : WriteLn (F, 'low');
    qtMedium : WriteLn (F, 'medium');
    qtHigh   : WriteLn (F, 'high');
  end;

  CloseFile  (F);
end;

//* Load - Загружает настройки из файла
//* Входной параметр : FileName - путь к файлу настроек

procedure TOptions.Load(const FileName: ShortString);
var
  F : TextFile;
  S : ShortString;
begin
  AssignFile (F, FileName);
  Reset      (F);

  ReadLn (F, S); if S = 'true' then Music     := True else Music     := False;
  ReadLn (F, S); if S = 'true' then Sound     := True else Sound     := False;
  ReadLn (F, S); if S = 'true' then NewBlocks := True else NewBlocks := False;
  ReadLn (F, S);

  if S = 'low'    then Quility := qtLow;
  if S = 'medium' then Quility := qtMedium;
  if S = 'high'   then Quility := qtHigh;

  CloseFile  (F);
end;

//* Apply - применяет текущие настройки к игре
//*
//* Входные параметры:
//*   Scene - сцена игры
//*   Audio - аудио менеджер игры

procedure TOptions.Apply(const Scene: TGLScene; const Audio : TAudioManager);
var
  i,j : Integer;
  c   : TGLTextureCompression;
  f   : TGLTextureFormat;
begin
  c := tcNone; f := tfDefault;

  case Quility of
    qtLow    : f := tfRGBA16;
    qtMedium : c := tcHighSpeed;
    qtHigh   : // None
  end;

  for i := 0 to Scene.Objects.Count-1 do
    for j := 0 to Scene.Objects[i].Count-1 do
    begin
      with (Scene.Objects[i] as TGLCustomSceneObject).Material.Texture do
      begin
        TextureFormat := f;
        Compression   := c ;
        MagFilter     := maNearest;
        MinFilter     := miNearest;
      end;

      with (Scene.Objects[i].Children[j] as TGLCustomSceneObject).Material.Texture do
      begin
        TextureFormat := f;
        Compression   := c;
        MagFilter     := maNearest;
        MinFilter     := miNearest;
      end;
    end;

  with Sprite.Material.Texture do
  begin
    TextureFormat := f;
    Compression   := c;
    MagFilter     := maLinear;
    MinFilter     := miLinear;
  end;
end;

end.
