//* uMain - главный модуль, уровни и события игры описаны в этом модуле

unit uMain;

interface

uses
  //* Основные библиотеки
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLCoordinates, GLScene, GLWin32Viewer, GLCrossPlatform,
  BaseClasses, GLObjects, GLMaterial, PNGImage, GLHUDObjects, VectorGeometry,
  GLParticleFX, GLRenderContextInfo, ExtCtrls, GLCadencer, GLGraphics,
  JPEG, GLBitmapFont, GLBitmapFontNFW, GLTexture, GLWindowsFont, DDS,

  //* Библиотеки игры
  uKeyboard, uPngCopy, uBlock, uClasses, uField, uConsts, uRooms,
  uInterfaces, uDrawsGUI, uAudio, uHightScore;

type
  //* TfMain - Класс формы

  TfMain = class(TForm)
    Scene: TGLScene;
    Viewer: TGLSceneViewer;
    Cadencer: TGLCadencer;
    FallTimer: TTimer;
    Camera: TGLCamera;
    ElBlock: TGLHUDSprite;
    prtBlock: TGLHUDSprite;
    Renderer: TGLDirectOpenGL;
    GLSceneLogo: TGLHUDSprite;
    procedure RendererRender(Sender: TObject; var rci: TRenderContextInfo);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FallTimerTimer(Sender: TObject);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    //* Private declarations

    mx,my : Integer;
  public
    //* Public declarations

    MainDir : ShortString; //* Корневая директория игры
    MouseX  : Integer;     //* Координаты курсора
    MouseY  : Integer;     //*

    //

    HightScore   : THightScore;   //* Табилца рекордов
    RoomsManager : TRoomsManager; //* Менеджер комнат (уровней)
    AudioManager : TAudioManager; //* Аудио менеджер
    GUIManager   : TGUIManager;   //* Менеджер интерфейсов
    Fade         : TGUIFade;      //* "Зановеска", скрывает и показывает уровни

    //* Шрифты

    GreenFont    : TGLBitmapFontNFW;
    RedFont      : TGLBitmapFontNFW;
    YellowFont   : TGLBitmapFontNFW;

    //

    Block      : TBlock;  //* Фигра
    PreBlock   : TBlock;  //* Следующая фигура
    Field      : TField;  //* Поле (стакан) тетриса
    Speed      : Integer; //* Скорость игры
    Paused     : Boolean; //* Пауза, если true

    //

    Options    : TOptions; //* Настройки игры
    Mode       : TModes;   //* Режим игры

    //

    PlayerName : ShortString;
    ShowHS     : Boolean;

    //
    procedure SetRooms;

    // Room Steps
    //
    procedure ShowHideMenu (const Visible : Boolean);
    function  MainMenuStep (const deltaTime: Double) : Boolean;
    function  LevelStep    (const deltaTime: Double) : Boolean;

    // Room Renders
    //
    procedure LevelRender  (var rci: TRenderContextInfo);
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

//* События объектов на форме
//

// RendererRender - Основной рендерер, рисует комнаты

procedure TfMain.RendererRender(Sender: TObject;
  var rci: TRenderContextInfo);
begin
  RoomsManager.Render (rci);
end;

//* FormCreate - Событие формы OnCreate. Создает все объекты и настраивает их

procedure TfMain.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  Randomize;

  ShowHS  := False;
  MainDir := ExtractFileDir (Application.ExeName);

  // Create All

  GUIManager   := TGUIManager  .Create(Scene);
  GUIManager.Cursor.Visible := False;
  //
  AudioManager := TAudioManager.Create;
  RoomsManager := TRoomsManager.Create;
  HightScore   := THightScore  .Create;
  SetRooms;

  PlayerName := 'Player';

  RoomsManager.GUIManager := GUIManager;

  //

  Options := TOptions.Create;
  Options.Load (MainDir+'\Options.dat');

  //

  GreenFont  := TGLBitmapFontNFW.Create(nil);
  RedFont    := TGLBitmapFontNFW.Create(nil);
  YellowFont := TGLBitmapFontNFW.Create(nil);

  GreenFont .PathGGFnt := MainDir+'\Fonts\Main.nfw';
  RedFont   .PathGGFnt := MainDir+'\Fonts\Main.nfw';
  YellowFont.PathGGFnt := MainDir+'\Fonts\Main.nfw';
  
  GreenFont .LoadFromPngTexture(MainDir+'\Fonts\Green.png');
  RedFont   .LoadFromPngTexture(MainDir+'\Fonts\Red.png');
  YellowFont.LoadFromPngTexture(MainDir+'\Fonts\Yellow.png');

  //

  try
    HightScore.LoadFromFile(MainDir + '\HightScore.dat');
  except
    HightScore.SaveToFile(MainDir + '\HightScore.dat');
  end;

  Fade := TGUIFade.CreateAsChild(Scene.Objects);

  with Fade do
  begin
    Width  := ClientWidth;
    Height := ClientHeight;
    Position.SetPoint(Width / 2,Height / 2,0);
  end;

  Screen.Cursor := crCross;

  //

  Speed   := 500;
  FallTimer.Interval := Speed;

  Field := TField.Create (PrtBlock);
  Field.Sprite  := ElBlock;
  Field.Audio   := AudioManager;
  Field.Options := Options;

  Block         := TBlock.Create;
  Block.Sprite  := ElBlock;
  Block.Field   := Field;
  Block.Audio   := AudioManager;
  Block.Options := Options;

  PreBlock := TBlock.Create;
  PreBlock.Color  := VectorMake(Random, Random, Random);
  PreBlock.Sprite := ElBlock;

  //

  PreBlock.Vx := 10.70;
  PreBlock.Vy := 1;

  Block.PreBlock := PreBlock;

  // Init Audio

  with AudioManager.SoundLibrary do
  begin
    // Buffers
    AddBuffer(MainDir + '\Audio\Menu.ogg').Name := 'Menu';
    AddBuffer(MainDir + '\Audio\Game.ogg').Name := 'Game';
    AddBuffer(MainDir + '\Audio\Line.wav').Name := 'Line';
    AddBuffer(MainDir + '\Audio\Put.wav') .Name := 'Put';

    // Sources
    AddSource(Buffer('Menu'),True ,2.0).Name := 'Menu';
    AddSource(Buffer('Game'),True ,2.0).Name := 'Game';
    AddSource(Buffer('Line'),False,2.0).Name := 'Line';
    AddSource(Buffer('Put') ,False,2.0).Name := 'Put';
  end;

  AudioManager.Start;
  AudioManager.InitBuffers;
  AudioManager.InitSources;

  //

  Options.Sprite := ElBlock;
end;

//* FormKeyDown - событие формы OnKeyDown. если нажата клавиша то проверяем
//* на столкновение фигуры с полем. Проверяем нажата ли клавиша Escape, и если
//* нажата, и если не пауза, то пауза, иначе игра

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Block.ScanCollisionH (key);

  if key = keyEscape then
    if Paused then Paused := False
    else Paused := True;
end;

//* CadencerProgress - Основной цикл игры

procedure TfMain.CadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  Fade.MoveLast;
  GUIManager.OnMouseMove(MouseX,MouseY);
  Renderer.MoveLast;
  GLSceneLogo.MoveLast;
  RoomsManager.Step (deltaTime);

  Viewer.Buffer.Render;
end;

//* FallTimerTimer - Спускает (или поднимает если режим перевернутый) фигуру
//* на поле

procedure TfMain.FallTimerTimer(Sender: TObject);
begin
  if not Paused then
    case Mode of
      mMirror : Block.Y := Block.Y-1;
      else Block.Y := Block.Y+1;
    end;
end;

//* ViewerMouseDown - Событие на нажатие мыши. Проверяет на нажате всех
//* элементов интверфейса

procedure TfMain.ViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GUIManager.OnMouseDown (X,Y);
  mx := x; my := y;
end;

//* ViewerMouseUp - событие на отпускание мыши. Проверяет на отпускание
//* всех элементов интверфейса

procedure TfMain.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GUIManager.OnMouseUp (X,Y);
end;

//* ViewerMouseMove - Событие на движение мыши. Задает Координаты мышки
//* (MouseX, MouseY). Перемещаем форму если зажали левую кнопку мыши (ssLeft)

procedure TfMain.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MouseX := X;
  MouseY := Y;

  if Shift = [ssLeft] then
  begin
    Left := (Left + X - mx);
    Top  := (Top  + Y - my);
  end;

  if Shift <> [ssLeft] then
  begin
    mx := X; my := Y;
  end;
end;

//* FormKeyPress - событие формы FormKeyPress. Вводим с клавиатуры имя
//* игрока для таблицы рекордов

procedure TfMain.FormKeyPress(Sender: TObject; var Key: Char);
var
  i : Integer;
  j : Char;
begin
  if Field.GameOver then
  begin
    for i := 0 to RedFont.Ranges.Count - 1 do
      for j := RedFont.Ranges[i].StartASCII to RedFont.Ranges[i].StopASCII do
        if (Key = j) and (Length(PlayerName) < 10) then
          PlayerName := PlayerName + Key;

    if Ord(Key) = 8 then
      PlayerName := Copy(PlayerName,1,length(PlayerName)-1);
  end;
end;

//* Функции Public класса
//                       

//* SetRooms - Создает основные комнаты (уровни) и настраивает их

procedure TfMain.SetRooms;
begin
  with RoomsManager.AddRoom do
  begin
    Step   := MainMenuStep;
    Name   := 'MainMenu';
  end;

  with RoomsManager.AddRoom do
  begin
    Render := LevelRender;
    Step   := LevelStep;
    Name   := 'Level';
  end;
end;

// Rooms Step

// Main Menu

//* ShowHideMenu - Скрывает или показывает элементы интерфейса в зависимости
//* от значения переменной Visible
//* Входной параметр : Visible - если true то показываем иначе скрываем

procedure TfMain.ShowHideMenu (const Visible : Boolean);
begin
  (GUIManager.GetElementByName ('MenuTitle')     as TGUISprite).Hide := Visible;
  (GUIManager.GetElementByName ('StartButton')   as TGUIButton).Hide := Visible;
  (GUIManager.GetElementByName ('OptionsButton') as TGUIButton).Hide := Visible;
  (GUIManager.GetElementByName ('HelpButton')    as TGUIButton).Hide := Visible;
  (GUIManager.GetElementByName ('RecordsButton') as TGUIButton).Hide := Visible;
  (GUIManager.GetElementByName ('ExitButton')    as TGUIButton).Hide := Visible;
end;

//* MainMenuStep - Основной цикл главного меню
//*
//* Входной параметр  : deltaTime - приращение времени
//* Выходной параметр - переходим на другой уровень, если true

function TfMain.MainMenuStep (const deltaTime: Double): Boolean;
var
  i,j : Integer;

  // Titles
  tMenu      : TGUISprite;
  tHelp      : TGUISprite;
  tRecords   : TGUISprite;
  tOptions   : TGUISprite;
  tMode      : TGUISprite;
  
  // Main
  bStart     : TGUIButton;
  bOptions   : TGUIButton;
  bHelp      : TGUIButton;
  bRecords   : TGUIButton;
  bExit      : TGUIButton;
  bClose     : TGUIButton;
  bApply     : TGUIButton;

  // Help
  txtHelp    : TGUISprite;

  // Options
  txtOptions : TGUISprite;
  //
  cbBlockYes : TGUIButton;
  cbBlockNo  : TGUIButton;
  //
  cbMusicYes : TGUIButton;
  cbMusicNo  : TGUIButton;
  //
  cbSoundYes : TGUIButton;
  cbSoundNo  : TGUIButton;
  //
  cbLow      : TGUIButton;
  cbHigh     : TGUIButton;
  cbMedium   : TGUIButton;

  // Records
  trNames    : TGUIText;
  trScores   : TGUIText;
  trNums     : TGUIText;

  // Mode
  bMode1     : TGUIButton;
  bMode2     : TGUIButton;
  bMode3     : TGUIButton;
  bMode4     : TGUIButton;
label
  Close;

//

procedure ApplyMusicOpt;
begin
  if Options.Music then
  begin
    if not AudioManager.SoundLibrary.Source('Menu').Playing then
      AudioManager.PlaySource('Menu');
  end else
  begin
    if AudioManager.SoundLibrary.Source('Menu').Playing then
      AudioManager.PauseSource('Menu');
  end;
end;

//

begin
  Result := False;

  if RoomsManager.Time = 0 then
  begin
    FallTimer.Enabled := False;
    DrawMainMenuGUI (GUIManager, MainDir);

    Fade.Material.FrontProperties.Diffuse.Alpha := 1;
    Options.Apply (Scene, AudioManager);

    AudioManager.StopSource('Game');
    ApplyMusicOpt;
    //
  end else
  begin
    //
    with GUIManager do
    begin
      tMenu      := GetElementByName ('MenuTitle     ') as TGUISprite;
      tHelp      := GetElementByName ('HelpTitle     ') as TGUISprite;
      tRecords   := GetElementByName ('RecordsTitle  ') as TGUISprite;
      tOptions   := GetElementByName ('OptionsTitle  ') as TGUISprite;
      tMode      := GetElementByName ('ModeTitle     ') as TGUISprite;
      //
      bStart     := GetElementByName ('StartButton   ') as TGUIButton;
      bOptions   := GetElementByName ('OptionsButton ') as TGUIButton;
      bHelp      := GetElementByName ('HelpButton    ') as TGUIButton;
      bRecords   := GetElementByName ('RecordsButton ') as TGUIButton;
      bExit      := GetElementByName ('ExitButton    ') as TGUIButton;
      bClose     := GetElementByName ('CloseButton   ') as TGUIButton;
      bApply     := GetElementByName ('ApplyButton   ') as TGUIButton;
      //
      txtHelp    := GetElementByName ('HelpText      ') as TGUISprite;
      //
      txtOptions := GetElementByName ('OptionsText   ') as TGUISprite;
      //
      cbBlockYes := GetElementByName ('BlockYesCheck ') as TGUIButton;
      cbBlockNo  := GetElementByName ('BlockNoCheck  ') as TGUIButton;
      //
      cbSoundYes := GetElementByName ('SoundYesCheck ') as TGUIButton;
      cbSoundNo  := GetElementByName ('SoundNoCheck  ') as TGUIButton;
      //
      cbMusicYes := GetElementByName ('MusicYesCheck ') as TGUIButton;
      cbMusicNo  := GetElementByName ('MusicNoCheck  ') as TGUIButton;
      //
      cbLow      := GetElementByName ('LowCheck      ') as TGUIButton;
      cbMedium   := GetElementByName ('MediumCheck   ') as TGUIButton;
      cbHigh     := GetElementByName ('HighCheck     ') as TGUIButton;
      //
      trNums     := GetElementByName ('RecordsNums   ') as TGUIText;
      trNames    := GetElementByName ('RecordsNames  ') as TGUIText;
      trScores   := GetElementByName ('RecordsScores ') as TGUIText;
      //
      bMode1     := GetElementByName ('ClassicMode   ') as TGUIButton;
      bMode2     := GetElementByName ('MirrorMode    ') as TGUIButton;
      bMode3     := GetElementByName ('LeftMode      ') as TGUIButton;
      bMode4     := GetElementByName ('RightMode     ') as TGUIButton;
    end;

    if ShowHS then
    begin
      ShowHS := False;
      ShowHideMenu (True);
      //
      tRecords.Hide  := False;
      bRecords.Click := False;
      //
      trNums  .Hide  := False;
      trNames .Hide  := False;
      trScores.Hide  := False;
      //
      bClose  .Hide  := False;
    end;

    //

    if not Assigned(trNames.Sprite.BitmapFont) then
    begin
      trNums  .Sprite.BitmapFont := YellowFont;
      trNames .Sprite.BitmapFont := GreenFont;
      trScores.Sprite.BitmapFont := Redfont;
      //
      trNames .Sprite.Text := HightScore.GetNames;
      trScores.Sprite.Text := HightScore.GetScores;
      trNums  .Sprite.Text := '1'+#13+'2'+#13+'3'+#13+'4'+#13+'5'+#13+
                              '6'+#13+'7'+#13+'8'+#13+'9'+#13+'10';
      // Music

      if Options.Music then
      begin
        cbMusicYes.Checked := True;
        cbMusicNo .Checked := False;
      end else
      begin
        cbMusicYes.Checked := False;
        cbMusicNo .Checked := True;
      end;

      cbMusicYes.Init(500,24);
      cbMusicNo .Init(500,24);

      // Sound

      if Options.Sound then
      begin
        cbSoundYes.Checked := True;
        cbSoundNo .Checked := False;
      end else
      begin
        cbSoundYes.Checked := False;
        cbSoundNo .Checked := True;
      end;

      cbSoundYes.Init(500,24);
      cbSoundNo .Init(500,24);

      // New Blocks

      if Options.NewBlocks then
      begin
        cbBlockYes.Checked := True;
        cbBlockNo .Checked := False;
      end else
      begin
        cbBlockYes.Checked := False;
        cbBlockNo .Checked := True;
      end;

      cbBlockYes.Init(500,24);
      cbBlockNo .Init(500,24);

      // Quility

      case Options.Quility of
        qtLow :
          begin
            cbLow   .Checked := True;
            cbMedium.Checked := False;
            cbHigh  .Checked := False;
          end;

        qtMedium :
          begin
            cbLow   .Checked := False;
            cbMedium.Checked := True;
            cbHigh  .Checked := False;
          end;

        qtHigh :
          begin
            cbLow   .Checked := False;
            cbMedium.Checked := False;
            cbHigh  .Checked := True;
          end;
      end;

      cbLow   .Init(500,24);
      cbMedium.Init(500,24);
      cbHigh  .Init(500,24);
    end;

    //

    for i := 0 to GUIManager.Count-1 do
      GUIManager.Elements[i].Animation.K := deltaTime*6;

    //

    if (RoomsManager.Time < 3) then Fade.HideFade(0,3*deltaTime);
    if (RoomsManager.Time > 2) and (RoomsManager.Time < 3) then
    begin
      RoomsManager.Time := 2.1;

      //

      if bStart.Click then
      begin
        ShowHideMenu (True);
        //
        tMode .Hide  := False;
        bMode1.Hide  := False;
        bMode2.Hide  := False;
        bMode3.Hide  := False;
        bMode4.Hide  := False;
        bClose.Hide  := False;
        bStart.Click := False;
      end;

      if bMode1.Click then Mode := mClassic;
      if bMode2.Click then Mode := mMirror;
      if bMode3.Click then Mode := mLeft;
      if bMode4.Click then Mode := mRight;

      if (bMode1.Click) or (bMode2.Click) or
         (bMode3.Click) or (bMode4.Click) then RoomsManager.Time := 5;

      //

      if bOptions.Click then
      begin
        ShowHideMenu (True);
        //
        tOptions  .Hide  := False; bClose    .Hide  := False;
        txtOptions.Hide  := False; bApply    .Hide  := False;
        //
        cbBlockYes.Hide  := False; cbSoundYes.Hide  := False;
        cbBlockNo .Hide  := False; cbSoundNo .Hide  := False;
        cbMusicYes.Hide  := False; cbMusicNo .Hide  := False;
        //
        cbLow     .Hide  := False;
        cbMedium  .Hide  := False;
        cbHigh    .Hide  := False;
        //
        bOptions  .Click := False;
      end;

      if bHelp.Click then
      begin
        ShowHideMenu (True);
        //
        tHelp  .Hide  := False;
        txtHelp.Hide  := False;
        bClose .Hide  := False;
        bHelp  .Click := False;
      end;

      // Hight Score

      if bRecords.Click then
      begin
        try
          HightScore.LoadFromFile(MainDir+'\HightScore.dat');
        except
          HightScore.SaveToFile(MainDir + '\HightScore.dat');
        end;

        trNames .Sprite.Text := HightScore.GetNames;
        trScores.Sprite.Text := HightScore.GetScores;
        
        ShowHS := False;
        ShowHideMenu (True);
        //
        tRecords.Hide  := False;
        bRecords.Click := False;
        //
        trNums  .Hide  := False;
        trNames .Hide  := False;
        trScores.Hide  := False;
        //
        bClose  .Hide  := False;
      end;

      if bExit.Click then
      begin
        bExit.Click       := False;
        RoomsManager.Time := 3;
      end;

      //

      if bClose.Click then
      begin
        Close :

        ShowHideMenu (False);
        //
        tHelp     .Hide  := True; tOptions  .Hide  := True;
        tRecords  .Hide  := True; tMode     .Hide  := True;
        //
        txtOptions.Hide  := True; bClose    .Hide  := True;
        txtHelp   .Hide  := True; bApply    .Hide  := True;
        //
        cbBlockYes.Hide  := True; cbSoundYes.Hide  := True;
        cbBlockNo .Hide  := True; cbSoundNo .Hide  := True;
        cbMusicYes.Hide  := True; cbMusicNo .Hide  := True;
        //
        cbLow     .Hide  := True; trNums    .Hide  := True;
        cbMedium  .Hide  := True; trNames   .Hide  := True;
        cbHigh    .Hide  := True; trScores  .Hide  := True;
        //
        bMode1    .Hide  := True; bMode3    .Hide  := True;
        bMode2    .Hide  := True; bMode4    .Hide  := True;
        //
        bClose    .Click := False;
      end;

      if bApply.Click then
      begin
        bApply .Click := False;

        Options.Music     := cbMusicYes.Checked;
        Options.Sound     := cbSoundYes.Checked;
        Options.NewBlocks := cbBlockYes.Checked;

        if cbLow   .Checked then Options.Quility := qtLow;
        if cbMedium.Checked then Options.Quility := qtMedium;
        if cbHigh  .Checked then Options.Quility := qtHigh;

        Options.Save  (MainDir+'\Options.dat');
        Options.Apply (Scene, AudioManager);
        ApplyMusicOpt;

        goto Close;
      end;

      // Options

      if cbBlockYes.Click then
      begin
        cbBlockYes.Checked := True;
        cbBlockNo .Checked := False;
      end;

      if cbBlockNo.Click then
      begin
        cbBlockYes.Checked := False;
        cbBlockNo .Checked := True;
      end;

      //

      if cbSoundYes.Click then
      begin
        cbSoundYes.Checked := True;
        cbSoundNo .Checked := False;
      end;

      if cbSoundNo.Click then
      begin
        cbSoundYes.Checked := False;
        cbSoundNo .Checked := True;
      end;

      //

      if cbMusicYes.Click then
      begin
        cbMusicYes.Checked := True;
        cbMusicNo .Checked := False;
      end;

      if cbMusicNo.Click then
      begin
        cbMusicYes.Checked := False;
        cbMusicNo .Checked := True;
      end;

      //

      if cbLow.Click then
      begin
        cbLow   .Checked := True;
        cbMedium.Checked := False;
        cbHigh  .Checked := False;
      end;

      if cbMedium.Click then
      begin
        cbLow   .Checked := False;
        cbMedium.Checked := True;
        cbHigh  .Checked := False;
      end;

      if cbHigh.Click then
      begin
        cbLow   .Checked := False;
        cbMedium.Checked := False;
        cbHigh  .Checked := True;
      end;
    end;

    if (RoomsManager.Time > 3)  and (RoomsManager.Time < 4) then
       Fade.ShowFade(1,3*deltaTime);

    if (RoomsManager.Time >= 4) and (RoomsManager.Time < 5) then
       Application.Terminate;

    // Next Room

    if (RoomsManager.Time > 5) and (RoomsManager.Time < 6) then
       Fade.ShowFade(1,3*deltaTime);

    if (RoomsManager.Time >= 6) then
    begin
      Result := True;
      RoomsManager.NextRoom;
    end;
  end;
end;

//* LevelStep - Основной цикл игры
//*
//* Входной параметр  : deltaTime - приращение времени
//* Выходной параметр - переходим на другой уровень, если true

function TfMain.LevelStep (const deltaTime: Double): Boolean;
var
  i : Integer;
  //
  trScore   : TGUIText;
  trLevel   : TGUIText;
  // Paused
  pbg       : TGUISprite;
  ptxt      : TGUISprite;
  //
  bMenu     : TGUIButton;
  bContinue : TGUIButton;
  //
  GameOver  : TGUISprite;
  EnterName : TGUIText;
//

procedure ApplyMusicOpt;
begin
  if Options.Music then
  begin
    if not AudioManager.SoundLibrary.Source('Game').Playing then
      AudioManager.PlaySource('Game');
  end else
  begin
    if AudioManager.SoundLibrary.Source('Game').Playing then
      AudioManager.PauseSource('Game');
  end;
end;

//
begin
  Result := False;

  if RoomsManager.Time = 0 then
  begin
    DrawGameGUI (GUIManager, MainDir);
    Fade.Material.FrontProperties.Diffuse.Alpha := 1;
    FallTimer.Enabled := True;

    Field.Score := 0;
    Field.Lines := 0;

    PreBlock.LoadFormFile('Blocks.dat');
    Block   .LoadFormFile('Blocks.dat');

    if Options.NewBlocks then
    begin
      PreBlock.AppendFormFile('PlusBlocks.dat');
      Block   .AppendFormFile('PlusBlocks.dat');
    end;

    Block.Mode := Mode;
    Field.Mode := Mode;
    Block.NextBlock (Random(Length(Block.BlocksData)));

    AudioManager.StopSource('Menu');
    ApplyMusicOpt;
    Options.Apply(SCene,AudioManager);
  end else
  begin

    with GUIManager do
    begin
      trScore   := GetElementByName ('TextScore') as TGUIText;
      trLevel   := GetElementByName ('TextLevel') as TGUIText;
      //
      pbg       := GetElementByName ('pbg      ') as TGUISprite;
      ptxt      := GetElementByName ('ptxt     ') as TGUISprite;
      //
      bMenu     := GetElementByName ('bMenu    ') as TGUIButton;
      bContinue := GetElementByName ('bContinue') as TGUIButton;
      //
      GameOver  := GetElementByName ('GameOver ') as TGUISprite;
      EnterName := GetElementByName ('EnterName') as TGUIText;
    end;

    //

    for i := 0 to GUIManager.Count-1 do
      GUIManager.Elements[i].Animation.K := deltaTime*6;

    //

    // Paused
    pbg  .MoveLast;  ptxt     .MoveLast;
    bMenu.MoveLast;  bContinue.MoveLast;
    // GameOver
    GameOver.MoveLast; EnterName.MoveLast;

    //

    EnterName.Sprite.Text := PlayerName;
    trScore  .Sprite.Text := IntToStr (Field.Score);
    trLevel  .Sprite.Text := IntToStr (Field.Level);

    if (RoomsManager.Time < 3) then Fade.HideFade(0,3*deltaTime);
    if (RoomsManager.Time > 1) and (RoomsManager.Time < 3) then
    begin
      RoomsManager.Time := 2.1;
      Viewer.Buffer.Render;

      if not Assigned(trScore.Sprite.BitmapFont) then
      begin
        EnterName.Sprite.Scale.SetVector(0.8,0.8,1);
        //
        EnterName.Sprite.BitmapFont := RedFont;
        trScore  .Sprite.BitmapFont := RedFont;
        trLevel  .Sprite.BitmapFont := RedFont;
      end;

      if not Field.GameOver then
      begin
        if not Paused then
        begin
          pbg  .Hide := True; ptxt     .Hide := True;
          bMenu.Hide := True; bContinue.Hide := True;
          //
          Field.Step (deltaTime);

          Block.ScanCollisionV;
          Field.ScanLines;

          if (OnKey(keySpace)) or (OnKey(KeyUpArrow)) then
          begin
            if not Block.keyRotate then
              if Block.SizeBlockV < FieldWidth-Block.vx+1 then
              begin
                Block.keyRotate := True;
                Block.Rotate;
              end;
          end else Block.keyRotate := False;

          if OnKey(keyDownArrow) then
          begin
            FallTimer.Interval := 10;
            Block.Vx := Block.X; Block.Vy := Block.Y;
          end else
          begin
            FallTimer.Interval := Round(1000 / (Field.Level/1.3));
            Block.Step (DeltaTime);
          end;
        end else
        begin
          pbg.Hide   := False; ptxt.Hide      := False;
          bMenu.Hide := False; bContinue.Hide := False;

          //

          if bMenu.Click then
          begin
            RoomsManager.Time := 4;
            //
            bMenu.Click := False;
            Paused      := False;
            Result      := True;
          end;

          if bContinue.Click then
          begin
            Paused := False;
            bContinue.Click := False;
          end;
        end;
      end else
      begin
        Paused := False;
        //
        GameOver.Hide := False; EnterName.Hide := False;
        //
        pbg  .Hide := True; ptxt     .Hide := True;
        bMenu.Hide := True; bContinue.Hide := True;
        //
        if (OnKey (keyEnter)) and (not ShowHS) then
        begin
          Field.Clear;
          Field.GameOver := False;

          HightScore.AddToChart (PlayerName, Field.Score, false);
          HightScore.SaveToFile (MainDir + '\HightScore.dat');

          ShowHS := True;
          RoomsManager.Time := 4;
        end;
        //
      end;
      //
    end;
    //

    if (RoomsManager.Time > 4) and (RoomsManager.Time < 5) then
       Fade.ShowFade(1,3*deltaTime);

    if (RoomsManager.Time >= 5) then
    begin
      Result := True;
      Field.Clear;
      RoomsManager.PrevRoom;
    end;
  end;
end;

//* LevelRender - Рендер уровня, рисует все элементы игры

procedure TfMain.LevelRender(var rci: TRenderContextInfo);
begin
  Field   .Render (rci);
  Block   .Render (rci);
  PreBlock.Render (rci);
end;

end.
