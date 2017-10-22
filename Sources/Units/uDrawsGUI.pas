//* uDrawsGUI - модуль хранит функции рисующие интерфейс для игр

unit uDrawsGUI;

interface

uses
  uInterfaces, uPngCopy, GLTexture, GLMaterial, Classes;

procedure DrawMainMenuGUI (GUIManager : TGUIManager; Dir : ShortString);
procedure DrawGameGUI     (GUIManager : TGUIManager; Dir : ShortString);

implementation

//* DrawMainMenuGUI - Рисует интерфейс для меню
//*
//* Входные параметры :
//*   GUIManager - Менеджер интерфейса
//*   Dir        - Корневая директория игры

procedure DrawMainMenuGUI (GUIManager : TGUIManager; Dir : ShortString);
var
  bg         : TGUISprite;

  // Titles

  tMenu      : TGUISprite;
  tOptions   : TGUISprite;
  tHelp      : TGUISprite;
  tRecords   : TGUISprite;
  tMode      : TGUISprite;

  // Main Buttons

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
begin
  try
    bg         := TGUISprite(GUIManager.AddElement (TGUISprite, 'Background'));
    tMenu      := TGUISprite(GUIManager.AddElement (TGUISprite, 'MenuTitle'));
    tHelp      := TGUISprite(GUIManager.AddElement (TGUISprite, 'HelpTitle'));
    tRecords   := TGUISprite(GUIManager.AddElement (TGUISprite, 'RecordsTitle'));
    tOptions   := TGUISprite(GUIManager.AddElement (TGUISprite, 'OptionsTitle'));
    tMode      := TGUISprite(GUIManager.AddElement (TGUISprite, 'ModeTitle'));

    // Main Buttons

    bStart     := TGUIButton(GUIManager.AddElement (TGUIButton, 'StartButton'));
    bOptions   := TGUIButton(GUIManager.AddElement (TGUIButton, 'OptionsButton'));
    bHelp      := TGUIButton(GUIManager.AddElement (TGUIButton, 'HelpButton'));
    bRecords   := TGUIButton(GUIManager.AddElement (TGUIButton, 'RecordsButton'));
    bExit      := TGUIButton(GUIManager.AddElement (TGUIButton, 'ExitButton'));
    bClose     := TGUIButton(GUIManager.AddElement (TGUIButton, 'CloseButton'));
    bApply     := TGUIButton(GUIManager.AddElement (TGUIButton, 'ApplyButton'));

    // Help

    txtHelp    := TGUISprite(GUIManager.AddElement (TGUISprite, 'HelpText'));

    // Options

    txtOptions := TGUISprite(GUIManager.AddElement (TGUISprite, 'OptionsText'));
    //
    cbBlockYes := TGUIButton(GUIManager.AddElement (TGUIButton, 'BlockYesCheck'));
    cbBlockNo  := TGUIButton(GUIManager.AddElement (TGUIButton, 'BlockNoCheck'));
    //
    cbMusicYes := TGUIButton(GUIManager.AddElement (TGUIButton, 'MusicYesCheck'));
    cbMusicNo  := TGUIButton(GUIManager.AddElement (TGUIButton, 'MusicNoCheck'));
    //
    cbSoundYes := TGUIButton(GUIManager.AddElement (TGUIButton, 'SoundYesCheck'));
    cbSoundNo  := TGUIButton(GUIManager.AddElement (TGUIButton, 'SoundNoCheck'));
    //
    cbLow      := TGUIButton(GUIManager.AddElement (TGUIButton, 'LowCheck'));
    cbMedium   := TGUIButton(GUIManager.AddElement (TGUIButton, 'MediumCheck'));
    cbHigh     := TGUIButton(GUIManager.AddElement (TGUIButton, 'HighCheck'));

    // Records

    trNames    := TGUIText(GUIManager.AddElement (TGUIText, 'RecordsNames'));
    trScores   := TGUIText(GUIManager.AddElement (TGUIText, 'RecordsScores'));
    trNums     := TGUIText(GUIManager.AddElement (TGUIText, 'RecordsNums'));

    // Mode

    bMode1     := TGUIButton(GUIManager.AddElement (TGUIButton, 'ClassicMode'));
    bMode2     := TGUIButton(GUIManager.AddElement (TGUIButton, 'MirrorMode'));
    bMode3     := TGUIButton(GUIManager.AddElement (TGUIButton, 'LeftMode'));
    bMode4     := TGUIButton(GUIManager.AddElement (TGUIButton, 'RightMode'));

    // Records

    trNums  .Hide := True;
    trNames .Hide := True;
    trScores.Hide := True;

    trNums  .Position.SetPoint (45 ,170,0);
    trNames .Position.SetPoint (100,170,0);
    trScores.Position.SetPoint (335,170,0);

    // Menu background

    with bg.Sprite.Material.Texture, bg.Sprite do
    begin
      Width := 500; Height := 640;
      Position.SetPoint(Width/2,Height/2,0);

      SetMaterialPngTexture(Dir+'\Menu\Background.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;

      SetGamma(Material,1);
    end;

    // Menu Title

    with tMenu.Sprite.Material.Texture, tMenu.Sprite do
    begin
      Width := 500; Height := 44;
      Position.SetPoint(250,130,0);

      SetMaterialPngTexture(Dir+'\Menu\MenuTitle.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;

      SetGamma(Material,1);
    end;

    // Help Title

    with tOptions.Sprite.Material.Texture, tOptions.Sprite do
    begin
      Width := 500; Height := 44;
      Position.SetPoint(250,130,0);

      SetMaterialPngTexture(Dir+'\Menu\OptionsTitle.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;
      tOptions.Hide  := True;
      
      SetGamma(Material,1);
    end;

    // Help Title

    with tHelp.Sprite.Material.Texture, tHelp.Sprite do
    begin
      Width := 500; Height := 44;
      Position.SetPoint(250,130,0);

      SetMaterialPngTexture(Dir+'\Menu\HelpTitle.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;
      tHelp.Hide  := True;
      
      SetGamma(Material,1);
    end;

    // Records Title

    with tRecords.Sprite.Material.Texture, tRecords.Sprite do
    begin
      Width := 500; Height := 44;
      Position.SetPoint(250,130,0);

      SetMaterialPngTexture(Dir+'\Menu\RecordsTitle.png',Material);
      TextureMode   := tmModulate;
      TextureWrap   := twNone;
      tRecords.Hide := True;

      SetGamma(Material,1);
    end;

    // Mode Title

    with tMode.Sprite.Material.Texture, tMode.Sprite do
    begin
      Width := 500; Height := 44;
      Position.SetPoint(250,130,0);

      SetMaterialPngTexture(Dir+'\Menu\ModeTitle.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;
      tMode.Hide  := True;

      SetGamma(Material,1);
    end;

    //* Main Buttons *//
    //

    // Close button

    with bClose do
    begin
      Width := 28; Height := 48;
      Position.SetPoint(455,600,0);
      Animation.State := gaNone;
      ShowHint        := False;
      Hide            := True;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bCloseLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bCloseEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Init(28,48);
    end;

    // Accept button

    with bApply do
    begin
      Width := 28; Height := 48;
      Position.SetPoint(45,600,0);
      Animation.State := gaNone;
      ShowHint        := False;
      Hide            := True;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bAcceptLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bAcceptEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Init(28,48);
    end;

    // Start button

    with bStart do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,238,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bStartLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bStartEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Init(500,36);
    end;

    // Options button

    with bOptions do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,279,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bOptionsLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bOptionsEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Init(500,36);
    end;

    // Help button

    with bHelp do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,320,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bHelpLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 1;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bHelpEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 1;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Init(500,36);
    end;

    // Records button

    with bRecords do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,361,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bRecordsLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bRecordsEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Init(500,36);
    end;

    // Exit button

    with bExit do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,402,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bExitLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bExitEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Init(500,36);
    end;

    //* Help *//
    //

    txtHelp.Hide := True;
    with txtHelp.Sprite.Material.Texture, txtHelp.Sprite do
    begin
      Width := 500; Height := 640;
      Position.SetPoint(250,320,0);

      SetMaterialPngTexture(Dir+'\Menu\Help.png',Material);
      Material.FrontProperties.Diffuse.Alpha := 0;
      TextureMode := tmModulate;
      TextureWrap := twNone;
      SetGamma(Material,1);
    end;

    //* Options *//
    //

    txtOptions.Hide := True;
    with txtOptions.Sprite.Material.Texture, txtOptions.Sprite do
    begin
      Width := 500; Height := 640;
      Position.SetPoint(250,320,0);

      SetMaterialPngTexture(Dir+'\Menu\Options.png',Material);
      Material.FrontProperties.Diffuse.Alpha := 0;
      TextureMode := tmModulate;
      TextureWrap := twNone;
      SetGamma(Material,1);
    end;

    // Block Yes Check

    with cbBlockYes do
    begin
      Width := 500; Height := 24;
      Position.SetPoint(295,210,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbYesLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbYesEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Check Sprite

      with CheckSprite.Material.Texture, CheckSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbYesCheck.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := False;
        SetGamma(Material,1);
      end;

      Hide  := True;
      Check := True;
      Radio := True;
      Init (500,24);
    end;

    // Block No Check

    with cbBlockNo do
    begin
      Width := 500; Height := 24;
      Position.SetPoint(295,235,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbNoLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := False;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbNoEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := False;
        SetGamma(Material,1);
      end;

      // Check Sprite

      with CheckSprite.Material.Texture, CheckSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbNoCheck.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := True;
        SetGamma(Material,1);
      end;

      Hide    := True;
      Check   := True;
      Radio   := True;
      Checked := True;
      Init (500,24);
    end;

    // Sound Yes Check

    with cbSoundYes do
    begin
      Width   := 250; Height := 24;
      OffsetX :=-175;

      Position.SetPoint (295,405,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      LeavSprite .Material := cbBlockYes.LeavSprite .Material;
      EnterSprite.Material := cbBlockYes.EnterSprite.Material;
      CheckSprite.Material := cbBlockYes.CheckSprite.Material;

      Hide    := True;
      Check   := True;
      Radio   := True;
      Checked := True;
      Init (500,24);
    end;

    // Sound No Check

    with cbSoundNo do
    begin
      Width   := 250; Height := 24;
      OffsetX :=-175;

      Position.SetPoint (295,430,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      LeavSprite .Material := cbBlockNo.LeavSprite .Material;
      EnterSprite.Material := cbBlockNo.EnterSprite.Material;
      CheckSprite.Material := cbBlockNo.CheckSprite.Material;

      Hide  := True;
      Check := True;
      Radio := True;
      Init (500,24);
    end;

    // Music Yes Check

    with cbMusicYes do
    begin
      Width   := 250; Height := 24;
      OffsetX :=-100;

      Position.SetPoint (483,405,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      LeavSprite .Material := cbBlockYes.LeavSprite .Material;
      EnterSprite.Material := cbBlockYes.EnterSprite.Material;
      CheckSprite.Material := cbBlockYes.CheckSprite.Material;

      Hide    := True;
      Check   := True;
      Radio   := True;
      Checked := True;
      Init (500,24);
    end;

    // Music No Check

    with cbMusicNo do
    begin
      Width   := 250; Height := 24;
      OffsetX :=-100;

      Position.SetPoint (483,430,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      LeavSprite .Material := cbBlockNo.LeavSprite .Material;
      EnterSprite.Material := cbBlockNo.EnterSprite.Material;
      CheckSprite.Material := cbBlockNo.CheckSprite.Material;

      Hide  := True;
      Check := True;
      Radio := True;
      Init (500,24);
    end;

    // Low Check

    with cbLow do
    begin
      Width := 500; Height := 24;
      Position.SetPoint(295,295,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbLowLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbLowEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Check Sprite

      with CheckSprite.Material.Texture, CheckSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbLowCheck.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := False;
        SetGamma(Material,1);
      end;

      Hide  := True;
      Check := True;
      Radio := True;
      Init (500,24);
    end;

    // Medium Check

    with cbMedium do
    begin
      Width := 500; Height := 24;
      Position.SetPoint(295,320,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbMediumLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := False;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbMediumEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := False;
        SetGamma(Material,1);
      end;

      // Check Sprite

      with CheckSprite.Material.Texture, CheckSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbMediumCheck.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := True;
        SetGamma(Material,1);
      end;

      Hide    := True;
      Check   := True;
      Radio   := True;
      Checked := True;
      Init (500,24);
    end;

    with cbHigh do
    begin
      Width := 500; Height := 24;
      Position.SetPoint(295,345,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbHighLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbHighEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Check Sprite

      with CheckSprite.Material.Texture, CheckSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\cbHighCheck.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        Visible     := False;
        SetGamma(Material,1);
      end;

      Hide  := True;
      Check := True;
      Radio := True;
      Init (500,24);
    end;

    //* Mode *//
    //

    // Classic Mode button

    with bMode1 do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,259,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bClassicModeLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bClassicModeEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Hide := True;
      Init(500,44);
    end;

    // Mirror Mode button

    with bMode2 do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,300,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bMirrorModeLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bMirrorModeEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Hide := True;
      Init(500,44);
    end;

    // Left Mode button

    with bMode3 do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,382,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bLeftModeLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bLeftModeEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Hide := True;
      Init(500,44);
    end;

    // Right Mode button

    with bMode4 do
    begin
      Width := 500; Height := 36;
      Position.SetPoint(250,341,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bRightModeLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bRightModeEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Hide := True;
      Init(500,44);
    end;
  except
  end;
end;

//* DrawGameGUI - Рисует интерфейс для игрового процесса
//*
//* Входные параметры :
//*   GUIManager - Менеджер интерфейса
//*   Dir        - Корневая директория игры

procedure DrawGameGUI (GUIManager : TGUIManager; Dir : ShortString);
var
  bg        : TGUISprite;
  //
  trScore   : TGUIText;
  trLevel   : TGUIText;
  // Paused
  pbg       : TGUISprite;
  ptxt      : TGUISprite;
  //
  bMenu     : TGUIButton;
  bContinue : TGUIButton;
  // Game Over
  GameOver  : TGUISprite;
  EnterName : TGUIText;
begin
  try
    bg        := TGUISprite (GUIManager.AddElement (TGUISprite, 'Background'));
    //
    trScore   := TGUIText   (GUIManager.AddElement (TGUIText  , 'TextScore '));
    trLevel   := TGUIText   (GUIManager.AddElement (TGUIText  , 'TextLevel '));
    // Paused
    pbg       := TGUISprite (GUIManager.AddElement (TGUISprite, 'pbg       '));
    ptxt      := TGUISprite (GUIManager.AddElement (TGUISprite, 'ptxt      '));
    //
    bMenu     := TGUIButton (GUIManager.AddElement (TGUIButton, 'bMenu     '));
    bContinue := TGUIButton (GUIManager.AddElement (TGUIButton, 'bContinue '));
    // Game Ovevr
    GameOver  := TGUISprite (GUIManager.AddElement (TGUISprite, 'GameOver  '));
    EnterName := TGUIText   (GUIManager.AddElement (TGUIText  , 'EnterName '));

    //

    trScore.Position.SetPoint (338,220,0);
    trLevel.Position.SetPoint (338,290,0);
    //
    EnterName.Position.SetPoint(160,293,0);
    EnterName.Sprite.Alignment := taCenter;
    //

    //

    pbg  .Hide := True; ptxt     .Hide := True;
    bMenu.Hide := True; bContinue.Hide := True;
    //
    GameOver.Hide := True; EnterName.Hide := True;

    // Menu background

    with bg.Sprite.Material.Texture, bg.Sprite do
    begin
      Width := 500; Height := 640;
      Position.SetPoint(Width/2,Height/2,0);

      SetMaterialPngTexture(Dir+'\Menu\TetrisMain.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;

      SetGamma(Material,1);
    end;

    //

    with pbg.Sprite.Material.Texture, pbg.Sprite do
    begin
      Width := 232; Height := 156;
      Position.SetPoint(162,273,0);

      SetMaterialPngTexture(Dir+'\Menu\Paused_Back.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;

      SetGamma(Material,1);
    end;

    //

    with ptxt.Sprite.Material.Texture, ptxt.Sprite do
    begin
      Width := 232; Height := 156;
      Position.SetPoint(162,273,0);

      SetMaterialPngTexture(Dir+'\Menu\PausedText.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;

      SetGamma(Material,1);
    end;

    //* To Menu Button

    with bMenu do
    begin
      Width := 232; Height := 24;
      Position.SetPoint(162,261,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bMainMenuLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bMainMenuEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Hide := True;
      Init(232,24);
    end;

    //* Continued Button

    with bContinue do
    begin
      Width := 232; Height := 24;
      Position.SetPoint(162,290,0);
      Animation.State := gaNone;
      ShowHint        := False;

      // Leav Sprite

      with LeavSprite.Material.Texture, LeavSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bContinuedLeav.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      // Enter Sprite

      with EnterSprite.Material.Texture, EnterSprite do
      begin
        SetMaterialPngTexture(Dir+'\Menu\bContinuedEnter.png',Material);
        Material.FrontProperties.Diffuse.Alpha := 0;
        TextureMode := tmModulate;
        TextureWrap := twNone;
        SetGamma(Material,1);
      end;

      Hide := True;                                           
      Init(232,24);
    end;

    // Game Over

    with GameOver.Sprite.Material.Texture, GameOver.Sprite do
    begin
      Width := 500; Height := 640;
      Position.SetPoint(Width/2,Height/2,0);

      SetMaterialPngTexture(Dir+'\Menu\GameOver.png',Material);
      TextureMode := tmModulate;
      TextureWrap := twNone;

      SetGamma(Material,1);
    end;
  except
  end;
end;

end.
