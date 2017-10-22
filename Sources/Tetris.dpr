program Tetris;

uses
  Forms,
  uMain in 'uMain.pas' {fMain},
  uAudio in 'Units\uAudio.pas',
  uBlock in 'Units\uBlock.pas',
  uClasses in 'Units\uClasses.pas',
  uConsts in 'Units\uConsts.pas',
  uDrawsGUI in 'Units\uDrawsGUI.pas',
  uEffects in 'Units\uEffects.pas',
  uField in 'Units\uField.pas',
  uHightScore in 'Units\uHightScore.pas',
  uInterfaces in 'Units\uInterfaces.pas',
  uKeyboard in 'Units\uKeyboard.pas',
  uPngCopy in 'Units\uPngCopy.pas',
  uRooms in 'Units\uRooms.pas',
  uUtils in 'Units\uUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
