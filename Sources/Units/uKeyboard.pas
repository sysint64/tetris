//* uKeyboard - модуль для работы с клавиатурой

unit uKeyboard;

interface

uses
  Windows;

function OnKey(key: Char): Boolean; overload;
function OnKey(key: Word): Boolean; overload;

const
  //* Код клавиш с мышки

  mouseLeft   = 1;
  mouseRight  = 2;
  mouseMiddle = 4;

  // Код клавиш с клавиатуры

  keyCancel       = 3;
  keyBack         = 8;
  keyTab          = 9;
  keyClear        = 12;
  keyEnter        = 13;
  keyShift        = 16;
  keyControl      = 17;
  keyAlt          = 18;
  keyPause        = 19;
  keyCapsLock     = 20;
  keyKana         = 21;
  keyHangul       = 21;
  keyJunja        = 23;
  keyFinal        = 24;
  keyHanja        = 25;
  keyKanji        = 25;
  keyConvert      = 28;
  keyNonConvert   = 29;
  keyAccept       = 30;
  keyModeChange   = 31;
  keyEscape       = 27;
  keySpace        = 32;
  keyPrior        = 33;
  keyNext         = 34;
  keyEnd          = 35;
  keyHome         = 36;
  keyLeftArrow    = 37;
  keyUpArrow      = 38;
  keyRightArrow   = 39;
  keyDownArrow    = 40;
  keySelect       = 41;
  keyPrint        = 42;
  keyExecute      = 43;
  keySnapshot     = 44;
  keyInsert       = 45;
  keyDelete       = 46;
  keyHelp         = 47;
  key0            = 48;
  key1            = 49;
  key2            = 50;
  key3            = 51;
  key4            = 52;
  key5            = 53;
  key6            = 54;
  key7            = 55;
  key8            = 56;
  key9            = 57;
  keyA            = 65;
  keyB            = 66;
  keyC            = 67;
  keyD            = 68;
  keyE            = 69;
  keyF            = 70;
  keyG            = 71;
  keyH            = 72;
  keyI            = 73;
  keyJ            = 74;
  keyK            = 75;
  keyL            = 76;
  keyM            = 77;
  keyN            = 78;
  keyO            = 79;
  keyP            = 80;
  keyQ            = 81;
  keyR            = 82;
  keyS            = 83;
  keyT            = 84;
  keyU            = 85;
  keyV            = 86;
  keyW            = 87;
  keyX            = 88;
  keyY            = 89;
  keyZ            = 90;
  keyLeftWin      = 91;
  keyRightWin     = 92;
  keyApps         = 93;
  keyNumpad0      = 96;
  keyNumpad1      = 97;
  keyNumpad2      = 98;
  keyNumpad3      = 99;
  keyNumpad4      = 100;
  keyNumpad5      = 101;
  keyNumpad6      = 102;
  keyNumpad7      = 103;
  keyNumpad8      = 104;
  keyNumpad9      = 105;
  keyMultiply     = 106;
  keyAdd          = 107;
  keySeparator    = 108;
  keySubtract     = 109;
  keyDecimal      = 110;
  keyDivide       = 111;
  keyF1           = 112;
  keyF2           = 113;
  keyF3           = 114;
  keyF4           = 115;
  keyF5           = 116;
  keyF6           = 117;
  keyF7           = 118;
  keyF8           = 119;
  keyF9           = 120;
  keyF10          = 121;
  keyF11          = 122;
  keyF12          = 123;
  keyF13          = 124;
  keyF14          = 125;
  keyF15          = 126;
  keyF16          = 127;
  keyF17          = 128;
  keyF18          = 129;
  keyF19          = 130;
  keyF20          = 131;
  keyF21          = 132;
  keyF22          = 133;
  keyF23          = 134;
  keyF24          = 135;
  keyNumLock      = 144;
  keyScrollLock   = 145;
  keyLeftShift    = 160;
  keyRightShift   = 161;
  keyLeftControl  = 162;
  keyRightControl = 163;
  keyLeftAlt      = 164;
  keyRightAlt     = 165;
  keyProcess      = 229;
  keyAttn         = 246;
  keyCrsel        = 247;
  keyEexsel       = 248;
  keyEreof        = 249;
  keyPlay         = 250;
  keyZoom         = 251;
  keyNoname       = 252;
  keyPa1          = 253;
  keyOemClear     = 254;

implementation

//* OnKey - Проверяет нажата ли клавиша с кодом key, если да, то возвращает
//* True, если нет, то возвращает Fakse
//*
//* Входной парамтр : key - код, сивол клавиши с клавиатуры
//* Выходной параметр - нажата или нет клавиша
//*
//* Пример : if (OnKey(keyLeftArrow)) then Sprite.MoveX (Speed);

function OnKey(key: Char): Boolean;
var
   vk : Integer;
begin
   vk := VkKeyScan(key) and $FF;

   if vk <> $FF then Result := (GetAsyncKeyState(vk) < 0)
   else Result := False;
end;

function OnKey(key: Word): Boolean;
begin
  Result := (GetAsyncKeyState(key) < 0);
end;

end.
