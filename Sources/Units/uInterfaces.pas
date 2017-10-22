//* uInterfaces - модуль для работы с интерфейсом игры

unit uInterfaces;

interface

uses
  GLTexture, JPEG, GLObjects, GLScene, GLHudObjects, Classes, GLColor,
  GLMaterial, SysUtils, uKeyboard, Dialogs, GLRenderContextInfo,
  GLCoordinates, GLCrossPlatform, uPngCopy;

type
  //* TGUIAnimState - Тип анимации GUI элемента
  //*
  //* gaNone       - Нет анимации
  //* gaSmooth     - Плавное исчезновение/появление в зависимости от
  //*                состояни элемента
  //* gaSmoothShow - Плавное появление
  //* gaSmoothHide - Плавное исчезновение
  //* gaBlink      - Мерцание

  TGUIAnimState    = (gaNone,gaSmooth,gaSmoothShow,gaSmoothHide,gaBlink);

  //* Класс GUI элемента
  TGUIElementClass = class of TBaseGUIElement;

  //* TGUIAnimation - Управление анимацией
  //*
  //* BI    - Переменная для работы анимации gaBlink
  //* State - Тип анимации
  //* K,A,B - Параметры анимации

  TGUIAnimation = class
  private
    BI    : Integer;
  public
    State : TGUIAnimState;
    K,A,B : Single;

    constructor Create;
    procedure   Apply (Color : TGLColor);
  end;

  //* TBaseGUIElement - Базовый элемент GUI
  //*
  //* Width      - Ширина
  //* Height     - Высота
  //* OffsetX    - Смещение по оси X
  //* OffsetY    - Смещение по оси Y
  //*
  //* Animation  - Анимация
  //* Hide       - Скрыта если True иначе видна

  TBaseGUIElement  = class (TGLDummyCube)
  public
    Width       : Single;
    Height      : Single;
    OffsetX     : Single;
    OffsetY     : Single;

    //

    Animation   : TGUIAnimation;
    Hide        : Boolean;

    constructor Create (AOwner  : TComponent); override;
    //
    procedure   OnMouseMove (X,Y: Integer);    virtual;
    procedure   OnMouseDown (X,Y: Integer);    virtual;
    procedure   OnMouseUp   (X,Y: Integer);    virtual;
  end;

  //* TGUIText - GUI Текст с эффектами
  //* Sprite - Стандартный текст GLScene

  TGUIText = class (TBaseGUIElement)
  public
    Sprite : TGLHUDText;

    constructor Create (AOwner  : TComponent); override;
    procedure   OnMouseMove (X,Y: Integer);    override;
  end;

  //* TGUICursor - Курсор мышки

  TGUICursor = class(TGLHUDSprite)
  public
    procedure OnMouseMove(X,Y: Integer);
  end;

  //* TGUIHint - Подсказки
  //*
  //* Private :
  //*   lw, lh       - Паарметры изменение размеров левой части
  //*   rw, rh       - Параметры изменение размеров правой стороны
  //*   cw, ch       - Параметры изменение размеров середины
  //*   сww          - Параметр изменение ширины центра (дополнительный)
  //*   cpx,lpx,hpx  - Параметры смещения
  //*
  //* Public:
  //*   LeftSprite   - Левая часть подсказки
  //*   RightSprite  - Правая часть подсказки
  //*   CenterSprite - Середина подсказки
  //*
  //*   Hint         - Текст подсказки
  //*   Cursor       - Курсор мышки
  //*
  //*   Speed        - Скорость эффектов
  //*   Show         - Счетчик видимых подсказок

  TGUIHint = class (TGLDummyCube)
  private
    lw,lh,rw : Single;
    rh,cw,ch : Single;
    cww      : Single;
    cpx,lpx  : Single;
    hpx      : Single;
  public
    LeftSprite   : TGLHUDSprite;
    RightSprite  : TGLHUDSprite;
    CenterSprite : TGLHUDSprite;

    Hint         : TGUIText;
    Cursor       : TGUICursor;

    Speed        : Single;
    Show         : Integer;

    constructor Create   (AOwner  : TComponent); override;
    procedure   Init;
    procedure   UpdateHintShow;
    //
    procedure   ShowHint;
    procedure   HideHint;
    //
    procedure   DoRender (var rci : TRenderContextInfo;
                          renderSelf, renderChildren : Boolean); override;
  end;

  //* TGUIFade - Сменение уровня
  //* плавное появление/исчезновение черного экрана

  TGUIFade = class(TGLHUDSprite)
  public
    constructor Create(AOwner : TComponent); override;

    procedure   ShowFade(r,k : Single);
    procedure   HideFade(r,k : Single);
  end;

  //* TGUISprite - Спрайт с эффектами
  //* Sprite - Стандартный спрайт GLScene

  TGUISprite = class(TBaseGUIElement)
  public
    Sprite      : TGLHUDSprite;

    constructor Create (AOwner : TComponent); override;
    procedure   OnMouseMove   (X,Y: Integer); override;
  end;

  //* TGUIButton - Кнопка с эффектами
  //*
  //* LeavSprite  - Спрайт кнопки не активной
  //* EnterSprite - Спрайт кнопки активной
  //* ClickSprite - Спрайт нажатой кнопки
  //* CheckSprite - Спрайт кнопки включенной (CheckBox)
  //*
  //* PreClick    - Нажата ли кнопка
  //* Click       - Опущена ли кнопка
  //* Check       - Использовать как CheckBox
  //* Radio       - Использовать как RadioBox
  //* Checked     - Включен/Выключен
  //* OnHint      -
  //*
  //* GUIHint     - Класс подсказки
  //* Hint        - Текст подсказки
  //* ShowHint    - Показывать подсказку если true

  TGUIButton = class (TBaseGUIElement)
  private
    procedure SetVisible (Leav, Enter, Click, Check : Boolean);
  public
    LeavSprite  : TGLHUDSprite;
    EnterSprite : TGLHUDSprite;
    ClickSprite : TGLHUDSprite;
    CheckSprite : TGLHUDSprite;

    //

    PreClick    : Boolean;
    Click       : Boolean;
    Check       : Boolean;
    Radio       : Boolean;
    Checked     : Boolean;
    OnHint      : Boolean;

    //

    GUIHint     : TGUIHint;
    Hint        : AnsiString;
    ShowHint    : Boolean;

    constructor Create      (AOwner   : TComponent); override;
    procedure   Init        (aWidth, aHeight : Single);
    //
    procedure   OnMouseMove (X,Y: Integer); override;
    procedure   OnMouseDown (X,Y: Integer); override;
    procedure   OnMouseUp   (X,Y: Integer); override;
    //
    function    inPoint     (X,Y: Integer): Boolean;
  end;

  //* TGUIManager - Управление интерфейсом игры
  //*
  //* Elements - Элементы GUI интерфейса
  //* Cursor   - Курсор мышки
  //* Scene    - GL сцена игры
  //* Count    - Количество записей в Elements

  TGUIManager = class
  public
    Elements : array of TBaseGUIElement;
    Cursor   : TGUICursor;
    Scene    : TGLScene;
    Count    : Integer;

    constructor Create(aScene: TGLScene);

    procedure OnMouseMove (X,Y: Integer);
    procedure OnMouseDown (X,Y: Integer);
    procedure OnMouseUp   (X,Y: Integer);

    procedure AddElement  ( El : TBaseGUIElement); overload;
    function  AddElement  (cEl : TGUIElementClass; Name : AnsiString): TBaseGUIElement;      overload;
    procedure MoveLast;
    function  GetElementByName(Name: AnsiString): TBaseGUIElement;

    procedure Clear (NoClearTag : Integer);
  end;

  //* Прототипы процедур для работы с материалами объектов

  procedure SmoothHideObject  (Color : TGLColor; r,k   : Single);
  procedure SmoothShowObject  (Color : TGLColor; r,k   : Single);
  procedure Blink             (Color : TGLColor; k,a,b : Single; var i : Integer);
  procedure SetGamma          (Material : TGLMaterial; a : Single);

implementation

//* SmoothHideObject - Плавно скрывает объект
//*
//* Входные параметры:
//*   Color - Цвет материала объекта
//*   r     - Диапазон меньше которого не может быть Alpha
//*   k     - Коэффицент убывания Alpha значение цвета

procedure SmoothHideObject(Color : TGLColor; r,k: Single);
begin
  with Color do
  begin
    if Alpha > r then Alpha := Alpha - k
    else Alpha := r;
  end;
end;

//* SmoothHideObject - Плавно показывает объект
//*
//* Входные параметры:
//*   Color - Цвет материала объекта
//*   r     - Диапазон больше которого не может быть Alpha
//*   k     - Коэффицент приращения Alpha значение цвета

procedure SmoothShowObject(Color : TGLColor; r,k: Single);
begin
  with Color do
  begin
    if Alpha < r then Alpha := Alpha + k
    else Alpha := r;
  end;
end;

//* Blink - Мерцание объекта
//*
//* Входные параметры:
//*   Color - Цвет материала объекта
//*   a     - Диапазон больше которого не может быть Alpha
//*   b     - Диапазон меньше которого не может быть Alpha
//*   i     - Знак k если i = -1 то убавляем иначе прибавляем Alpha
//*   k     - Коэффицент приращения/убывание Alpha значение цвета

procedure Blink(Color: TGLColor; k,a,b: Single; var i : Integer);
begin
  with Color do
  begin
    if Alpha <= a then i :=  1;
    if Alpha >= b then i := -1;

    Alpha := Alpha+(k*i)
  end;
end;

//* SetGamma - Задает яркость материалу
//*
//* Входные параметры:
//*   Material - Материал объекта
//*   a        - Яркость

procedure SetGamma (Material : TGLMaterial; a : Single);
begin
  with Material.FrontProperties do
  begin
    Ambient .SetColor(a,a,a,1);
    Diffuse .SetColor(a,a,a,1);
    Specular.SetColor(a,a,a,1);
    Emission.SetColor(a,a,a,1);
  end;
end;

//* TGUIAnimation *//

//* Create - Конструктор класса, задаем BI равную -1,
//* для мерцающей анимации

constructor TGUIAnimation.Create;
begin
  BI := -1;
end;

//* Apply - Применяет анимацию к материалу
//* Входной параметр : Color - цвет материала

procedure TGUIAnimation.Apply (Color : TGLColor);
begin
  if State = gaBlink      then Blink            (Color,k,a,b,BI);
  if State = gaSmoothShow then SmoothShowObject (Color,a,k);
  if State = gaSmoothHide then SmoothHideObject (Color,a,k);
end;

//* TBaseGUIElement *//

//* Create - конструктор класса, создает основные классы

constructor TBaseGUIElement.Create (AOwner  : TComponent);
begin
  inherited;
  Animation := TGUIAnimation.Create;
end;

//* OnMouseDown - Событие при нажатии кнопки мыши
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TBaseGUIElement.OnMouseDown(X, Y: Integer);
begin

end;

//* OnMouseMove - Событие при движении курсора мыши
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TBaseGUIElement.OnMouseMove(X, Y: Integer);
begin

end;

//* OnMouseUp - Событие при отпускании кнопки мыши
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TBaseGUIElement.OnMouseUp(X, Y: Integer);
begin

end;

//* TGUICursor *//

//* OnMouseMove - Событие при движении курсора мыши,
//* Задает мыши координаты спрайта мыши равные координатам курсора
//*
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TGUICursor.OnMouseMove(X, Y: Integer);
begin
  Position.SetPoint (X + (Width / 2),Y + (Height / 2),0);
end;

//* TGUIText *//

//* Create - конструктор класса, создает основные классы

constructor TGUIText.Create(AOwner: TComponent);
begin
  inherited;
  Sprite := TGLHUDText.CreateAsChild(Self);
end;

procedure TGUIText.OnMouseMove (X, Y: Integer);
begin
  Sprite.Position := Position;
  
  if Hide then
  begin
    Animation.A     := 0;
    Animation.State := gaSmoothHide;
    Animation.Apply (Sprite.ModulateColor);
  end else
  begin
    Animation.A     := 1;
    Animation.State := gaSmoothShow;
    Animation.Apply (Sprite.ModulateColor);
  end;
end;

//* TGUIFade *//

//* Create - конструктор класса, задает черный цвет материалу и
//* тип материала - прозрачный

constructor TGUIFade.Create(AOwner: TComponent);
begin
  inherited;

  SetGamma (Material, 0);
  Material.BlendingMode := bmTransparency;
end;

//* HideFade - плавно скрывает объект

procedure TGUIFade.HideFade(r, k: Single);
begin
  SmoothHideObject(Material.FrontProperties.Diffuse,r,k);
end;

//* ShowFade - Плавное показывает объект

procedure TGUIFade.ShowFade(r, k: Single);
begin
  SmoothShowObject(Material.FrontProperties.Diffuse,r,k);
end;

//* TGUISprite *//

//* Create - конструктор класса, создает основные классы

constructor TGUISprite.Create(AOwner: TComponent);
begin
  inherited;
  Sprite := TGLHUDSprite.CreateAsChild(Self);
end;

//* OnMouseMove - Событие при движении курсора мыши, проверяет скрыт ли
//* спрайт, и если да то плавно скрывает его иначе плавно показывает его
//*
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TGUISprite.OnMouseMove(X, Y: Integer);
begin
  if Hide then
  begin
    Animation.A     := 0;
    Animation.State := gaSmoothHide;
    Animation.Apply (Sprite.Material.FrontProperties.Diffuse);
  end else
  begin
    Animation.A     := 1;
    Animation.State := gaSmoothShow;
    Animation.Apply (Sprite.Material.FrontProperties.Diffuse);
  end;
end;

//* TGUIButton *//

//* SetVisible - Устанавливает значение видимости всем спрайтам
//*
//* Входные параметры:
//*   Leav  - Значение вилимости спрайту LeavSprite
//*   Enter - Значение вилимости спрайту EnterSprite
//*   Click - Значение вилимости спрайту ClickSprite
//*   Check - Значение вилимости спрайту CheckSprite

procedure TGUIButton.SetVisible(Leav, Enter, Click, Check: Boolean);
begin
  LeavSprite .Visible := Leav;
  EnterSprite.Visible := Enter;
  ClickSprite.Visible := Click;
  CheckSprite.Visible := Check;
end;

//* Create - конструктор класса, создает основные классы

constructor TGUIButton.Create(AOwner : TComponent);
begin
  inherited;
  //
  LeavSprite  := TGLHUDSprite.CreateAsChild (Self);
  EnterSprite := TGLHUDSprite.CreateAsChild (Self);
  ClickSprite := TGLHUDSprite.CreateAsChild (Self);
  CheckSprite := TGLHUDSprite.CreateAsChild (Self);
  //
  ClickSprite.Visible := False;
  CheckSprite.Visible := False;
end;

//* Init - Задает всем спрайтам кнопки положение равное положениею
//* кнопки и размеры aWidth и aHeight
//*
//* Входные параметры :
//*   aWidth  - Ширина кнопки
//*   aHeight - Высота кнопки

procedure TGUIButton.Init (aWidth, aHeight : Single);
begin
  with LeavSprite do
  begin
    Width    := aWidth;
    Height   := aHeight;
    Position := Self.Position;
  end;

  with EnterSprite do
  begin
    Width    := aWidth;
    Height   := aHeight;
    Position := Self.Position;
  end;

  with ClickSprite do
  begin
    Width    := aWidth;
    Height   := aHeight;
    Position := Self.Position;
  end;

  with CheckSprite do
  begin
    Width    := aWidth;
    Height   := aHeight;
    Position := Self.Position;
  end;

  //

  if Checked then SetVisible (False,False,ClickSprite.Visible,True);
end;

//* inPoint - Проверяет, лежит ли точка (X,Y) в кнопке
//* Входные параметры : X, Y - координаты точки

function TGUIButton.inPoint(X, Y: Integer): Boolean;
begin
  Result:= False;
  
  if (X >= Position.X - (Width /2)+OffsetX) and
     (X <= Position.X + (Width /2)+OffsetX) and
     (Y >= Position.Y - (Height/2)+OffsetY) and
     (Y <= Position.Y + (Height/2)+OffsetY) then Result:= True;
end;

//* OnMouseMove - Событие при движении курсора мыши, проверяет, находится
//* ли курсор мыши в кнопке, если да, то проверяем все состояние
//* кнопок (Нажата, включена, активна, не активна, скрыта) и в соответствии
//* с этим применяем анимацию к спрайтам или просто скрываем спрайт,
//* если анимации нет
//*
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TGUIButton.OnMouseMove(X, Y: Integer);
begin
  if inPoint(X,Y) then
  begin
    if (not PreClick) or (not ClickSprite.Visible) then
    begin
      if Animation.State = gaSmooth then
      begin
        ClickSprite.Material.FrontProperties.Diffuse.Alpha := 0;

        Animation.A     := 0;
        Animation.State := gaSmoothHide;
        Animation.Apply (LeavSprite.Material.FrontProperties.Diffuse);

        Animation.A     := 1;
        Animation.State := gaSmoothShow;
        Animation.Apply (EnterSprite.Material.FrontProperties.Diffuse);

        Animation.State := gaSmooth;
      end else
      begin
        if Radio then
        begin
          if not Checked then SetVisible (False, True, False, False);
        end else SetVisible (False, True, False, CheckSprite.Visible);
      end;

      if ShowHint then
      begin
        if not OnHint then
        begin
          OnHint       := True;
          GUIHint.Show := GUIHint.Show+1;

          GUIHint.Hint.Sprite.Text := Hint;
          GUIHint.UpdateHintShow;
        end;

        GUIHint.ShowHint;
      end;
    end else
    begin
      if Animation.State = gaSmooth then
      begin
        ClickSprite.Material.FrontProperties.Diffuse.Alpha := 1;
        LeavSprite .Material.FrontProperties.Diffuse.Alpha := 0;
        EnterSprite.Material.FrontProperties.Diffuse.Alpha := 0;
      end else
      begin
        if Radio then
        begin
          if not Checked then SetVisible (False, False, True, False);
        end else SetVisible (False, False, True, CheckSprite.Visible);
      end;
    end;
  end else
  begin
    if Animation.State = gaSmooth then
    begin
      ClickSprite.Material.FrontProperties.Diffuse.Alpha := 0;

      Animation.A     := 0;
      Animation.State := gaSmoothHide;
      Animation.Apply(EnterSprite.Material.FrontProperties.Diffuse);

      Animation.A     := 1;
      Animation.State := gaSmoothShow;
      Animation.Apply(LeavSprite.Material.FrontProperties.Diffuse);

      Animation.State := gaSmooth;
    end else
    begin
      if Radio then
      begin
        if not Checked then SetVisible (True, False, False, False);
      end else SetVisible (True, False, False, CheckSprite.Visible);
    end;

    if ShowHint then
    begin
      if OnHint then
      begin
        OnHint       := False;
        GUIHint.Show := GUIHint.Show-1;
      end;
    end;
  end;

  //

  if Hide then
  begin
    Animation.A     := 0;
    Animation.State := gaSmoothHide;

    Animation.Apply (LeavSprite .Material.FrontProperties.Diffuse);
    Animation.Apply (EnterSprite.Material.FrontProperties.Diffuse);
    Animation.Apply (CheckSprite.Material.FrontProperties.Diffuse);
  end else
  begin
    Animation.A     := 1;
    Animation.State := gaSmoothShow;

    Animation.Apply (LeavSprite .Material.FrontProperties.Diffuse);
    Animation.Apply (EnterSprite.Material.FrontProperties.Diffuse);
    Animation.Apply (CheckSprite.Material.FrontProperties.Diffuse);
  end;
end;

//* OnMouseDown - Событие при нажатии кнопки мыши, проверяет скрыта ли кнопка,
//* и если нет, то PreClick становится равным true, и вызваем процедуру
//* OnMouseMove, для проверки всех состояний кнопки
//*
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TGUIButton.OnMouseDown(X, Y: Integer);
begin
  if not Hide then
  begin
    PreClick := True;
    OnMouseMove(X,Y);
  end;
end;

//* OnMouseUp - Событие при отпускании кнопки мыши, проверяем скрыта ли кнопка,
//* и если нет, то PreClick становится равным False, проверяет находится ли
//* курсор мыши в кнопке и если да то задаем значение Click равнм True
//* иначе False
//*
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TGUIButton.OnMouseUp(X, Y: Integer);
begin
  if not Hide then
  begin
    PreClick := False;

    if inPoint(X,Y) then
    begin
      if Check then
        if not Checked then
        begin
          SetVisible (False, False, False, True);
          Checked := True;
        end else if not Radio then
        begin
          SetVisible (True, False, False, False);
          Checked := False;
        end;

      Click := True;
    end else Click := False;
  end;
end;

//* TGUIManager *//

//* Create - конструктор класса, создает и настраивает основные переменные
//* Входной параметр : aScene - GL сцена игры

constructor TGUIManager.Create(aScene: TGLScene);
begin
  inherited Create;

  Count   := 0;
  Scene   := aScene;
  Cursor  := TGUICursor(aScene.Objects.AddNewChild(TGUICursor));
  Cursor.Name:= 'cursor';
end;

//* AddElement - Добавляет новый эелемент интерфейса в массив Elements
//* Входной параметр : El - GUI элемент

procedure TGUIManager.AddElement(El: TBaseGUIElement);
begin
  SetLength(Elements,Count+1);
  Elements[Count] := El;
  Count := Count+1;
  Cursor.MoveLast;
end;

//* AddElement - Добавляет новый эелемент интерфейса в массив Elements
//* Выходной параметр : Новый элемент GUI

function TGUIManager.AddElement (cEl : TGUIElementClass; Name : AnsiString) : TBaseGUIElement;
var
  El : TBaseGUIElement;
begin
  SetLength(Elements,Count+1);

  El := cEl.CreateAsChild(Scene.Objects);
  El.Name := Trim(Name);
  Cursor.MoveLast;

  Elements[Count] := El;
  Result          := El;
  Count           := Count+1;
end;

//* Clear - Удаляет все элементы GUI кроме элементов с Tag равным  NoClearTag
//* Входной параметр : NoClearTag - номер Tag элементов, которые не нужно
//* удалять

procedure TGUIManager.Clear (NoClearTag : Integer);
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    FreeAndNil(Elements[i]);

  Count := 0;
  SetLength(Elements,0);
end;

//* GetElementByName - Ищет эллемент с именем Name в массиве Elements
//* Входной параметр : Name - имя элемента

function TGUIManager.GetElementByName(Name: AnsiString): TBaseGUIElement;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if Trim(Elements[i].Name) = Trim(Name) then
    begin
      Result:= Elements[i];
      Exit;
    end;
end;

//* MoveLast - Передвигат все элементы по иерархии в конец списка

procedure TGUIManager.MoveLast;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Elements[i].MoveLast;

  Cursor.MoveLast;
end;

//* OnMouseDown - Событие при нажатии кнопки мыши, вызывает все события
//* нажатия кнопки мыши всех элементам
//*
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TGUIManager.OnMouseDown(X, Y: Integer);
var
  i: Integer;
  Element: TBaseGUIElement;
begin
  //Cursor.OnMouseDown(X,Y);

  for i:= 0 to Count - 1 do
  begin
    Element := Elements[i];
    Element.OnMouseDown(X,Y);
  end;
end;

//* OnMouseMove - Событие при движении кнопки мыши, вызывает все события
//* движении кнопки мыши всех элементам
//*
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TGUIManager.OnMouseMove(X, Y: Integer);
var
  i: Integer;
  Element: TBaseGUIElement;
begin
  Cursor.OnMouseMove(X,Y);

  for i:= 0 to Count - 1 do
  begin
    Element:= Elements[i];
    Element.OnMouseMove(X,Y);
  end;
end;

//* OnMouseUp - Событие при отпускании кнопки мыши, вызывает все события
//* отпускания кнопки мыши всех элементам
//*
//* Входные параметры : X,Y - Координаты курсора мыши

procedure TGUIManager.OnMouseUp(X, Y: Integer);
var
  i: Integer;
  Element: TBaseGUIElement;
begin
  //Cursor.OnMouseUp(X,Y);

  for i:= 0 to Count - 1 do
  begin
    Element:= Elements[i];
    Element.OnMouseUp(X,Y);
  end;
end;

//* TGUIHint *//

//* Create - конструктор класса, создает основные классы

constructor TGUIHint.Create(AOwner: TComponent);
begin
  inherited;

  LeftSprite     := TGLHUDSprite.CreateAsChild(Self);
  RightSprite    := TGLHUDSprite.CreateAsChild(Self);
  CenterSprite   := TGLHUDSprite.CreateAsChild(Self);
  //
  Hint           := TGUIText.CreateAsChild(Self);
  Hint.Sprite.Alignment := taCenter;
  Hint.Sprite.Layout    := tlCenter;
end;

//* DoRender - Рисует подсказку, изменяет размеры и положения в соответствии
//* с переменными: lw,lh,rw,rh,cw,ch,cww,cpx,lpx,hpx

procedure TGUIHint.DoRender(var rci: TRenderContextInfo; renderSelf,
  renderChildren: Boolean);
var
  i  : Integer;
  c  : AnsiString;
  ch : Char;
  w  : Single;
begin
  inherited;

  w := 0;
  c := '';

  for i:= 1 to Length(Hint.Sprite.Text) do
  begin
    c:= Copy(Hint.Sprite.Text,i,1);
    ch:= c[1];
    w:= w + (Hint.Sprite.BitmapFont.GetCharWidth(ch)/2);
  end;

  w := w+(Length(Hint.Sprite.Text)/2);

  Hint.Position.X := Position.X+hpx;
  Hint.Position.Y := Position.Y;

  cw := w*2;
  CenterSprite.Position.X := Position.X+cpx;
  CenterSprite.Position.Y := Position.Y;
  CenterSprite.Width      := w*2+cww;

  LeftSprite.Position.X := Position.X-w-(LeftSprite.Width/2);
  LeftSprite.Position.Y := Position.Y;

  RightSprite.Position.X := Position.X+w+(RightSprite.Width/2)+lpx;
  RightSprite.Position.Y := Position.Y;

  //

  if Assigned(Cursor) then
  begin
    Position.X := Cursor.Position.X+w+(LeftSprite.Width /2)+(Cursor.Width /2);
    Position.Y := Cursor.Position.Y+0+(LeftSprite.Height/2);
  end;
end;

//* HideHint - Скрывает подсказку, убавляет  размер подсказки

procedure TGUIHint.HideHint;
begin
  if Hint.Scale.X > 0 then
  begin
    Hint.Scale.X    := Hint.Scale.X-Speed;
    Hint.Scale.Y    := Hint.Scale.Y-Speed;

    LeftSprite.Width  := LeftSprite.Width -Speed*lw;
    LeftSprite.Height := LeftSprite.Height-Speed*lh;

    RightSprite.Width  := RightSprite.Width -Speed*rw;
    RightSprite.Height := RightSprite.Height-Speed*rh;

    cww  := cww-Speed*cw;
    CenterSprite.Height := CenterSprite.Height-Speed*ch;

    hpx := hpx-Speed*(cw/2);
    cpx := cpx-Speed*(cw/2);
    lpx := lpx-Speed*(cw);
  end else
  begin
    Hint.Visible := False;
    LeftSprite  .Visible := False;
    CenterSprite.Visible := False;
    RightSprite .Visible := False;

    Hint.Scale.SetVector(0,0,0);
    LeftSprite.Width  := 0;
    LeftSprite.Height := 0;
    //cww := 0;
    CenterSprite.Height := 0;
    RightSprite .Width  := 0;
    RightSprite .Height := 0;
  end;
end;

//* Init - задает начальные значения переменных lw,lh,rw,rh,cw,ch

procedure TGUIHint.Init;
begin
  lw := LeftSprite.Width;  rw := RightSprite.Width;  cw := CenterSprite.Width;
  lh := LeftSprite.Height; rh := RightSprite.Height; ch := CenterSprite.Height;
end;

//* ShowHint - Показывает подсказку, прибавляет размеры подсказки

procedure TGUIHint.ShowHint;
begin
  Hint.Visible := True;
  LeftSprite  .Visible := True;
  CenterSprite.Visible := True;
  RightSprite .Visible := True;

  if Hint.Scale.X < 1 then
  begin
    Hint.Scale.X    := Hint.Scale.X+Speed;
    Hint.Scale.Y    := Hint.Scale.Y+Speed;

    LeftSprite.Width  := LeftSprite.Width +Speed*lw;
    LeftSprite.Height := LeftSprite.Height+Speed*lh;

    RightSprite.Width  := RightSprite.Width +Speed*rw;
    RightSprite.Height := RightSprite.Height+Speed*rh;

    cww  := cww+Speed*cw;
    CenterSprite.Height := CenterSprite.Height+Speed*ch;

    hpx := hpx+Speed*(cw/2);
    cpx := cpx+Speed*(cw/2);
    lpx := lpx+Speed*(cw);
  end else
  begin
    Hint.Scale.SetVector(1,1,1);
    LeftSprite.Width  := lw;
    LeftSprite.Height := lh;
    cww := 0;
    CenterSprite.Height := ch;
    RightSprite.Width   := rw;
    RightSprite.Height  := rh;

    hpx := 0;
    cpx := 0;
    lpx := 0;
  end;
end;

//* UpdateHintShow - измеряет длину строки и задает ширину центра подсказки
//* равную этой длине строки умноженная на 2

procedure TGUIHint.UpdateHintShow;
var
  i  : Integer;
  c  : AnsiString;
  ch : Char;
  w  : Single;
begin
  inherited;

  w := 0;
  c := '';

  for i:= 1 to Length(Hint.Sprite.Text) do
  begin
    c  := Copy(Hint.Sprite.Text,i,1);
    ch := c[1];
    w  := w + (Hint.Sprite.BitmapFont.GetCharWidth(ch)/2);
  end;

  cw := w*2;
end;

end.
