//* uHightScore - модулья для работы с таблицей рекордов

unit uHightScore;

interface

uses
  Classes, SysUtils;

type
  //* THightScore - Табилца рекордов
  //*
  //* FNames  - Список имен
  //* FScores - Список очков
  //* FMax    - Максимальное количество записей в таблице 

  THightScore = class
  private
    FNames  : TStringList;
    FScores : TStringList;
    FMax    : Integer;

    procedure LimitChart(Max: Integer);
    procedure Sort;
  public
    constructor Create;
    //
    procedure FillChart;
    procedure SetMax(Max: Integer);
    procedure AddToChart(Player: AnsiString; Score: Integer; Zero : Boolean);
    procedure ClearChart;
    function  GetScores: AnsiString;
    function  GetNames: AnsiString;
    procedure SaveToFile(FileName: AnsiString);
    procedure LoadFromFile(FileName: AnsiString);
    //
    destructor Destroy;
  end;

implementation

//* THightScore *//

//* FillTable - заполняет табилцу пустыми записями

procedure THightScore.FillChart;
var
  i: Integer;
begin
  for i:= 0 to FMax - 1 do
    AddToChart ('Nobody',0,true);
end;

//* LimitChart - Ограничивает количество записей в табилце
//* Входной параметр : Max - максимально кол-во записей

procedure THightScore.LimitChart(Max: Integer);
var
  i: Integer;
begin
  if FNames.Count > Max then
    for i := Max to FNames.Count-1 do
    begin
      FNames .Delete(i);
      FScores.Delete(i);
    end;
end;

//* Sort - Сортирует записи

procedure THightScore.Sort;
var
  i,j : Integer;
  buf : AnsiString;
  max : Integer;
begin
  for i := 0 to FScores.Count - 1 do
  begin
    max := i;
    
    for j := i+1 to FScores.Count - 1 do
      if StrToInt(FScores.Strings[j]) > StrToInt(FScores.Strings[max]) then
        max := j;

    buf := FScores.Strings[i];
    FScores.Strings[i]   := FScores.Strings[max];
    FScores.Strings[max] := buf;

    buf := FNames.Strings[i];
    FNames.Strings[i]    := FNames.Strings[max];
    FNames.Strings[max]  := buf;
  end;
end;

//* Public

//* Create - Конструктор класса, создает основные классы и заполняет
//* переменные

constructor THightScore.Create;
begin
  FMax    := 10;
  FNames  := TStringList.Create;
  FScores := TStringList.Create;

  FillChart;
end;

//* SetMax - Задает максимальное значение записей в табилце
//* Входной параметр : Max - кол-во записей

procedure THightScore.SetMax(Max: Integer);
begin
  FMax := Max;
end;

//* AddToChart - Добавляет в табилцу новую запись
//*
//* Входные параметры :
//*   Player - Имя игрока
//*   Score  - Очки которые он набрал

procedure THightScore.AddToChart(Player: AnsiString; Score: Integer; Zero : Boolean);
var
  a : AnsiString;
  n : Integer;
begin
  if Zero then n := -1 else n := 0;
  if Score > n then
  begin
    if  Score < 10                         then a:= '0000';
    if (Score < 100)    and (Score > 9)    then a:= '000';
    if (Score < 1000)   and (Score > 99)   then a:= '00';
    if (Score < 10000)  and (Score > 999)  then a:= '0';
    if (Score < 100000) and (Score > 9999) then a:= '';
    if  Score > 99999                      then Score:= 99999;

    FNames .Add(Player);
    FScores.Add(a + IntToStr(Score));

    Sort;
    LimitChart(FMax);
  end;
end;

//* GetNames - Возвращает все имена в виде стрки
//* Выходной параметр - строка с именами

function THightScore.GetNames: AnsiString;
var
  i: Integer;
begin
  Result := FNames.Text;
end;

//* GetScores - Возвращает все очки в виде стрки
//* Выходной параметр - строка с очками

function THightScore.GetScores: AnsiString;
begin
  Result := FScores.Text;
end;

//* LoadFromFile - Загружает таблицу рекордов из файла
//* Входной параметр : FileName - путь к таблице рекордов

procedure THightScore.LoadFromFile(FileName: AnsiString);
var
  F  : TextFile;
  i  : Integer;
  S1 : AnsiString;
  S2 : Integer;
begin
  ClearChart;

  AssignFile(F,FileName);
  Reset(F);

  for i:= 0 to FMax - 1 do
  begin
    ReadLn(F,S1); ReadLn(F,S2);
    AddToChart (S1,S2,true);
  end;

  CloseFile(F);
  Sort;
end;

//* SaveToFile - Сохраняет таблицу рекордов в файла
//* Входной параметр : FileName - путь таблицы рекордов

procedure THightScore.SaveToFile(FileName: AnsiString);
var
  F : TextFile;
  i : Integer;
begin
  Sort;
  AssignFile(F,FileName);
  Rewrite(F);

  for i:= 0 to FMax - 1 do
  begin
    WriteLn(F, FNames .Strings[i]);
    WriteLn(F, FScores.Strings[i]);
  end;

  CloseFile(F);
end;

//* Destroy - Деструктор класса, освобождает память

destructor THightScore.Destroy;
begin
  FreeAndNil (FNames);
  FreeAndNil (FScores);
end;

//* ClearChart - Очищает табилцу

procedure THightScore.ClearChart;
begin
  FNames .Clear;
  FScores.Clear;
end;

end.
