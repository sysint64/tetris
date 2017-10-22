{
Для использования нужно в TGLCustomBitmapFont
procedure ResetCharWidths(w : Integer = -1);
and
procedure OnGlyphsChanged(Sender : TObject);
определить как виртуал, потому как их оверрайдим

формат файла nfw:
0. Высота
1. Ширина
2. кол-во символов
3. символьная строка
4.... символы

Ну и как всегда в GLSceneRegister добавляем:
1. В uses -> GLBitmapFontNFW
2. в GLRegisterPropertiesInCategories ->
   RegisterPropertiesInCategory(sVisualCategoryName,
     [TypeInfo(TGLBitmapFontNFW), TypeInfo(TTextLayout)]);
   RegisterPropertiesInCategory(sLocalizableCategoryName,
     [TypeInfo(TGLBitmapFontNFW)]);
   RegisterPropertiesInCategory(sLayoutCategoryName, TGLBitmapFontNFW,
     ['Char*', '*Interval*', '*Space']);
   RegisterPropertiesInCategory(sLocalizableCategoryName, TGLBitmapFontNFW,
     ['Glyphs']);
   RegisterPropertiesInCategory(sVisualCategoryName, TGLBitmapFontNFW,
     ['Char*', '*Interval*', '*Space', 'Glyphs']);
3. в Register
   RegisterComponents('GLScene',[TGLBitmapFontNFW]);

Вообщем всё как у TGLBitmapFont.
}

unit GLBitmapFontNFW; // NFW = Not Fixed Width

interface

uses Classes, GLScene, VectorGeometry, GLContext, GLCrossPlatform,
   GLTexture, GLState, GLUtils, GLGraphics, GLBitmapFont, SysUtils{, QStrings};

{------------------------------------------------------------------------------}
type
{------------------------------------------------------------------------------}

  TGLCustomBitmapFontNFW = class (TGLCustomBitmapFont)
  private
    FPathGGFnt: String;
    procedure SetPathGGFnt(const val : String);
    procedure ResetCharWidths(w : Integer = -1); override;
    procedure OnGlyphsChanged(Sender : TObject); override;
  protected
  public
    property PathGGFnt : String read FPathGGFnt write SetPathGGFnt;
  end;

{------------------------------------------------------------------------------}

   TGLBitmapFontNFW = class (TGLCustomBitmapFontNFW)
   private
   published
     property Glyphs;
     property GlyphsIntervalX;
     property GlyphsIntervalY;
     property Ranges;
     property CharWidth;
     property CharHeight;
     property HSpace;
     property VSpace;
     property MagFilter;
     property MinFilter;
     property GlyphsAlpha;
     property PathGGFnt;
  end;

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

implementation

uses Dialogs, StrUtils;

{------------------------------------------------------------------------------}

function GetStr(const s: String): String;
var tmps: String;
begin
  tmps := s;
  Delete(tmps,1,2);
  Result := tmps;
end;

{------------------------------------------------------------------------------}

procedure TGLCustomBitmapFontNFW.OnGlyphsChanged(Sender: TObject);
var
  SaveMF: TGLMagFilter;
begin
  inherited;
  SaveMF := MagFilter;
  if MagFilter = maLinear then
    MagFilter := maNearest
  else
    MagFilter := maLinear;
  MagFilter := SaveMF;
end;

{------------------------------------------------------------------------------}

procedure TGLCustomBitmapFontNFW.ResetCharWidths(w : Integer = -1);
var
  i, cnt, L: Integer;
  SL: TStrings;
  s: string;
  ch_beg, ch_end, ch_cur: Char;
begin
  inherited;

  if FPathGGFnt<>'' then
  begin
    if FileExists(PathGGFnt) then
    begin
      SL := TStringList.Create;
      SL.LoadFromFile(FPathGGFnt);
      CharWidth := StrToInt(SL[0]);
      CharHeight := StrToInt(SL[1]);
      cnt := StrToInt(SL[2]);
      // etc.
      for i:=4 to cnt-1 do
      begin
        s := SL[i][1];
        CharWidths[Integer(s[1])] := StrToInt(GetStr(SL[i]));
      end;

      Ranges.Clear;
      s := SL[3];
      L := Length(s);
      ch_beg := s[1];
      cnt := 0;
      for i:=1 to L do
      begin
        ch_cur := s[i];
        ch_end := s[i+1];
        if Integer(ch_end)-Integer(ch_cur)<>1 then
        begin
          Ranges.Add(ch_beg, ch_cur).StartGlyphIdx := cnt;
          ch_beg := ch_end;
          cnt := i;
        end;
      end;
      SL.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------}

procedure TGLCustomBitmapFontNFW.SetPathGGFnt(const val: String);
begin
   FPathGGFnt:=val;
   InvalidateUsers;
end;

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

initialization

   // class registrations
   RegisterClasses([TGLBitmapFontNFW, TGLFlatText]);

end.



