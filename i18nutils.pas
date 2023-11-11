unit i18nutils;

{$mode ObjFPC}{$H+}

{Some helper routines for translated lazarus programs
 Armin Diehl <ad@ardiehl.de> 11/2023

 Free Software
}

interface

uses
  Classes, SysUtils, Forms, Menus, LCLTranslator;

procedure poSetSearchDirs(Dirs: string = '');                // search dirs separated by , ; or :
function poGetProgramBaseName : string;                        // name of program w/o path and extension
function poSelectLanguageMenuClick(Sender: TObject) : string;          // extract lang from menu entry and set the language
procedure poGenerateLanguageSelectMenuEntries(base: TMenuItem; p: TNotifyEvent);
function setLanguage(s: String): String;

implementation

type
 TIsoLanguages = record
   code: string[3];
   name: string[30];
 end;

// TODO: find localised ISO table
const
 // TODO: Find a table with localized names
 IsoLanguages: array  of TIsoLanguages = (
    (Code: 'ab'; Name: 'Abkhazian'),
    (Code: 'om'; Name: '(Afan) Oromo'),
    (Code: 'aa'; Name: 'Afar'),
    (Code: 'af'; Name: 'Afrikaans'),
    (Code: 'sq'; Name: 'Albanian'),
    (Code: 'am'; Name: 'Amharic'),
    (Code: 'ar'; Name: 'Arabic'),
    (Code: 'hy'; Name: 'Armenian'),
    (Code: 'as'; Name: 'Assamese'),
    (Code: 'ae'; Name: 'Avestan'),
    (Code: 'ay'; Name: 'Aymara'),
    (Code: 'az'; Name: 'Azerbaijani'),
    (Code: 'ba'; Name: 'Bashkir'),
    (Code: 'eu'; Name: 'Basque'),
    (Code: 'be'; Name: 'Belarusian'),
    (Code: 'bn'; Name: 'Bengali'),
    (Code: 'bh'; Name: 'Bihari'),
    (Code: 'bi'; Name: 'Bislama'),
    (Code: 'bs'; Name: 'Bosnian'),
    (Code: 'br'; Name: 'Breton'),
    (Code: 'bg'; Name: 'Bulgarian'),
    (Code: 'my'; Name: 'Burmese'),
    (Code: 'ca'; Name: 'Catalan'),
    (Code: 'ch'; Name: 'Chamorro'),
    (Code: 'ce'; Name: 'Chechen'),
    (Code: 'ny'; Name: 'Chichewa; Nyanja'),
    (Code: 'zh'; Name: 'Chinese'),
    (Code: 'cu'; Name: 'Church Slavic'),
    (Code: 'cv'; Name: 'Chuvash'),
    (Code: 'kw'; Name: 'Cornish'),
    (Code: 'co'; Name: 'Corsican'),
    (Code: 'hr'; Name: 'Croatian'),
    (Code: 'cs'; Name: 'Czech'),
    (Code: 'da'; Name: 'Danish'),
    (Code: 'nl'; Name: 'Dutch'),
    (Code: 'dz'; Name: 'Dzongkha'),
    (Code: 'en'; Name: 'English'),
    (Code: 'eo'; Name: 'Esperanto'),
    (Code: 'et'; Name: 'Estonian'),
    (Code: 'fo'; Name: 'Faroese'),
    (Code: 'fj'; Name: 'Fijian'),
    (Code: 'fi'; Name: 'Finnish'),
    (Code: 'fr'; Name: 'French'),
    (Code: 'fy'; Name: 'Frisian'),
    (Code: 'fur'; Name: 'Friulian'),
    (Code: 'gd'; Name: 'Gaelic'),
    (Code: 'gl'; Name: 'Galician'),
    (Code: 'ka'; Name: 'Georgian'),
    (Code: 'de'; Name: 'Deutsch'),
    (Code: 'el'; Name: 'Greek'),
    (Code: 'gn'; Name: 'Guarani'),
    (Code: 'gu'; Name: 'Gujarati'),
    (Code: 'ha'; Name: 'Hausa'),
    (Code: 'he'; Name: 'Hebrew'),
    (Code: 'hz'; Name: 'Herero'),
    (Code: 'hi'; Name: 'Hindi'),
    (Code: 'ho'; Name: 'Hiri Motu'),
    (Code: 'hu'; Name: 'Hungarian'),
    (Code: 'is'; Name: 'Icelandic'),
    (Code: 'id'; Name: 'Indonesian'),
    (Code: 'ia'; Name: 'Interlingua'),
    (Code: 'ie'; Name: 'Interlingue'),
    (Code: 'iu'; Name: 'Inuktitut'),
    (Code: 'ik'; Name: 'Inupiaq'),
    (Code: 'ga'; Name: 'Irish'),
    (Code: 'it'; Name: 'Italian'),
    (Code: 'ja'; Name: 'Japanese'),
    (Code: 'jv'; Name: 'Javanese'),
    (Code: 'kl'; Name: 'Kalaallisut'),
    (Code: 'kn'; Name: 'Kannada'),
    (Code: 'ks'; Name: 'Kashmiri'),
    (Code: 'kk'; Name: 'Kazakh'),
    (Code: 'km'; Name: 'Khmer'),
    (Code: 'ki'; Name: 'Kikuyu'),
    (Code: 'rw'; Name: 'Kinyarwanda'),
    (Code: 'kv'; Name: 'Komi'),
    (Code: 'ko'; Name: 'Korean'),
    (Code: 'kj'; Name: 'Kuanyama'),
    (Code: 'ku'; Name: 'Kurdish'),
    (Code: 'ky'; Name: 'Kyrgyz'),
    (Code: 'lo'; Name: 'Lao'),
    (Code: 'la'; Name: 'Latin'),
    (Code: 'lv'; Name: 'Latvian'),
    (Code: 'lb'; Name: 'Letzeburgesch'),
    (Code: 'ln'; Name: 'Lingala'),
    (Code: 'lt'; Name: 'Lithuanian'),
    (Code: 'mk'; Name: 'Macedonian'),
    (Code: 'mg'; Name: 'Malagasy'),
    (Code: 'ms'; Name: 'Malay'),
    (Code: 'ml'; Name: 'Malayalam'),
    (Code: 'mt'; Name: 'Maltese'),
    (Code: 'mi'; Name: 'Maori'),
    (Code: 'mr'; Name: 'Marathi'),
    (Code: 'mh'; Name: 'Marshall'),
    (Code: 'mo'; Name: 'Moldavian'),
    (Code: 'mn'; Name: 'Mongolian'),
    (Code: 'na'; Name: 'Nauru'),
    (Code: 'nv'; Name: 'Navajo'),
    (Code: 'nr'; Name: 'Ndebele, South'),
    (Code: 'ng'; Name: 'Ndonga'),
    (Code: 'ne'; Name: 'Nepali'),
    (Code: 'se'; Name: 'Northern Sami'),
    (Code: 'nb'; Name: 'Norwegian Bokmal'),
    (Code: 'nn'; Name: 'Norwegian Nynorsk'),
    (Code: 'oc'; Name: 'Occitan'),
    (Code: 'or'; Name: 'Oriya'),
    (Code: 'os'; Name: 'Ossetian; Ossetic'),
    (Code: 'pi'; Name: 'Pali'),
    (Code: 'pa'; Name: 'Panjabi'),
    (Code: 'ps'; Name: 'Pashto, Pushto'),
    (Code: 'fa'; Name: 'Persian'),
    (Code: 'pl'; Name: 'Polish'),
    (Code: 'pt'; Name: 'Portuguese'),
    (Code: 'qu'; Name: 'Quechua'),
    (Code: 'rm'; Name: 'Rhaeto-Romance'),
    (Code: 'ro'; Name: 'Romanian'),
    (Code: 'rn'; Name: 'Rundi'),
    (Code: 'ru'; Name: 'Russian'),
    (Code: 'sm'; Name: 'Samoan'),
    (Code: 'sg'; Name: 'Sangro'),
    (Code: 'sa'; Name: 'Sanskrit'),
    (Code: 'sc'; Name: 'Sardinian'),
    (Code: 'sr'; Name: 'Serbian'),
    (Code: 'sr_RS@latin'; Name: 'Serbian (Latin)'),
    (Code: 'sh'; Name: 'Serbo-Croatian'),
    (Code: 'st'; Name: 'Sesotho'),
    (Code: 'tn'; Name: 'Setswana'),
    (Code: 'sn'; Name: 'Shona'),
    (Code: 'sd'; Name: 'Sindhi'),
    (Code: 'si'; Name: 'Sinhalese'),
    (Code: 'ss'; Name: 'Siswati'),
    (Code: 'sk'; Name: 'Slovak'),
    (Code: 'sl'; Name: 'Slovenian'),
    (Code: 'so'; Name: 'Somali'),
    (Code: 'es'; Name: 'Spanish'),
    (Code: 'su'; Name: 'Sundanese'),
    (Code: 'sw'; Name: 'Swahili'),
    (Code: 'sv'; Name: 'Swedish'),
    (Code: 'tl'; Name: 'Tagalog'),
    (Code: 'ty'; Name: 'Tahitian'),
    (Code: 'tg'; Name: 'Tajik'),
    (Code: 'ta'; Name: 'Tamil'),
    (Code: 'tt'; Name: 'Tatar'),
    (Code: 'te'; Name: 'Telugu'),
    (Code: 'th'; Name: 'Thai'),
    (Code: 'bo'; Name: 'Tibetan'),
    (Code: 'ti'; Name: 'Tigrinya'),
    (Code: 'to'; Name: 'Tonga'),
    (Code: 'ts'; Name: 'Tsonga'),
    (Code: 'tr'; Name: 'Turkish'),
    (Code: 'tk'; Name: 'Turkmen'),
    (Code: 'tw'; Name: 'Twi'),
    (Code: 'uk'; Name: 'Ukrainian'),
    (Code: 'ur'; Name: 'Urdu'),
    (Code: 'ug'; Name: 'Uyghur'),
    (Code: 'uz'; Name: 'Uzbek'),
    (Code: 'vi'; Name: 'Vietnamese'),
    (Code: 'vo'; Name: 'Volapuk'),
    (Code: 'wa'; Name: 'Walloon'),
    (Code: 'cy'; Name: 'Welsh'),
    (Code: 'wo'; Name: 'Wolof'),
    (Code: 'xh'; Name: 'Xhosa'),
    (Code: 'yi'; Name: 'Yiddish'),
    (Code: 'yo'; Name: 'Yoruba'),
    (Code: 'za'; Name: 'Zhuang'),
    (Code: 'zu'; Name: 'Zulu'));



var
 poSearchDirs: TStringList;

procedure poSetSearchDirs(Dirs: string);
var
 s : string;
 i : integer;
begin
  if poSearchDirs = NIL then poSearchDirs := TStringList.Create;
  s := Dirs;
  for i := 1 to length(s) do
    if s[i] = ',' then s[i] := ';';

  if poSearchDirs <> NIL then poSearchDirs.Clear;

  poSearchDirs.AddDelimitedText(s,';',true);
  // add current dir
  poSearchDirs.Add('.');

  if poSearchDirs.Count > 0 then
  begin
    // make sure all pathes are absolute and ends with a path separator
    for i := 0 to poSearchDirs.Count-1 do
    begin
      s := poSearchDirs[i];
      s := expandFileName(s);
      if length(s) > 0 then
        if s[length(s)] <> PathDelim then s := s + PathDelim;
      poSearchDirs[i] := s;
    end;
  end;
end;

function poGetProgramBaseName : string;
begin
  result := ChangeFileExt(ExtractFileName(paramStr(0)),'');
end;

// extract the language from a menu item and activate the language
function poSelectLanguageMenuClick(Sender: TObject) : string;
var
 s: string;
 p: integer;
begin
  result := '';
  if sender is TMenuItem then
  begin
    s := TMenuItem(Sender).Caption;
    p := pos('(',s);
    if p>0 then
    begin
      system.delete(s,1,p);
      p := pos(')',s);
      if p > 0 then
      begin
        delete(s,p,length(s));
        SetLanguage(s);
        result := s;
      end;
    end;
  end;
end;


function setLanguage(s: String): String;
begin
  SetDefaultLang(s,'locale','lclstrconsts',true);
  SetDefaultLang(s,'locale','',true);
  result := s;
end;

function poFindPoFile(baseName,IsoLanguage: string): string;
var
 path,fn : String;
begin
  for path in poSearchDirs do
  begin
    fn := path+baseName+'.'+IsoLanguage;
    if fileExists(fn+'.mo') then exit (fn);
    if fileExists(fn+'.po') then exit (fn);
  end;
  result := '';
end;

procedure poGenerateLanguageSelectMenuEntries(base: TMenuItem; p: TNotifyEvent);
var
 i,count: integer;
 baseName : string;
 poName : string;
 tNew: TMenuItem;
begin
  count := 0;
  baseName := poGetProgramBaseName;
  for i := low(IsoLanguages) to high(IsoLanguages) do
  begin
    poName := poFindPoFile(baseName,IsoLanguages[i].code);
    if length(poName) > 0 then
    begin
      tNew:= TMenuItem.Create(base);
      tNew.Caption :=IsoLanguages[i].name+' ('+IsoLanguages[i].code+')';
      tNew.OnClick := p;
      base.add(tNew);
      inc(count);
    end;
  end;
  if count = 0 then base.Enabled:=false;  // dont show laguages menu if no langiage files present
end;

initialization
  poSetSearchDirs('locale');
finalization
  if poSearchDirs <> NIL then
    poSearchDirs.Free;
end.

