unit languages;

{ this unit provides support for various (human) languages }

interface

type
   { ------------------- Language (human) string indices ------------- }
   { a string in a (human) language; the case of the identifiers is
     significant; the strings should have the same case as the
     identifiers here }
   TLangString = (AAA_str, Access_str, constant_str, Contents_str, Class_tree_str,
                  Declaration_str, Declarations_str,
                  Description_str, field_str, file_str, global_str, ID_str,
                  Immediate_ancestor_str, Immediate_ancestors_str,
                  Immediate_descendants_str, Implemented_by_str,
                  Implemented_interface_str, Implemented_interfaces_str,
                  Interface_tree_str, interface_str, local_str,
                  Members_str, Modules_str, Name_str,
                  Notes_on_the_implementation_str,
                  on_line_str, Parameters_str, private_str, protected_str,
                  public_str, property_str, record_str, Reference_for_str,
                  Related_methods_str, See_also_str, Source_str, Symbol_index_str,
                  Synopsis_str,
                  Table_of_contents_str, thread_local_str, ttype_str,
                  Type_str, Uses_in_implementation_str,
                  Uses_in_interface_str,
                  variable_str,
                  Visibility_str, ZZZ_str);
   
{ if <lang> is a valid language identifier then sets the output
  (human) language to that language and returns true; otherwise
  returns false }
function SetLanguage(lang : String) : Boolean;
{ returns a string in a (human) language corresponding to the
  specified index }
function GetLangString(s : TLangString) : String;
{ returns the name of character set to be used for the current
  language }
function LangCharsetName : String;


implementation

type
   TLangTable = array[AAA_str..ZZZ_str] of String;
   PLangTable = ^TLangTable;
   
const
   EnglishCharset = 'ISO-8859-1';
   PolishCharset = 'ISO-8859-2';
var
   EnglishTable : TLangTable;
   PolishTable : TLangTable;
   CurrLangTable : PLangTable;
   CurrCharset : String;

function SetLanguage(lang : String) : Boolean;
begin
   Result := true;
   if (lang = 'en') or (lang = 'eng') then
   begin
      CurrLangTable := @EnglishTable;
      CurrCharset := EnglishCharset;
   end else if (lang = 'pl') then
   begin
      CurrLangTable := @PolishTable;
      CurrCharset := PolishCharset;
   end else
      Result := false;
end;

function GetLangString(s : TLangString) : String;
begin
   Result := CurrLangTable^[s];
end;

function LangCharsetName : String;
begin
   Result := CurrCharset;
end;

initialization
   { English }
   EnglishTable[AAA_str] := 'AAA';
   EnglishTable[Access_str] := 'Access';
   EnglishTable[constant_str] := 'constant';
   EnglishTable[Contents_str] := 'Contents';
   EnglishTable[Class_tree_str] := 'Class tree';
   EnglishTable[Declaration_str] := 'Declaration';
   EnglishTable[Declarations_str] := 'Declarations';
   EnglishTable[Description_str] := 'Description';
   EnglishTable[field_str] := 'field';
   EnglishTable[file_str] := 'file';
   EnglishTable[global_str] := 'global';
   EnglishTable[ID_str] := 'ID';
   EnglishTable[Immediate_ancestor_str] := 'Immediate ancestor';
   EnglishTable[Immediate_ancestors_str] := 'Immediate ancestors';
   EnglishTable[Immediate_descendants_str] := 'Immediate descendants';
   EnglishTable[Implemented_by_str] := 'Implemented by';
   EnglishTable[Implemented_interface_str] := 'Implemented interface';
   EnglishTable[Implemented_interfaces_str] := 'Implemented interfaces';
   EnglishTable[Interface_tree_str] := 'Interface tree';
   EnglishTable[interface_str] := 'interface';
   EnglishTable[local_str] := 'local';
   EnglishTable[Members_str] := 'Members';
   EnglishTable[Modules_str] := 'Modules';
   EnglishTable[Name_str] := 'Name';
   EnglishTable[Notes_on_the_implementation_str] := 'Notes on the implementation';
   EnglishTable[on_line_str] := 'on line';
   EnglishTable[Parameters_str] := 'Parameters';
   EnglishTable[private_str] := 'private';
   EnglishTable[protected_str] := 'protected';
   EnglishTable[property_str] := 'property';
   EnglishTable[public_str] := 'public';
   EnglishTable[record_str] := 'record';
   EnglishTable[Reference_for_str] := 'Reference for';
   EnglishTable[Related_methods_str] := 'Related methods';
   EnglishTable[See_also_str] := 'See also';
   EnglishTable[Source_str] := 'Source';
   EnglishTable[Symbol_index_str] := 'Symbol index';
   EnglishTable[Synopsis_str] := 'Synopsis';
   EnglishTable[Table_of_contents_str] := 'Table of contents';
   EnglishTable[thread_local_str] := '(thread local)';
   EnglishTable[Type_str] := 'Type';
   EnglishTable[ttype_str] := 'type';
   EnglishTable[Uses_in_implementation_str] := 'Uses in implementation';
   EnglishTable[Uses_in_interface_str] := 'Uses in interface';
   EnglishTable[variable_str] := 'variable';
   EnglishTable[Visibility_str] := 'Visibility';
   EnglishTable[ZZZ_str] := 'ZZZ';
   
   { Polish }
   PolishTable[AAA_str] := 'AAA';
   PolishTable[Access_str] := 'Dostêp';
   PolishTable[constant_str] := 'sta³a';
   PolishTable[Contents_str] := 'Spis tre¶ci';
   PolishTable[Class_tree_str] := 'Drzewo klas';
   PolishTable[Declaration_str] := 'Deklaracja';
   PolishTable[Declarations_str] := 'Deklaracje';
   PolishTable[Description_str] := 'Opis';
   PolishTable[field_str] := 'pole';
   PolishTable[file_str] := 'plik';
   PolishTable[global_str] := 'globalny';
   PolishTable[ID_str] := 'ID';
   PolishTable[Immediate_ancestor_str] := 'Bezpo¶redni przodek';
   PolishTable[Immediate_ancestors_str] := 'Bezpo¶redni przodkowie';
   PolishTable[Immediate_descendants_str] := 'Klasy wywodz±ce siê';
   PolishTable[Implemented_by_str] := 'Implementowany przez';
   PolishTable[Implemented_interface_str] := 'Implementowany interfejs';
   PolishTable[Implemented_interfaces_str] := 'Implementowane interfejsy';
   PolishTable[Interface_tree_str] := 'Drzewo interfejsów';
   PolishTable[interface_str] := 'interfejs';
   PolishTable[local_str] := 'lokalny';
   PolishTable[Members_str] := 'Cz³onkowie';
   PolishTable[Modules_str] := 'Modu³y';
   PolishTable[Name_str] := 'Nazwa';
   PolishTable[Notes_on_the_implementation_str] := 'Uwagi o implementacji';
   PolishTable[on_line_str] := 'od wiersza';
   PolishTable[Parameters_str] := 'Parametry';
   PolishTable[private_str] := 'prywatny';
   PolishTable[protected_str] := 'chroniony';
   PolishTable[property_str] := 'property';
   PolishTable[public_str] := 'publiczny';
   PolishTable[record_str] := 'rekord';
   PolishTable[Reference_for_str] := 'Dokumentacja dla';
   PolishTable[Related_methods_str] := 'Powi±zane metody';
   PolishTable[Table_of_contents_str] := 'Spis tre¶ci';
   PolishTable[thread_local_str] := '(lokalny w w±tku)';
   PolishTable[See_also_str] := 'Zobacz te¿';
   PolishTable[Source_str] := '¬ród³o';
   PolishTable[Symbol_index_str] := 'Indeks symboli';
   PolishTable[Synopsis_str] := 'Krótki opis';
   PolishTable[Type_str] := 'Typ';
   PolishTable[ttype_str] := 'typ';
   PolishTable[Uses_in_implementation_str] := 'Modu³y u¿ywane w implementacji';
   PolishTable[Uses_in_interface_str] := 'Modu³y u¿ywane w interfejsie';
   PolishTable[variable_str] := 'zmienna';
   PolishTable[Visibility_str] := 'Widoczno¶æ';
   PolishTable[ZZZ_str] := 'ZZZ';
   
   { the default }
   CurrLangTable := @EnglishTable;
   CurrCharset := EnglishCharset;
end.

