(*  This file is a part of the srcdoc program for creating
    documentation of Delphi/Pascal programs from comments in
    source files.

    Copyright (C) 2005 by Lukasz Czajka

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
    02110-1301 USA *)

unit delphiparser;

{ this unit implements a parser for delphi syntax }

interface

uses
   CommonClasses, Classes, SysUtils, input, adtcont, adtmap;

var
   { a set of extensions of pascal files - the files recognized and
     parsed by TDelphiParser }
   PascalExtensions : TStringSet;

type
   TDeclText = class;

   TVisibility = (visGlobal, visLocal, visLocalThreadLocal, visGlobalThreadLocal,
                  visPublic, visPrivate, visProtected);
   TVisibilityRec = record
      FName : String;
      FType : TVisibilityType;
   end;

   TDelphiParser = class (TLanguageParser)
   private
      FFileName : String;
      reader : TStreamReader;
      { true if the interface part of the unit is currently being
        parsed }
      fInInterface : Boolean;
      { true if the implementation part of the unit is currently being
        parsed }
      fInImplementation : Boolean;
      { the current visibility modifier }
      currentVisibility : TVisibility;
      { this is set by the ParseFile function to either the list of
        units used in the interface or the implementation section;
        this is really a pointer to either interfaceUses or implUses,
        so it should never be destroyed }
      currentUses : TStrings;
      { the units the unit currently being parsed uses in its
        interface section; this should be created and destroyed in the
        ParseFile method only }
      interfaceUses : TStrings;
      { the units the unit currently being parsed uses in its
        implementation section; this should be created and destroyed in the
        ParseFile method only }
      implUses : TStrings;
      { the name of the unit currently being parsed }
      unitNameStr : String;
      { the comment of the current unit }
      unitComment : String;
      { comment on the implementation of the unit being parsed }
      unitImplComment : String;
      { the comment most recently read; it is guaranteed that after
        this comment there has not yet been any keyword read }
      recentComment : String;
      { the symbol most recently encountered }
      recentSymbol : String;
      { the declaration text currently being formed }
      declText : TDeclText;
      { this is set by CallGlobalHandler to signal that the name
        passed has not been recognized as any valid keyword or
        identifier; this flag should always be checked after calling
        that function }
      errUnknownIdentifier : Boolean;
      { stores the symbols declared in the _current_ block; the only
        method that may add symbols to this set is CheckValidName; if
        this object is nil it does not add the symbol (used in
        ReadVariableList to avoid adding local variable names as
        global symbols); this is owned by the object and always
        destroyed in FinishBlock, so if should be nil on the
        destruction }
      declaredSymbols : TStringSet;
      { a stack of TStringSet's; used to save declaredSymbols when
        entering a new block }
      declaredSymbolsStack : TStackAdt;

      { a set of AnsiString's of names of valid calling convention
        modifiers }
      CallModifiers : TStringSet;
      { a set of AnsiString's of names of valid routine modifiers }
      RoutineModifiers : TStringSet;
      { a set of modifiers that may come before names of parameters
        (const, var, ...) }
      ParameterModifiers : TStringSet;
      { a set of keywords that may begin a new block }
      BlockStarters : TStringSet;
      { a set of keywords that may begin a block that is always ended
        with 'end' }
      EndBlockStarters : TStringSet;
      { maps of keyword names to functions that handle them; for
        different levels and sections }
      GlobalKeywords : TMap;
      TypeSectionKeywords : TMap;
      ClassSectionKeywords : TMap;
      InterfaceSectionKeywords : TMap;

      { a map from the visibility type to the data associated with
        this visibility }
      VisibilityRec : array[visGlobal..visProtected] of TVisibilityRec;

      { gives an error }
      procedure Error(msg : String);
      { reads a delphi token; the original case is preserved, so
        before comparing it with any names of keywords call the
        LowerCase function }
      function ReadToken : String;
      { checks whether the next token is equal to t; if no generates an
        error }
      procedure Expect(const t : String);
      { reads consequtive comments joining them into one comment or
        discarding all but the last one (depending on whether
        optJoinComments is active); sets token to the first
        non-comment token; if token2 <> '' then assumes the first
        token to be token2, otherwise reads the first analysed token
        from input }
      function ReadComments(var token : String; token2 : String) : String;
      { checks whether <symbol> is a valid pascal name; gives an error
        if it is not }
      procedure CheckValidName(const symbol : String);

      procedure StartBlock;
      procedure FinishBlock;

      { calls the handler associated with token; <table> is a table of
        handler functions associated with keywords; passes token to
        the handler function and returns the result of this function;
        if token does not represent any valid identifier then sets the
        errUnknownIdentifier flag and returns its argument; the
        errUnknownIdentifier flag should always be checked after
        calling this routine and an appropriate action should be
        taken; automatically calls declText.Reset if resetDecl is
        true; also, keep in mind that this function does not handle
        comments }
      function CallHandler(const token : String;
                           const table : TMap;
                           resetDecl : Boolean) : String;

      { creates a new reader for <afile>; the old reader is freed }
      procedure OpenReader(afile : TFileStream);
      { closes the current reader and the file associated with it;
        it is important for systems which do not allow a file to be
        referenced by more than one file handle at a time }
      procedure CloseReader;
      { this function should be called each time when beginning to
        parse a unit; resets some vital variables }
      procedure InitializeUnit(afile : TFileStream);
      { frees the objects used in parsing the units that would be
        normally passed to ProvideUnitInfo or
        ProvideUnitImplementationInfo; call this only if you don't
        call the aforementioned methods, but end the parsing
        prematurely }
      procedure FreeObjectsUsedInParsingUnit;
      { reads in the name of a unit; should be called at the very
        beginning or not t all; <token> is used as the first input token }
      procedure ReadUnitName(token : String);
      { skips till the implementation part or the end of file }
      procedure SkipToImplementation;
      { skips to the nearest uses clause, parses it and returns a
        string composed of all the files of units in the uses clause }
      function ParseNearestUses : TStrings;
      { executes the main parsing loop; <token> is used as the first
        input token }
      procedure ExecuteParsingLoop(token : String);
      { Reads in a list of variables terminated with either ';', ')'
        or ']'. Returns the terminating token but does not append it
        to the declaration text. If ignoreSymbols is true then the
        symbols declared are not added to the set of symbols.  }
      function ReadVariableList(const token : String; ignoreSymbols : Boolean;
                                list : TStrings) : String;

      { various parsing functions (conforming to the THandler
        specification) }
      function ParseUses(const token : String) : String;
      function ParseType(const token : String) : String;
      function ParseConst(const token : String) : String;
      function ParseVar(const token : String) : String;
      function ParseThreadVar(const token : String) : String;
      function ParseLabel(const token : String) : String;
      function ParseRoutine(const token : String) : String;
      function ParseClassRoutine(const token : String) : String;
      function ParseProperty(const token : String) : String;
      function ParseClass(const token : String) : String;
      function ParseRecord(const token : String) : String;
      function ParseInterface(const token : String) : String;
      function ParseProceduralType(const token : String) : String;
      function ParseVisibility(const token : String) : String;
      function ParseCodeBlock(const token : String) : String;
      function ParseEmptyStatement(const token : String) : String;
      function ParseBegin(const token : String) : String;
      function ParseEnd(const token : String) : String;

   public
      constructor Create;
      destructor Destroy; override;
      { parses a unit in the file named filename }
      procedure ParseFile(afile : TFileStream); override;
      procedure ParseImplementation(afile : TFileStream); override;
      { returns a list of all files on which the unit or program in
        <afile> depends }
      function GetDependencies(afile : TFileStream) : TStrings; override;
      function GetImplementationDependencies(afile : TFileStream) : TStrings;
      override;
      { parses a piece of code converting it to a TTextObject }
      function ParseCode(code : String) : TTextObject; override;
      { converts a qualified reference to an identifier into a
        reference to a section }
      function ParseIdentifierRef(ref : String) : String; override;
      { returns the token used for scope resolution }
      function ScopeResolutionToken : String; override;
      function IgnoreCase : Boolean; override;
   end;

   { an object of this class should be created and re-used later as
     the creation imposes certain overhead; }
   TDeclText = class
   private
      { the parser which created the object }
      FParser : TDelphiParser;
      { a text object representing the declaration text of the
        declaration currently being parsed }
      declText : TParagraph;
      { the line at which the current declaration starts }
      declLine : Integer;
      { the string currently being build that is to be added at the
        end of the next object in declText }
      currentDeclStr : String;
      { true if the token most recently appended to currentDeclStr
        requires no space after it (used exclusively by AppendToken,
        StartDeclText and FinishDeclText) }
      NoSpace : Boolean;
      { a set of highlighted keywords' names }
      HighlightedKeywords : TStringSet;
      { a set of tokens before which a space shouldn't be put }
      NoSpaceBeforeToken : TStringSet;
      { a set of token2 after which a space shouldn't be put }
      NoSpaceAfterToken : TStringSet;

      procedure FinishDeclTextObject(keyword, symbol : Boolean);
   public
      { the constructor should be called when all fields of <parser>
        are already initialized }
      constructor Create(parser : TDelphiParser);
      destructor Destroy; override;
      { append a token to the declaration text formatting it as a
        piece of code; inserts an appropriate number of whitespace
        before <token>; }
      procedure AppendToken(const token : String);
      { the same as above, but appends <token> as a global symbol }
      procedure AppendGlobalSymbol(const token : String);
      { inserts a newline }
      procedure NewLine;
      { makes an indentation }
      procedure Indent;
      { clears the declaration text }
      procedure Reset;
      { this function returns the declaration text that have been
        built up till now and resets self to contain _no_ text }
      function GetText : TTextObject;
      { the same as above but does not return the declaration text as
        a paragraph, but as a plain list of text objects }
      function GetTextList : TTextObject;
      property LineNumber : Integer read declLine;
   end;

implementation

uses
   adthash, adtfunct, adthashfunct, adtalgs, adtlist, adtiters, languages;

{ ----------------------------- TDeclText ------------------------------ }

constructor TDeclText.Create(parser : TDelphiParser);
begin
   FParser := parser;

   HighlightedKeywords := TStringSet.Create;
   HighlightedKeywords.ItemComparer := NoCaseStringComparer;

   NoSpaceBeforeToken := TStringSet.Create;
   NoSpaceBeforeToken.ItemComparer := NoCaseStringComparer;

   NoSpaceAfterToken := TStringSet.Create;
   NoSpaceAfterToken.ItemComparer := NoCaseStringComparer;

   SetUnionCopyToArg(HighlightedKeywords, FParser.CallModifiers, StringIdentity);
   SetUnionCopyToArg(HighlightedKeywords, FParser.RoutineModifiers, StringIdentity);
   SetUnionCopyToArg(HighlightedKeywords, FParser.ParameterModifiers, StringIdentity);

   with HighlightedKeywords do
   begin
      Insert('absolute');
      Insert('and');
      Insert('automated');
      Insert('array');
      Insert('as');
      Insert('begin');
      Insert('break');
      Insert('case');
      Insert('class');
      Insert('constructor');
      Insert('contains');
      Insert('continue');
      Insert('default');
      Insert('destructor');
      Insert('div');
      Insert('do');
      Insert('downto');
      Insert('else');
      Insert('end');
      Insert('except');
      Insert('exit');
      Insert('exports');
      Insert('false');
      Insert('file');
      Insert('finalization');
      Insert('finally');
      Insert('for');
      Insert('function');
      Insert('goto');
      Insert('if');
      Insert('implementation');
      Insert('in');
      Insert('inherited');
      Insert('initialization');
      Insert('inline');
      Insert('interface');
      Insert('is');
      Insert('label');
      Insert('library');
      Insert('mod');
      Insert('nil');
      Insert('not');
      Insert('object');
      Insert('of');
      Insert('on');
      Insert('operator');
      Insert('or');
      Insert('package');
      Insert('packed');
      Insert('private');
      Insert('procedure');
      Insert('program');
      Insert('property');
      Insert('protected');
      Insert('public');
      Insert('published');
      Insert('raise');
      Insert('read');
      Insert('record');
      Insert('repeat');
      Insert('requires');
      Insert('self');
      Insert('set');
      Insert('shl');
      Insert('shr');
      Insert('then');
      Insert('threadvar');
      Insert('to');
      Insert('try');
      Insert('true');
      Insert('type');
      Insert('unit');
      Insert('until');
      Insert('uses');
      Insert('var');
      Insert('while');
      Insert('with');
      Insert('write');
      Insert('xor');
   end;

   with NoSpaceBeforeToken do
   begin
      Insert(';');
      Insert(',');
      Insert('.');
      Insert('(');
      Insert(')');
      Insert('[');
      Insert(']');
   end;

   with NoSpaceAfterToken do
   begin
      Insert('(');
      Insert('[');
      Insert('^');
      Insert('$');
      Insert('#');
      Insert('.');
   end;

   currentDeclStr := '';
   NoSpace := true;
   declText := TParagraph.Create(TSingleList.Create);
end;

destructor TDeclText.Destroy;
begin
   HighlightedKeywords.Free;
   NoSpaceBeforeToken.Free;
   NoSpaceAfterToken.Free;
   declText.Free;
end;

procedure TDeclText.FinishDeclTextObject(keyword, symbol : Boolean);
var
   attr : TTextAttributeSet;
begin
   Assert(declText <> nil);
   if currentDeclStr <> '' then
   begin
      attr := [saCode];
      if keyword then
         Include(attr, saKeyword);
      if symbol then
         Include(attr, saGlobalSymbol);
      declText.TextObjectList.PushBack(TText.Create(currentDeclStr, attr));
   end;
   currentDeclStr := '';
end;

procedure TDeclText.AppendToken(const token : String);
begin
   Assert(declText <> nil);
   if not (NoSpaceBeforeToken.Has(token) or NoSpace) then
      currentDeclStr := currentDeclStr + ' ';

   if HighlightedKeywords.Has(token) then
   begin
      FinishDeclTextObject(false, false);
      currentDeclStr := token;
      FinishDeclTextObject(true, false);
   end else
      currentDeclStr := currentDeclStr + token;

   NoSpace := NoSpaceAfterToken.Has(token);
end;

procedure TDeclText.AppendGlobalSymbol(const token : String);
begin
   if not (NoSpaceBeforeToken.Has(token) or NoSpace) then
      currentDeclStr := currentDeclStr + ' ';
   FinishDeclTextObject(false, false);
   AppendToken(token);
   currentDeclStr := Trim(currentDeclStr);
   FinishDeclTextObject(false, true);
end;

procedure TDeclText.NewLine;
begin
   FinishDeclTextObject(false, false);
   declText.TextObjectList.PushBack(TNewLine.Create);
end;

procedure TDeclText.Indent;
begin
   FinishDeclTextObject(false, false);
   declText.TextObjectList.PushBack(TIndent.Create);
end;

procedure TDeclText.Reset;
begin
   Assert(declText <> nil);
   currentDeclStr := '';
   NoSpace := true;
   declLine := FParser.reader.LineNumber;
   declText.TextObjectList.Clear;
end;

function TDeclText.GetText : TTextObject;
begin
   Assert(declText <> nil);
   FinishDeclTextObject(false, false);
   Result := declText;
   declText := TParagraph.Create(TSingleList.Create);
   Reset;
end;

function TDeclText.GetTextList : TTextObject;
begin
   Assert(declText <> nil);
   FinishDeclTextObject(false, false);
   Result := TTextObjectList.Create(TListAdt(declText.TextObjectList.CopySelf(
                                       Adapt(@CopyTextObject)
                                                                             )));
   Reset;
end;


{ ---------------------------- TDelphiParser ------------------------- }

constructor TDelphiParser.Create;
begin
   inherited;

   GlobalKeywords := TMap.Create(THashTable.Create);
   TypeSectionKeywords := TMap.Create(THashTable.Create);
   ClassSectionKeywords := TMap.Create(THashTable.Create);
   InterfaceSectionKeywords := TMap.Create(THashTable.Create);

   CallModifiers := TStringHashTable.Create;
   CallModifiers.ItemComparer := NoCaseStringComparer;

   ParameterModifiers := TStringSet.Create;
   ParameterModifiers.ItemComparer := NoCaseStringComparer;

   BlockStarters := TStringSet.Create;
   BlockStarters.ItemComparer := NoCaseStringComparer;

   EndBlockStarters := TStringSet.Create;
   EndBlockStarters.ItemComparer := NoCaseStringComparer;

   GlobalKeywords['uses'] := THandler.Create(@ParseUses);
   GlobalKeywords['type'] := THandler.Create(@ParseType);
   GlobalKeywords['const'] := THandler.Create(@ParseConst);
   GlobalKeywords['var'] := THandler.Create(@ParseVar);
   GlobalKeywords['threadvar'] := THandler.Create(@ParseThreadvar);
   GlobalKeywords['label'] := THandler.Create(@ParseLabel);
   GlobalKeywords['function'] := THandler.Create(@ParseRoutine);
   GlobalKeywords['procedure'] := THandler.Create(@ParseRoutine);
   GlobalKeywords['operator'] := THandler.Create(@ParseRoutine);
   GlobalKeywords['class'] := THandler.Create(@ParseClassRoutine);
   GlobalKeywords['constructor'] := THandler.Create(@ParseRoutine);
   GlobalKeywords['destructor'] := THandler.Create(@ParseRoutine);
   GlobalKeywords['implementation'] := THandler.Create(@ParseEnd);
   GlobalKeywords['begin'] := THandler.Create(@ParseBegin);
   GlobalKeywords['end'] := THandler.Create(@ParseEnd);
   GlobalKeywords['initialization'] := THandler.Create(@ParseEnd);
   GlobalKeywords['finalization'] := THandler.Create(@ParseEnd);
   GlobalKeywords[';'] := THandler.Create(@ParseEmptyStatement);
   { necessary to handle @decl comment-start commands }
   GlobalKeywords['}'] := THandler.Create(@ParseEmptyStatement);

   TypeSectionKeywords['class'] := THandler.Create(@ParseClass);
   TypeSectionKeywords['object'] := THandler.Create(@ParseClass);
   TypeSectionKeywords['record'] := THandler.Create(@ParseRecord);
   TypeSectionKeywords['interface'] := THandler.Create(@ParseInterface);
   TypeSectionKeywords['procedure'] := THandler.Create(@ParseProceduralType);
   TypeSectionKeywords['function'] := THandler.Create(@ParseProceduralType);

   ClassSectionKeywords['function'] := THandler.Create(@ParseRoutine);
   ClassSectionKeywords['procedure'] := THandler.Create(@ParseRoutine);
   ClassSectionKeywords['constructor'] := THandler.Create(@ParseRoutine);
   ClassSectionKeywords['destructor'] := THandler.Create(@ParseRoutine);
   ClassSectionKeywords['class'] := THandler.Create(@ParseClassRoutine);
   ClassSectionKeywords['property'] := THandler.Create(@ParseProperty);
   ClassSectionKeywords['public'] := THandler.Create(@ParseVisibility);
   ClassSectionKeywords['private'] := THandler.Create(@ParseVisibility);
   ClassSectionKeywords['protected'] := THandler.Create(@ParseVisibility);
   ClassSectionKeywords['published'] := THandler.Create(@ParseVisibility);
   ClassSectionKeywords['automated'] := THandler.Create(@ParseVisibility);
   ClassSectionKeywords['end'] := THandler.Create(@ParseEnd);
   ClassSectionKeywords[';'] := THandler.Create(@ParseEmptyStatement);
   ClassSectionKeywords['}'] := THandler.Create(@ParseEmptyStatement);

   InterfaceSectionKeywords['function'] := THandler.Create(@ParseRoutine);
   InterfaceSectionKeywords['procedure'] := THandler.Create(@ParseRoutine);
   InterfaceSectionKeywords['property'] := THandler.Create(@ParseProperty);
   InterfaceSectionKeywords['end'] := THandler.Create(@ParseEnd);
   InterfaceSectionkeywords[';'] := THandler.Create(@ParseEmptyStatement);
   InterfaceSectionkeywords['}'] := THandler.Create(@ParseEmptyStatement);

   with VisibilityRec[visGlobal] do
   begin
      FName := GetLangString(global_str);
      FType := vtPublic;
   end;
   with VisibilityRec[visLocal] do
   begin
      FName := GetLangString(local_str);
      FType := vtPrivate;
   end;
   with VisibilityRec[visLocalThreadLocal] do
   begin
      FName := GetLangString(local_str) + ' (' +
         GetLangString(thread_local_str) + ')';
      FType := vtPrivate;
   end;
   with VisibilityRec[visGlobalThreadLocal] do
   begin
      FName := GetLangString(global_str) + ' (' +
         GetLangString(thread_local_str) + ')';
      FType := vtPublic;
   end;
   with VisibilityRec[visPublic] do
   begin
      FName := GetLangString(public_str);
      FType := vtPublic;
   end;
   with VisibilityRec[visPrivate] do
   begin
      FName := GetLangString(private_str);
      FType := vtPrivate;
   end;
   with VisibilityRec[visProtected] do
   begin
      FName := GetLangString(protected_str);
      FType := vtProtected;
   end;

   with CallModifiers do
   begin
      Insert('register');
      Insert('cdecl');
      Insert('pascal');
      Insert('stdcall');
      Insert('safecall');
      Insert('saveregisters');
      Insert('popstack');
   end;

   RoutineModifiers := TStringHashTable(CallModifiers.CopySelf);

   with RoutineModifiers do
   begin
      Insert('[public]');
      Insert('abstract');
      Insert('alias');
      Insert('assembler');
      Insert('dynamic');
      Insert('export');
      Insert('external');
      Insert('far');
      Insert('forward');
      Insert('inline');
      Insert('interrupt');
      Insert('message');
      Insert('near');
      Insert('overload');
      Insert('override');
      Insert('reintroduce');
      Insert('virtual');
   end;

   with ParameterModifiers do
   begin
      Insert('const');
      Insert('var');
      Insert('out');
   end;

   with BlockStarters do
   begin
      Insert('begin');
      Insert('var');
      Insert('type');
      Insert('const');
      Insert('asm');
   end;

   { stores everything that is ended with 'end' }
   with EndBlockStarters do
   begin
      Insert('begin');
      Insert('asm');
      Insert('case');
      Insert('try');
      Insert('record');
      Insert('class');
   end;

   declText := TDeclText.Create(self);
   declaredSymbolsStack := TStack.Create; { no disposer }
   declaredSymbolsStack.OwnsItems := false;

   Assert(declaredSymbols = nil);
end; { end constructor }

destructor TDelphiParser.Destroy;
begin
   declText.Free;
   Assert((declaredSymbolsStack = nil) or (declaredSymbolsStack.Empty));
   Assert(declaredSymbols = nil);
   declaredSymbolsStack.Free;

   reader.Free;

   GlobalKeywords.Free;
   TypeSectionKeywords.Free;
   ClassSectionKeywords.Free;
   InterfaceSectionKeywords.Free;

   CallModifiers.Free;
   RoutineModifiers.Free;
   ParameterModifiers.Free;

   BlockStarters.Free;
   EndBlockStarters.Free;

   inherited;
end;

procedure TDelphiParser.Error(msg : String);
begin
   Driver.Error(msg, FFileName, reader.LineNumber);
end;

function TDelphiParser.ReadToken : String;
var
   c, c2 : Char;
begin
   Result := reader.ReadWord;
   if Result = '' then
   begin
      c := reader.ReadChar;
      case c of
         '<':
            begin
               c2 := reader.ReadChar;
               if (c2 = '=') or (c2 = '>') then
                  Result := c + c2
               else begin
                  Result := c;
                  reader.UnReadChar;
               end;
            end;
         '>':
            begin
               c2 := reader.ReadChar;
               if (c2 = '=') then
                  Result := c + c2
               else begin
                  Result := c;
                  reader.UnReadChar;
               end;
            end;
         ':':
            begin
               c2 := reader.ReadChar;
               if (c2 = '=') then
                  Result := c + c2
               else begin
                  Result := c;
                  reader.UnReadChar;
               end;
            end;
         '(':
            begin
               c2 := reader.ReadChar;
               if (c2 = '*') then
                  Result := c + c2
               else begin
                  Result := c;
                  reader.UnReadChar;
               end;
            end;
         '*':
            begin
               c2 := reader.ReadChar;
               if (c2 = ')') or (c2 = '*') then
                  Result := c + c2
               else begin
                  Result := c;
                  reader.UnReadChar;
               end;
            end;
         '/':
            begin
               c2 := reader.ReadChar;
               if (c2 = '/') then
                  Result := c + c2
               else begin
                  Result := c;
                  reader.UnReadChar;
               end;
            end;
         '''': // handle a '
            begin
               Result := c;
               c := reader.ReadChar;
               while c <> '''' do
               begin
                  Result := Result + c;
                  c := reader.ReadChar;
               end;
               Result := Result + '''';
            end;
         '"': { handle a " }
            begin
               Result := c;
               c := reader.ReadChar;
               while c <> '"' do
               begin
                  Result := Result + c;
                  c := reader.ReadChar;
               end;
               Result := Result + '"';
            end;
         '#':
            begin
               Result := c + reader.ReadWord;
            end;
         '{':
            begin
               if reader.PeekChar = '$' then
                  { skip }
               begin
                  reader.ReadUntil(['}']);
                  reader.ReadChar; { skip the closing bracket }
                  Result := ReadToken();
               end else
                  Result := c;
            end
         else
            begin
               Result := c;
            end;
      end; { end case }
   end; { end if }
end; { end ReadToken }

procedure TDelphiParser.Expect(const t : String);
begin
   if LowerCase(ReadToken) <> t then
      Error('Expected ' + t);
end;

function TDelphiParser.ReadComments(var token : String;
                                    token2 : String) : String;
var
   rbrace : String;
   c : Char;
begin
   Result := '';
   with reader do
   begin
      while true do
      begin
         if token2 <> '' then
         begin
            token := token2;
            token2 := '';
         end else
         begin
            token := ReadToken;
         end;

         if (token = '{') or (token = '(*') or (token = '//') then
         begin
            if not (optJoinComments in Driver.Options) then
               Result := ''
            else if optNewParagraphBeforeJoinedComment in Driver.Options then
               Result := Result + ' @para ';
         end;

         if (token = '{') or (token = '(*') then
         begin
            if token = '{' then
               rbrace := '}'
            else
               rbrace := '*)';

            if reader.PeekChar = '@' then
            begin
               reader.ReadChar;
               token := ReadToken;
               if token = 'decl' then
               begin
                  token := ReadToken;
                  if token <> rbrace then
                     break;
                  { else another iteration of the loop }
               end else
               begin
                  if token <> 'discard' then
                     Error('Invalid comment-start command.');
                  while token <> rbrace do
                  begin
                     token := ReadToken;
                  end;
               end;
            end else if reader.PeekChar <> '$' then
            begin
               token := ReadToken;
               if (token <> rbrace) then
               begin
                  Result := Result + token;
                  while true do
                  begin
                     Result := Result + ReadUntil(['}', '*']);
                     c := ReadChar;
                     if (c = rbrace) then
                     begin
                        break;
                     end else if (c = '*') and (rbrace = '*)') then
                     begin
                        c := ReadChar;
                        if c = ')' then
                        begin
                           break;
                        end else
                           Result := Result + '*' + c;
                     end else
                        Result := Result + c;
                  end;
               end;
            end else { the next char is '$' }
            begin
               reader.ReadChar;
               token := ReadToken;
               while token <> rbrace do
               begin
                  token := ReadToken;
               end;
            end;
         end else if token = '//' then
         begin
            Result := Result + ' ' + ReadLine;
         end else
            break;
      end; { end main while }
   end; { end with reader do }
end;

procedure TDelphiParser.CheckValidName(const symbol : String);
begin
   { note that we only have to check whether the first character is
     not a letter since the symbol was fetched using ReadWord }
   if (symbol = '') or (not (chAlpha in reader.CharTable[symbol[1]])) then
      Error('Invalid identifier ''' + symbol + '''');
   if declaredSymbols <> nil then
   begin
      declaredSymbols.Insert(symbol);
   end;
end;

procedure TDelphiParser.StartBlock;
begin
   declaredSymbolsStack.PushBack(declaredSymbols);
   declaredSymbols := TStringHashTable.Create;
end;

procedure TDelphiParser.FinishBlock;
begin
   declaredSymbols.Free;
   if not declaredSymbolsStack.Empty then
   begin
      declaredSymbols := TStringSet(declaredSymbolsStack.Back);
      declaredSymbolsStack.PopBack;
   end else
      declaredSymbols := nil;
end;

function TDelphiParser.CallHandler(const token : String;
                                   const table : TMap;
                                   resetDecl : Boolean) : String;
var
   fun : THandler;
begin
   if resetDecl then
      declText.Reset;
   fun := THandler(table.Find(LowerCase(token)));
   if fun <> nil then
   begin
      errUnknownIdentifier := false;
      Result := fun.Invoke(token);
   end else
   begin
      errUnknownIdentifier := true;
      Result := token;
   end;
end;

{ ------------------------------------------------------------------ }

procedure TDelphiParser.OpenReader(afile : TFileStream);
begin
   Assert(afile <> nil);
   FFileName := afile.FileName;
   reader.Free;
   reader := TFileReader.Create(afile);
end;

procedure TDelphiParser.CloseReader;
begin
   FFileName := '';
   reader.Free;
   reader := nil;
end;

procedure TDelphiParser.InitializeUnit(afile : TFileStream);
begin
   OpenReader(afile);

   fInImplementation := false;
   fInInterface := false;
   currentVisibility := visLocal;
   unitImplComment := '';
   interfaceUses := TStringList.Create;
   implUses := TStringList.Create;
   currentUses := interfaceUses;
   unitNameStr := '';

   StartBlock;
end;

procedure TDelphiParser.FreeObjectsUsedInParsingUnit;
begin
   FinishBlock;
   interfaceUses.Free;
   implUses.Free;
   interfaceUses := nil;
   implUses := nil;
   currentUses := nil;
end;

procedure TDelphiParser.ReadUnitName(token : String);
begin
   if token = '' then
      token := ReadToken;
   token := LowerCase(token);
   if (token = 'unit') or (token = 'program') or (token = 'library') then
   begin
      if token = 'library' then
         currentVisibility := visGlobal;

      unitNameStr := reader.ReadWord;
      if unitNameStr = '' then
         Error('Expected unit or program name.');

      { skip optional program params if present }
      token := ReadToken;
      if token = '(' then
      begin
         while token <> ')' do
            token := Readtoken;
         token := ReadToken;
      end;

      if token <> ';' then
         Error('Expected ;');
   end else
      Error('Syntax error: unit or program identifier expected.');
end;

procedure TDelphiParser.SkipToImplementation;
var
   token : String;
begin
   token := '';
   while (not reader.Eof) and (LowerCase(token) <> 'implementation') do
   begin
      token := ReadToken;
      ReadComments(token, token);
   end;
end;

function TDelphiParser.ParseNearestUses : TStrings;
var
   token2, token : String;
   iter : TStringForwardIterator;
begin
   Result := TStringList.Create;
   token := ReadToken;
   while not reader.Eof do
   begin
      ReadComments(token, token);
      if LowerCase(token) = 'uses' then
      begin
         repeat
            token2 := ReadToken;
            CheckValidName(token2);
            if token2 = '' then
               Error('Expected unit name');
            token := ReadToken;
            if LowerCase(token) = 'in' then
            begin
               token := ReadToken;
               token := system.Copy(token, 2, Length(token) - 2);
               Result.Add(Trim(token));
               token := ReadToken;
            end else
            begin
               token2 := LowerCase(token2);
               iter := PascalExtensions.Start;
               while not iter.IsFinish do
               begin
                  Result.Append(token2 + iter.Item);
                  iter.Advance;
               end;
               iter.Destroy;
            end;
         until token <> ',';
         if token <> ';' then
            Error('Expected ;');
         token := ReadToken;
         Exit;
      end else if (LowerCase(token) = 'implementation') then
      begin
         Exit;
      end else
         token := ReadToken;
   end;
end;

procedure TDelphiParser.ExecuteParsingLoop(token : String);
begin
   while not (reader.Eof or (token = 'end')) do
   begin
      recentComment := ReadComments(token, token);
      token := CallHandler(token, GlobalKeywords, true);
      if errUnknownIdentifier then
      begin
         Error('Expected keyword or identifier: ' + token);
         token := Readtoken;
      end;
      if token = '' then
         token := ReadToken;
   end;

   FinishBlock;
end;

function TDelphiParser.ReadVariableList(const token : String;
                                        ignoreSymbols : Boolean;
                                        list : TStrings) : String;
var
   token2 : String;
   savedSymbols : TStringSet;
begin
   if ignoreSymbols then
   begin
      savedSymbols := declaredSymbols;
      declaredSymbols := nil;
   end;

   token2 := token;

   { read in a parameter modifier, if any is present }
   if ParameterModifiers.Has(token2) then
   begin
      declText.AppendToken(token2);
      token2 := ReadToken;
   end;

   { read in the names }
   while true do
   begin
      CheckValidName(token2);
      list.Add(token2);
      declText.AppendToken(token2);

      token2 := ReadToken;
      if token2 <> ',' then
         break;
      declText.AppendToken(token2);

      token2 := ReadToken;
   end;

   if token2 = ':' then
   begin
      declText.AppendToken(token2);
      token2 := ReadToken;
      declText.AppendGlobalSymbol(token2);
      token2 := ReadToken;
   end;
   { read in everything till the ';' token (or the ')' token if we're
     reading a variable list of a routine }
   while (token2 <> ';') and (token2 <> ')') and (token2 <> ']') do
   begin
      declText.AppendToken(token2);
      token2 := ReadToken;
   end;

   Result := token2;

   if ignoreSymbols then
      declaredSymbols := savedSymbols;
end;

{ -------------------------------------------------------------------- }
{                        Parse- functions                              }

function TDelphiParser.ParseUses(const token : String) : String;
var
   token2 : String;
begin
   Result := '';
   repeat
      token2 := ReadToken;
      CheckValidName(token2);
      if token2 = '' then
         Error('Expected unit name');
      currentUses.Append(LowerCase(token2));
      token2 := ReadToken;
      if LowerCase(token2) = 'in' then
      begin
         ReadToken; { skip the name of the file }
         token2 := ReadToken;
      end;
   until token2 <> ',';
   if token2 <> ';' then
      Error('Expected ;');
end;

function TDelphiParser.ParseType(const token : String) : String;
var
   token2 : String;
   linenum : Integer;
begin
   token2 := ReadToken;
   repeat
      recentComment := ReadComments(token2, token2);
      token2 := CallHandler(token2, GlobalKeywords, true);
      if errUnknownIdentifier then
      begin
         recentSymbol := token2;
         CheckValidName(recentSymbol);
         Expect('=');

         with declText do
         begin
            AppendToken(recentSymbol);
            AppendToken('=');
         end;

         token2 := ReadToken;
         if LowerCase(token2) = 'packed' then
         begin
            token2 := ReadToken;
            declText.AppendToken('packed');
         end;

         token2 := CallHandler(token2, TypeSectionKeywords, false);
         { if the keyword hasn't been recognised then just read
           everythig till a ';' and consider it to be the whole type
           declaration }
         if errUnknownIdentifier then
         begin
            while token2 <> ';' do
            begin
               declText.AppendToken(token2);
               token2 := ReadToken;
            end;
            declText.AppendToken(';');

            { register the declaration }
            linenum := declText.LineNumber;
            Driver.RegisterDeclaration(declText.GetText, recentSymbol, nil,
                                       VisibilityRec[currentVisibility].FName,
                                       VisibilityRec[currentVisibility].FType,
                                       GetLangString(ttype_str), linenum,
                                       recentComment);
            token2 := ReadToken;
         end;

         if token2 = '' then
            token2 := ReadToken;
      end else
         break;
   until reader.Eof;
   Result := token2;
end;

function TDelphiParser.ParseConst(const token : String) : String;
var
   token2, symbol : String;
   linenum : Integer;
begin
   token2 := ReadToken;
   repeat
      recentComment := ReadComments(token2, token2);
      token2 := CallHandler(token2, GlobalKeywords, true);
      if errUnknownIdentifier then
      begin
         symbol := token2;
         CheckValidName(symbol);

         declText.AppendToken(symbol);

         token2 := ReadToken;
         while token2 <> ';' do
         begin
            declText.AppendToken(token2);
            { there may be some colons inside braces, so we have to
              skip them separately }
            if token2 = '(' then
            begin
               token2 := ReadToken;
               while token2 <> ')' do
               begin
                  declText.AppendToken(token2);
                  token2 := ReadToken;
               end;
               declText.AppendToken(')');
            end;
            token2 := ReadToken;
         end;
         declText.AppendToken(';');
         { we have to read the next token because otherwise ; would be
           interpreted as an empty statement by the handler above }
         token2 := ReadToken;

         linenum := declText.LineNumber;
         Driver.RegisterDeclaration(declText.GetText, symbol, nil,
                                    VisibilityRec[currentVisibility].FName,
                                    VisibilityRec[currentVisibility].FType,
                                    GetLangString(constant_str), linenum,
                                    recentComment);
      end;
   until reader.Eof or not errUnknownIdentifier;
   Result := token2;
end;

function TDelphiParser.ParseVar(const token : String) : String;
var
   token2 : String;
   symbols : TStringList;
   linenum : Integer;
begin
   token2 := ReadToken;
   repeat
      recentComment := ReadComments(token2, token2);
      token2 := CallHandler(token2, GlobalKeywords, true);
      if errUnknownIdentifier then
      begin
         symbols := TStringList.Create;
         declText.AppendToken(ReadVariableList(token2, false, symbols));
         linenum := declText.LineNumber;
         Driver.RegisterDeclarations(declText.GetText, symbols, nil,
                                     VisibilityRec[currentVisibility].FName,
                                     VisibilityRec[currentVisibility].FType,
                                     GetLangString(variable_str), linenum,
                                     recentComment);
         token2 := ReadToken;
      end;
   until reader.Eof or not errUnknownIdentifier;
   Result := token2;
end;

function TDelphiParser.ParseThreadVar(const token : String) : String;
var
   savedVisibility : TVisibility;
begin
   savedVisibility := currentVisibility;
   if currentVisibility = visLocal then
      currentVisibility := visLocalThreadLocal
   else if currentVisibility = visGlobal then
      currentVisibility := visGlobalThreadLocal
   else
      raise ESrcDocError.Create('Internal error');
   Result := ParseVar(token);
   currentVisibility := savedVisibility;
end;

function TDelphiParser.ParseLabel(const token : String) : String;
var
   token2 : String;
begin
   repeat
      token2 := ReadToken;
      recentComment := ReadComments(token2, token2);
      token2 := CallHandler(token2, GlobalKeywords, true);
   until not errUnknownIdentifier;
   Result := token2;
end;

function TDelphiParser.ParseRoutine(const token : String) : String;
var
   token2, symbol : String;
   arguments : TStringList;
   isForward, wasCodeBlock : Boolean;
   linenum : Integer;
begin
   arguments := TStringList.Create;

   declText.AppendToken(token);
   { don't get confused by a class member routine (a dot inside the
     name) }
   Include(reader.CharTable['.'], chAlpha);
   symbol := ReadToken;
   Exclude(reader.CharTable['.'], chAlpha);
   if LowerCase(token) <> 'operator' then
      CheckValidName(symbol);
   declText.AppendToken(symbol);

   token2 := ReadToken;
   if token2 = '(' then
   begin
      declText.AppendToken('(');

      token2 := ReadToken;
      if token2 <> ')' then
      begin
         while (token2 <> ')') do
         begin
            token2 := ReadVariableList(token2, true, arguments);
            declText.AppendToken(token2);
            if token2 = ';' then
               token2 := ReadToken;
         end;
      end else
         declText.AppendToken(token2);
      token2 := ReadToken;
   end;

   { read the optional function return }
   if token2 = ':' then
   begin
      declText.AppendToken(token2);
      token2 := ReadToken;
      declText.AppendGlobalSymbol(token2);
      token2 := ReadToken;
   end;

   { read whatever else might be there }
   while token2 <> ';' do
   begin
      declText.AppendToken(token2);
      token2 := Readtoken;
   end;
   declText.AppendToken(';');

   token2 := ReadToken;
   isForward := false;
   while RoutineModifiers.Has(token2) do
   begin
      if LowerCase(token2) = 'forward' then
         isForward := true;
      declText.AppendToken(token2);
      token2 := ReadToken;
      while token2 <> ';' do
      begin
         declText.AppendToken(token2);
         token2 := ReadToken;
      end;
      declText.AppendToken(';');
      token2 := ReadToken;
   end;

   if arguments.Count = 0 then
   begin
      arguments.Free;
      arguments := nil;
   end;

   if not (isForward or fInInterface) and BlockStarters.Has(token2) then
      { we have to skip the actual code of the routine }
   begin
      token2 := ParseCodeBlock(token2);
      wasCodeBlock := true;
   end else
      wasCodeBlock := false;

   if (Pos('.', symbol) = 0) and not
         (declaredSymbols.Has(symbol) and wasCodeBlock) then
      { don't duplicate class members or forward declarations }
   begin
      linenum := declText.LineNumber;
      Driver.RegisterDeclaration(declText.GetText, symbol, arguments,
                                 VisibilityRec[currentVisibility].FName,
                                 VisibilityRec[currentVisibility].FType,
                                 LowerCase(token), linenum, recentComment);
   end else
      arguments.Free;

   Result := token2;
end; { end ParseRoutine }

function TDelphiParser.ParseClassRoutine(const token : String) : String;
begin
   declText.AppendToken(token);
   Result := ParseRoutine(ReadToken);
end;

function TDelphiParser.ParseProperty(const token : String) : String;
var
   args : TStringList;
   token2, symbol : String;
   linenum : Integer;
begin
   declText.AppendToken(token);
   symbol := ReadToken;
   CheckValidName(symbol);
   declText.AppendToken(symbol);

   token2 := ReadToken;
   if token2 = '[' then
   begin
      args := TStringList.Create;
      declText.AppendToken('[');
      token2 := ReadToken;
      while token2 <> ']' do
      begin
         token2 := Readvariablelist(token2, true, args);
         declText.AppendToken(token2);
         if token2 = ';' then
            token2 := ReadToken;
      end;
      token2 := ReadToken;
   end else
      args := nil;

   { read the type }
   if token2 <> ':' then
      Error('Expected '':''');
   declText.AppendToken(token2);
   token2 := ReadToken;
   declText.AppendGlobalSymbol(token2);
   token2 := ReadToken;

   while token2 <> ';' do
   begin
      declText.AppendToken(token2);
      token2 := ReadToken;
   end;
   declText.AppendToken(';');

   token2 := ReadToken;
   if token2 = 'default' then
   begin
      declText.AppendToken('default');
      Expect(';');
      declText.AppendToken(';');
      token2 := '';
   end;

   linenum := declText.Linenumber;
   Driver.RegisterDeclaration(declText.GetText, symbol, args,
                              VisibilityRec[currentVisibility].FName,
                              VisibilityRec[currentVisibility].FType,
                              GetLangString(property_str), linenum, recentComment);

   Result := token2;
end; { end ParseProperty }

function TDelphiParser.ParseClass(const token : String) : String;
var
   token2 : String;
   ancestors, interfaces, symbols : TStringList;
   savedVisibility : TVisibility;
   linenum : Integer;
begin
   ancestors := TStringList.Create;
   interfaces := TStringList.Create;

   declText.AppendToken(token);

   token2 := ReadToken;
   if token2 = ';' then
      { it is a forward declaration - return }
   begin
      Result := '';
      ancestors.Free;
      interfaces.Free;
      Exit;
   end else if token2 = '(' then
      { read heritage and supported interfaces }
   begin
      declText.AppendToken('(');
      token2 := ReadToken;
      if token2 <> ')' then
      begin
         CheckValidName(token2);
         ancestors.Add(token2);
         declText.AppendToken(token2);

         token2 := ReadToken;
         while token2 <> ')' do
         begin
            if token2 <> ',' then
               Error('Expected ; or )');
            declText.AppendToken(',');

            token2 := ReadToken;
            CheckValidName(token2);
            interfaces.Add(token2);
            declText.AppendToken(token2);

            token2 := ReadToken;
         end; { end while token2 <> ')' }
      end; { end if token2 <> ')' }
      declText.AppendToken(token2);
      token2 := ReadToken;
   end; { end main if }

   linenum := declText.LineNumber;
   Driver.StartClass(declText.GetText, recentSymbol, ancestors, interfaces,
                     VisibilityRec[currentVisibility].FName,
                     VisibilityRec[currentVisibility].FType,
                     token, linenum, recentComment);

   savedVisibility := currentVisibility;
   currentVisibility := visPublic;

   StartBlock;

   while Lowercase(token2) <> 'end' do
   begin
      recentComment := ReadComments(token2, token2);
      token2 := CallHandler(token2, ClassSectionKeywords, true);
      if errUnknownIdentifier then
         { assume we are reading a field }
      begin
         symbols := TStringList.Create;
         declText.AppendToken(ReadVariableList(token2, false, symbols));

         linenum := declText.LineNumber;
         Driver.RegisterDeclarations(declText.GetText, symbols, nil,
                                     VisibilityRec[currentVisibility].FName,
                                     VisibilityRec[currentVisibility].FType,
                                     GetLangString(field_str), linenum,
                                     recentComment);

         token2 := ReadToken;
      end;

      if token2 = '' then
         token2 := ReadToken;
   end;
   Expect(';');

   FinishBlock;

   Driver.FinishClass;
   currentVisibility := savedVisibility;
   Result := '';

end; { end ParseClass }

function TDelphiParser.ParseInterface(const token : String) : String;
var
   ancestors : TStringList;
   token2, guid : String;
   c : Char;
   savedVisibility : TVisibility;
   linenum : Integer;
begin
   declText.AppendToken(token);

   token2 := ReadToken;
   if token2 = ';' then
      { this is only a forward declaration - return }
   begin
      Result := '';
      Exit;
   end;

   ancestors := TStringList.Create;
   if token2 = '(' then
   begin
      declText.AppendToken('(');
      token2 := ReadToken;
      if token2 <> ')' then
      begin
         CheckValidName(token2);
         declText.AppendToken(token2);
         ancestors.Add(token2);
         Expect(')');
      end;
      declText.AppendToken(')');
      token2 := ReadToken;
   end;

   with reader do
   begin
      if token2 = '[' then
      begin
         guid := '';
         c := ReadChar;
         while c <> ']' do
         begin
            guid := guid + c;
            c := Readchar;
         end;
         guid := guid + ']';
         token2 := REadToken;
      end else
         guid := '';
   end;

   linenum := declText.LineNumber;
   Driver.StartInterface(declText.GetText, recentSymbol, ancestors, guid,
                         VisibilityRec[currentVisibility].FName,
                         VisibilityRec[currentVisibility].FType,
                         GetLangString(interface_str), linenum, recentComment);

   savedVisibility := currentVisibility;
   currentVisibility := visPublic;

   StartBlock;

   while Lowercase(token2) <> 'end' do
   begin
      recentComment := ReadComments(token2, token2);
      token2 := CallHandler(token2, InterfaceSectionKeywords, true);
      if errUnknownIdentifier then
      begin
         Error('Unknown identifier: ' + token2);
         token2 := REadToken;
      end;
      if token2 = '' then
         token2 := REadToken;
   end;
   Expect(';');
   currentVisibility := savedVisibility;

   FinishBlock;

   Driver.FinishInterface;

   Result := '';
end; { end ParseInterface }

function TDelphiParser.ParseRecord(const token : String) : String;
var
   token2 : String;
   variables : TStrings;
   linenum : Integer;
begin
   declText.AppendToken(token);

   variables := TStringList.Create;
   ReadComments(token2, '');
   while LowerCase(token2) <> 'end' do
   begin
      declText.NewLine;
      declText.Indent;
      if LowerCase(token2) <> 'case' then
      begin
         declText.AppendToken(ReadVariableList(token2, true, variables));
         ReadComments(token2, '');
      end else
      begin
         while LowerCase(token2) <> 'end' do
         begin
            declText.AppendToken(token2);
            if token2 = ';' then
            begin
               declText.NewLine;
               declText.Indent;
            end;
            token2 := ReadToken;
         end;
      end;
   end;
   declText.NewLine;
   declText.AppendToken('end');
   declText.AppendToken(';');
   Expect(';');

   linenum := declText.LineNumber;
   Driver.RegisterDeclaration(declText.GetText, recentSymbol, variables,
                              VisibilityRec[currentVisibility].FName,
                              VisibilityRec[currentVisibility].FType,
                              GetLangString(record_str), linenum, recentComment);

   Result := '';
end;

function TDelphiParser.ParseProceduralType(const token : String) : String;
var
   token2 : String;
   linenum : Integer;
begin
   token2 := token;
   while token2 <> ';' do
   begin
      declText.AppendToken(token2);
      if token2 = '(' then
      begin
         token2 := ReadToken;
         while token2 <> ')' do
         begin
            declText.AppendToken(token2);
            token2 := ReadToken;
         end;
         declText.AppendToken(')');
      end;
      token2 := ReadToken;
   end;
   declText.AppendToken(';');

   { check for a calling convention modifier }
   token2 := ReadToken;
   if CallModifiers.Has(token2) then
   begin
      declText.AppendToken(token2);
      declText.AppendToken(';');
      Expect(';');
      Result := '';
   end else
      Result := token2;

   linenum := declText.LineNumber;
   Driver.RegisterDeclaration(declText.GetText, recentSymbol, nil,
                              VisibilityRec[currentVisibility].FName,
                              VisibilityRec[currentVisibility].FType,
                              GetLangString(ttype_str), linenum, recentComment);
end;

function TDelphiParser.ParseVisibility(const token : String) : String;
var
   vis : String;
begin
   vis := LowerCase(token);
   if vis = 'public' then
      currentVisibility := visPublic
   else if vis = 'private' then
      currentVisibility := visPrivate
   else if vis = 'protected' then
      currentVisibility := visProtected
   else
      raise ESrcDocError.Create('Internal error');
   Result := '';
end;

function TDelphiParser.ParseCodeBlock(const token : String) : String;
var
   token2 : String;
   nestCount : Integer;
   metBegin : Boolean;
begin
   if LowerCase(token) = 'begin' then
   begin
      metBegin := true;
      nestCount := 1
   end else
   begin
      { we will meet the 'begin' keyword }
      metBegin := false;
      nestCount := 0;
   end;

   while not ((nestCount = 0) and metBegin) do
   begin
      REadComments(token2, '');
      token2 := LowerCase(token2);
      if token2 = 'end' then
         Dec(nestCount)
      else if token2 = 'begin' then
      begin
         Inc(nestCount);
         metBegin := true;
      end else if EndBlockStarters.Has(token2) then
         Inc(nestCount)
      else if (token2 = 'function') or (token2 = 'procedure') then
         { a nested routine }
      begin
         while not BlockStarters.Has(token2) do
         begin
            token2 := ReadToken;
            if token2 = '(' then
               { var and const parameters inside parentheses would be
                 confused with block starters }
            begin
               while token2 <> ')' do
                  token2 := ReadToken;
            end;
         end;
         ParseCodeBlock(token2);
      end;
   end;
   Result := '';
end;

function TDelphiParser.ParseEmptyStatement(const token : String) : String;
begin
   Result := '';
end;

function TDelphiParser.ParseBegin(const token : String) : String;
begin
   ParseCodeBlock(token);
   Result := 'end';
end;

function TDelphiParser.ParseEnd(const token : String) : String;
begin
   Result := 'end';
end;

{ ------------------------------------------------------------------ }

procedure TDelphiParser.ParseFile(afile : TFileStream);
var
   str, token : String;
begin
   Assert(afile <> nil);

   InitializeUnit(afile);

   str := ReadComments(token, '');
   ReadUnitName(token);
   unitcomment := ReadComments(token, '');
   if unitcomment = '' then
      unitcomment := str;

   if not Driver.RegisterUnit(unitNameStr, unitcomment) then
   begin
      FreeObjectsUsedInParsingUnit;
      Exit;
   end;

   token := LowerCase(token);

   if token = 'interface' then
   begin
      fInInterface := true;
      currentVisibility := visGlobal;
      token := LowerCase(ReadToken);
   end; { else we are reading a program file }

   ExecuteParsingLoop(token);

   try
      Driver.ProvideUnitInfo(interfaceUses);
   finally
      interfaceUses := nil;
      implUses.Free;
      implUses := nil;
      currentUses := nil;
      CloseReader;
   end;
end;

procedure TDelphiParser.ParseImplementation(afile : TFileStream);
var
   token : String;
begin
   Assert(afile <> nil);
   Assert((optShowImplementationInfo in Driver.Options));
   try
      InitializeUnit(afile);
      currentUses := implUses;
      currentVisibility := visLocal;

      ReadComments(token, '');
      ReadUnitName(token);

      SkipToImplementation;
      if (not reader.Eof) and Driver.OpenUnit(unitNameStr) then
      begin
         unitImplComment := ReadComments(token, '');

         ExecuteParsingLoop(token);
      end else
      begin
         FreeObjectsUsedInParsingUnit;
         Exit;
      end;

   except
      FreeObjectsUsedInParsingUnit;
      raise;
   end;

   try
      Driver.ProvideUnitImplementationInfo(implUses, unitImplComment);
   finally
      interfaceUses.Free;
      interfaceUses := nil;
      implUses := nil;
      currentUses := nil;
      CloseReader;
   end;
end;

function TDelphiParser.GetDependencies(afile : TFileStream) : TStrings;
begin
   OpenReader(afile);
   Result := ParseNearestUses;
   { note: we have to close the file first for systems
     where a file cannot be referenced by more than one
     file handle }
   CloseReader;
end;

function TDelphiParser.GetImplementationDependencies(afile : TFileStream) : TStrings;
begin
   OpenReader(afile);
   SkipToImplementation;
   Result := ParseNearestUses;
   CloseReader;
end;

function TDelphiParser.ParseCode(code : String) : TTextObject;
var
   savedReader : TStreamReader;
   lastLine : Cardinal;
   indents, i : Integer;
   token : String;
begin
   declText.Reset;
   savedReader := reader;
   reader := TStringReader.Create(code);
   indents := 0;
   with reader do
   begin
      FetchLine;
      lastLine := LineNumber;
      while not reader.Eof do
      begin
         token := ReadToken;
         declText.AppendToken(token);
         if BlockStarters.Has(token) or (LowerCase(token) = 'repeat') then
            Inc(indents)
         else if (LowerCase(token) = 'end') or (LowerCase(token) = 'until') then
            Dec(indents);
         if LineNumber > lastLine then
         begin
            declText.NewLine;
            lastLine := LineNumber;
            for i := 1 to indents do
               declText.Indent;
         end;
         SkipWhiteSpace; { in order for reader.Eof to indicate end of file
                           even if there are some whitespace }
      end;
   end; { end with reader do }
   reader.Free;
   reader := savedReader;
   Result := declText.GetTextList;
end;

function TDelphiParser.ParseIdentifierRef(ref : String) : String;
var
   i : Integer;
begin
   Result := '';
   for i := 1 to Length(ref) do
   begin
      if ref[i] = '.' then
         Result := Result + docwSectionSeparator
      else
         Result := Result + ref[i];
   end;
end;

function TDelphiParser.ScopeResolutionToken : String;
begin
   Result := '.';
end;

function TDelphiParser.IgnoreCase : Boolean;
begin
   Result := true;
end;

initialization
   PascalExtensions := TStringSet.Create();
   PascalExtensions.ItemComparer := TNoCaseStringComparer.Create;

   with PascalExtensions do
   begin
      Insert('.pas');
      Insert('.pp');
      Insert('.p');
   end;

finalization
   PascalExtensions.Free;

end.
