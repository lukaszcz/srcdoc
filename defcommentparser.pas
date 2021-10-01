(*  This file is a part of the makedoc program for creating
    documentation of Delphi/Pascal programs from comments in source
    files.

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

unit defcommentparser;

{ this unit provides a standard comment parser }

interface

uses
   SysUtils, Classes, commonclasses, input, adtcont, adtmap;

const
   MAX_ARGS = 256;
   declInfinite = $FFFF;
   maxRepeatedCharCount = 7;

type
   TCommentParagraph = class;

   TDefaultCommentParser = class (TCommentParser)
   private
      reader : TStringReader;
      { the current paragraph }
      currPara : TCommentParagraph;
      { currently allowed profiles }
      currProfiles : TStrings;
      { a list of the paragraphs of the comment currently being
        parsed; this object should be maintained by the ParseComment
        function and it is not destroyed in the destructor }
      paras : TListAdt;
      { the current synopsis }
      synopsis : TParagraph;
      { the current title }
      currTitle : String;
      { indicates whether to fetch comments from related methods or
        not }
      currFetchRelated : Boolean;
      { the current parameters }
      Parameters : TParagraph;
      { true if the description paragraph has been encountered }
      encounteredDescription : Boolean;
      { the number of declarations to associate with the comment
        (the default is 1) }
      includeDecl : Cardinal;
      { whether to discard commands or not }
      fDiscardCommands : Boolean;
      { holds the recently read arguments }
      args : array[1..MAX_ARGS] of String;
      { indicates whether a new sentence has just been begun (no word
        from this sentence has yet been read) }
      isSentenceStart : Boolean;
      { a set of unrecognized commands; every time an unknown command
        is encountered not present in this set, a warning is issue and
        the command name is added to UnrecognizedCommands; this is in
        order to give a warning only once for each unrecognized
        command - they are probably just section headings }
      UnrecognizedCommands : TStringSet;
      { the map associating command names with functions that handle
        them }
      CommandHandlers : TMap;

      { reads arguments for a command from input and puts them into
        the <params> array; num is the number of arguments to read if
        "(" is not encountered; returns the number of arguments read
        }
      function ReadArgs(num : Integer) : Integer;
      { ends the current paragraph and begins a new one }
      procedure FinishCurrentParagraph;
      { calls an appropriate command corresponding to <command> }
      procedure CallCommandHandler(command : String);

      { methods handling commands; they all return ''; they are
        implemented as functions only to make it possible to pass them
        to THandler }
      function HandleCode(const command : String) : String;
      function HandleDiscardCommands(const command : String) : String;
      function HandleDiscardComments(const command : String) : String;
      function HandleEmphasis(const command : String) : String;
      function HandleHeading(const command : String) : String;
      function HandleIgnoreDecl(const command : String) : String;
      function HandleIncludeDecl(const command : String) : String;
      function HandleKw(const command : String) : String;
      function HandleLink(const command : String) : String;
      function HandleLinkFile(const command : String) : String;
      function HandleNl(const command : String) : String;
      function HandleNoDescription(const command : String) : String;
      function HandlePara(const command : String) : String;
      function HandleParam(const command : String) : String;
      function HandlePre(const command : String) : String;
      function HandleProfile(const command : String) : String;
      function HandleSee(const command : String) : String;
      function HandleSynopsis(const command : String) : String;
      function HandleTitle(const command : String) : String;
      function HandleUntilNextComment(const command : String) : String;

   public
      constructor Create;
      destructor Destroy; override;
      { parses a comment and writes it out }
      function ParseComment(comment : String) : TComment; override;
   end;

   { an object of this class should be created once and later reused }
   TCommentParagraph = class
   private
      FParser : TDefaultCommentParser;
      { the heading of the paragraph }
      FHeading : THeading;
      { the most recent formatted text; should be added to <list> }
      FText : String;
      { the list of TTextObject's of the paragraph which is currently
        being built }
      FList : TListAdt;
      { current attributes of the text }
      FAttr : TTextAttributeSet;

      procedure FinishTextObject;

   public
      constructor Create(parser : TDefaultCommentParser);
      destructor Destroy; override;
      { clears the paragraph }
      procedure Reset;
      { sets the text attributes }
      procedure SetAttributes(attr : TTextAttributeSet);
      { adds attr to the text attributes }
      procedure AddAttributes(attr : TTextAttributeSet);
      { removes attr from the text attributes }
      procedure RemoveAttributes(attr : TTextAttributeSet);
      { adds a piece of text to the paragraph using the current text
        attributes }
      procedure AddText(const str : String);
      { the same as above, but uses attr for text attributes; does not
        change the text attributes permanently }
      procedure AddText(const str : String; attr : TTextAttributeSet);
      { adds a link to <destination> with the text <description> }
      procedure AddLink(const description, destination : String);
      { adds a specified text object to the paragraph }
      procedure AddTextObject(obj : TTextObject);
      { removes at most num chars from the back of the paragraph text;
        works only if the recent text object was not finished after
        those characters were added }
      procedure RemoveChars(num : Integer);
      { inserts a line break }
      procedure NewLine;
      { sets the heading of the paragraph }
      procedure SetHeading(const heading : String; size : TSize);
      { removes the heading of the paragraph if any exists }
      procedure RemoveHeading;
      { returns true if the paragraph does not contain any text (the
        heading is _not_ taken into account) }
      function IsEmpty : Boolean;
      { returns the text of the heading or '' if no heading is specified }
      function Heading : String;
      { returns the TParagraph object composed from the paragraph
        represented by self; resets self }
      function GetParagraph : TParagraph;
      { returns the TParagraph object composed from the paragraph
        represented by self, but does not reset self; does not modify
        self in any way }
      function PeekParagraph : TParagraph;
   end;

implementation

uses
   adtlist, adthash, adthashfunct, adtfunct, adtiters, adtstralgs, languages;

{ ----------------------- TCommentParagraph ------------------------------- }

constructor TCommentParagraph.Create(parser : TDefaultCommentParser);
begin
   FParser := parser;
   FList := TSingleList.Create;
   Reset;
end;

destructor TCommentParagraph.Destroy;
begin
   FList.Free;
   FHeading.Free;
   inherited;
end;

procedure TCommentParagraph.FinishTextObject;
begin
   Assert(Assigned(FList));
   if FText <> '' then
   begin
      FList.PushBack(TText.Create(FText, FAttr));
      FText := '';
   end;
end;

procedure TCommentParagraph.Reset;
begin
   FText := '';
   FAttr := [];
   FList.Clear;
   FHeading.Free;
   FHeading := nil;
end;

procedure TCommentParagraph.SetAttributes(attr : TTextAttributeSet);
begin
   if attr <> FAttr then
      FinishTextObject;
   FAttr := attr;
end;

procedure TCommentParagraph.AddAttributes(attr : TTextAttributeSet);
begin
   if attr - FAttr <> [] then
      FinishTextObject;
   FAttr := FAttr + attr;
end;

procedure TCommentParagraph.RemoveAttributes(attr : TTextAttributeSet);
begin
   if attr * FAttr <> [] then
      FinishTextObject;
   FAttr := FAttr - attr;
end;

procedure TCommentParagraph.AddText(const str : String);
begin
   FText := FText + str;
end;

procedure TCommentParagraph.AddText(const str : String; attr : TTextAttributeSet);
var
   pattr : TTextAttributeSet;
begin
   pattr := FAttr;
   FAttr := attr;
   if attr <> pattr then
      FinishTextObject;
   FText := FText + str;
   if attr <> pattr then
      FinishTextObject;
   FAttr := pattr;
end;

procedure TCommentParagraph.AddLink(const description, destination : String);
begin
   FinishTextObject;
   FList.PushBack(TLink.Create(description, destination));
end;

procedure TCommentParagraph.AddTextObject(obj : TTextObject);
begin
   FinishTextObject;
   FList.PushBack(obj);
end;

procedure TCommentParagraph.RemoveChars(num : Integer);
begin
   if num > Length(FText) then
      num := Length(FText);
   SetLength(FText, Length(FText) - num);
end;

procedure TCommentParagraph.NewLine;
begin
   FinishTextObject;
   FList.PushBack(TNewLine.Create);
end;

procedure TCommentParagraph.SetHeading(const heading : String; size : TSize);
begin
   FHeading.Free;
   FHeading := THeading.Create(heading, size);
end;

procedure TCommentParagraph.RemoveHeading;
begin
   FHeading.Free;
   FHeading := nil;
end;

function TCommentParagraph.IsEmpty : Boolean;
begin
   Assert(Assigned(FList));
   Result := FList.Empty and (Trim(FText) = '');
end;

function TCommentParagraph.Heading : String;
begin
   if FHeading <> nil then
      Result := FHeading.HeadingText
   else
      Result := '';
end;

function TCommentParagraph.GetParagraph : TParagraph;
begin
   FText := TrimRight(FText);
   if (FText <> '') and (optBeautifyComments in Driver.Options) then
   begin
      if (DefaultCharTable[FText[Length(FText)]] * [chAlpha, chDigit] <> []) or
            (FText[Length(FText)] = ')') then
      begin
         FText := FText + '.';
      end;
   end;
   FinishTextObject;
   if Assigned(FHeading) then
   begin
      FList.PushFront(FHeading);
      FHeading := nil;
   end;
   Result := TParagraph.Create(FList);
   FList := TSingleList.Create;
   Reset;
   FParser.isSentenceStart := true;
end;

function TCommentParagraph.PeekParagraph : TParagraph;
var
   lst : TListAdt;
begin
   FinishTextObject;
   lst := TListAdt(FList.CopySelf(Adapt(@CopyTextObject)));
   Result := TParagraph.Create(lst);
end;


{ ---------------------- TDefaultCommentParser ---------------------------- }

constructor TDefaultCommentParser.Create;
begin
   inherited;
   UnrecognizedCommands := TStringSet.Create;
   CommandHandlers := TMap.Create(THashTable.Create);

   CommandHandlers['code'] :=
      THandler.Create(@HandleCode);
   CommandHandlers['c'] :=
      THandler.Create(@HandleCode);
   CommandHandlers['discard-subsequent-commands'] :=
      THandler.Create(@HandleDiscardCommands);
   CommandHandlers['discard-commands'] :=
      THandler.Create(@HandleDiscardCommands);
   CommandHandlers['discard-recent-comment'] :=
      THandler.Create(@HandleDiscardComments);
   CommandHandlers['discard-recent-comments'] :=
      THandler.Create(@HandleDiscardComments);
   CommandHandlers['discard-comment'] :=
      THandler.Create(@HandleDiscardComments);
   CommandHandlers['e'] :=
      THandler.Create(@HandleEmphasis);
   CommandHandlers['emp'] :=
      THandler.Create(@HandleEmphasis);
   CommandHandlers['emphasis'] :=
      THandler.Create(@HandleEmphasis);
   CommandHandlers['h'] :=
      THandler.Create(@HandleHeading);
   CommandHandlers['heading'] :=
      THandler.Create(@HandleHeading);
   CommandHandlers['ignore-declarations'] :=
      THandler.Create(@HandleIgnoreDecl);
   CommandHandlers['include-declarations'] :=
      THandler.Create(@HandleIncludeDecl);
   CommandHandlers['kw'] :=
      THandler.Create(@HandleKw);
   CommandHandlers['keyword'] :=
      THandler.Create(@HandleKw);
   CommandHandlers['link'] :=
      THandler.Create(@HandleLink);
   CommandHandlers['linkfile'] :=
      THandler.Create(@HandleLinkFile);
   CommandHandlers['lf'] :=
      THandler.Create(@HandleLinkFile);
   CommandHandlers['nl'] :=
      THandler.Create(@HandleNl);
   CommandHandlers['newline'] :=
      THandler.Create(@HandleNl);
   CommandHandlers['no-description'] :=
      THandler.Create(@HandleNoDescription);
   CommandHandlers['para'] :=
      THandler.Create(@HandlePara);
   CommandHandlers['param'] :=
      THandler.Create(@HandleParam);
   CommandHandlers['pre'] :=
      THandler.Create(@HandlePre);
   CommandHandlers['profile'] :=
      THandler.Create(@HandleProfile);
   CommandHandlers['profiles'] :=
      THandler.Create(@HandleProfile);
   CommandHandlers['see'] :=
      THandler.Create(@HandleSee);
   CommandHandlers['synopsis'] :=
      THandler.Create(@HandleSynopsis);
   CommandHandlers['title'] :=
      THandler.Create(@HandleTitle);
   CommandHandlers['until-next-comment'] :=
      THandler.Create(@HandleUntilNextComment);

   currPara := TCommentParagraph.Create(self);
end; { end TDefaultCommentParser.Create }

destructor TDefaultCommentParser.Destroy;
begin
   currPara.Free;
   CommandHandlers.Free;
   UnrecognizedCommands.Free;
   inherited;
end;

function TDefaultCommentParser.ReadArgs(num : Integer) : Integer;
begin
   Result := 0;
   with reader do
   begin
      if PeekChar = '(' then
      begin
         ReadChar;
         repeat
            SkipWhiteSpace;
            Inc(Result);
            if Result > MAX_ARGS then
            begin
               Driver.CommentError('Too many arguments. The maximum is ' +
                                      IntToStr(MAX_ARGS) + '.');
               Dec(Result);
               Exit;
            end;
            args[Result] := ReadUntil([')', ',']);
         until ReadChar = ')';
      end else
      begin
         while (Result < num) and not Eof do
         begin
            SkipWhiteSpace;
            Inc(Result);
            if Result > MAX_ARGS then
            begin
               Driver.CommentError('Too many arguments. The maximum is ' +
                                      IntToStr(MAX_ARGS) + '.');
               Dec(Result);
               Exit;
            end;
            args[Result] := ReadUntilType([chSpace]);
         end;
      end; { end not PeekChar = '(' }
   end; { end with reader }
end;

procedure TDefaultCommentParser.FinishCurrentParagraph;
var
   p : TParagraph;
   iter : TForwardIterator;
begin
   if not currPara.IsEmpty then
   begin
      if currPara.Heading = GetLangString(Synopsis_str) then
      begin
         currPara.RemoveHeading;
         synopsis.Free;
         synopsis := currPara.GetParagraph;
      end else if currPara.Heading = GetLangString(Parameters_str) then
      begin
         currPara.RemoveHeading;
         p := currPara.GetParagraph;
         if not Parameters.TextObjectList.Empty then
            Parameters.TextObjectList.PushBack(TNewLine.Create);
         iter := p.TextObjectList.ForwardStart;
         while not iter.IsFinish do
         begin
            Parameters.TextObjectList.PushBack(iter.Extract);
         end;
         iter.Destroy;
         p.Destroy;
      end else
      begin
         if currPara.Heading = GetLangString(Description_str) then
            encounteredDescription := true;
         paras.PushBack(currPara.GetParagraph);
      end;
   end;
   currPara.Reset;
end;

procedure TDefaultCommentParser.CallCommandHandler(command : String);
var
   handler : THandler;
begin
   Assert(length(command) >= 1);

   handler := THandler(CommandHandlers.Find(LowerCase(command)));

   if handler <> nil then
   begin
      handler.Invoke(command);
   end else
   begin
      { give a warning - this might be a misspelled command - but give
        a warning for each command name only once }
      if not UnrecognizedCommands.Has(command) then
      begin
         Driver.CommentWarn('Unknown comment command ''@' + command +
                               ''' - assuming a section heading.');
         UnrecognizedCommands.Insert(command);
      end;
      { begin a new paragraph with the string <command> as a heading;
        replace all '-' and '_' in <command> with ' ' }
      FinishCurrentParagraph;
      command[1] := UpCase(command[1]);
      command := ReplaceAll(command, ['-', '_'], ' ');
      currPara.SetHeading(command, subHeadingSize);
   end;
end;

{ ==================================================================== }
{ --------------------- command handlers ----------------------------- }

function TDefaultCommentParser.HandleCode(const command : String) : String;
var
   code, word : String;
   c : Char;
begin
   Result := '';
   with reader do
   begin
      code := '';
      while true do
      begin
         code := code + ReadUntil(['@'], [chEol]);
         c := ReadChar;
         if c = '@' then
         begin
            word := ReadWord;
            if (LowerCase(word) = 'ec') or (LowerCase(word) = 'end-code') then
               break
            else
               code := code + '@' + word;
         end else
            code := code + #13#10 + c;
      end;
      currPara.AddTextObject(Driver.LanguageParser.ParseCode(code));
   end;
end;

function TDefaultCommentParser.HandleDiscardCommands(const command : String) :
   String;
begin
   Result := '';
   fDiscardCommands := true;
end;

function TDefaultCommentParser.HandleDiscardComments(const command : String) :
   String;
begin
   Result := '';
   currPara.Free;
   paras.Clear;
   currPara := TCommentParagraph.Create(self);
end;

function TDefaultCommentParser.HandleEmphasis(const command : String) : String;
begin
   Result := '';
   if ReadArgs(1) <> 1 then
      Driver.CommentError('Expected one argument')
   else begin
      currPara.AddAttributes([saEmphasis]);
      currPara.AddText(args[1]);
      currPara.RemoveAttributes([saEmphasis]);
   end;
end;

function TDefaultCommentParser.HandleHeading(const command : String) : String;
var
   size, num : Integer;
begin
   Result := '';
   num := ReadArgs(2);
   if num = 1 then
      size := subHeadingSize
   else if num = 2 then
   begin
      try
         size := StrToInt(args[2]);
      except
         on EConvertError do
         begin
            Driver.CommentError('Expected a number between 1 and 6');
            Exit;
         end;
      end;
   end else
   begin
      Driver.CommentError('Expected two arguments');
      Exit;
   end;
   if (size >= 1) and (size <= 6) then
      currPara.AddTextObject(THeading.Create(args[1], TSize(size)))
   else
      Driver.CommentError('Expected a number between 1 and 6');
end;

function TDefaultCommentParser.HandleIgnoreDecl(const command : String) : String;
begin
   Result := '';
   try
      Driver.SetIgnoreDeclarations(StrToInt(reader.ReadWord));
   except
      on EConvertError do
         Driver.CommentError('Expected a number');
   end;
end;

function TDefaultCommentParser.HandleIncludeDecl(const command : String) : String;
var
   i : Integer;
begin
   Result := '';
   try
      i := StrToInt(reader.ReadWord);
      if i >= 0 then begin
         IncludeDecl := i;
      end else begin
         IncludeDecl := declInfinite;
      end
   except
      on EConvertError do
         Driver.CommentError('Expected a number');
   end;
end;

function TDefaultCommentParser.HandleKw(const command : String) : String;
begin
   Result := '';
   if ReadArgs(1) <> 1 then
      Driver.CommentError('Expected one argument.')
   else
      currPara.AddText(args[1], [saKeyword]);
end;

function TDefaultCommentParser.HandleLink(const command : String) : String;
var
   num : Integer;
begin
   Result := '';
   num := ReadArgs(1);
   if (num = 1) or (num = 2) then
   begin
      currPara.AddLink(args[num], Driver.LanguageParser.
                                     ParseIdentifierRef(args[1]));
   end else
      Driver.CommentError('Expected one or two arguments.');
end;

function TDefaultCommentParser.HandleLinkFile(const command : String) : String;
var
   num : Integer;
begin
   Result := '';
   num := REadArgs(1);
   if (num = 1) or (num = 2) then
   begin
      Driver.RegisterFile(args[1]);
      currPara.AddLink(args[num], args[1]);
   end else
      Driver.CommentError('Expected one or two arguments.');
end;

function TDefaultCommentParser.HandleNl(const command : String) : String;
begin
   Result := '';
   currPara.NewLine;
end;

function TDefaultCommentParser.HandleNoDescription(const command : String) : String;
begin
   Result := '';
   if currPara.Heading = GetLangString(Description_str) then
      currPara.RemoveHeading;
   { inhibits generating a descripion para }
   encounteredDescription := true;
end;

function TDefaultCommentParser.HandlePara(const command : String) : String;
var
   num : Integer;
begin
   Result := '';
   num := ReadArgs(0);

   FinishCurrentParagraph;

   if num = 1 then
   begin
      currPara.SetHeading(args[1], subHeadingSize);
   end else if num <> 0 then
      Driver.CommentError('Expected 0 or 1 argument.');
end;

function TDefaultCommentParser.HandleParam(const command : String) : String;
var
   paramName : String;
begin
   Result := '';
   with reader, currPara do
   begin
      FinishCurrentParagraph;
      SetHeading(GetLangString(Parameters_str), subHeadingSize);
      paramName := ReadWord;
      AddAttributes([saLocalSymbol]);
      AddText(paramName);
      RemoveAttributes([saLocalSymbol]);
      AddText(' - ');
      SkipWhiteSpace;
      if PeekChar = '-' then
         ReadChar;
      isSentenceStart := false;
      { note: all paragraph with the heading 'Parameters' are picked
        out by FinishCurrentParagraph and added to <Parameters> }
   end;
end;

function TDefaultCommentParser.HandlePre(const command : String) : String;
var
   cmd : String;
begin
   Result := '';
   currPara.AddAttributes([saPreformatted]);
   with reader do
   begin
      while not Eof do
      begin
         currPara.AddText(ReadUntil(['@']));
         ReadChar;
         if chAlpha in CharTable[PeekChar] then
         begin
            cmd := REadWord;
            if cmd = 'endpre' then
               break
            else
               currPara.AddText('@' + cmd);
         end;
      end; { end while true }
   end; { end with reader }
   currPara.RemoveAttributes([saPreformatted]);
end; { end HandlePre }

function TDefaultCommentParser.HandleProfile(const command : String) : String;
var
   i, num : Integer;
begin
   Result := '';
   num := ReadArgs(1);
   if num > 0 then
   begin
      if currProfiles = nil then
         currProfiles := TStringList.Create;
      for i := 1 to num do
         currProfiles.Add(args[i]);
   end else
      Driver.CommentError('Expected a list of profile names.');
end;

function TDefaultCommentParser.HandleSee(const command : String) : String;
var
   lnk : String;
   c : Char;
begin
   Result := '';

   FinishCurrentParagraph;

   currPara.SetHeading(GetLangString(See_also_str), subHeadingSize);

   with reader do
   begin
      while not Eof do
      begin
         SkipWhiteSpace;
         lnk := ReadUntil(['@', ',', ';'], [chSpace]);
         lnk := Trim(lnk);
         if lnk <> '' then
         begin
            if lnk[Length(lnk)] = '.' then
               lnk := system.Copy(lnk, 1, Length(lnk) - 1);
            currPara.AddLink(lnk, Driver.LanguageParser.ParseIdentifierRef(lnk));
         end;
         c := PeekChar;
         if c = '@' then
            break
         else if c in [',', ';'] then
         begin
            ReadChar;
            currPara.AddText(', ');
         end;
      end; { end while not Eof }
   end; { end with reader }
end;

function TDefaultCommentParser.HandleSynopsis(const command : String) : String;
begin
   Result := '';
   synopsis.Free; { discard any earlier synopsis }
   synopsis := nil;
   FinishCurrentParagraph;
   currPara.SetHeading(GetLangString(Synopsis_str), subHeadingSize);
end;

function TDefaultCommentParser.HandleTitle(const command : String) : String;
begin
   Result := '';
   if ReadArgs(1) <> 1 then
      Driver.CommentError('Expected one argument')
   else
      currTitle := args[1];
end;

function TDefaultCommentParser.HandleUntilNextComment(const command : String) :
   String;
begin
   Result := '';
   IncludeDecl := declInfinite;
end;


{ ------------------------- ParseComment ------------------------------ }

function TDefaultCommentParser.ParseComment(comment : String) : TComment;
var
   iter : TForwardIterator;
   word, word2, text, prevWord : String;
   c, c2, lastChar : Char;
   repeatedCharCount, i : Integer;
   sentenceEnd : set of Char;
   wordEnd : set of Char;
   { these variables are true if the most recently read text was 'e.'
     or 'i.' respectively; used to handle some common abbreviations
     specially when beautifying comments }
   wasE, wasI : Boolean;

   { in both of the below functions it is assumed that cc is the
     character most recently read from input }
   function IsSentenceEnd(cc : Char) : Boolean;
   begin
      Result := (cc in sentenceEnd) and
         (chSpace in reader.CharTable[reader.PeekChar]);
   end;

   function IsWordEnd(cc : Char) : Boolean;
   begin
      Result := (chSpace in reader.CharTable[cc]) or
         ((cc in WordEnd) and (chSpace in reader.CharTable[reader.PeekChar]));
   end;

begin
   wasE := false;
   wasI := false;
   sentenceEnd := ['.', ';', '!', '?'];
   wordEnd := sentenceEnd + [',', ':', '''', '"', '(', ')'];
   comment := Trim(comment);
   if comment = '' then
   begin
      { ignore the comment if it is '' }
      Result := nil;
      Exit;
   end else if not (comment[Length(comment) - 1] in sentenceEnd) then
   begin
      comment := comment + '.';
   end;

   { should be initially set to some alpha-numeric char }
   lastChar := 'a';
   repeatedCharCount := 0;

   reader := TStringReader.Create(comment);
   { make the reader treat '-' as a letter; '-' appears in several
     command names }
   Include(reader.CharTable['-'], chAlpha);

   { reset the variables }

   isSentenceStart := true;
   encounteredDescription := false;
   { if the @synopsis command does not begin the comment then the
     synopsis is created from the first sentence of the comment if
     optMakeSynopsis is on }
   synopsis := nil;
   includedecl := 1;
   currTitle := '';
   currFetchRelated := false;

   Parameters := TParagraph.Create(TSingleList.Create);
   paras := TSingleList.Create;

   currPara.Reset;
   currPara.SetHeading(GetLangString(Description_str), subHeadingSize);

   word := '';
   with reader do
   begin
      while not Eof do
      begin
         if chSpace in CharTable[PeekChar] then
         begin
            currPara.AddText(' ');
            { for handling abbreviations: i.e., e.g. and ex. }
            wasE := false;
            wasI := false;
         end;

         prevWord := LowerCase(word);
         word := ReadWord;

         if word <> '' then
         begin
            if optIgnoreHandMadeSeparators in Driver.Options then
            begin
               { since '-' is treated as a letter separators composed
                 of '-' must be handled separately (see the code below
                 in the else block) }
               i := 1;
               while (i <= Length(word)) and (word[i] = '-') and
                        (i <= maxRepeatedCharCount)  do
               begin
                  Inc(i);
               end;
               if i > maxRepeatedCharCount then
               begin
                  if optDiscardParagraphOnHandMadeSeparators in Driver.Options then
                     currPara.Reset;
                  isSentenceStart := true;
                  continue;
               end;
            end;

            if (optEmphasis in Driver.Options) and (Length(word) > 2) and
                  (word[1] = '_') and (word[Length(word)] = '_') then
            begin
               word2 := Copy(word, 2, Length(word) - 2);
               if (Pos('_', word2) = 0) then
               begin
                  currPara.AddAttributes([saEmphasis]);
                  currPara.AddText(word2);
                  currPara.RemoveAttributes([saEmphasis]);
                  continue;
               end;
            end;

            if isSentenceStart and (optBeautifyComments in Driver.Options) then
            begin
               word[1] := UpCase(word[1]);
               isSentenceStart := false;
            end;
            currPara.AddText(word);

            lastChar := 'a'; { should be any alpha-numeric char }
            repeatedCharCount := 0;

         end else { not word <> '' }
         begin
            c := ReadChar;

            if optIgnoreHandMadeSeparators in Driver.Options then
            begin
               { ignore things like: ----------------- some heading --------- }
               if (lastChar = c) then
               begin
                  Inc(repeatedCharCount);
                  if repeatedCharCount > maxRepeatedCharCount then
                  begin
                     currPara.RemoveChars(repeatedCharCount - 1);
                     while c = lastChar do
                        c := ReadChar;
                     isSentenceStart := true;
                  end;
               end else
                  repeatedCharCount := 1;
               lastChar := c;
            end;

            if optBeautifyComments in Driver.Options then
            begin
               if IsSentenceEnd(c) then
               begin
                  { avoid empty sentences }
                  if isSentenceStart then
                     continue;

                  { handle some common abbreviations }
                  if c = '.' then
                  begin
                     if prevWord = 'e' then
                     begin
                        if wasI then
                        begin
                           { recognized 'i.e.' - avoid normal end of
                             sentence parsing }
                           currPara.AddText('.');
                           continue;
                        end;
                     end else if prevWord = 'g' then
                     begin
                        if wasE then
                        begin
                           { recognized 'e.g.' - avoid normal end of
                             sentence parsing }
                           currPara.AddText('.');
                           continue;
                        end;
                     end else if prevWord = 'ex' then
                     begin
                        { recognized 'ex.' - avoid normal end of
                          sentence parsing }
                        currPara.AddText('.');
                        continue;
                     end;
                  end;

                  if c = ';' then
                     c := '.';
                  isSentenceStart := true;

               end else if c = '.' then
               begin
                  if prevWord = 'e' then
                  begin
                     wasE := true;
                     wasI := false;
                  end else if prevWord = 'i' then
                  begin
                     wasE := false;
                     wasI := true;
                  end;
               end else
               begin
                  wasE := false;
                  wasI := false;
               end;
            end; { end if should beautify comments }

            if (synopsis = nil) and (optMakeSynopsis in Driver.Options) and
                  IsSentenceEnd(c) then
            begin
               currPara.AddText(c);
               synopsis := currPara.PeekParagraph;
            end else if c = '@' then
               { possibly a command }
            begin
               c2 := PeekChar;
               if c2 = '<' then
                  { a simple link }
               begin
                  ReadChar;
                  text := '';
                  c := ReadChar;
                  while c <> '>' do
                  begin
                     text := text + c;
                     c := ReadChar;
                  end;
                  currPara.AddLink(text, Driver.LanguageParser.
                                            ParseIdentifierRef(text));

               end else if c2 = '@' then
               begin
                  ReadChar;
                  currPara.AddText('@');
               end else if chAlpha in CharTable[c2] then
                  { a command or a new block }
               begin
                  word := ReadWord;
                  CallCommandHandler(word);
               end else
               begin
                  currPara.AddText(c);
               end;

            end else if ((c = '<') and
                         (optExplicitLocalSymbols in Driver.Options) and
                         (chAlpha in CharTable[PeekChar]))  then
            begin
               { make the text between < and > an identifier; if it is
                 a word and another > follows immediately after it }
               word := ReadWord;
               if PeekChar = '>' then
               begin
                  currPara.SetAttributes([saLocalSymbol]);
                  currPara.AddText(word);
                  currPara.RemoveAttributes([saLocalSymbol]);
                  ReadChar;
               end else
               begin
                  currPara.AddText(c);
                  currPara.AddText(word);
               end;
            end else if c = #0 then
            begin
               { may be eof here if the buffer ends with whitespace; #0
                 indicates eof }
               break;
            end else
            begin
               currPara.AddText(c);
            end; { end not multiple if testing the value of c }
         end; { end not if word <> '' }
      end; { end while not Eof }
   end; { end with reader do }

   FinishCurrentParagraph;

   { if no @description command was encountered then assume the first
     paragraph without a heading to be the description }
   if not encounteredDescription then
   begin
      iter := paras.ForwardStart;
      while (not iter.IsFinish) and
               (TTextObject(TParagraph(iter.Item).TextObjectList.Front) is
                   THeading) do
      begin
         iter.Advance;
         Assert(iter.IsFinish or
                   ((TObject(iter.Item) is TParagraph) and
                       not TParagraph(iter.Item).TextObjectList.Empty));
      end;
      if not iter.IsFinish then
      begin
         TParagraph(iter.Item).TextObjectList.PushFront(
            THeading.Create(GetLangString(Description_str), subHeadingSize));
      end;
   end;

   if not Parameters.TextObjectList.Empty then
   begin
      Parameters.TextObjectList.PushFront(THeading.Create(
                                             GetLangString(Parameters_str),
                                             subHeadingSize)
                                         );
      paras.PushFront(Parameters);
   end else
      Parameters.Free;

   if paras.Empty then
      includedecl := 0;

   Result := TComment.Create(paras, synopsis, currTitle,
                             currProfiles, includedecl, currFetchRelated);

   includeDecl := 0;
   currProfiles := nil;
   synopsis := nil;
   Parameters := nil;

   reader.Destroy;
end;

end.
