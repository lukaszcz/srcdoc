(*  This file is a part of the srcdoc program for creating the
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

unit commonclasses;

{ basic classes used by all units of srcdoc }

interface

uses
   SysUtils, Classes, adtdarray, adtcont, adthash, adtarray;

type
   TIntegerArray = array of Integer;
   TStringSet = TStringHashTable;
   TStack = TArray;

   { global options }
   TOption = (optDependencies, optShowPrivate, optShowProtected,
              optShowImplementationInfo, optJoinComments,
              optNewParagraphBeforeJoinedComment,
              optMakeSynopsis, optBeautifyComments, optTableBorder,
              optExplicitLocalSymbols, optEmphasis, optIgnoreHandMadeSeparators,
              optDiscardParagraphOnHandMadeSeparators, optLinkPlurals,
              optVerbose, optShowWarnings, optFetchCommentsFromAncestors,
              optGenerateClassTree, optGenerateInterfaceTree,
              optGenerateSymbolIndex, optGenerateContents);
   TOptions = set of TOption;

   { TVisibilityType indicates a general visibility type regardless of
     the actual visibility string used to describe the visibility; in
     this way, for instance, both 'global' and 'public' are qualified
     as vtPublic }
   TVisibilityType = (vtPublic=1, vtProtected=2, vtPrivate=3);

   TTextAttribute = (saKeyword, saLocalSymbol, saGlobalSymbol,
                     saPreformatted, saCode, saEmphasis);
   TTextAttributeSet = set of TTextAttribute;

   TSize = 1..6;

   { ---------------------- Wrappers ------------------------------ }
   THandlerFunction = function(const command : String) : String of object;

   THandler = class
   private
      FProc : THandlerFunction;
   public
      constructor Create(proc : THandlerFunction);
      function Invoke(const command : String) : String;
   end;

   { ---------------------- Text objects ----------------------------- }

   TSection = class;

   { represents an object that can be written out }
   TTextObject = class
   public
      { writes self out inside <section>; <section> must be opened }
      procedure WriteOut(section : TSection); virtual; abstract;
      function CopySelf : TTextObject; virtual; abstract;
   end;

   TTextObjectList = class (TTextObject)
   private
      FList : TListAdt;
   public
      { @synopsis <alist> is a list of text objects }
      constructor Create(alist : TListAdt);
      destructor Destroy; override;
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
   end;

   { represents a consecutive sequence of characters all with the same
     attributes }
   TText = class (TTextObject)
   private
      text : String;
      attributes : TTextAttributeSet;
   public
      constructor Create(atext : String; attr : TTextAttributeSet);
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
   end;

   { inserts a newline }
   TNewLine = class (TTextObject)
   public
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
   end;

   TIndent = class (TTextObject)
   public
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
   end;

   TLink = class (TTextObject)
   private
      {@discard the characters '@' in the following comment are
       doubled for srcdoc to work correctly }
      { the format of the destination should be as follows:
        section1@@section2@@ ... @@sectionN, where section(i+1) is
        contained in section(i) and sectionN is the section to which
        the link is supposed to point; if a section name contains the
        character @@ it should be prefixed by another @@ }
      description, destination : String;
   public
      constructor Create(descr, dest : String);
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
   end;

   THeading = class (TTextObject)
   private
      text : String;
      size : 1..6;
   public
      constructor Create(atext : String; asize : TSize);
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
      property HeadingText : String read text;
   end;

   TParagraph = class (TTextObject)
   private
      { a list of TTextObjects that constitute the paragraph in the
        given order }
      objects : TListAdt;
   public
      { the argument is owned by the object and destroyed together
        with it }
      constructor Create(objs : TListAdt);
      destructor Destroy; override;
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
      property TextObjectList : TListAdt read objects;
   end;

   TTable = class (TTextObject)
   private
      { a list of lists of TTextObject's }
      rows : TListAdt;
      widths : TIntegerArray;
      cols : Integer;
   public
      { the argument is owned by the object and destroyed together
        with it }
      constructor Create(arows : TListAdt; awidths : TIntegerArray;
                         acols : Integer);
      destructor Destroy; override;
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
   end;

   { represents a whole parsed, formatted comment }
   TComment = class (TTextObject)
   private
      { a list of TParagraphs }
      paras : TListAdt;
      { a synopsis is a short (at most one sentence) summary of the
        whole comment; non-nil if the comment has a synopsis; the
        synopsis paragraph should not include a heading }
      FSynopsis : TParagraph;
      FInclDecl : Cardinal;
      FTitle : String;
      FProfiles : TStrings;
      FFetchRelated : Boolean;
   public
      { the argument is owned by the object and destroyed together
        with it; aparas is a list of TParagraph's, asynopsis is the
        sysopsis for the comment or nil if there's no synopsis }
      constructor Create(aparas : TListAdt; asynopsis : TParagraph;
                         atitle : String; aprofiles : TStrings;
                         inclDecl : Cardinal; afetchRelated : Boolean);
      destructor Destroy; override;
      procedure WriteOut(section : TSection); override;
      function CopySelf : TTextObject; override;
      { returns the paragraph constituting the synopsis of the comment
        or nil if the comment does not have a synopsis; a synopsis is
        a short (at most one sentence) summary of the whole comment;
        non-nil if the comment has a synopsis; }
      property Synopsis : TParagraph read FSynopsis;
      property Title : String read FTitle;
      { if nil then all profiles are allowed }
      property Profiles : TStrings read FProfiles;
      { the maximum number of following declarations that should be
        associated with this comment, provided that no other comment
        is found before these declarations }
      property IncludeDeclarations : Cardinal read FInclDecl;
      { indicates whether to prepend a comment of a related method to
        self }
      property FetchRelated : Boolean read FFetchRelated;
   end;


   { ---------------------- abstract class interfaces ---------------------- }

   { Represents an output section. Each section should correspond to a
     single syntatic unit (a module, class, routine declaration,
     ...). }
   TSection = class
   private
      FName : String;

   public
      constructor Create(aname : String);

      { opens the section for writing; only _one_ from _all_ sections
        may be opened at a time; the methods writing to the output may
        be invoked only after calling Open and before calling Close;
        <title> is the section title; it may be taken into account or
        completely disregarded }
      procedure Open(title : String); virtual; abstract;
      { the same as above, but passes '' as the title }
      procedure Open;
      { closes the section; the section may not be written to any
        more, nor may it be re-opened earlier }
      procedure Close; virtual; abstract;

      { ------------------- methods writing to output ----------------------- }
      { writes out <text>; <attr> are the attributes of the text;
        <highlight> is true if the text should be highlighted }
      procedure WriteText(text : String;
                          attr : TTextAttributeSet); virtual; abstract;
      { the same as above but assumes attr = [] }
      procedure WriteText(const text : String);
      { writes out a link to a section; if <section> is nil writes
        <descr> as a plain text; uses <descr> as a label for the link
        }
      procedure WriteLink(descr : String; section : TSection); virtual; abstract;
      { writes a heading; size may be a number 1 through 6, where 1 is
        the largest, 6 the smallest; a heading begins a new paragraph }
      procedure WriteHeading(const text : String;
                             size : TSize); virtual; abstract;
      { begins a new paragraph }
      procedure NewParagraph; virtual; abstract;
      { begins a new line }
      procedure NewLine; virtual; abstract;
      { makes an indentation; for one line only; this should be called
        just after calling NewLine }
      procedure Indent; virtual; abstract;
      { starts writing a table; cols is the number of columns; widths
        should contain cols entries with the width of each column (in
        the percentage of the width of the whole table); after calling
        this routine the next output will be written in the first cell
        of the table }
      procedure StartTable(const widths : TIntegerArray;
                           cols : Integer); virtual; abstract;
      { moves to the next cell in the current row in the table being
        currently writed out; if there is no next cell in the current
        row then does nothing }
      procedure NextCell; virtual; abstract;
      { starts a new row }
      procedure NextRow; virtual; abstract;
      { finishes writing a table; if the last row contains no data
        then it is discarded }
      procedure FinishTable; virtual; abstract;
      { starts writing a list; the list is numbered if <numbered> is
        true }
      procedure StartList(numbered : Boolean); virtual; abstract;
      { starts writing the next item in the current list }
      procedure NextListItem; virtual; abstract;
      { finished writing a list;  }
      procedure FinishList; virtual; abstract;
      { suppresses automatic line breaking (not for all formats) }
      procedure SuppressLineBreaking; virtual; abstract;
      { resumes automatic line breaking (not for all formats) }
      procedure ResumeLineBreaking; virtual; abstract;
      property Name : String read FName;
   end;

   { an abstract interface of a documentation writer; it only returns
     the right type of a section, and the section does the main job }
   TDocWriter = class
   private
      FOutputDir : String;
   protected
      property OutputDirectory : String read FOutputDir;
   public
      { creates a writer }
      constructor Create(aOutputDir : String);
      { registeres a section named name; <parent> is the section which
        contains the registered section; should be nil if no such
        section exists; if the section is expected to contain many
        subsections then <big> should be true; such a section is
        typically written to a separate file; the section returned by
        this function should be freed be the caller }
      function RegisterSection(name : String; parent : TSection;
                               big : Boolean) : TSection; virtual; abstract;
      { registeres a special section for the table of contents; the
        returned section should be destroyed by the caller }
      function RegisterContentsSection : TSection; virtual; abstract;
      { registeres a special section for the tree of classes; the
        returned section should be destroyed by the caller }
      function RegisterClassTreeSection : TSection; virtual; abstract;
      { registeres a special section for the tree of interfaces; the
        returned section should be destroyed by the caller }
      function RegisterInterfaceTreeSection : TSection; virtual; abstract;
      { registeres a special section for the index of symbols; the
        returned section should be destroyed by the caller }
      function RegisterSymbolIndexSection : TSection; virtual; abstract;
   end;

   { Parses input files. Parsing comments is done by TCommentParser,
     except for the comment-start commands, which are recognized and
     parsed by TLanguageParser.  }
   TLanguageParser = class
   public
      { parses a file; }
      procedure ParseFile(afile : TFileStream); virtual; abstract;
      { parses an implementation of a given file if the file has a
        separate implementation; if the interface and the
        implementation reside both in the same file (like in
        pascal/Delphi) then ParseFile should parse the interface only
        and ParseImplementation the implementation part; If the
        implementation is in a separate file (like in C/C++) then this
        should register this file to be parsed via
        Driver.RegisterFile; it should not parse the file in this
        case, since the file may have already been parsed and it would
        be parsed twice, but instead ParseFile should parse whole
        files regardless of whether they are an interface or an
        implementation; }
      procedure ParseImplementation(afile : TFileStream); virtual; abstract;
      { returns a list of all files on which the unit or program in
        <afile> depends; if the implementation and the interface
        reside in the same file it should retrun a list of the
        interface dependencies; the result should be destroyed by the
        caller }
      function GetDependencies(afile : TFileStream) : TStrings; virtual; abstract;
      { returns a list of files on which the implementation of the
        module in <afile> depends; if the implementation is _not_ in
        the same file as the interface, then this method should return
        an empty list and GetDependencies should return _all_
        dependencies for a given file, regardless of whether the file
        is an interface or an implementation file; the result should
        be destroyed by the caller; }
      function GetImplementationDependencies(afile : TFileStream) : TStrings;
      virtual; abstract;
      { converts a qualified reference to an identifier into a
        reference to a section within TDocWriter }
      function ParseIdentifierRef(ref : String) : String; virtual; abstract;
      { parses a piece of code converting it to a TTextObject }
      function ParseCode(code : String) : TTextObject; virtual; abstract;
      { returns the token used for scope resolution }
      function ScopeResolutionToken : String; virtual; abstract;
      { returns true if the case of identifiers is insignificant }
      function IgnoreCase : Boolean; virtual; abstract;
   end;

   { Parses comments. The only comments it does not parse are those
     beginning with a @. They are handled by TLanguageParser. }
   TCommentParser = class
   public
      { parses a comment; may invoke some Driver's methods if
        encounters commands that require it; may return nil if the
        passed string is empty; }
      function ParseComment(comment : String) : TComment; virtual; abstract;
   end;

   { an abstract interface of a main program 'driver'; it performs all
     the basic high-level operations, parses comments, controls the
     generation of the document and provides the 'glue' to make
     everything work together; drivers for specific languages should
     inherit from this class }
   TDriver = class
   private
      FOptions : TOptions;
      FSrcDirs : TStrings;
      FDocWriter : TDocWriter;
      FCommentParser : TCommentParser;
      FLanguageParser : TLanguageParser;

   protected
      { sets FCommentParser; destroyes the object previously stored there }
      procedure SetCommentParser(arg : TCommentParser); virtual;
      { sets FLanguageParser; destroyes the object previously stored there }
      procedure SetLanguageParser(arg : TLanguageParser); virtual;

   public
      { @param aoptions - the global options }
      { @param adocwriter - the documentation writer to use }
      { @param asrcDirs - a list of directories to search for input
        files }
      constructor Create(aoptions : TOptions; adocwriter : TDocWriter;
                         asrcDirs : TStrings);
      destructor Destroy; override;

      { runs the driver; <afilequeue> is a queue of files to be
        processed; the queue is owned by the driver and disposed by
        it; it should contain files in the order in which they should
        be processed; The Run method should be called only once from
        the main program; }
      procedure Run(afilequeue : TStringDequeAdt); virtual; abstract;
      { registeres the given file to be processed }
      procedure RegisterFile(filename : String); virtual; abstract;
      { registeres the unit currently being parsed; this should be
        called as soon as the name of the unit is known and before
        anything else is registered; returns true if the language
        parser should continue with parsing the unit, false if it
        should exit immediately }
      { @param name - the name of the unit }
      { @param unitComment - the comment associated with the unit }
      function RegisterUnit(name : String;
                            unitComment : String) : Boolean; virtual; abstract;
      { re-opens an already registered unit for parsing; after calling
        this method all declarations are added to the opened unit;
        returns true if the language parser should continue with
        parsing the unit, false if it should exit immediately }
      function OpenUnit(name : String) : Boolean; virtual; abstract;
      { provides additional information on the unit currently being
        parsed; }
      { @param ainterfaceUses - the units the current unit uses in its
        interface }
      procedure ProvideUnitInfo(ainterfaceUses : TStrings); virtual; abstract;
      { provides additional information on the implementation of the
        unit currently being parsed; }
      { @param aimplUses - the units the current unit uses in its
        implementation }
      { @param aunitImplComment - the comment associated with the
        implementation of the unit }
      procedure ProvideUnitImplementationInfo(aimplUses : TStrings;
                                              aunitImplComment : String);
      virtual; abstract;
      { registeres a declaration description; decl is the whole text
        of the declaration; it is the responsibility of the language
        parser to format this text and highlight keywords; symbol is
        the name of the symbol being declared; args is a list of
        arguments if the symbol is a routine, may be empty; visibility
        is the visibility of the symbol; symbolType is the type of the
        symbol, i.e. variable, function, procedure, etc; linenum is
        the line number at which the declaration appears in the source
        file; comment is the comment associated with the declaration;
        the declaration string may be later formatted inside the
        routine, as well as the comment string }
      procedure RegisterDeclaration(decl : TTextObject; symbol : String;
                                    args : TStrings; visibility : String;
                                    visibilityType : TVisibilityType;
                                    symboltype : String; linenum : Integer;
                                    comment : String);
      { the same as above, but for multiple symbols in one declaration }
      procedure RegisterDeclarations(adecl : TTextObject; asymbols : TStrings;
                                     args : TStrings; avisibility : String;
                                     avisibilityType : TVisibilityType;
                                     asymboltype : String; alinenum : Integer;
                                     acomment : String); virtual; abstract;
      { starts registering a class declaration; decl is the full text
        of the class'es heading (not necessarily the whole
        declaration, but only the part including the class'es name and
        inheritance info); ancestors is a list of the immediate
        ancestors of the class; interfaces is a list of the interfaced
        implemented by the class; <linenum> is the line number at
        which the declaration appears in the source file; comment is
        the comment associated with the class; }
      procedure StartClass(decl : TTextObject; name : String;
                           ancestors : TStrings; interfaces : TStrings;
                           visibility : String; visibilityType : TVisibilityType;
                           symboltype : String; linenum : Integer;
                           comment : String); virtual; abstract;
      procedure FinishClass; virtual; abstract;
      { starts registering an interface; id is an optional id of the
        interface; in delphi it is the GUID of the interface; @see
        StartClass }
      procedure StartInterface(decl : TTextObject; name : String;
                               ancestors : TStrings; id : String;
                               visibility : String;
                               visibilityType : TVisibilityType;
                               symboltype : String; linenum : Integer;
                               comment : String); virtual; abstract;
      procedure FinishInterface; virtual; abstract;

      { indicated that <num> following decalrations should be ignored }
      procedure SetIgnoreDeclarations(num : Cardinal); virtual; abstract;
      { returns the section of the given declaration name or nil if
        not found }
      function FindSection(name : String) : TSection; virtual; abstract;

      { prints an error message <msg>; if there have been too many
        errors raises an exception }
      procedure Error(msg, filename : String;
                      linenum : Cardinal); virtual; abstract;
      { prints an error message <msg> not associated with any
        particular line, but associated with some file; if there have
        been too many errors raises an exception }
      procedure Error(msg, filename : String); virtual; abstract;
      { prints an error message <msg> not associated with any file; if
        there have been too many errors raises an exception }
      procedure Error(msg : String); virtual; abstract;
      { prints an error message <msg>; use this method if the error is
        encountered inside a comment; @see Error; }
      procedure CommentError(msg : String); virtual; abstract;
      { prints a warning }
      procedure Warn(msg, filename : String; line : Integer); virtual; abstract;
      procedure Warn(msg : String); virtual; abstract;
      { prints a warning from inside a comment }
      procedure CommentWarn(msg : String); virtual; abstract;
      { writes a message; messages are written only if <optVerbose> is
        in @<Options> }
      procedure WriteMessage(msg : String); virtual; abstract;

      { opens a file for reading and returns a file stream for this
        file; returns nil if could not open }
      function OpenInputFile(const path : String) : TFileStream;
      { returns true if a given file exists in one of the source
        directories }
      function ExistsInputFile(const path : String) : Boolean;
      { validates the input file; if it is a path to a file then adds
        the directory part to the source directories and returns the
        basename of the file }
      function ValidateInputFile(const path : String) : String;
      { adds <opt> to <Options> }
      procedure SetOption(opt : TOption);
      { removes <opt> from <Options> }
      procedure ResetOption(opt : TOption);

      property Options : TOptions read FOptions;
      property DocWriter : TDocWriter read FDocWriter;
      property CommentParser : TCommentParser read FCommentParser;
      property LanguageParser : TLanguageParser read FLanguageParser;
   end;

   ESrcdocError = class (Exception)
   public
      constructor Create(msg : String);
   end;

   ENoPreviousSection = class (ESrcdocError)
   end;

   { extracts the basename of a file, without the path or extension }
   function ExtractBaseName(const path : String) : String;

   { writes out a list of TTextObject's }
   procedure WriteTextObjects(textlist : TListAdt);
   { interprets ptr as a TTextObject and copies it }
   function CopyTextObject(obj : TObject) : TObject;

const
   unitHeadingSize = 1;
   bigHeadingSize = 2;
   subHeadingSize = 3;
   smallHeadingSize = 4;
   srcdocVersion = 'SrcDoc 0.1';
   srcdocCopyright = 'Copyright (C) 2005 by Lukasz Czajka';
   docwSectionSeparator = '@#@';
{$ifdef WINDOWS }
   directorySeparator = '\';
{$else }
   directorySeparator = '/';
{$endif }

var
   { this global variable should be set only by the main program and
     only read by other classes; this object is automatically
     destroyed in the finalization of this unit }
   Driver : TDriver;

implementation

uses
   adtfunct, adtalgs;

{ note: if ever trying to make this work in a multi-threaded
  environment change this var to threadvar }
var
   currSection : TSection;

function ExtractBaseName(const path : String) : String;
var
   i : Integer;
begin
   Result := ExtractFileName(path);
   i := Length(Result);
   while (i > 0) and (Result[i] <> '.') do
      Dec(i);
   if i > 1 then
      Result := system.Copy(Result, 1, i - 1);
end;

function CopyTextObject(obj : TObject) : TObject;
begin
   Assert(obj <> nil);
   Assert(obj is TTextObject);
   Result := TTextObject(obj).CopySelf;
end;

procedure WriteTextObject(obj : TObject);
begin
   Assert(obj <> nil);
   Assert(obj is TTextObject);
   TTextObject(obj).WriteOut(currSection);
end;

{ ------------------------------------------------------ }

procedure WriteTableCell(obj : TObject);
begin
   Assert(obj <> nil);
   Assert(obj is TTextObject);
   TTextObject(obj).WriteOut(currSection);
   currSection.NextCell;
end;

procedure WriteTableRow(obj : TObject);
var
   list : TListAdt;
begin
   Assert(obj <> nil);
   Assert(obj is TListAdt);
   list := TListAdt(obj);
   ForEach(list.ForwardStart, list.ForwardFinish, Adapt(@WriteTableCell));
   currSection.NextRow;
end;

procedure WriteTextObjects(textlist : TListAdt);
begin
   Assert(textlist <> nil);
   ForEach(textlist.ForwardStart, textlist.ForwardFinish,
           Adapt(@WriteTextObject));
end;

{ this should be called on every directory name }
procedure ValidateDirectoryName(var dir : String);
begin
   Assert(dir <> '');
   if dir[Length(dir)] <> directorySeparator then
      dir := dir + directorySeparator;
end;


{ ---------------------- wrappers -------------------------- }

constructor THandler.Create(proc : THandlerFunction);
begin
   Assert(proc <> nil);
   FProc := proc;
end;

function THandler.Invoke(const command : String) : String;
begin
   Assert(FProc <> nil);
   Result := FProc(command);
end;

{ --------------------- text objects ---------------------------- }

constructor TTextObjectList.Create(alist : TListAdt);
begin
   Assert(alist <> nil);
   FList := alist;
end;

destructor TTextObjectList.Destroy;
begin
   FList.Free;
end;

procedure TTextObjectList.WriteOut(section : TSection);
begin
   Assert(section <> nil);
   currSection := section;
   WriteTextObjects(FList);
end;

function TTextObjectList.CopySelf : TTextObject;
begin
   Assert(FList <> nil);
   Result := TTextObjectList.Create(TListAdt(FList.CopySelf(
                                                Adapt(@CopyTextObject)
                                                           )));
end;

constructor TText.Create(atext : String; attr : TTextAttributeSet);
begin
   text := atext;
   attributes := attr;
end;

procedure TText.WriteOut(section : TSection);
begin
   section.WriteText(text, attributes);
end;

function TText.CopySelf : TTextObject;
begin
   Result := TText.Create(text, attributes);
end;

procedure TNewLine.WriteOut(section : TSection);
begin
   section.NewLine;
end;

function TNewLine.CopySelf : TTextObject;
begin
   Result := TNewLine.Create;
end;

procedure TIndent.WriteOut(section : TSection);
begin
   section.Indent;
end;

function TIndent.CopySelf : TTextObject;
begin
   Result := TIndent.Create;
end;

constructor TLink.Create(descr, dest : String);
begin
   description := descr;
   destination := dest;
end;

procedure TLink.WriteOut(section : TSection);
begin
   section.WriteLink(description, Driver.FindSection(destination));
end;

function TLink.CopySelf : TTextObject;
begin
   Result := TLink.Create(description, destination);
end;

constructor THeading.Create(atext : String; asize : TSize);
begin
   text := atext;
   size := asize;
end;

procedure THeading.WriteOut(section : TSection);
begin
   section.WriteHeading(text, size);
end;

function THeading.CopySelf : TTextObject;
begin
   Result := THeading.Create(text, size);
end;

constructor TParagraph.Create(objs : TListAdt);
begin
   inherited Create;
   Assert(objs <> nil);
   objects := objs;
end;

destructor TParagraph.Destroy;
begin
   objects.Free;
   inherited;
end;

procedure TParagraph.WriteOut(section : TSection);
begin
   section.NewParagraph;
   currSection := section;
   ForEach(objects.ForwardStart, objects.ForwardFinish,
           Adapt(@WriteTextObject));
end;

function TParagraph.CopySelf : TTextObject;
var
   newObjs : TListAdt;
begin
   newObjs := TListAdt(objects.CopySelf(Adapt(@CopyTextObject)));
   Result := TParagraph.Create(newObjs);
end;

constructor TTable.Create(arows : TListAdt; awidths : TIntegerArray;
                          acols : Integer);
begin
   inherited Create;
   rows := arows;
   widths := awidths;
   cols := acols;
end;

destructor TTable.Destroy;
begin
   rows.Free;
   inherited;
end;

procedure TTable.WriteOut(section : TSection);
begin
   currSection := section;
   section.StartTable(widths, cols);
   ForEach(rows.ForwardStart, rows.ForwardFinish, Adapt(@WriteTableRow));
   section.FinishTable;
end;

function TTable.CopySelf : TTextObject;
begin
   Result := TTable.Create(TListAdt(rows.CopySelf(Adapt(@CopyTextObject))),
                           widths, cols);
end;

constructor TComment.Create(aparas : TListAdt; asynopsis : TParagraph;
                            atitle : String; aprofiles : TStrings;
                            inclDecl : Cardinal; afetchRelated : Boolean);
var
   i : Integer;
begin
   inherited Create;
   paras := aparas;
   FSynopsis := asynopsis;
   FInclDecl := inclDecl;
   FTitle := atitle;
   FProfiles := aprofiles;
   FFetchRelated := afetchRelated;
end;

destructor TComment.Destroy;
begin
   paras.Free;
   FSynopsis.Free;
   FProfiles.Free;
   inherited;
end;

procedure TComment.WriteOut(section : TSection);
begin
   currSection := section;
   ForEach(paras.ForwardStart, paras.ForwardFinish, Adapt(@WriteTextObject));
end;

function TComment.CopySelf : TTextObject;
var
   newSynopsis : TParagraph;
   profilesCopy : TStrings;
   i : Integer;
begin
   if FSynopsis <> nil then
      newSynopsis := TParagraph(FSynopsis.CopySelf)
   else
      newSynopsis := nil;
   if FProfiles <> nil then
   begin
      profilesCopy := TStringList.Create;
      for i := 0 to FProfiles.Count - 1 do
         profilesCopy.Add(FProfiles[i]);
   end else
      profilesCopy := nil;
   Result := TComment.Create(TListAdt(paras.CopySelf(Adapt(@CopyTextObject))),
                             newSynopsis, Title, profilesCopy, FInclDecl,
                             FFetchRelated);
end;

{ ----------------------- TSection ----------------------- }

constructor TSection.Create(aname : String);
begin
   FName := aname;
end;

procedure TSection.Open;
begin
   Open('');
end;

procedure TSection.WriteText(const text : String);
begin
   WriteText(text, []);
end;

{ --------------------- TDocWriter --------------------------- }

constructor TDocWriter.Create(aOutputDir : String);
begin
   inherited Create;
   FOutputDir := aOutputDir;
   ValidateDirectoryName(FOutputDir);
end;

{ ----------------------- TDriver --------------------------------- }

constructor TDriver.Create(aoptions : TOptions; adocwriter : TDocWriter;
                           asrcDirs : TStrings);
var
   i : Integer;
   str : String;
begin
   FOptions := aoptions;
   FSrcDirs := asrcDirs;
   FDocWriter := adocwriter;
   for i := 0 to FSrcDirs.Count - 1 do
   begin
      str := FSrcDirs[i];
      ValidateDirectoryName(str);
      FSrcDirs[i] := str;
   end;
end;

destructor TDriver.Destroy;
begin
   FSrcDirs.Free;
   FDocWriter.Free;
   FCommentParser.Free;
   FLanguageParser.Free;
end;

procedure TDriver.SetCommentParser(arg : TCommentParser);
begin
   FCommentParser.Free;
   FCommentParser := arg;
end;

procedure TDriver.SetLanguageParser(arg : TLanguageParser);
begin
   FLanguageParser.Free;
   FLanguageParser := arg;
end;

procedure TDriver.RegisterDeclaration(decl : TTextObject; symbol : String;
                                      args : TStrings; visibility : String;
                                      visibilityType : TVisibilityType;
                                      symboltype : String; linenum : Integer;
                                      comment : String);
var
   symbols : TStringList;
begin
   symbols := TStringList.Create;
   symbols.Add(symbol);
   RegisterDeclarations(decl, symbols, args, visibility, visibilityType,
                        symboltype, linenum, comment);
end;

function TDriver.OpenInputFile(const path : String) : TFileStream;
var
   i : Integer;
   fname : String;
begin
   try
      fname := ValidateInputFile(path);
      if FileExists(fname) then
         Result := TFileStream.Create(fname, fmOpenRead)
      else begin
         { try all directories in SrcDirs }
         for i := 0 to FSrcDirs.Count - 1 do
         begin
            if FileExists(FSrcDirs[i] + fname) then
            begin
               Result := TFileStream.Create(FSrcDirs[i] + fname, fmOpenRead);
               Exit;
            end;
         end;
         Result := nil;
      end;
   except
      on EFOpenError do
         Result := nil;
   end;
end;

function TDriver.ExistsInputFile(const path : String) : Boolean;
var
   i : Integer;
   fname : String;
begin
   fname := ValidateInputFile(path);
   if FileExists(fname) then
      Result := true
   else begin
      { try all directories in SrcDirs }
      for i := 0 to FSrcDirs.Count - 1 do
      begin
         if FileExists(FSrcDirs[i] + fname) then
         begin
            Result := true;
            Exit;
         end;
      end;
      Result := false;
   end;
end;

function TDriver.ValidateInputFile(const path : String) : String;
var
   dir : String;
   dirPresent : Boolean;
   i : Integer;
begin
   dir := ExtractFilePath(ExpandFileName(path));
   Result := ExtractFileName(path);
   dirPresent := false;
   for i := 0 to FSrcDirs.Count - 1 do
   begin
      if FSrcDirs[i] = dir then
      begin
         dirPresent := true;
         break;
      end;
   end;
   if not dirPresent then
      FSrcDirs.Add(dir);
end;

procedure TDriver.SetOption(opt : TOption);
begin
   FOptions := FOptions + [opt];
end;

procedure TDriver.ResetOption(opt : TOption);
begin
   FOptions := FOptions - [opt];
end;

{ -------------------------- ESrcdocError -------------------------- }

constructor ESrcdocError.Create(msg : String);
begin
   inherited Create('srcdoc: Fatal error: ' + msg);
end;

initialization

   Driver := nil;

finalization

   Driver.Free;
   Driver := nil;

end.
