(*  This file is a part of the srcdoc program for creating
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

unit defdriver;

interface

uses
   CommonClasses, SysUtils, Classes, adtfunct, adtcont, adtarray, adtmap;

type
   TDeclaration = class;
   TBlockDeclaration = class;
   TRootDeclaration = class;

   TDefaultDriver = class (TDriver)
   private
      Profile : String;
      FileQueue : TStringDequeAdt;
      currentFile : String;
      recentLine : Cardinal;

      { the title of the main contents file }
      ContentsTitle : String;
      { maps the names of the duplicated symbols in the current block
        to the first unused function number that may be appended to
        the symbol name to make it unique; the items stored are
        strings which may be interpreted as integers }
      duplicatedSymbols : TStringStringMap;
      duplicatedSymbolsStack : TStackAdt;

      { a string comparer appropriate for the current language (either
        case-sensitive or case-insensitive) }
      StringComparer : IStringBinaryComparer;

      { the _copy_ of the recent comment; owned by the driver }
      recentComment : TComment;
      { the number of declarations left to which recentComment should
        be applied if no other comment found }
      includeDecl : Cardinal;
      { the number of declarations to ignore }
      ignoreDecl : Cardinal;

      { the root of the whole tree of declarations; does not represent
        any declaration itself }
      rootDeclaration : TRootDeclaration;
      { the current block declaration inside which declarations are
        currently being declared, i.e. the current block }
      currDecl : TBlockDeclaration;
      { a stack of saved currDecls (TBlockDeclarations); they are
        saved each time a new block is entered, and restored when the
        block is finished }
      declStack : TStackAdt;

      { a map from unit names to TUnitDeclarations; <Units> does not
        own the declarations; they are owned by <rootDeclaration>;
        automatically sorted (a sorted set) }
      Units : TMap;
      { a list of TParsedFiles for files parsed that are not modules
        (units); <OtherFiles> _owns_ its items }
      OtherFiles : TListAdt;

      { a list of all classes; does not own its items }
      AllClasses : TArrayAdt;
      { a list of all interfaces; does not own its items }
      AllInterfaces : TArrayAdt;

      { a hash table of all declarations; maps declaration names to
        the declaration objects; may store duplicated items; used to
        speed up searching for declarations by their names }
      AllDeclarations : TMap;

      { a map of interface names to the declarations of classes
        implementing them; does not own its items; items may be
        repeated }
      InterfacesImplementedBy : TMap;
      { a map of class names to the declarations of classes that are
        immediate descendants of the given class name }
      DerivedClasses : TMap;

      { sets containing file extensions belonging to particular groups }
      RawTextExtensions : TStringSet;
      RawCommentExtensions : TStringSet;

      errorCount : Cardinal;

      procedure GiveError(msg : String);
      function GetTitle(title : String) : String;
      { registeres a section for <name>; handles duplicate section
        names; returns the section }
      function GetSection(const name : String; big : Boolean) : TSection;
      { checks if the comment matches the right profile }
      function CheckProfile(comment : TComment) : Boolean;
      { starts a new declaration block, like in a class or an
        interface; obj is the declaration of the new block }
      procedure StartBlock(obj : TBlockDeclaration);
      { ends the current declaration block }
      procedure EndBlock;
      { returns an appropriate comment object taking into account
        <comment>, includeDecl and recentComment; name is the name of
        the symbol }
      function GetCommentObject(name, comment : String) : TComment;

      function CheckedOpenInputFile(const path : String) : TFileStream;
      { reads in a whole ASCII file and returns its contents in a
        string }
      function ReadWholeFile(filename : String) : String;
      { creates or returns an already existing section for the given
        file; for raw text/comment files only ; the returned section
        should _not_ be destroyed by the caller }
      function GetRawFileSection(filename : String) : TSection;

      { reads the entire contents of a file and processes it like a
        comment }
      procedure ProcessRawCommentFile;
      procedure ProcessRawTextFile;

      procedure WriteTree(section : TSection; classTree : Boolean);
      procedure GenerateClassTree;
      procedure GenerateInterfaceTree;
      procedure GenerateSymbolIndex;
      procedure GenerateContents;

   public
      { @param aContentsTitle - the title of the documentation }
      { @param aprofile - the name of the profile to use }
      constructor Create(aoptions : TOptions; adocwriter : TDocWriter;
                         asourceDirs : TStrings;
                         aContentsTitle, aprofile : String);
      destructor Destroy; override;
      procedure Run(afilequeue : TStringDequeAdt); override;
      procedure RegisterFile(filename : String); override;
      function RegisterUnit(name : String;
                            unitComment : String) : Boolean; override;
      function OpenUnit(name : String) : Boolean; override;
      procedure ProvideUnitInfo(ainterfaceUses : TStrings); override;
      procedure ProvideUnitImplementationInfo(aimplUses : TStrings;
                                              aunitImplComment : String); override;
      procedure RegisterDeclarations(adecl : TTextObject; asymbols : TStrings;
                                     args : TStrings; avisibility : String;
                                     avisibilityType : TVisibilityType;
                                     asymboltype : String; alinenum : Integer;
                                     acomment : String); override;
      procedure StartClass(decl : TTextObject; name : String;
                           ancestors : TStrings; interfaces : TStrings;
                           visibility : String; visibilitytype : TVisibilityType;
                           symbolType : String; linenum : Integer;
                           comment : String); override;
      procedure FinishClass; override;
      procedure StartInterface(decl : TTextObject; aname : String;
                               ancestors : TStrings; id : String;
                               visibility : String;
                               visibilitytype : TVisibilityType;
                               symboltype : String;
                               lineNum : Integer; comment : String); override;
      procedure FinishInterface; override;

      procedure SetIgnoreDeclarations(num : Cardinal); override;
      function FindSection(name : String) : TSection; override;

      procedure Error(msg, filename : String; linenum : Cardinal); override;
      procedure Error(msg, filename : String); override;
      procedure Error(msg : String); override;
      procedure CommentError(msg : String); override;
      procedure Warn(msg, filename : String; line : Integer); override;
      procedure Warn(msg : String); override;
      procedure CommentWarn(msg : String); override;
      procedure WriteMessage(msg : String); override;
   end;

   { -------------- several helper classes ---------------- }

   { an abstract class representing a very general notion of a
     declaration; units, linked text and comment files are all treated
     as such declarations; this class is sometimes instantiated for
     declarations that were written out and it is not necessary to
     store their data any more, except for some basic things present
     here }
   { note: the parent-child relationship represents the fact that the
     child is declared inside parent (i.e. as the member of a class,
     interface, unit, etc.)  }
   { note: declarations are uniquely identified by their sections, not
     by the Names field }
   TBasicDeclaration = class
   protected
      { names of all the things declared }
      Names : TStrings;
      { the declaration inside which self was declared; nil if it is a
        root declaration (the root declaration must be of type
        TRootDeclaration). }
      ParentDecl : TBlockDeclaration;
      { the section containing the declaration }
      Section : TSection;
      { this should be set in the constructor of TBlockDeclaration to
        true; purely for efficiency reasons }
      IsBlockDeclaration : Boolean;

      { returns the name of the declaration together with the scope
        qualifier }
      function QualifiedName : String;

   public
      constructor Create(anames : TStrings; asection : TSection;
                         aparent : TBlockDeclaration);
      destructor Destroy; override;
   end;

   { represents a more specific notion of a declaration; }
   TDeclaration = class (TBasicDeclaration)
   private
      { this is false if the comment was fetched from some related
        method }
      ShouldDisposeComment : Boolean;
   protected
      SymbolType : String;
      { the text of the declaration }
      DeclText : TTextObject;
      { global, local, public, ... }
      Visibility : String;
      VisibilityType : TVisibilityType;
      { read, write, ... used with properties only }
      Accessibility : String;
      FileName : String;
      LineNum : Integer;
      Comment : TComment;
      Title : String;

      { writes out <decls> as links separating them with commas; also
        writes unrecognized as a plain text; <decls> and
        <unrecognized> must be non-nil }
      procedure WriteLinks(decls : TArrayAdt; unrecognized : TStrings);

      { writes a navigation panel; these are just several links for
        navigation purposes; decl is the declaration after which the
        panel is written; decl should be nil if it is a panel after
        the unit-level description }
      procedure WriteNavigationPanel;
      { opens all requisite sections }
      procedure OpenSections;
      { writes the beginning of the declaration }
      procedure WriteHead; virtual;
      procedure WriteMiddle; virtual;
      { writes the closing part of the declaration (members + the
        navigation panel) }
      procedure WriteTail; virtual;

   public
      { <acomment> may be nil to indicate no comment }
      constructor Create(adecl : TTextObject; asymbols : TStrings;
                         avisibility : String; avisibilityType : TVisibilityType;
                         asymboltype : String; alinenum : Cardinal;
                         acomment : TComment; asection : TSection;
                         aparentDecl : TBlockDeclaration);
      constructor CreateSimple(anames : TStrings; asection : TSection;
                               aparentDecl : TBlockDeclaration);
      destructor Destroy; override;
      procedure WriteOut; virtual;
   end;

   { a declaration containing other declarations; this class is
     abstract; it should never be instantiated }
   TBlockDeclaration = class (TDeclaration)
   private
      { an array of @<TDeclarations> }
      Declarations : TArray;
      { a map of declaration names to the declaration objects;
        <DeclsMap> does not own its items; it only caches objects from
        @<Declarations> }
      DeclsMap : TMap;
      { an array of TBasicDeclarations }
      Ancestors : TArrayAdt;
      { ancestors for which no TBasicDeclaration object was found }
      UnrecognizedAncestors : TStrings;

   protected
      { writes links to declarations which are associated in the table
        with the name of self; writes a declaration only if it is
        either an immediate descendant of self or if self is an
        interface and the declaration implements it }
      procedure WriteFromHashTable(table : TMap;
                                   heading : String);
      { <map> is a map of strings to TListAdt's. the procedure adds
        <decl> to each list mapped to by one of the strings in <strs>;
        if some strings in <strs> does not have an associated item in
        <map>, then creates a list for this item and inserts it into
        the map }
      procedure FillMap(map : TMap; strs : TStrings;
                        decl : TDeclaration);
      { for each string in <strs> tires to find a declaration with
        this name by calling SearchDeclaration; if found adds this
        declaration to decls, otherwise adds the string to
        unrecognized; if decls or unrecognized is nil then creates
        them; destroys strs }
      procedure StringsToDeclarations(var decls : TArrayAdt;
                                      var unrecognized : TStrings;
                                      var strs : TStrings);

      { sorts <decls> according to the symbolType field as
        the primary key and to the Name fileld as the secondary key }
      procedure SortDeclarations;
      { writes out the declarations <decl>; first writes a table with
        the synopsis'es of all the declarations; then writes a
        navigation panel and then the declarations themselves; the
        argument should be a vector of TDeclaration objects; decls
        must be non-nil; parent is the delaration to which decls
        belong; if there is no such declaration (i.e. decls are
        unit-level declarations), then parent should be nil }
      procedure WriteDeclarations;

      { returns true if self contains some declarations, i.e. some
        declarations were declared in the block of self }
      function HasDeclarations : Boolean;
      { returns the most recently added declaration or nil if self
        contains no declarations }
      function LastDeclaration : TDeclaration;
      { adds a declaration to the block declaration }
      procedure AddDeclaration(decl : TDeclaration);
      { finds a declaration with the name <name> in the declarations
        in the current block _only_ }
      function FindDeclaration(name : String) : TDeclaration;
      { searches a declaration in _all_ declarations starting from
        self; performs a breadth-first search on the whole tree of all
        declarations, treating it as a graph and starting from self;
        (in fact, AllDeclarations table is used to speed up the
        search, but the effect is roughly the same) }
      function SearchDeclaration(aname : String) : TBasicDeclaration;
      { searches for the nearest methods above and including self in
        the class hierarchy and with the name <name>; adds found
        declarations to <decls> }
      procedure SearchRelated(name : String; decls : TArrayAdt);
      { fetches a related comment by calling SearchRelated }
      function FetchRelatedComment(aname : String) : TComment;
      { returns true if <decl> is present among the ancestors of self }
      function IsDerivedFrom(decl : TBasicDeclaration) : Boolean;
      { should push all declarations considered to be ancestors of
        self at the front of <deque> }
      procedure PushAncestorsAtFront(deque : TDequeAdt); virtual;

      procedure WriteHead; override;
      procedure WriteMiddle; override;
      procedure WriteTail; override;

   public
      constructor Create(adecl : TTextObject; aname : String;
                         aancestors : TStrings;
                         avisibility : String; avisibilitytype : TVisibilityType;
                         asymboltype : String; alinenum : Cardinal;
                         acomment : TComment; asection : TSection;
                         aparentDecl : TBlockDeclaration);
      destructor Destroy; override;
      procedure WriteOut; override;
   end;

   TClassDeclaration = class (TBlockDeclaration)
   protected
      { an array of TBasicDeclarations  }
      Interfaces : TArrayAdt;
      UnrecognizedInterfaces : TStrings;

      { retruns true if self implements <int> }
      function ImplementsInterface(int : TBasicDeclaration) : Boolean;
      procedure PushAncestorsAtFront(deque : TDequeAdt); override;
      procedure WriteHead; override;
   public
      constructor Create(adecl : TTextObject; aname : String;
                         aancestors, ainterfaces : TStrings;
                         avisibility : String; avisibilitytype : TVisibilityType;
                         asymboltype : String; alinenum : Cardinal;
                         acomment : TComment; asection : TSection;
                         aparentDecl : TBlockDeclaration);
      destructor Destroy; override;
   end;

   TInterfaceDeclaration = class (TBlockDeclaration)
   protected
      Id : String;

      procedure WriteHead; override;
      procedure WriteMiddle; override;
   public
      constructor Create(adecl : TTextObject; aname : String;
                         aancestors : TStrings; aid : String;
                         avisibility : String; avisibilityType : TVisibilityType;
                         asymboltype : String; alinenum : Cardinal;
                         acomment : TComment; asection : TSection;
                         aparentDecl : TBlockDeclaration);
   end;

   TUnitDeclaration = class (TBlockDeclaration)
   protected
      { arrays of TBasicDeclarations  }
      InterfaceUses, ImplUses : TArrayAdt;
      UnrecognizedInterfaceUses, UnrecognizedImplUses : TStrings;
      Implcomment : TComment;
      FFileName : String;
   public
      constructor Create(aunitName : String; aunitComment : TComment);
      destructor Destroy; override;
      { this method should be called before writing the unit; it
        provides necessary information }
      procedure ProvideInfo(ainterfaceUses : TStrings);
      procedure ProvideImplementationInfo(aimplUses : TStrings;
                                          aImplComment : TComment);
      procedure WriteOut; override;
      property FileName : String read FFileName;
   end;

   { does not represent anything particular; just serves as the root
     of the whole tree of declarations }
   TRootDeclaration = class (TBlockDeclaration)
   public
      constructor Create;
   end;

implementation

uses
   adtiters, adthash, adthashfunct, adtalgs, adtlist, adtqueue,
   adtavltree, adtstralgs, defcommentparser, delphiparser, languages;

const
   MaxErrors = 15;

var
   DDriver : TDefaultDriver; { the same as Driver }
   DeclarationComparer : IBinaryComparer;
   { present only for efficiency reasons }
   ignoreCase : Boolean;

type
   StringArrayType = array of String;

   TParsedFile = class
   public
      FileName : String;
      Title : String;
      FileSect : TSection;
   end;

   TDeclarationComparer = class (TFunctor, IBinaryComparer)
   public
      function Compare(obj1, obj2 : TObject) : IndexType;
   end;

   TDeclarationNameComparer = class (TFunctor, IUnaryPredicate)
   private
      Name : String;
   public
      constructor Create(aname : String);
      function Test(obj : TObject) : Boolean;
   end;

   { used only to avoid checking for special cases }
   TRootSection = class (TSection)
   end;


{ --------------------- helper routines -------------------------- }

function DeclarationNameComparer(name : String) : IUnaryPredicate;
begin
   Result := TDeclarationNameComparer.Create(name);
end;

function BreakPathIntoSections(path : String) : StringArrayType;
var
   i, j : IndexType;
   tab : TKmpTable;
begin
   i := 0; j := 0;
   tab := KmpComputeTable(docwSectionSeparator);
   SetLength(Result, CountSubstrings(path, docwSectionSeparator, tab) + 1);
   Result[0] := path;
   while i <> -1 do
   begin
      i := KmpFindSubstr(Result[j], docwSectionSeparator, 1, tab);
      if i <> -1 then
      begin
         { copy the part of the string after the separator }
         Result[j + 1] := system.Copy(Result[j], i + Length(docwSectionSeparator),
                                      Length(Result[j]));
         Result[j] := system.Copy(Result[j], 1, i - 1);
         Inc(j);
      end;
   end;
end;

{ returns true if the strings are equal, false otherwise }
function CmpStr(const str1, str2 : String) : Boolean;
begin
   if ignoreCase then
      Result := AnsiCompareText(str1, str2) = 0
   else
      Result := str1 = str2;
end;

{ ---------------------- TDeclarationComparer ----------------------- }

function TDeclarationComparer.Compare(obj1, obj2 : TObject) : IndexType;
var
   vis1, vis2 : TVisibilityType;
begin
   Assert(obj1 is TDeclaration);
   Assert(obj2 is TDeclaration);
   { compare the visibility; publicly visible come first }
   vis1 := TDeclaration(obj1).VisibilityType;
   vis2 := TDeclaration(obj2).VisibilityType;
   Result := Ord(vis1) - Ord(vis2);
   if Result = 0 then
   begin
      Result := AnsiCompareStr(TDeclaration(obj1).Visibility,
                               TDeclaration(obj2).Visibility);
      if Result = 0 then
      begin
         Result := AnsiCompareStr(TDeclaration(obj1).SymbolType,
                                  TDeclaration(obj2).SymbolType);
      end;
   end;
end;

{ ------------------- TDeclarationNameComparer ---------------------- }

constructor TDeclarationNameComparer.Create(aname : String);
begin
{$ifdef DEBUG }
   inherited Create;
{$endif }
   Name := aname;
end;

function TDeclarationNameComparer.Test(obj : TObject) : Boolean;
begin
   Assert(obj is TBasicDeclaration);
   { note: declarations are uniquely identified by their section
     names }
   Result := CmpStr(TBasicDeclaration(obj).Section.Name, Name);
end;

{ --------------------- TBasicDeclaration ---------------------------- }

constructor TBasicDeclaration.Create(anames : TStrings; asection : TSection;
                                     aparent : TBlockDeclaration);
begin
   Names := anames;
   Section  := asection;
   ParentDecl := aparent;
   IsBlockDeclaration := false;
end;

destructor TBasicDeclaration.Destroy;
begin
   Names.Free;
   Section.Free;
   inherited;
end;

function TBasicDeclaration.QualifiedName : String;
var
   i : Integer;
begin
   Assert (Names <> nil);
   Assert (Names.Count > 0);
   if Names.Count > 1 then
   begin
      Result := '';
      for i := 0 to Names.Count - 2 do
         Result := Result + Names[i] + ', ';
      Result := Result + Names[Names.Count - 1];
   end else if (ParentDecl <> nil) and
                  ((ParentDecl is TClassDeclaration) or
                      (ParentDecl is TInterfaceDeclaration)) then
   begin
      Result := ParentDecl.Names[0] +
         DDriver.LanguageParser.ScopeResolutionToken + Names[0];
   end else
      Result := Names[0];
end;

{ ------------------------ TDeclaration ------------------------------ }

constructor TDeclaration.Create(adecl : TTextObject; asymbols : TStrings;
                                avisibility : String;
                                avisibilityType : TVisibilityType;
                                asymboltype : String; alinenum : Cardinal;
                                acomment : TComment; asection : TSection;
                                aparentDecl : TBlockDeclaration);
begin
   Assert(asymbols <> nil);
   Assert(asymbols.Count <> 0);
   inherited Create(asymbols, asection, aparentDecl);
   Symboltype := asymboltype;
   Decltext := adecl;
   Visibility := avisibility;
   VisibilityType := avisibilityType;
   FileName := TDefaultDriver(Driver).currentFile;
   LineNum := alinenum;
   Comment := acomment;
   if (Comment <> nil) and (Comment.Title <> '') then
      Title := Comment.Title
   else
      Title := GetLangString(Reference_for_str) + ' ' + asymbols[0];
   ShouldDisposeComment := true;

   if (Comment = nil) and (ParentDecl <> nil) and
         not (ParentDecl is TRootDeclaration) and
         (optFetchCommentsFromAncestors in Driver.Options) then
   begin
      comment := ParentDecl.FetchRelatedComment(Names[0]);
      if comment <> nil then
         ShouldDisposeComment := false;
   end;
end; { end constructor Create }

constructor TDeclaration.CreateSimple(anames : TStrings; asection : TSection;
                                      aparentDecl : TBlockDeclaration);
begin
   Create(nil, anames, GetLangString(global_str), vtPublic,
          GetLangString(file_str), 0,
          nil, asection, aparentDecl);
end;

destructor TDeclaration.Destroy;
begin
   Decltext.Free;
   if ShouldDisposeComment then
      Comment.Free;
   inherited;
end;

procedure TDeclaration.WriteLinks(decls : TArrayAdt; unrecognized : TStrings);
var
   i : Integer;
   decl : TBasicDeclaration;
begin
   Assert(decls <> nil);
   Assert(unrecognized <> nil);
   Assert(Section <> nil);

   for i := decls.LowIndex to decls.HighIndex do
   begin
      decl := TBasicDeclaration(decls[i]);
      Section.WriteLink(decl.Names[0], decl.Section);
      if i <> decls.HighIndex then
         Section.WriteText(', ');
   end;
   if (unrecognized.Count <> 0) and (decls.Size <> 0) then
      Section.WriteText(', ');
   for i := 0 to unrecognized.Count - 1 do
   begin
      Section.WriteText(unrecognized[i]);
      if i <> unrecognized.Count - 1 then
         Section.WriteText(', ');
   end;
end;

procedure TDeclaration.OpenSections;
var
   i : Integer;
   sect : TSection;
begin
   Assert(ParentDecl <> nil);
   Assert(Names <> nil);
   for i := 1 to Names.Count - 1 do
   begin
      if ParentDecl.SearchDeclaration(Names[i]) = nil then
      begin
         sect := Driver.DocWriter.RegisterSection(Names[i], ParentDecl.Section,
                                                  false);
         sect.Open(Title);
         sect.Close;
         sect.Destroy;
      end;
   end;
   Section.Open(Title);
end;

procedure TDeclaration.WriteNavigationPanel;
var
   widths : TIntegerArray;
   cols : Integer;
begin
   with Section do
   begin
      if self <> nil then
      begin
         NewParagraph;

         cols := 3;
         SetLength(widths, cols);
         widths[0] := 33;
         widths[1] := 33;
         widths[2] := 33;

         StartTable(widths, cols);
         WriteLink(Names[0], Section);
         if not (ParentDecl is TRootDeclaration) then
         begin
            NextCell;
            WriteLink(ParentDecl.Names[0], ParentDecl.Section);
            if not (ParentDecl.ParentDecl is TRootDeclaration) then
            begin
               NextCell;
               WriteLink(ParentDecl.ParentDecl.Names[0],
                         ParentDecl.ParentDecl.Section);
            end;
         end;
         FinishTable;
      end;
   end;
end;

procedure TDeclaration.WriteHead;
begin
   with Section do
   begin
      WriteHeading(QualifiedName(), bigHeadingSize);

      WriteHeading(GetLangString(Declaration_str), subHeadingSize);
      SuppressLineBreaking;
      DeclText.WriteOut(Section);
      ResumeLineBreaking;

      if visibility <> '' then
      begin
         WriteHeading(GetLangString(Visibility_str), subHeadingSize);
         WriteText(visibility);
      end;

      if accessibility <> '' then
      begin
         WriteHeading(GetLangString(Access_str), subHeadingSize);
         WriteText(accessibility);
      end;
   end; { end with }
end;

procedure TDeclaration.WriteMiddle;
var
   decls : TArray;
   i : Integer;
   dd : TBasicDeclaration;
begin
   Assert(ParentDecl <> nil);
   with Section do
   begin
      if not (ParentDecl is TRootDeclaration) then
      begin
         decls := TArray.Create;
         decls.OwnsItems := false;
         try
            ParentDecl.SearchRelated(Names[0], decls);
            if not decls.Empty then
            begin
               WriteHeading(GetLangString(Related_methods_str), subHeadingSize);
               for i := decls.LowIndex to decls.HighIndex do
               begin
                  dd := TBasicDeclaration(decls[i]);
                  WriteLink(dd.QualifiedName, dd.Section);
                  if i <> decls.HighIndex then
                     WriteText(', ');
               end;
            end;
         finally
            decls.Free;
         end;
      end;

      if filename <> '' then
      begin
         WriteHeading(GetLangString(Source_str), subHeadingSize);
         WriteText(filename);
         if lineNum <> 0 then
            WriteText(' ' + GetLangString(on_line_str) + ' ' + IntToStr(lineNum));
      end;
   end; { end with Section }

   if comment <> nil then
      comment.WriteOut(Section);
end;

procedure TDeclaration.WriteTail;
begin
   WriteNavigationPanel;
end;

procedure TDeclaration.WriteOut;
begin
   OpenSections;
   WriteHead;
   WriteMiddle;
   WriteTail;
   Section.Close;
end;

{ ---------------------- TBlockDeclaration --------------------------------- }

constructor TBlockDeclaration.Create(adecl : TTextObject; aname : String;
                                     aancestors : TStrings;
                                     avisibility : String;
                                     avisibilityType : TVisibilityType;
                                     asymboltype : String; alinenum : Cardinal;
                                     acomment : TComment; asection : TSection;
                                     aparentDecl : TBlockDeclaration);
var
   asymbols : TStrings;
begin
   asymbols := TStringList.Create;
   asymbols.Add(aname);

   inherited Create(adecl, asymbols, avisibility, avisibilityType,
                    asymboltype, alinenum, acomment, asection, aparentDecl);

   Declarations := TArray.Create;
   { Driver.LanguageParser is nil when creating the root declaration }
   { DeclsMap does not own its items }
   DeclsMap := TMap.Create(THashTable.Create);
   if (Driver.LanguageParser <> nil) and Driver.LanguageParser.IgnoreCase then
      DeclsMap.KeyComparer := NoCaseStringComparer;
   DeclsMap.OwnsItems := false;

   IsBlockDeclaration := true;

   FillMap(DDriver.DerivedClasses, aancestors, self);

   StringsToDeclarations(Ancestors, UnrecognizedAncestors, aancestors);
end;

destructor TBlockDeclaration.Destroy;
begin
   Declarations.Free;
   DeclsMap.Free;
   Ancestors.Free;
   UnrecognizedAncestors.Free;
   inherited;
end;

procedure TBlockDeclaration.FillMap(map : TMap; strs : TStrings;
                                    decl : TDeclaration);
var
   i : Integer;
begin
   for i := 0 to strs.Count - 1 do
   begin
      map.Insert(strs[i], decl);
   end;
end;

procedure TBlockDeclaration.StringsToDeclarations(var decls : TArrayAdt;
                                                  var unrecognized : TStrings;
                                                  var strs : TStrings);
var
   decl : TBasicDeclaration;
   i : Integer;
begin
   Assert(strs <> nil);

   if decls = nil then
   begin
      decls := TArray.Create;
      decls.OwnsItems := false;
   end;

   if unrecognized = nil then
      unrecognized := TStringList.Create;

   for i := 0 to strs.Count - 1 do
   begin
      decl := SearchDeclaration(strs[i]);
      if decl <> nil then
      begin
         decls.PushBack(decl);
      end else
         unrecognized.Add(strs[i]);
   end;

   strs.Free;
   strs := nil;
end;

procedure TBlockDeclaration.SortDeclarations;
begin
   StableSort(Declarations.Start, Declarations.Finish, DeclarationComparer);
end;

procedure TBlockDeclaration.WriteDeclarations;
var
   i, j : Integer;
   widths : TIntegerArray;
   decl : TDeclaration;
begin
   with Section do
   begin
      SortDeclarations;

      SetLength(widths, 4);
      widths[0] := 30; { name }
      widths[1] := 15; { type }
      widths[2] := 15; { visibility }
      widths[3] := 40; { synopsis }

      StartTable(widths, 4);

      WriteHeading(GetLangString(Name_str), smallHeadingSize);
      NextCell;
      WriteHeading(GetLangString(Type_str), smallHeadingSize);
      NextCell;
      WriteHeading(GetLangString(Visibility_str), smallHeadingSize);
      NextCell;
      WriteHeading(GetLangString(Synopsis_str), smallHeadingSize);
      NextRow;

      for i := Declarations.LowIndex to Declarations.HighIndex do
      begin
         Assert(TObject(Declarations[i]) is TDeclaration);
         decl := TDeclaration(Declarations[i]);
         for j := 0 to decl.Names.Count - 1 do
         begin
            WriteLink(decl.Names[j], decl.Section);
            NextCell;
            WriteText(decl.SymbolType);
            NextCell;
            WriteText(decl.Visibility);
            NextCell;
            if (decl.Comment <> nil) and (decl.Comment.Synopsis <> nil) then
               decl.Comment.Synopsis.WriteOut(Section);
            NextRow;
         end;
      end;

      FinishTable;

      WriteNavigationPanel;
      Newline;

      { now write the descriptions of the declarations themselves }
      for i := Declarations.LowIndex to Declarations.HighIndex do
      begin
         Assert(TObject(Declarations[i]) is TDeclaration);
         TDeclaration(Declarations[i]).WriteOut;
      end;
   end; { end with DocWriter }
end;

function TBlockDeclaration.HasDeclarations : Boolean;
begin
   Result := Declarations.Empty;
end;

function TBlockDeclaration.LastDeclaration : TDeclaration;
begin
   if not Declarations.Empty then
      Result := TDeclaration(Declarations.Back)
   else
      Result := nil;
end;

procedure TBlockDeclaration.AddDeclaration(decl : TDeclaration);
var
   i : Integer;
begin
   Assert(decl <> nil);
   Assert(decl.ParentDecl = self);
   Assert(DeclsMap.Find(decl.Section.Name) = nil);
   Declarations.PushBack(decl);
   DeclsMap[decl.Section.Name] := decl;

   DDriver.AllDeclarations.Insert(decl.Section.Name, decl);
   for i := 1 to decl.Names.Count - 1 do
      DDriver.AllDeclarations.Insert(decl.Names[i], decl);
end;

function TBlockDeclaration.FindDeclaration(name : String) : TDeclaration;
begin
   Result := TDeclaration(DeclsMap.Find(name));
   { note: SearchDeclaration uses DeclsMap directly instead of calling
     this function for efficiency reasons }
end;

function TBlockDeclaration.SearchDeclaration(aname : String) :
   TBasicDeclaration;
type
   TPathArray = array of TBasicDeclaration;
var
   decl : TBasicDeclaration;
   iter : TMapIterator;
   range : TMapIteratorRange;
   path, path2 : TPathArray;
   matchingLevels, bestMatchingLevels, depthLevels, bestDepthLevels : Integer;
   i : Integer;
   secNames : array of String;

   { decl is the declaration to start the search with; secNames[0] is
     not checked; returns the pointed declaration or nil if not all
     requisite sections present }
   function AllSectionsPresent(adecl : TBasicDeclaration) : TBasicDeclaration;
   var
      j : Integer;
   begin
      j := 1;
      Result := adecl;
      while (j < Length(secNames)) do
      begin
         if (Result.IsBlockDeclaration) then
         begin
            Result := TBasicDeclaration(TBlockDeclaration(Result).
                                           DeclsMap.Find(secNames[j]));
            if Result <> nil then
            begin
               Inc(j);
            end else
               break;
         end else
         begin
            Result := nil;
            break;
         end;
      end;
   end;

   function GetPath(adecl : TBasicDeclaration) : TPathArray;
   begin
      SetLength(Result, 10);
      i := 0;
      while adecl <> nil do
      begin
         Inc(i);
         if i > Length(Result) then
            SetLength(Result, Length(Result)*2);
         Result[i] := adecl;
         adecl := adecl.ParentDecl;
      end;
      SetLength(Result, i);
   end;

var
   j : Integer;

begin
   if aname = '' then
   begin
      Result := nil;
      Exit;
   end;

   secNames := nil;
   Result := nil;
   aname := Driver.LanguageParser.ParseIdentifierRef(Trim(aname));
   { break up <aname> into section names }
   secNames := BreakPathIntoSections(aname);

   if (CmpStr(Section.Name, secNames[0])) then
   begin
      Result := AllSectionsPresent(self);
      if Result <> nil then
         Exit;
   end;
   decl := FindDeclaration(secNames[0]);
   if decl <> nil then
   begin
      Result := AllSectionsPresent(decl);
      if Result <> nil then
         Exit;
   end;

   { get the declarations on the path from self to the root }
   path := GetPath(self);

   bestMatchingLevels := 0;
   bestDepthLevels := MAXINT;
   range := DDriver.AllDeclarations.EqualRange(secNames[0]);
   iter := range.Start;
   while not iter.Equal(range.Finish) do
   begin
      decl := TBasicDeclaration(iter.Item);
      matchingLevels := 0;
      depthLevels := MAXINT;

      path2 := GetPath(decl);

      i := Length(path);
      j := Length(path2);
      while (i > 0) and (j > 0) do
      begin
         Dec(j);
         Dec(i);
         if path[i] <> path2[j] then
            break;
      end;
      if (i = 0) and (j >= 0) then
      begin
         depthLevels := j;
      end;
      matchingLevels := Length(path2) - j;

      if (matchingLevels >= bestMatchingLevels) and
            (depthLevels <= bestDepthLevels) then
      begin
         Result := AllSectionsPresent(decl);
         if Result <> nil then
         begin
            bestMatchingLevels := matchingLevels;
            bestDepthLevels := Depthlevels;
         end;
      end;

      iter.Advance;
   end;
   range.Destroy;
end;

procedure TBlockDeclaration.SearchRelated(name : String; decls : TArrayAdt);
var
   decl : TBasicDeclaration;
   block : TBlockDeclaration;
   deque : TDequeAdt;
begin
   { perform a depth-first search on the graph of the ancestors }

   deque := TDeque.Create;
   deque.OwnsItems := false;
   try
      PushAncestorsAtFront(deque);
      while not deque.Empty do
      begin
         decl := TBasicDeclaration(deque.Front);
         deque.PopFront;
         if decl is TBlockDeclaration then
         begin
            block := TBlockDeclaration(decl);
            decl := block.FindDeclaration(name);
            if decl <> nil then
            begin
               decls.PushBack(decl);
            end;
            block.PushAncestorsAtFront(deque);
         end;
      end;

   finally
      deque.Free;
   end;
end;

function TBlockDeclaration.FetchRelatedComment(aname : String) : TComment;
var
   decls : TArray;
   i : Integer;
   ptr : Pointer;
begin
   Result := nil;
   decls := TArray.Create;
   decls.OwnsItems := false;
   ParentDecl.SearchRelated(aname, decls);
   for i := decls.LowIndex to decls.HighIndex do
   begin
      ptr := decls[i];
      if (TBasicDeclaration(ptr) is TDeclaration) and
            (TDeclaration(ptr).Comment <> nil) then
      begin
         Result := TDeclaration(ptr).Comment;
         break;
      end;
   end;
   decls.Destroy;
end;

function TBlockDeclaration.IsDerivedFrom(decl : TBasicDeclaration) : Boolean;
var
   i : Integer;
begin
   Result := false;
   for i := 0 to Ancestors.Size - 1 do
   begin
      if TBasicDeclaration(Ancestors[i]) = decl then
      begin
         Result := true;
         break;
      end;
   end;
end;

procedure TBlockDeclaration.PushAncestorsAtFront(deque : TDequeAdt);
var
   i : Integer;
begin
   for i := Ancestors.LowIndex to Ancestors.HighIndex do
      deque.PushFront(Ancestors[i]);
end;

procedure TBlockDeclaration.WriteHead;
begin
   inherited;
   with Section do
   begin
      if (Ancestors <> nil) and
            (Ancestors.Size + UnrecognizedAncestors.Count <> 0) then
      begin
         if Ancestors.Size + UnrecognizedAncestors.Count = 1 then
            WriteHeading(GetLangString(Immediate_ancestor_str), subHeadingSize)
         else
            WriteHeading(GetLangString(Immediate_ancestors_str), subHeadingSize);
         WriteLinks(Ancestors, UnrecognizedAncestors);
      end;
   end;
end;

procedure TBlockDeclaration.WriteFromHashTable(table : TMap;
                                               heading : String);
var
   iter : TMapIterator;
   range : TMapIteratorRange;
   decl : TBlockDeclaration;
   headingWritten : Boolean;
begin
   with Section do
   begin
      headingWritten := false;
      range := table.EqualRange(Names[0]);
      iter := range.Start;
      while not iter.Equal(range.Finish) do
      begin
         Assert(TBasicDeclaration(iter.Item) is TBlockDeclaration);
         decl := TBlockDeclaration(iter.Item);
         if decl.IsDerivedFrom(self) or
               ((decl is TClassDeclaration) and
                   (TClassDeclaration(decl).ImplementsInterface(self))
               ) then
         begin
            if not headingWritten then
            begin
               NewParagraph;
               WriteHeading(heading, subHeadingSize);
               headingWritten := true;
            end else
               WriteText(', ');
            WriteLink(decl.QualifiedName, decl.Section);
         end;
         iter.Advance;
      end;
      range.Destroy;
   end;
end;

procedure TBlockDeclaration.WriteMiddle;
begin
   WriteFromHashTable(DDriver.DerivedClasses,
                      GetLangString(Immediate_descendants_str));
   inherited;
end;

procedure TBlockDeclaration.WriteTail;
begin
   if not Declarations.Empty then
   begin
      Section.WriteHeading(GetLangString(Members_str), subHeadingSize);
      WriteDeclarations;
   end else
      WriteNavigationPanel;
end;

procedure TBlockDeclaration.WriteOut;
begin
   DDriver.currDecl := self;
   inherited;
end;

{ ---------------------- TClassDeclaration --------------------------------- }

constructor TClassDeclaration.Create(adecl : TTextObject; aname : String;
                                     aancestors, ainterfaces : TStrings;
                                     avisibility : String;
                                     avisibilityType : TVisibilityType;
                                     asymboltype : String; alinenum : Cardinal;
                                     acomment : TComment; asection : TSection;
                                     aparentDecl : TBlockDeclaration);
begin
   inherited Create(adecl, aname, aancestors, avisibility, avisibilityType,
                    asymboltype, alinenum, acomment, asection, aparentDecl);

   FillMap(DDriver.InterfacesImplementedBy, ainterfaces, self);
   StringsToDeclarations(Interfaces, UnrecognizedInterfaces, ainterfaces);
end;

destructor TClassDeclaration.Destroy;
begin
   Interfaces.Free;
   UnrecognizedInterfaces.Free;
   inherited;
end;

function TClassDeclaration.ImplementsInterface(int : TBasicDeclaration) : Boolean;
var
   i : Integer;
begin
   Result := false;
   for i := 0 to Interfaces.Size - 1 do
   begin
      if TBasicDeclaration(Interfaces[i]) = int then
      begin
         Result := true;
         break;
      end;
   end;
end;

procedure TClassDeclaration.PushAncestorsAtFront(deque : TDequeAdt);
var
   i : Integer;
begin
   inherited;
   { first ancestor classes should be searched, interfaces later;
     interfaces are searched using breadth-first search, classes using
     depth-first search }
   for i := Interfaces.LowIndex to Interfaces.HighIndex do
      deque.PushBack(Interfaces[i]);
end;

procedure TClassDeclaration.WriteHead;
begin
   inherited;
   with Section do
   begin
      if (Interfaces <> nil) and
            (Interfaces.Size + UnrecognizedInterfaces.Count <> 0) then
      begin
         if Interfaces.Size + UnrecognizedInterfaces.Count = 1 then
            WriteHeading(GetLangString(Implemented_interface_str), subHeadingSize)
         else
            WriteHeading(GetLangString(Implemented_interfaces_str),
                         subHeadingSize);
         WriteLinks(Interfaces, UnrecognizedInterfaces);
      end;
   end;
end;

{ --------------------- TInterfaceDeclaration ------------------------------ }

constructor TInterfaceDeclaration.Create(adecl : TTextObject; aname : String;
                                         aancestors : TStrings; aid : String;
                                         avisibility : String;
                                         avisibilityType : TVisibilityType;
                                         asymboltype : String; alinenum : Cardinal;
                                         acomment : TComment; asection : TSection;
                                         aparentDecl : TBlockDeclaration);
begin
   inherited Create(adecl, aname, aancestors, avisibility, avisibilityType,
                    asymboltype, alinenum, acomment, asection, aparentDecl);
   Id := aid;
end;

procedure TInterfaceDeclaration.WriteHead;
begin
   inherited;
   with Section do
   begin
      if Id <> '' then
      begin
         WriteHeading(GetLangString(ID_str), subHeadingSize);
         WriteText(Id);
      end;
   end;
end;

procedure TInterfaceDeclaration.WriteMiddle;
begin
   WriteFromHashTable(DDriver.InterfacesImplementedBy,
                      GetLangString(Implemented_by_str));
   inherited;
end;

{ --------------------- TUnitDeclaration ----------------------------- }

constructor TUnitDeclaration.Create(aunitName : String; aunitComment : TComment);
begin
   inherited Create(nil, aunitName, TStringList.Create, GetLangString(global_str),
                    vtPublic, '', 0, nil,
                    Driver.DocWriter.RegisterSection(aunitName, nil, true),
                    DDriver.rootDeclaration);
   FFileName := DDriver.currentFile;
   Title := GetLangString(Reference_for_str) + ' ' + aunitName;
   Comment := aunitComment;
end;

destructor TUnitDeclaration.Destroy;
begin
   InterfaceUses.Free;
   UnrecognizedInterfaceUses.Free;
   ImplUses.Free;
   UnrecognizedImplUses.Free;
   Implcomment.Free;
   inherited;
end;

procedure TUnitDeclaration.ProvideInfo(ainterfaceUses : TStrings);
begin
   { this method should be called only once }
   Assert(InterfaceUses = nil);
   Assert(UnrecognizedInterfaceUses = nil);

   StringsToDeclarations(Interfaceuses, UnrecognizedInterfaceUses,
                         ainterfaceUses);
end;

procedure TUnitDeclaration.ProvideImplementationInfo(aimplUses : TStrings;
                                                     aImplComment : TComment);
begin
   Assert(ImplUses = nil);
   Assert(UnrecognizedImplUses = nil);
   Assert(ImplComment = nil);
   StringsToDeclarations(impluses, UnrecognizedImplUses,
                         aimplUses);
   implcomment := aImplComment;
end;

procedure TUnitDeclaration.WriteOut;
var
   cmt : TComment;
begin
   { ProvideInfo() should have been called earlier }
   Assert((interfaceUses <> nil) and
             ((implUses <> nil) or
                 not (optShowImplementationInfo in Driver.Options)));

   with Section do
   begin
      DDriver.currDecl := self;
      Open;
      WriteHeading(name, unitHeadingSize);

      if InterfaceUses.Size + UnrecognizedInterfaceUses.Count <> 0 then
      begin
         WriteHeading(GetLangString(Uses_in_interface_str), subHeadingSize);
         WriteLinks(interfaceUses, UnrecognizedInterfaceUses);
      end;

      if (implUses <> nil) and
            (implUses.Size + UnrecognizedImplUses.Count <> 0) and
            (optShowImplementationInfo in Driver.Options) then
      begin
         WriteHeading(GetLangString(Uses_in_implementation_str), subHeadingSize);
         WriteLinks(implUses, UnrecognizedImplUses);
      end;

      if comment <> nil then
      begin
         if comment.FetchRelated then
         begin
            cmt := FetchRelatedComment(Names[0]);
            if cmt <> nil then
               cmt.WriteOut(Section);
         end;
         comment.WriteOut(Section);
      end;

      if (implComment <> nil) and
            (optShowImplementationInfo in Driver.Options) then
      begin
         WriteHeading(GetLangString(Notes_on_the_implementation_str),
                      bigHeadingSize);
         Implcomment.WriteOut(Section);
      end;

      if not Declarations.Empty then
      begin
         WriteHeading(GetLangString(Declarations_str), bigHeadingSize);
         WriteDeclarations;
      end;

      Close;
   end;
end;

{ --------------------------- TRootDeclaration ----------------------------- }

constructor TRootDeclaration.Create;
const
   rootName = '@#$%^&root_declaration&^%$#@';
begin
   inherited Create(nil, rootName, TStringList.Create,
                    'root', vtPublic, '', 0, nil, TRootSection.Create(rootName),
                    nil);
end;


{ ========================================================================== }
{ ----------------------------- TDefaultDriver ----------------------------- }

constructor TDefaultDriver.Create(aoptions : TOptions;
                                  adocwriter : TDocWriter;
                                  asourcedirs : TStrings;
                                  aContentsTitle, aprofile : String);
begin
   inherited Create(aoptions, adocwriter, asourcedirs);

   Profile := aprofile;
   ContentsTitle := aContentsTitle;
   StringComparer := NoCaseStringComparer;

   { Units does not own the declarations! rootDeclaration should own
     them!  }
   Units := TMap.Create(TAvlTree.Create);
   Units.OwnsItems := false;

   { OtherFiles does own! }
   OtherFiles := TSingleList.Create;

   AllClasses := TArray.Create;
   AllClasses.OwnsItems := false;

   AllInterfaces := TArray.Create;
   AllInterfaces.OwnsItems := false;

   AllDeclarations := TMap.Create(THashTable.Create);
   AllDeclarations.RepeatedItems := true;
   AllDeclarations.OwnsItems := false;

   InterfacesImplementedBy := TMap.Create(THashTable.Create);
   InterfacesImplementedBy.OwnsItems := false;
   InterfacesImplementedBy.RepeatedItems := true;

   DerivedClasses := TMap.Create(THashTable.Create);
   DerivedClasses.OwnsItems := false;
   DerivedClasses.RepeatedItems := true;

   duplicatedSymbols := TStringStringMap.Create(THashTable.Create);

   duplicatedSymbolsStack := TStack.Create;
   duplicatedSymbolsStack.OwnsItems := false;

   declStack := TStack.Create;
   declStack.OwnsItems := false;

   RawTextExtensions := TStringSet.Create;
   RawTextExtensions.ItemComparer := NoCaseStringComparer;

   RawCommentExtensions := TStringSet.Create;
   RawCommentExtensions.ItemComparer := NoCaseStringComparer;

   with RawTextExtensions do
   begin
      Insert('.txt');
      Insert('.text');
   end;

   with RawCommentExtensions do
   begin
      Insert('.srd');
      Insert('.comment');
   end;

   DDriver := self;
   Driver := self;

   rootDeclaration := TRootDeclaration.Create;
end;

destructor TDefaultDriver.Destroy;
begin
   FileQueue.Free;

   Assert (duplicatedSymbolsStack.Empty, 'Duplicated symbols stack not empty');
   duplicatedSymbols.Free;
   duplicatedSymbolsStack.Free;

   recentComment.Free;

   Units.Free;
   OtherFiles.Free;

   AllClasses.Free;
   AllInterfaces.Free;

   AllDeclarations.Free;

   InterfacesImplementedBy.Free;
   DerivedClasses.Free;

   rootDeclaration.Free;
   declStack.Free;

   RawCommentExtensions.Free;
   RawTextExtensions.Free;

   inherited;
end;

procedure TDefaultDriver.GiveError(msg : String);
begin
   Inc(errorCount);
   WriteLn('srcdoc:' + msg);
   if errorCount > maxErrors then
   begin
      raise ESrcdocError.Create('Too many errors. Aborting.');
   end;
end;

function TDefaultDriver.GetTitle(title : String) : String;
begin
   if title <> '' then
      Result := title
   else begin
      Result := ExtractBaseName(currentFile);
      Result[1] := UpCase(Result[1]);
   end;
end;

function TDefaultDriver.GetSection(const name : String; big : Boolean) : TSection;
var
   unusedSectNumStr : String;
   sect : String;
begin
   Assert(currDecl <> nil);

   if currDecl.FindDeclaration(name) <> nil then
   begin
      { if not found then Find returns nil - exactly what we want }
      if duplicatedSymbols.Has(name) then begin
         unusedSectNumStr := duplicatedSymbols.Find(name);
      end else begin
         unusedSectNumStr := '0';
      end;
      sect := name + '___overloaded___' + unusedSectNumStr;
      duplicatedSymbols[name] := IntToStr(StrToInt(unusedSectNumStr) + 1);
   end else
      sect := name;
   Result := DocWriter.RegisterSection(sect, currDecl.Section, big);
end;

function TDefaultDriver.CheckProfile(comment : TComment) : Boolean;
var
   i : Integer;
begin
   if (comment <> nil) and (comment.Profiles <> nil) then
   begin
      with comment do
      begin
         Result := false;
         for i := 0 to Profiles.Count - 1 do
         begin
            if Profiles[i] = Profile then
            begin
               Result := true;
               break;
            end;
         end;
      end;
   end else
      Result := true;
end;

procedure TDefaultDriver.StartBlock(obj : TBlockDeclaration);
begin
   currDecl.AddDeclaration(obj);
   declStack.PushBack(currDecl);
   currDecl := obj;
   duplicatedSymbolsStack.PushBack(duplicatedSymbols);
   duplicatedSymbols := TStringStringMap.Create(THashTable.Create);
end;

procedure TDefaultDriver.EndBlock;
begin
   { currecl should not be freed here  }
   Assert(declStack.Back is TBlockDeclaration);
   currDecl := TBlockDeclaration(declStack.Back);
   declStack.PopBack;
   duplicatedSymbols.Free;
   Assert(duplicatedSymbolsStack.Back is TStringStringMap);
   duplicatedSymbols := TStringStringMap(duplicatedSymbolsStack.Back);
   duplicatedSymbolsStack.PopBack;
end;

function TDefaultDriver.GetCommentObject(name, comment : String) : TComment;
begin
   if (Trim(comment) = '') then
   begin
      if ( (includeDecl <> 0) or
              ( (not currDecl.HasDeclarations) and
                   (CmpStr(TDeclaration(currDecl.LastDeclaration).Names[0],
                           name))
              )
         ) then
      begin
         if includeDecl <> 0 then
            Dec(includeDecl);
         if recentComment <> nil then
            Result := TComment(recentComment.CopySelf)
         else
            Result := nil;
      end else
      begin
         includeDecl := 0;
         Result := nil;
         recentComment.Free;
         recentComment := nil;
      end;
   end else
   begin
      Result := CommentParser.ParseComment(comment);
      if Result.IncludeDeclarations = 0 then
      begin
         Result.Free;
         Result := nil;
      end else begin
         { -1 because one comment object is just now being used }
         includeDecl := Result.IncludeDeclarations - 1;
         recentComment.Free;
         recentComment := TComment(Result.CopySelf);
      end;
   end;
end;

function TDefaultDriver.CheckedOpenInputFile(const path : String) : TFileStream;
begin
   Result := OpenInputFile(path);
   if (Result = nil) then
      Warn('Could not open file ' + path + ' - skipping.');
end;

function TDefaultDriver.ReadWholeFile(filename : String) : String;
var
   stream : TFileStream;
   size : Integer;
begin
   stream := CheckedOpenInputFile(filename);
   if stream <> nil then
   begin
      if stream.Size >= MAXINT then
      begin
         ESrcdocError.Create('Files bigger than ' + IntToStr(MAXINT) +
                                ' bytes not supported');
      end;
      {$warnings off }
      size := stream.Size;
      {$warnings on }
      SetLength(Result, size);
      stream.Read(Result[1], size);
      stream.Destroy;
   end else
      Result := '';
end;

{ for raw text/comment files only; the returned section should _not_
  be destroyed by the caller }
function TDefaultDriver.GetRawFileSection(filename : String) : TSection;
var
   decl : TBasicDeclaration;
   names : TStrings;
begin
   decl := rootDeclaration.FindDeclaration(filename);
   if decl = nil then
   begin
      Result := DocWriter.RegisterSection(filename, nil, true);
      names := TStringList.Create;
      names.Add(filename);
      rootDeclaration.AddDeclaration(
         TDeclaration.CreateSimple(names, Result, rootDeclaration)
                                    );
   end else
      Result := decl.Section;
end;

procedure TDefaultDriver.ProcessRawCommentFile;
var
   buffer : String;
   comment : TComment;
   section : TSection;
   pf : TParsedFile;
begin
   buffer := ReadWholeFile(currentFile);
   comment := CommentParser.ParseComment(buffer);
   if comment <> nil then
   begin
      pf := TParsedFile.Create;
      pf.FileName := currentFile;
      pf.Title := GetTitle(comment.Title);

      OtherFiles.PushBack(pf);
      section := GetRawFileSection(currentFile);
      section.Open(pf.Title);
      comment.WriteOut(section);
      section.Close;
      comment.Destroy;

      pf.FileSect := section;
   end;
end;

procedure TDefaultDriver.ProcessRawTextFile;
var
   buffer : String;
   section : TSection;
   pf : TParsedFile;
begin
   buffer := ReadWholeFile(currentFile);
   pf := TParsedFile.Create;
   pf.FileName := currentFile;
   pf.Title := GetTitle('');
   OtherFiles.PushBack(pf);
   section := GetRawFileSection(currentFile);
   section.Open(pf.Title);
   section.WriteText(buffer, [saPreformatted]);
   section.Close;
   pf.FileSect := section;
end;

{ types used by TDefaultDriver.WriteTree() }
type
   TVertex = class
   public
      { the name to be displayed }
      Name : String;
      { the section of the class/interface declaration which self
        represents; may be nil if the declaration was not parsed }
      Sect : TSection;
      { a list of TVertex'es representing declarations derived from
        the declaration represented by self }
      Derived : TArrayAdt;

      constructor Create(const aname : String; asect : TSection);
      destructor Destroy; override;
   end;

   constructor TVertex.Create(const aname : String; asect : TSection);
   begin
      Sect := asect;
      Derived := TArray.Create;
      Derived.OwnsItems := false;
      Name := aname;
   end;

   destructor TVertex.Destroy;
   begin
      Derived.Free;
   end;

type
   TVertexComparer = class (TFunctor, IBinaryComparer)
   public
      function Compare(obj1, obj2 : TObject) : IndexType;
   end;

   function TVertexComparer.Compare(obj1, obj2 : TObject) : IndexType;
   begin
      Assert(obj1 is TVertex);
      Assert(obj2 is TVertex);
      if ignoreCase then
         Result := CompareText(TVertex(obj1).Name, TVertex(obj2).Name)
      else
         Result := CompareStr(TVertex(obj1).Name, TVertex(obj2).Name);
   end;

procedure TDefaultDriver.WriteTree(section : TSection; classTree : Boolean);
var
   AllVerticies : TMap;
   list : TArrayAdt;
   decl : TBasicDeclaration;
   { the map of TVertex'es not derived from anything to the names of
     their declaration's section names; automatically sorted (a sorted
     map) }
   TopVerticies : TMap;
   v, v2 : TVertex;
   vertexComparer : IBinaryComparer;

   procedure WriteList(liter : TForwardIterator);
   begin
      with section do
      begin
         if not liter.IsFinish then
         begin
            StartList(false);
            while not liter.IsFinish do
            begin
               v := TVertex(liter.Item);
               WriteLink(v.Name, v.Sect);
               Sort(v.Derived.RandomAccessStart, v.Derived.RandomAccessFinish,
                    vertexComparer);
               WriteList(v.Derived.ForwardStart);
               liter.Advance;
               if not liter.IsFinish then
                  NextListItem;
            end;
            FinishList;
         end;
         liter.Destroy;
      end;
   end;

var
   i, j : Integer;
   bdecl, bdecl2 : TBlockDeclaration;
   str : String;

begin
   Assert(section <> nil);

   AllVerticies := nil;
   list := nil;
   TopVerticies := nil;
   try
      AllVerticies := TMap.Create(THashTable.Create);
      TopVerticies := TMap.Create(TAvlTree.Create);
      TopVerticies.OwnsItems := false;

      vertexComparer := TVertexComparer.Create;

      if classTree then
         list := AllClasses
      else
         list := AllInterfaces;

      { make a directed graph in which verticies are the
        class/interface declarations and edges indicate that the
        terminal vertex is derived from the initial vertex }
      for i := list.LowIndex to list.HighIndex do
      begin
         bdecl := TBlockDeclaration(list[i]);
         { get the vertex associated with bdecl }
         v := TVertex(AllVerticies.Find(bdecl.Section.Name));
         if v = nil then
         begin
            v := TVertex.Create(bdecl.Names[0], bdecl.Section);
            AllVerticies.Insert(bdecl.Section.Name, v);
         end;

         { add v as a child of all ancestors of v (bdecl) }
         for j := bdecl.Ancestors.LowIndex to bdecl.Ancestors.HighIndex do
         begin
            decl := TBasicDeclaration(bdecl.Ancestors[j]);
            if (classTree and (decl is TClassDeclaration)) or
                  ((not classTree) and (decl is TInterfaceDeclaration)) then
            begin
               bdecl2 := TBlockDeclaration(bdecl.Ancestors[j]);
               v2 := TVertex(AllVerticies.Find(bdecl2.Section.Name));
               if v2 = nil then
               begin
                  v2 := TVertex.Create(bdecl2.Names[0], bdecl2.Section);
                  AllVerticies.Insert(bdecl2.Section.Name, v2);
               end;
               v2.Derived.PushBack(v);
            end;
         end;
         for j := 0 to bdecl.UnrecognizedAncestors.Count - 1 do
         begin
            str := bdecl.UnrecognizedAncestors[j];
            v2 := TVertex(AllVerticies.Find(str));
            if v2 = nil then
            begin
               v2 := TVertex.Create(bdecl.UnrecognizedAncestors[j], nil);
               AllVerticies.Insert(str, v2);
               TopVerticies.Insert(str, v2);
            end;
            v2.Derived.PushBack(v);
         end;

         if bdecl.Ancestors.Size + bdecl.UnrecognizedAncestors.Count = 0 then
            TopVerticies.Insert(bdecl.Section.Name, v);
      end; { end for all items in list }

      { write the tree }
      WriteList(TopVerticies.Start);

   finally
      AllVerticies.Free;
      TopVerticies.Free;
   end;
end;

procedure TDefaultDriver.GenerateClassTree;
var
   section : TSection;
begin
   WriteMessage('Generating the class tree...');

   section := DocWriter.RegisterClassTreeSection;
   with section do
   begin
      Open(GetLangString(Class_tree_str));
      WriteHeading(GetLangString(Class_tree_str), bigHeadingSize);
      NewParagraph;
      WriteTree(section, true);
      Close;
      Destroy;
   end;
end;

procedure TDefaultDriver.GenerateInterfaceTree;
var
   section : TSection;
begin
   WriteMessage('Generating the interface tree...');

   section := DocWriter.RegisterInterfaceTreeSection;
   with section do
   begin
      Open(GetLangString(Interface_tree_str));
      WriteHeading(GetLangString(Interface_tree_str), bigHeadingSize);
      NewParagraph;
      WriteTree(section, false);
      Close;
      Destroy;
   end;
end;

{ types used by GenerateSymbolIndex }
type
   TLocalDeclComparer = class (TFunctor, IBinaryComparer)
   public
      function Compare(obj1, obj2 : TObject) : IndexType;
   end;

   function TLocalDeclComparer.Compare(obj1, obj2 : TObject) : IndexType;
   begin
      Assert(obj1 is TBasicDeclaration);
      Assert(obj2 is TBasicDeclaration);
      if ignoreCase then
         Result := AnsiCompareText(TBasicDeclaration(obj1).Section.Name,
                                   TBasicDeclaration(obj2).Section.Name)
      else
         Result := AnsiCompareText(TBasicDeclaration(obj1).Section.Name,
                                   TBasicDeclaration(obj2).Section.Name);
   end;

procedure TDefaultDriver.GenerateSymbolIndex;

   function GetPath(d : TDeclaration) : String;
   begin
      Assert(not (d is TRootDeclaration));
      Result := d.Names[0];
      d := d.ParentDecl;
      while not (d is TRootDeclaration) do
      begin
         Result := d.Names[0] + LanguageParser.ScopeResolutionToken + Result;
         d := d.ParentDecl;
      end;
   end;

var
   section : TSection;
   decls : TArray;
   comparer : IBinaryComparer;
   iter : TMapIterator;
   i : Integer;
   lastDeclName : String;
   decl : TBasicDeclaration;
begin
   WriteMessage('Generating the index of symbols...');

   decls := nil;
   section := nil;
   comparer := TLocalDeclComparer.Create;
   try
      decls := TArray.Create;
      decls.OwnsItems := false;
      decls.Capacity := AllDeclarations.Size;
      iter := AllDeclarations.Start;
      while not iter.IsFinish do
      begin
         decls.PushBack(iter.Item);
         iter.Advance;
      end;
      iter.Destroy;

      Sort(decls.Start, decls.Finish, comparer);

      section := DocWriter.RegisterSymbolIndexSection;
      with section do
      begin
         Open(GetLangString(Symbol_index_str));

         WriteHeading(GetLangString(Symbol_index_str), bigHeadingSize);
         NewParagraph;

         lastDeclName := '';
         for i := decls.LowIndex to decls.HighIndex do
         begin
            decl := TBasicDeclaration(decls[i]);
            if decl is TDeclaration then
            begin
               if decl.Names[0] <> lastDeclName then
               begin
                  { if not the first iteration then finish the previous
                    list }
                  if lastdeclname <> '' then
                     FinishList;
                  lastDeclName := decl.Names[0];
                  WriteText(lastDeclName);
                  StartList(false);
               end else
                  NextListItem;
               WriteLink(GetPath(TDeclaration(decl)), decl.Section);
            end;
         end;
         if not decls.Empty then
            FinishList;

         Close;
      end;

   finally
      section.Free;
      decls.Free;
   end;
end;

procedure TDefaultDriver.GenerateContents;
var
   section : TSection;
   iter : TForwardIterator;
   pf : TParsedFile;
   decl : TDeclaration;
   widths : TIntegerArray;
begin
   WriteMessage('Generating the table of contents...');

   section := DocWriter.RegisterContentsSection;
   if ContentsTitle = '' then
      ContentsTitle := GetLangString(Table_of_contents_str);

   with section do
   begin
      Open(ContentsTitle);
      WriteHeading(ContentsTitle, unitHeadingSize);
      NewParagraph;
      if not OtherFiles.Empty then
      begin
         StartList(true);

         iter := OtherFiles.ForwardStart;
         while not iter.IsFinish do
         begin
            pf := TParsedFile(iter.Item);
            WriteLink(pf.Title, pf.FileSect);
            iter.Advance;
            if not iter.IsFinish then
               NextListItem;
         end;
         iter.Destroy;

         FinishList;
         NewParagraph;
      end;

      WriteHeading(GetLangString(Modules_str), bigHeadingSize);

      SetLength(widths, 2);
      widths[0] := 40;
      widths[1] := 60;
      StartTable(widths, 2);
      iter := Units.Start;
      while not iter.IsFinish do
      begin
         Assert(iter.Item is TDeclaration);
         decl := TDeclaration(iter.Item);
         WriteLink(decl.QualifiedName, decl.Section);
         NextCell;
         if (decl.Comment <> nil) and (decl.Comment.Synopsis <> nil) then
            decl.Comment.Synopsis.WriteOut(section);
         NextRow;
         iter.Advance;
      end;
      iter.Destroy;
      FinishTable;

      Close;
   end;
   section.Destroy;
end;

{ types used by TDriver.Run }
type
   { used by @<TDefaultDriver.Run> }
   TVertex2 = class
   public
      Name : String;
      DependsOn : TListAdt;
      { used to implement topological sort }
      Visited : Boolean;

      constructor Create(aname : String);
      destructor Destroy; override;
   end;

   constructor TVertex2.Create(aname : String);
   begin
      Name := aname;
      DependsOn := TSingleList.Create;
      DependsOn.OwnsItems := false;
      Visited := false;
   end;

   destructor TVertex2.Destroy;
   begin
      DependsOn.Free;
   end;

procedure TDefaultDriver.Run(afilequeue : TStringDequeAdt);
var
   ext : String;
   strs : TStrings;
   i : Integer;
   afile : TFileStream;
   iter : TForwardIterator;
   deque2, deque3 : TStringDequeAdt;
   ParsedFiles : TStringSet;
   FilesWithParsedDependencies : TStringSet;
   { a hash table of all verticies; maps from file names to vertex
     objects; owns the verticies }
   AllVerticies : TMap;
   decl : TDeclaration;

   procedure TopSort(vv : TVertex2);
   var
      liter : TForwardIterator;
   begin
      Assert(not vv.Visited);
      vv.Visited := true;
      liter := vv.DependsOn.ForwardStart;
      while not liter.IsFinish do
      begin
         if not TVertex2(liter.Item).Visited then
            TopSort(TVertex2(liter.Item));
         liter.Advance;
      end;
      liter.Destroy;
      deque2.PushBack(vv.Name);
   end;

   { identifies and sets an appropriate language parser for
     <currentFile>; returns true if succeeded, false if <currentFile>
     has an unknown extension }
   function IdentifyLanguageParser : Boolean;
   begin
      Result := true;
      ext := ExtractFileExt(currentFile);
      if PascalExtensions.Has(ext) then
      begin
         if not (LanguageParser is TDelphiParser) then
            SetLanguageParser(TDelphiParser.Create);
      end else
      begin
         Result := false;
      end;
      if LanguageParser <> nil then
      begin
         ignoreCase := LanguageParser.IgnoreCase;
         if ignoreCase then
            StringComparer := NoCaseStringComparer
         else
            StringComparer := StringComparer;
      end;
   end;

var
   v, p : TVertex2;

begin
   SetCommentParser(TDefaultCommentParser.Create);

   FileQueue.Free;
   FileQueue := afilequeue;

   FilesWithParsedDependencies := nil;
   ParsedFiles := nil;
   AllVerticies := nil;
   deque2 := nil;
   deque3 := nil;

   try
      { deque2 is used to store files for which dependencies have been
        parsed and whose interface is to be parsed }
      deque2 := TStringDeque.Create;
      { deque3 stores the files whose implementation only remains to
        be parsed }
      deque3 := TStringDeque.Create;
      ParsedFiles := TStringSet.Create;
      FilesWithParsedDependencies := TStringSet.Create;
      AllVerticies := TMap.Create;

      WriteMessage('');
      WriteMessage(srcdocVersion);
      WriteMessage(srcdocCopyright);
      WriteMessage('');
      WriteMessage('Parsing source files...');

      while not (FileQueue.Empty and deque2.Empty) do
      begin
         { parse dependencies }
         while not FileQueue.Empty do
         begin
            if errorCount <> 0 then
               raise ESrcdocError.Create('Too many errors.');

            { get the next file from the queue }
            recentLine := 0;
            currentFile := ValidateInputFile(FileQueue.Front);
            FileQueue.PopFront;

            if FilesWithParsedDependencies.Has(currentFile) then
               continue;
            FilesWithParsedDependencies.Insert(currentFile);

            ext := ExtractFileExt(currentFile);
            if RawCommentExtensions.Has(ext) then
            begin
               if not ParsedFiles.Has(currentFile) then
                  ProcessRawCommentFile;
               ParsedFiles.Insert(currentFile);
               continue;
            end else if RawTextExtensions.Has(ext) then
            begin
               if not ParsedFiles.Has(currentFile) then
                  ProcessRawTextFile;
               ParsedFiles.Insert(currentFile);
               continue;
            end else if not IdentifyLanguageParser then
               { unknown extension }
            begin
               Error('Error: ' + currentFile + ' - unknown extension.');
               continue;
            end;

            if (optDependencies in Options) then
            begin
               afile := CheckedOpenInputFile(currentFile);
               if afile <> nil then
               begin
                  WriteMessage('Parsing dependencies for ' + currentFile + '...');

                  { this must be executed even if strs.Count = 0 in
                    order to add the file to AllVerticies if it is not
                    already there }
                  p := TVertex2(AllVerticies.Find(currentFile));
                  if p = nil then
                  begin
                     p := TVertex2.Create(currentFile);
                     AllVerticies.Insert(currentFile, p);
                  end;

                  strs := LanguageParser.GetDependencies(afile);
                  if strs.Count <> 0 then
                  begin
                     for i := 0 to strs.Count - 1 do
                     begin
                        if ExistsInputFile(strs[i]) then
                        begin
                           FileQueue.PushBack(strs[i]);
                           v := TVertex2(AllVerticies.Find(strs[i]));
                           if v = nil then
                           begin
                              v := TVertex2.Create(strs[i]);
                              AllVerticies.Insert(strs[i], v);
                           end;
                           p.DependsOn.PushBack(v);
                        end;
                     end;
                  end;
                  strs.Free;
               end;

               if optShowImplementationInfo in Options then
               begin
                  { get the implementation dependencies; since the
                    interface part does not depend on the units used
                    in the implementation, we don't have to connect
                    the vertex associated with the current file to the
                    verticies associated with these modules; we only
                    have to parse them, not matter in what order }
                  afile := CheckedOpenInputFile(currentFile);
                  if afile <> nil then
                  begin
                     strs := LanguageParser.GetImplementationDependencies(afile);
                     Assert(strs <> nil);
                     for i := 0 to strs.Count - 1 do
                     begin
                        if ExistsInputFile(strs[i]) then
                           FileQueue.PushBack(strs[i]);
                     end;
                     strs.Free;
                  end;
               end;

            end else { not if optDependencies in Options }
               deque2.PushBack(currentFile);
         end; { end while not FileQueue.Empty }

         { perform a topological sort on the graph of modules }
         if optDependencies in Options then
         begin
            WriteMessage('Computing dependencies...');
            iter := AllVerticies.Start;
            while not iter.Isfinish do
            begin
               v := TVertex2(iter.Item);
               if not v.Visited then
                  TopSort(v);
               iter.Advance;
            end;
            iter.Destroy;
         end;

         { parse the files themselves - except for their
           implementation parts }
         while not deque2.Empty do
         begin
            if errorCount <> 0 then
               raise ESrcdocError.Create('Too many errors.');

            { get the next file from the queue }
            recentLine := 0;
            currentFile := deque2.Front;
            deque2.PopFront;
            if not ParsedFiles.Has(currentFile) then
            begin
               ParsedFiles.Insert(currentFile);
               if optShowImplementationInfo in Options then
                  deque3.PushBack(currentFile);

               WriteMessage('Parsing file ' + currentFile + '...');

               Assert (IdentifyLanguageParser);

               afile := CheckedOpenInputFile(currentFile);
               if afile <> nil then
                  LanguageParser.ParseFile(afile)
               else
                  Warn('File not found: ' + currentFile);
            end;
         end;

         { parse the implementations of the files }
         while not deque3.Empty do
         begin
            currentFile := deque3.Front;
            deque3.PopFront;

            WriteMessage('Parsing the implementation of ' + currentFile + '...');

            Assert (IdentifyLanguageParser);

            afile := CheckedOpenInputFile(currentFile);
            if afile <> nil then
               LanguageParser.ParseImplementation(afile);
         end;
      end; { end while all queues are not empty }
      if errorCount <> 0 then
         raise ESrcdocError.Create('Too many errors.');

      WriteMessage('Writing output files...');

      iter := Units.Start;
      while not iter.IsFinish do
      begin
         Assert(iter.Item is TDeclaration);

         decl := TDeclaration(iter.Item);

         WriteMessage('Writing output for ' + decl.Section.Name + '...');

         if decl is TUnitDeclaration then
         begin
            currentFile := TUnitDeclaration(decl).FileName;
            if not IdentifyLanguageParser then
               Assert(false); { should never happen }
         end;

         decl.WriteOut;
         iter.Advance;
      end;
      iter.Destroy;

      if (optGenerateClassTree in Options) and (not AllClasses.Empty) then
      begin
         GenerateClassTree;
      end;
      if (optGenerateInterfaceTree in Options) and (not AllInterfaces.Empty) then
      begin
         GenerateInterfaceTree;
      end;
      if (optGenerateSymbolIndex in Options) and (not AllDeclarations.Empty) then
      begin
         GenerateSymbolIndex;
      end;
      if optGenerateContents in Options then
      begin
         GenerateContents;
      end;

      WriteMessage('Finished.');
   finally
      deque2.Free;
      deque3.Free;
      ParsedFiles.Free;
      FilesWithParsedDependencies.Free;
      AllVerticies.Free;
   end;
end;

procedure TDefaultDriver.RegisterFile(filename : String);
begin
   if ExistsInputFile(filename) then
   begin
      FileQueue.PushBack(filename);
   end else
   begin
      Warn('File ' + filename + ' not found - skipping.',
           currentFile, recentLine);
   end;
end;

function TDefaultDriver.RegisterUnit(name : String;
                                     unitComment : String) : Boolean;
var
   unitobj : TUnitDeclaration;
   comment : TComment;
begin
   comment := CommentParser.ParseComment(unitComment);
   if CheckProfile(comment) then
   begin
      unitobj := TUnitDeclaration.Create(name, comment);
      Units.Insert(name, unitobj);
      rootDeclaration.AddDeclaration(unitobj);
      declStack.Clear;
      currDecl := unitobj;
      Result := true;
   end else
   begin
      comment.Free;
      Result := false;
   end;
end;

function TDefaultDriver.OpenUnit(name : String) : Boolean;
var
   unitobj : TUnitDeclaration;
begin
   unitobj := TUnitDeclaration(Units.Find(name));
   if unitobj <> nil then
   begin
      declStack.Clear;
      currDecl := unitObj;
      Result := true;
   end else
      Result := false;
end;

procedure TDefaultDriver.ProvideUnitInfo(ainterfaceUses : TStrings);
var
   unitobj : TUnitDeclaration;
begin
   Assert(currDecl is TUnitDeclaration);
   unitobj := TUnitDeclaration(currDecl);
   unitobj.ProvideInfo(ainterfaceUses);
end;

procedure TDefaultDriver.ProvideUnitImplementationInfo(aimplUses : TStrings;
                                                       aunitImplComment : String);
var
   unitobj : TUnitDeclaration;
begin
   Assert(currDecl is TUnitDeclaration);
   unitobj := TUnitDeclaration(currDecl);
   unitobj.ProvideImplementationInfo(aimplUses,
                                     CommentParser.ParseComment(aunitImplComment));
end;

procedure TDefaultDriver.
   RegisterDeclarations(adecl : TTextObject; asymbols : TStrings;
                        args : TStrings; avisibility : String;
                        avisibilityType : TVisibilityType;
                        asymboltype : String; alinenum : Integer;
                        acomment : String);
var
   declObj : TDeclaration;
   commentObj : TComment;
   section : TSection;

   procedure FreeObjects;
   begin
      adecl.Free;
      asymbols.Free;
   end;

begin
   Assert(asymbols <> nil);
   Assert(asymbols.Count <> 0);
   Assert(adecl <> nil);
   Assert(currDecl <> nil);

   if ((avisibilityType = vtPrivate) and
          not (optShowPrivate in Options)
      ) or
         ((avisibilityType = vtProtected) and
             not (optShowProtected in Options)
         ) then
   begin
      FreeObjects;
      args.Free;
   end else
   begin
      { ignore args, for now }
      args.Free;

      recentLine := alinenum;
      commentObj := GetCommentObject(asymbols[0], acomment);
      if ignoreDecl = 0 then
      begin
         if CheckProfile(commentObj) then
         begin
            section := GetSection(asymbols[0], false);
            declobj := TDeclaration.Create(adecl, asymbols, avisibility,
                                           avisibilityType,
                                           asymboltype, alinenum, commentObj,
                                           section, currDecl);
            currDecl.AddDeclaration(declobj);
         end else
         begin
            FreeObjects;
            commentObj.Free;
         end;
      end else
      begin
         Dec(ignoreDecl);
         { includeDecl was decreased in GetCommentObject, but now it
           appeared that the comment will not be used }
         Inc(includeDecl);
         recentComment.Free;
         recentComment := commentObj;
         FreeObjects;
      end;
   end;
end;

procedure TDefaultDriver.StartClass(decl : TTextObject; name : String;
                                    ancestors : TStrings; interfaces : TStrings;
                                    visibility : String;
                                    visibilityType : TVisibilityType;
                                    symboltype : String;
                                    linenum : Integer; comment : String);
var
   classobj : TClassDeclaration;
   section : TSection;
   commentObj : TComment;

   procedure FreeObjects;
   begin
      decl.Free;
      ancestors.Free;
      interfaces.Free;
   end;

begin
   Assert(name <> '');
   recentLine := linenum;
   commentObj := GetCommentObject(name, comment);
   if ignoreDecl = 0 then
   begin
      if CheckProfile(commentObj) then
      begin
         section := GetSection(name, true);
         classobj := TClassDeclaration.Create(decl, name, ancestors, interfaces,
                                              visibility, visibilityType,
                                              symboltype, lineNum, commentObj,
                                              section, currDecl);
         AllClasses.PushBack(classobj);
         StartBlock(classobj);
      end else
      begin
         FreeObjects;
         commentObj.Free;
      end;
   end else
   begin
      Dec(ignoreDecl);
      { includeDecl was decreased in GetCommentObject, but now it
        appeared that the comment will not be used }
      Inc(includeDecl);
      recentComment.Free;
      recentComment := commentObj;
      FreeObjects;
   end;
end;

procedure TDefaultDriver.FinishClass;
begin
   EndBlock;
end;

procedure TDefaultDriver.StartInterface(decl : TTextObject; aname : String;
                                        ancestors : TStrings; id : String;
                                        visibility : String;
                                        visibilityType : TVisibilityType;
                                        symboltype : String;
                                        lineNum : Integer; comment : String);
var
   obj : TInterfaceDeclaration;
   section : TSection;
   commentObj : TComment;

   procedure FreeObjects;
   begin
      decl.Free;
      ancestors.Free;
   end;

begin
   Assert(aname <> '');
   recentLine := linenum;
   commentObj := GetCommentObject(aname, comment);
   if ignoreDecl = 0 then
   begin
      if CheckProfile(commentObj) then
      begin
         section := GetSection(aname, true);
         obj := TInterfaceDeclaration.Create(decl, aname, ancestors, id,
                                             visibility, visibilityType,
                                             symboltype, lineNum,
                                             commentObj, section, currDecl);
         AllInterfaces.PushBack(obj);
         StartBlock(obj);
      end else
      begin
         FreeObjects;
         commentObj.Free;
      end;
   end else
   begin
      Dec(ignoreDecl);
      { includeDecl was decreased in GetCommentObject, but now it has
        appeared that the comment will not be used }
      Inc(includeDecl);
      recentComment.Free;
      recentComment := commentObj;
      FreeObjects;
   end;
end;

procedure TDefaultDriver.FinishInterface;
begin
   EndBlock;
end;

procedure TDefaultDriver.SetIgnoreDeclarations(num : Cardinal);
begin
   ignoreDecl := num;
end;

function TDefaultDriver.FindSection(name : String) : TSection;
var
   decl : TBasicDeclaration;
begin
   decl := currDecl.SearchDeclaration(name);
   if decl <> nil then
      Result := decl.Section
   else
      Result := nil;
end;

procedure TDefaultDriver.Error(msg, filename : String; linenum : Cardinal);
begin
   filename := ExtractBaseName(filename);
   GiveError(filename + ':' + IntToStr(linenum) + ': Error: ' + msg);
end;

procedure TDefaultDriver.Error(msg, filename : String);
begin
   filename := ExtractBasename(filename);
   GiveError(filename + ': Error: ' + msg);
end;

procedure TDefaultDriver.Error(msg : String);
begin
   GiveError(' Error: ' + msg);
end;

procedure TDefaultDriver.CommentError(msg : String);
begin
   Error(msg, currentFile, recentLine);
end;

procedure TDefaultDriver.Warn(msg : String);
begin
   if optShowWarnings in Options then
      WriteLn('srcdoc: Warning: ' + msg);
end;

procedure TDefaultDriver.Warn(msg, filename : String; line : Integer);
begin
   if optShowWarnings in Options then
      WriteLn('srcdoc:' + ExtractBaseName(filename) + ':' + IntToStr(line) +
                 ': Warning: ' + msg);
end;

procedure TDefaultDriver.CommentWarn(msg : String);
begin
   if optShowWarnings in Options then
      WriteLn('srcdoc:' + currentFile + ':' + IntToStr(recentLine) +
                 ': Warning: ' + msg);
end;

procedure TDefaultDriver.WriteMessage(msg : String);
begin
   if optVerbose in Options then
      WriteLn(msg);
end;

initialization
   DeclarationComparer := TDeclarationComparer.Create;

end.
