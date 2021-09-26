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

unit htmlwriter;

{ this unit provides a class for creating html documents }

interface

uses
   SysUtils, Classes, commonclasses;

type
   THtmlSection = class (TSection)
   private
      { the path to the section - i.e. the section name in the html
        file with # prepended or the name of the file itself if it is
        a file section; }
      FPath : String;
      { stores the end tag appropriate for the current list }
      endListTag : String;
{$ifdef DEBUG }
      canWrite, wasOpened : Boolean;
{$endif } 
      currAttrs : TTextAttributeSet;
      
      procedure BreakLine;
      procedure WriteString(str : String);
      procedure WriteLnString(str : String);
      
      procedure CloseTextAttrsNotIn(attr : TTextAttributeSet);
      procedure OpenTextAttrsIn(attr : TTextAttributeSet);
      
   protected
      { returns the stream to which the section should be written }
      function Stream : TFileStream; virtual; abstract;
      
   public
      constructor Create(aname, asectionPath : String);
      
      { writes out <text>; <attr> are the attributes of the text;
        <highlight> is true if the text should be highlighted }
      procedure WriteText(text : String;
                          attr : TTextAttributeSet); override;
      { writes out a link to a section; if <section> is nil then
        writes it as a plain text; uses <descr> as a label for the
        link }
      procedure WriteLink(descr : String; section : TSection); override;
      { writes a heading; size may be a number 1 through 6, where 1 is
        the largest, 6 the smallest; a heading begins a new paragraph }
      procedure WriteHeading(const text : String; size : TSize); override;
      { begins a new paragraph }
      procedure NewParagraph; override;
      { begins a new line }
      procedure NewLine; override;
      { makes an indentation }
      procedure Indent; override;
      { starts writing a table; cols is the number of columns; widths
        should contain cols entries with the width of each column (in
        the percentage of the width of the whole table); after calling
        this routine the next output will be written in the first cell
        of the table }
      procedure StartTable(const widths : TIntegerArray;
                           cols : Integer); override;
      { moves to the next cell in the current row in the table being
        currently writed out; if there is no next cell in the current
        row then does nothing }
      procedure NextCell; override;
      { starts a new row }
      procedure NextRow; override;
      { finishes writing a table; if the last row contains no data
        then it is discarded }
      procedure FinishTable; override;
      { starts writing a list; the list is numbered if <numbered> is
        true }
      procedure StartList(numbered : Boolean); override;
      { starts writing the next item in the current list }
      procedure NextListItem; override;
      { finished writing a list;  }
      procedure FinishList; override;
      { suppresses automatic line breaking (not for all formats) }
      procedure SuppressLineBreaking; override;
      { resumes automatic line breaking (not for all formats) }      
      procedure ResumeLineBreaking; override;
   end;
   
   THtmlFileSection = class (THtmlSection)
   private
      { the stream associated with the section; it _is_ owned by this
        object }
      FStream : TFileStream;
      
   protected
      function Stream : TFileStream; override;
      
   public
      constructor Create(aname : String; parent : TSection);
      destructor Destroy; override;
      
      procedure Open(title : String); override;
      procedure Close; override;      
   end;
   
   THtmlSubSection = class (THtmlSection)
   private
      { the stream to which self should write; _not_ owned by self;
        owned by an appropriate THtmlFileSection }
      FStreamCache : TFileStream;
      
   protected
      function Stream : TFileStream; override;
      
   public
      constructor Create(aname : String; parent : TSection);
      
      procedure Open(title : String); override;
      procedure Close; override;      
   end;
   
   THtmlContentsSection = class (THtmlFileSection)
   public
      procedure Open(title : String); override;
   end;
   
   THtmlWriter = class (TDocWriter)
   public
      constructor Create(aoutputDir : String);
      function RegisterSection(name : String; parent : TSection;
                               big : Boolean) : TSection; override;
      function RegisterContentsSection : TSection; override;
      function RegisterClassTreeSection : TSection; override;
      function RegisterInterfaceTreeSection : TSection; override;
      function RegisterSymbolIndexSection : TSection; override;
   end;
   
implementation

uses
   adtstralgs, languages;

const
   newlineStr = #10;
   cssFile = 'default.css';
   htmlSectionSeparator = '__-';
   
var   
   { a map of characters that must be replaced with character
     entities to those entities }
   badChars : array[#0..#255] of String;
   { the widths of the current table }
   currTableWidths : TIntegerArray;
   { the current column in the current table }
   currTableCol : Integer;
   { the output directory; after setting once should be read only }
   outputDir : String;
   { indicate whether there are a class tree, interface tree and the
     symbol index written }
   isclasstree, isInterfaceTree, isSymbolIndex : Boolean;
   
{ ----------------------- helper routines ---------------------------------- }
   
procedure WriteLnToStream(s : TStream; str : String);
begin
   if str <> '' then
   begin
      str := str + newlineStr;
      s.WriteBuffer(str[1], Length(str));
   end;
end;

function ConvertBadChars(const str : String) : String;
var
   i : Integer;
begin
   Result := '';
   for i := 1 to Length(str) do
   begin
      if badChars[str[i]] = '' then
         Result := Result + str[i]
      else
         Result := Result + badChars[str[i]];
   end;
end;

{ --------------------------- THtmlSection ------------------------------ }

constructor THtmlSection.Create(aname, asectionPath : String);
begin
   inherited Create(aname);
   FPath := asectionPath;
{$ifdef DEBUG }
   canWrite := false;
   wasOpened := false;
{$endif }
   currAttrs := [];
end;

procedure THtmlSection.BreakLine;
var
   buf : String;
begin
   buf := newlineStr;
   stream.WriteBuffer(buf[1], Length(buf));
end;

procedure THtmlSection.WriteString(str : String);
begin
   if str = '' then
      Exit;
   stream.WriteBuffer(str[1], Length(str));
end;

procedure THtmlSection.WriteLnString(str : String);
begin
   str := str + newlineStr;
   stream.WriteBuffer(str[1], Length(str));
end;

procedure THtmlSection.CloseTextAttrsNotIn(attr : TTextAttributeSet);
begin   
   if not (saEmphasis in attr) and (saEmphasis in currAttrs) then
      WriteString('</em>');
   if not (saKeyword in attr) and (saKeyword in currAttrs) then
      WriteString('</span>');
   if not (saLocalSymbol in attr) and (saLocalSymbol in currAttrs) then
      WriteString('</span>');
   if not (saGlobalSymbol in attr) and (saGlobalSymbol in currAttrs) then
      WriteString('</span>');
   if not (saCode in attr) and (saCode in currAttrs) then
      WriteString('</span>');
end;

procedure THtmlSection.OpenTextAttrsIn(attr : TTextAttributeSet);
begin
   if (saCode in attr) and not (saCode in currAttrs) then
      WriteString('<span class="program_code">');
   if (saGlobalSymbol in attr) and not (saGlobalSymbol in currAttrs) then
      WriteString('<span class="global_symbol">');
   if (saLocalSymbol in attr) and not (saLocalSymbol in currAttrs) then
      WriteString('<span class="local_symbol">');
   if (saKeyword in attr) and not (saKeyword in currAttrs) then
      WriteString('<span class="keyword">');
   if (saEmphasis in attr) and not (saEmphasis in currAttrs) then
      WriteString('<em>');
end;

procedure THtmlSection.WriteText(text : String; attr : TTextAttributeSet);
begin
   text := ConvertBadChars(text);
   
   CloseTextAttrsNotIn(attr);
   OpenTextAttrsIn(attr);
   
   if saPreformatted in attr then
   begin
      BreakLine;
      WriteLnString('<pre>');
      stream.WriteBuffer(text[1], Length(text));
      BreakLine;
      WriteLnString('</pre>');
   end else if saGlobalSymbol in attr then
   begin
      WriteLink(text, Driver.FindSection(text));
   end else
   begin
      WriteString(text);
   end;
end;

procedure THtmlSection.WriteLink(descr : String; section : TSection);
var
   str : String;
begin
   Assert((section = nil) or (section is THtmlSection));
   str := ConvertBadChars(descr);
   if section <> nil then
   begin
      WriteString('<a href="' + THtmlSection(section).FPath +
                     '">' + str + '</a>')
   end else
   begin
      { if not found write as a plain text }
      WriteString(str);
   end;
end;

procedure THtmlSection.WriteHeading(const text : String; size : TSize);
var
   sizeStr : String;
begin
   sizeStr := IntToStr(size);
   BreakLine;
   WriteLnString('<h' + sizeStr + '>' + ConvertBadChars(text) +
                    '</h' + sizeStr +  '>');
end;

procedure THtmlSection.NewParagraph;
begin
   BreakLine;
   WriteLnString('</p><p>');
end;

procedure THtmlSection.NewLine;
begin
   BreakLine;
   WriteLnString('<br>');
end;

procedure THtmlSection.Indent;
begin
   BreakLine;
   WriteLnString('&nbsp;&nbsp;&nbsp;');
end;

procedure THtmlSection.StartTable(const widths : TIntegerArray;
                                  cols : Integer);
begin
   currTableWidths := widths;
   BreakLine;
   WriteLnString('</p>');
   WriteString('<table cols="' + IntToStr(cols) + '" ');
   if optTableBorder in Driver.Options then
      WriteString('border="1" ');
   WriteLnString('width="100%">');
   WriteLnString('<tr>');
   WriteLnString('<td width="' + IntToStr(currTableWidths[0]) + '%">');
   WriteLnString('<p>');
   currTableCol := 1;
end;

procedure THtmlSection.NextCell;
begin
   BreakLine;
   WriteLnString('</p>');
   WriteLnString('</td>');
   WriteLnString('<td width="' + IntToStr(currTableWidths[currTableCol]) + '%">');
   WriteLnString('<p>');
   Inc(currTableCol);
end;

procedure THtmlSection.NextRow;
begin
   BreakLine;
   WriteLnString('</p>');
   WriteLnString('</td>');
   WriteLnString('</tr>');
   WriteLnString('<tr>');
   WriteLnString('<td width="' + IntToStr(currTableWidths[0]) + '%">');
   WriteLnString('<p>');
   currTableCol := 1;
end;

procedure THtmlSection.FinishTable;
begin
   BreakLine;
   WriteLnString('</p>');
   WriteLnString('</td>');
   WriteLnString('</tr>');
   WriteLnString('</table>');
   WriteLnString('<p>');
   currTableCol := 0;
   currTableWidths := nil;
end;

procedure THtmlSection.StartList(numbered : Boolean);
begin
   WriteLnString('</p>');
   WriteLnString('<p>');
   if numbered then
   begin
      WriteLnString('<ol>');
      endListTag := '</ol>';
   end else
   begin
      WriteLnString('<ul>');
      endListTag := '</ul>';
   end;
   WriteLnString('<li>');
end;

procedure THtmlSection.NextListItem;
begin
   BreakLine;
   WriteLnString('</li>');
   WriteLnString('<li>');
end;

procedure THtmlSection.FinishList;
begin
   BreakLine;
   WriteLnString('</li>');
   WriteLnString(endListTag);
end;

procedure THtmlSection.SuppressLineBreaking;
begin
   BreakLine;
   WriteLnString('<nobr>');
end;

procedure THtmlSection.ResumeLineBreaking;
begin
   WriteLnString('</nobr>');
end;


{ ------------------------ THtmlFileSection ------------------------------ }

constructor THtmlFileSection.Create(aname : String; parent : TSection);
var
   afile, ppath : String;
begin
   aname := Trim(ExtractBasename(aname));
   afile := ReplaceAll(aname, ['.'], '_') + '.html';
   if parent <> nil then
   begin
      Assert(parent is THtmlSection);
      ppath := THtmlSection(parent).FPath;
      { afile := basename of the parent file (without extension) +
        separator + the section name + .html }
      afile := ExtractBasename(ppath) + htmlSectionSeparator + afile;
   end;
   inherited Create(aname, afile);
   FStream := TFileStream.Create(outputDir + afile, fmCreate);
end;

destructor THtmlFileSection.Destroy;
begin
   FStream.Free;
   inherited;
end;

function THtmlFileSection.Stream : TFileStream; 
begin
{$ifdef DEBUG }   
   Assert(canWrite);
{$endif }
   Result := FStream;
end;

procedure THtmlFileSection.Open(title : String);
begin
{$ifdef DEBUG }
   Assert(not wasOpened);
{$endif }
{$ifdef DEBUG }
   canWrite := true;
   wasOpened := true;
{$endif }
   WriteLnString('<html>');
   WriteLnString('<head>');
   WriteLnString('<title>' + title + '</title>');
   WriteLnString('<meta http-equiv="content-type" content="text/html; charset=' +
                    LangCharsetName + '">');
   WriteLnString('<meta name="generator" content="' + srcdocVersion + '">');
   WriteLnString('<link rel="stylesheet" type="text/css" href="' +
                    cssFile + '">');
   WriteLnString('</head>');
   WriteLnString('<body>');
   WriteLnString('<p>');   
end;

procedure THtmlFileSection.Close;      
begin
{$ifdef DEBUG }   
   Assert(wasOpened and canWrite);
{$endif }
   BreakLine;
   WriteLnString('</p>');   
   WriteLnString('</body>');
   WriteLnString('</html>');
{$ifdef DEBUG }
   canWrite := false;
{$endif }
end;

{ ------------------------ THtmlSubsection ----------------------------- }

constructor THtmlSubsection.Create(aname : String; parent : TSection);
var
   str : String;
   parentSect : THtmlSection;
{$ifdef DEBUG }
   savedCanWrite : Boolean;
{$endif }
begin
   Assert(parent <> nil);
   Assert(parent is THtmlSection);
   parentSect := THtmlSection(parent);
   aname := Trim(aname);
   if parent is THtmlFileSection then
      str := parentSect.FPath + '#' + aname
   else
      str := parentSect.FPath + htmlSectionSeparator + aname;
   inherited Create(aname, str);
   { the stream here is only cached; should not be disposed }
{$ifdef DEBUG }
   savedCanWrite := parentSect.canWrite;
   parentSect.canWrite := true;
{$endif }
   FStreamCache := parentSect.Stream;
{$ifdef DEBUG }
   parentSect.canWrite := savedCanWrite;
{$endif }
end;

function THtmlSubsection.Stream : TFileStream; 
begin
{$ifdef DEBUG }   
   Assert(canWrite);
{$endif }
   Result := FStreamCache;
end;

procedure THtmlSubsection.Open(title : String);
var
   path : String;
   i : Integer;
begin
{$ifdef DEBUG }   
   Assert(not wasOpened);
{$endif }
{$ifdef DEBUG }
   canWrite := true;
   wasOpened := true;
{$endif }
   i := Pos('#', FPath);
   path := ConvertBadChars(Copy(FPath, i + 1, Length(FPath) - i));
   BreakLine;
   WriteLnString('</p><p>');
   WriteLnString('<a name="' + path + '"><hr></a>');
end;

procedure THtmlSubsection.Close;      
begin
{$ifdef DEBUG }   
   Assert(wasOpened and canWrite);
{$endif }
{$ifdef DEBUG }
   canWrite := false;
{$endif }
end;

{ ----------------------- THtmlContentsSection ----------------------- }

procedure THtmlContentsSection.Open(title : String);
var
   f : TFileStream;
begin
   inherited;
   { create the index file }
   f := TFileStream.Create(outputDir + 'index.html', fmCreate);
   WriteLnToStream(f, '<html>');
   WriteLnToStream(f, '<head>');
   WriteLnToStream(f, '<title>' + title + '</title>');
   WriteLnToStream(f, '<link rel="stylesheet" type="text/css" href="' +
                         cssFile + '">');
   WriteLnToStream(f, '</head>');
   WriteLnToStream(f, '<frameset rows="90%,10%">');
   WriteLnToStream(f, '<frame src="srcdoc_autogenerated_contents.html" name="srcdoc_main">');
   WriteLnToStream(f, '<frame src="srcdoc_autogenerated_navigation.html" name="srcdoc_nav">');
   WriteLnToStream(f, '<noframes>');
   WriteLnToStream(f, 'Your browser does not support frames.<br>');
   WriteLnToStream(f, '<a href="srcdoc_autogenerated_contents.html">Contents</a><br>');
   WriteLnToStream(f, '<a href="srcdoc_autogenerated_navigation.html">Navigation panel</a>');
   WriteLnToStream(f, '</noframes>');
   WriteLnToStream(f, '</frameset>');
   WriteLnToStream(f, '</html>');
   f.Free;
   
   { create the navigation file  }
   f := TFileStream.Create(outputDir + 'srcdoc_autogenerated_navigation.html',
                           fmCreate);
   WriteLnToStream(f, '<html>');
   WriteLnToStream(f, '<head>');
   WriteLnToStream(f, '<title>' + title + '</title>');
   WriteLnToStream(f, '<base target="srcdoc_main">');
   WriteLnToStream(f, '<link rel="stylesheet" type="text/css" href="' +
                         cssFile + '">');
   WriteLnToStream(f, '</head>');
   WriteLnToStream(f, '<body>');
   WriteLnToStream(f, '<table width="100%" border="0">');
   WriteLnToStream(f, '<tr>');
   WriteLnToStream(f, '<td><p><a href="srcdoc_autogenerated_contents.html">' +
                         GetLangString(Contents_str) + '</a></p></td>');
   if isClassTree then
   begin
      WriteLnToStream(f, '<td><p><a href="srcdoc_autogenerated_class_tree.html">' +
                            GetLangString(Class_tree_str) + '</a></p></td>');
   end;
   if isInterfaceTree then
   begin
      WriteLnToStream(f, '<td><p><a href="srcdoc_autogenerated_interface_tree.html">'+
                            GetLangString(Interface_tree_str) + '</a></p></td>');
   end;
   if isSymbolIndex then
   begin
      WriteLnToStream(f, '<td><p><a href="srcdoc_autogenerated_symbol_index.html">' +
                            GetLangString(Symbol_index_str) + '</a></p></td>');
   end;
   WriteLnToStream(f, '</tr>');
   WriteLnToStream(f, '</table>');
   WriteLnToStream(f, '</body>');
   f.Free;
end;
 
{ --------------------------- THtmlWriter --------------------------- }

constructor THtmlWriter.Create(aoutputDir : String);
var
   i : Integer;
   stream : TFileStream;
begin
   inherited;
   
   for i := 0 to 255 do
      badChars[Char(i)] := '';
   badChars['"'] := '&quot;';
   badChars['&'] := '&amp;';
   badChars['<'] := '&lt;';
   badChars['>'] := '&gt;';
   
   outputDir := OutputDirectory;
   
   { create the .css file }
   stream := TFileStream.Create(outputDir + cssFile, fmCreate);
   WriteLnToStream(stream, '.program_code {font-family : monospace}');
   WriteLnToStream(stream, '.keyword {color : turquoise}');
   WriteLnToStream(stream, '.local_symbol {text-decoration : underline}');
   WriteLnToStream(stream, '.fixed_pitch {font-family : monospace}');
   WriteLnToStream(stream, 'ul li {list-style : disc}');
   WriteLnToStream(stream, 'ul ul li {list-style : circle}');
   WriteLnToStream(stream, 'ul ul ul li {list-style : square}');
   WriteLnToStream(stream, 'ul ul ul ul li {list-style : disc}');
   WriteLnToStream(stream, 'ul ul ul ul ul li {list-style : circle}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul li {list-style : square}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul li {list-style : disc}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul ul li {list-style : circle}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul ul ul li {list-style : square}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul ul ul ul li {list-style : disc}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul ul ul ul ul li {list-style : circle}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul ul ul ul ul ul li {list-style : square}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul ul ul ul ul ul ul li {list-style : disc}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul ul ul ul ul ul ul ul li {list-style : circle}');
   WriteLnToStream(stream, 'ul ul ul ul ul ul ul ul ul ul ul ul ul ul ul li {list-style : square}');
   stream.Destroy;
end;

function THtmlWriter.RegisterSection(name : String; parent : TSection;
                                     big : Boolean) : TSection;
begin
   name := Trim(name);
   if (parent = nil) or big then
      Result := THtmlFileSection.Create(name, parent)
   else
      Result := THtmlSubsection.Create(name, parent);
end;

function THtmlWriter.RegisterContentsSection : TSection;
begin
   Result := THtmlContentsSection.Create('srcdoc_autogenerated_contents', nil);
end;

function THtmlWriter.RegisterClassTreeSection : TSection;
begin
   Result := THtmlContentsSection.Create('srcdoc_autogenerated_class_tree', nil);
   isClassTree := true;
end;

function THtmlWriter.RegisterInterfaceTreeSection : TSection;
begin
   Result := THtmlContentsSection.Create('srcdoc_autogenerated_interface_tree', nil);
   isInterfaceTree := true;
end;

function THtmlWriter.RegisterSymbolIndexSection : TSection;
begin
   Result := THtmlContentsSection.Create('srcdoc_autogenerated_symbol_index', nil);
   isSymbolIndex := true;   
end;

end.
