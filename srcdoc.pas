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

program srcdoc;

{$apptype console }

uses
   SysUtils, Classes, Getopts, adtqueue,
   commonclasses, defdriver, htmlwriter, languages;

const
   skipSeparators = [optIgnoreHandMadeSeparators,
                     optDiscardParagraphOnHandMadeSeparators];
   joinComments = [optJoinComments, optNewParagraphBeforeJoinedComment];
   srcdoc_rawCommentParser = [optLinkPlurals, optMakeSynopsis];
   srcdocCommentParser = [optExplicitLocalSymbols, optEmphasis] +
      skipSeparators + srcdoc_rawCommentParser;
   beautifyComments = [optBeautifyComments];
   verboseOptions = [optVerbose, optShowWarnings];
   otherOptions = [optTableBorder, optShowWarnings,
                   optFetchCommentsFromAncestors, optGenerateClassTree,
                   optGenerateInterfaceTree, optGenerateSymbolIndex,
                   optGenerateContents] + joinComments;
   userProfile = [optShowProtected];
   developerProfile = userProfile + [optShowPrivate, optShowImplementationInfo];
   VERSION_CHAR = '0';
   PROTECTED_CHAR = '1';
   PRIVATE_CHAR = '2';
   SKIP_SEPARATORS_CHAR = '3';
   SYNOPSIS_CHAR = '4';
   FETCH_COMMENTS_CHAR = '5';
   TABLE_BORDER_CHAR = '6';
   
var   
   writer : TDocWriter;
   deque : TStringDeque;
   sourceDirectories : TStrings;
   options : TOptions;
   title, profileStr : String;
   
procedure CleanExit(exitCode : Byte);
begin
   writer.Free;
   sourceDirectories.Free;
   deque.Free;
   Halt(exitCode);
end;

{ used to exit when an invalid command line parameter specified }   
procedure ErrorExit;
begin
   CleanExit(3);
end;

procedure PrintUsage;
begin
   WriteLn('Usage: srcdoc [options] files_to_parse');
   WriteLn('For help type srcdoc --help');
   ErrorExit;
end;

procedure PrintHelp;
begin
   WriteLn('Usage: srcdoc [options] files_to_parse');
   WriteLn('Creates documentation of programs by parsing comments in source files.');
   WriteLn;
   WriteLn('Mandatory options to long arguments are mandatory for short options, too');
   WriteLn('-o, --output-directory=DIR      Sets the output directory to DIR.');
   WriteLn('-s, --source-directory=DIR      Adds DIR to the list of directories searched');
   WriteLn('                                for source files.');
   WriteLn('-f, --format=FORMAT             Sets the output format to FORMAT. For now,');
   WriteLn('                                FORMAT may be only ''html''.');
   WriteLn('-l, --language=LANG             Sets the (human) language for output files.');
   WriteLn('                                For now, LANG may be only ''en'' or ''pl''.');
   WriteLn('-p, --profile=PROF              Sets the output profile. PROF may be either');
   WriteLn('                                ''user'' (default) or ''devel''. In the');
   WriteLn('                                ''user'' profile only globally visible');
   WriteLn('                                declarations and only public and protected');
   WriteLn('                                class members are included. In the ''devel''');
   WriteLn('                                profile local declarations and private members');
   WriteLn('                                are also included. You may override this');
   WriteLn('                                with --protected, --private and -i.');
   WriteLn('-c, --comment-parser=PARSER     Sets the comment parser. PARSER may be');
   WriteLn('                                ''srcdoc'' (default) or ''srcdoc_raw''. The');
   WriteLn('                                raw parser just does not support several');
   WriteLn('                                features and thus leaves things like <name>');
   WriteLn('                                or _name_ intact.');
   WriteLn('-d, --dependencies              Try find and parse the modules (units) on');
   WriteLn('                                which a given module depends first.');
   WriteLn('-b, --beautify-comments         ''Beautify'' the comments. When beautifying');
   WriteLn('                                all '';'' are changed to dots and all words');
   WriteLn('                                beginning a sentence are capitalized.');
   WriteLn('-t, --title=TITLE               Sets the title for the whole documentation.');
   WriteLn('                                This should be the name of the program or');
   WriteLn('                                the library being documented.');
   WriteLn('-i, --implementation-info=on/off');
   WriteLn('                                Indicates whether to include local declarations');
   WriteLn('                                in the generated documentation.');
   WriteLn('--protected=on/off              Indicates whether to show protected members.');
   WriteLn('--private=on/off                Indicates whether to show private members.');
   WriteLn('--table-border=on/off           Indicates whether to draw borders for tables.');
   WriteLn('                                On by default.');
   WriteLn('--skip-separators=on/off        If on then any sequence consisting of one');
   WriteLn('                                non-alphanumeric character repeated more than');
   WriteLn('                                7 times is skipped. On by default.');
   WriteLn('-j, --join-comments=on/off      If on several comments one after another,');
   WriteLn('                                possibly separated with whitespace, are');
   WriteLn('                                joined into one comment. On by default.');
   WriteLn('--fetch-comments=on/pff         Indicates whether to fetch comments from');
   WriteLn('                                ancestor classes if no comment is present');
   WriteLn('                                for a given member.');
   WriteLn('--synopsis=on/off               If on then if a comment does not contain the');
   WriteLn('                                @synopsis command, the first sentence of the');
   WriteLn('                                comment is assumed to be the synopsis.');
   WriteLn('-v, --verbose                   Be verbose.');
   WriteLn('-q, --quiet, --silent           Suppress warnings.');
   WriteLn('-h, --help                      Show this screen.');
   WriteLn('--version                       Show the version information.');
   CleanExit(0);
end;

procedure PrintVersion;
begin
   WriteLn(srcdocVersion);
   WriteLn(srcdocCopyright);
   CleanExit(0);
end;

{ should set the <writer>, <title> and <options> variables }
procedure ParseCommandLine;
const
   OPTS_NUM = 22;
var
   opts : array[0..OPTS_NUM] of getopts.TOption;
   commentParser, profile, exclude_options : commonclasses.TOptions;
   unused : Integer;
   outputDirectory : String;
   c : Char;
   i : Integer;
begin   
   with opts[OPTS_NUM] do
   begin
      name := '';
      has_arg := 0;
      flag := nil;
      value := #0;
   end;
   with opts[0] do
   begin
      name := 'language';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 'l';
   end;
   with opts[1] do
   begin
      name := 'source-directory';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 's';
   end;
   with opts[2] do
   begin
      name := 'output-directory';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 'o';
   end;
   with opts[3] do
   begin
      name := 'comment-parser';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 'c';
   end;
   with opts[4] do
   begin
      name := 'format';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 'f';
   end;
   with opts[5] do
   begin
      name := 'profile';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 'p';
   end;
   with opts[6] do
   begin
      name := 'title';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 't';
   end;
   
   { options without args } 
   with opts[7] do
   begin
      name := 'dependencies';
      has_arg := 0;
      flag := nil;
      value := 'd';
   end;
   with opts[8] do
   begin
      name := 'beautify-comments';
      has_arg := 0;
      flag := nil;
      value := 'b';
   end;
   with opts[9] do
   begin
      name := 'help';
      has_arg := 0;
      flag := nil;
      value := 'h';
   end;
   with opts[10] do
   begin
      name := 'version';
      has_arg := 0;
      flag := nil;
      value := VERSION_CHAR;
   end;   
   
   { on/off options  }
   with opts[11] do
   begin
      name := 'table-border';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := TABLE_BORDER_CHAR;
   end;
   with opts[12] do
   begin
      name := 'implementation-info';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 'i';
   end;
   with opts[13] do
   begin
      name := 'join-comments';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := 'j';
   end;
   with opts[14] do
   begin
      name := 'protected';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := PROTECTED_CHAR;
   end;
   with opts[15] do
   begin
      name := 'private';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := PRIVATE_CHAR;
   end;
   with opts[16] do
   begin
      name := 'skip-separators';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := SKIP_SEPARATORS_CHAR;
   end;
   with opts[17] do
   begin
      name := 'synopsis';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := SYNOPSIS_CHAR;
   end;
   with opts[18] do
   begin
      name := 'fetch-comments';
      has_arg := REQUIRED_ARGUMENT;
      flag := nil;
      value := FETCH_COMMENTS_CHAR;
   end;
   
   { no args again }
   with opts[19] do
   begin
      name := 'verbose';
      has_arg := 0;
      flag := nil;
      value := 'v';
   end;
   
   with opts[20] do
   begin
      name := 'silent';
      has_arg := 0;
      flag := nil;
      value := 'q';
   end;
   
   with opts[21] do
   begin
      name := 'quiet';
      has_arg := 0;
      flag := nil;
      value := 'q';
   end;
   
   commentParser := srcdocCommentParser;
   profile := userProfile;
   profileStr := 'user';
   exclude_options := [];
   
   options := otherOptions;
   writer := nil;
   deque := TStringDeque.Create();
   sourceDirectories := TStringList.Create;
   sourceDirectories.Add('.');
   outputDirectory := '.';
   title := '';
   
   repeat
      c := GetLongOpts('l:s:o:c:f:p:dbht:i:j:vq', @opts[0], unused);
      case c of
         'l':
            begin
               if not SetLanguage(OptArg) then
                  PrintUsage;
            end;
         's':
            begin
               sourceDirectories.Add(OptArg);
            end;
         'o':
            begin
               outputDirectory := OptArg;
            end;
         'c':
            begin
               if OptArg = 'srcdoc' then
                  commentParser := srcdocCommentParser
               else if OptArg = 'srcdoc_raw' then
                  commentParser := srcdoc_rawCommentParser
               else
                  PrintUsage;
            end;
         'f':
            begin
               if OptArg <> 'html' then
                  PrintUsage;
            end;
         'p':
            begin
               if OptArg = 'user' then
                  profile := userProfile
               else if OptArg = 'devel' then
                  profile := developerProfile
               else
                  PrintUsage;
               profileStr := OptArg;
            end;
         'd':
            begin
               Include(options, optDependencies);
            end;
         'b':
            begin
               options := options + beautifyComments;
            end;
         'h':
            begin
               PrintHelp;
            end;
         't':
            begin
               title := OptArg;
            end;
         'i':
            begin
               if OptArg = 'on' then
                  Include(options, optShowImplementationInfo)
               else if OptArg = 'off' then
                  Include(exclude_options, optShowImplementationInfo)
               else
                  PrintUsage;
            end;
         'j':
            begin
               if OptArg = 'on' then
                  options := options + joinComments
               else if OptArg = 'off' then
                  exclude_options := exclude_options + joinComments
               else
                  PrintUsage;
            end;
         'v':
            begin
               options := options + verboseOptions;
            end;
         FETCH_COMMENTS_CHAR:
            begin               
               if OptArg = 'on' then
                  Include(options, optFetchCommentsFromAncestors)
               else if OptArg = 'off' then
                  Include(exclude_options, optFetchCommentsFromAncestors)
               else
                  PrintUsage;
            end;
         VERSION_CHAR:
            begin
               PrintVersion;
            end;
         PROTECTED_CHAR:
            begin
               if OptArg = 'on' then
                  Include(options, optShowProtected)
               else if OptArg = 'off' then
                  Include(exclude_options, optShowProtected)
               else
                  PrintUsage;
            end;
         PRIVATE_CHAR:
            begin
               if OptArg = 'on' then
                  Include(options, optShowPrivate)
               else if OptArg = 'off' then
                  Include(exclude_options, optShowPrivate)
               else
                  PrintUsage;
             end;
         SKIP_SEPARATORS_CHAR:
            begin
               if OptArg = 'on' then
                  options := options + skipSeparators
               else if OptArg = 'off' then
                  exclude_options := exclude_options + skipSeparators
               else
                  PrintUsage;
            end;
         SYNOPSIS_CHAR:
            begin
               if OptArg = 'on' then
                  Include(options, optMakeSynopsis)
               else if OptArg = 'off' then
                  Include(exclude_options, optMakeSynopsis)
               else
                  PrintUsage;
            end;
         TABLE_BORDER_CHAR:
            begin
               if OptArg = 'on' then
                  Include(options, optTableBorder)
               else if OptArg = 'off' then
                  Include(exclude_options, optTableBorder)
               else
                  PrintUsage;
            end;
         '?':
            begin
               PrintUsage;
            end;
      end;
   until c = EndOfOptions;
   
   if OptInd <= ParamCount then
   begin
      for i := OptInd to ParamCount do
         deque.PushBack(ParamStr(i));
   end else
      PrintUsage;
   
   options := options + commentParser + profile;
   options := options - exclude_options;

   if not DirectoryExists(outputDirectory) then
      MkDir(outputDirectory);
   
   if writer = nil then
      writer := THtmlWriter.Create(outputDirectory);
end;

begin
   ParseCommandLine;
   try
      Driver := TDefaultDriver.Create(options, writer, sourceDirectories,
                                      title, profileStr);
      Driver.Run(deque);
   except
      on err1 : ESrcDocError do
      begin
         WriteLn(err1.Message);
{$ifdef DEBUG }
         raise;
{$else }
         halt(3);
{$endif }
      end;
      on err2 : Exception do
      begin
         WriteLn('srcdoc: Fatal error: ' + err2.Message);
{$ifdef DEBUG }
         raise;
{$else }
         halt(3);
{$endif }
      end;   
   end;
end.
