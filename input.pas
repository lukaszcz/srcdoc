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

unit input;

interface

uses
   Classes, SysUtils;

type
   { @see TStreamReader }
   TCharType = (chAlpha, chSpace, chDigit, chXDigit, chPunct, chEol);
   TCharTypeSet = set of TCharType;
   TCharTable = array[#0..#255] of TCharTypeSet;

   TCharSet = set of Char;
   
   TStreamReader = class
   private
      linenum : Cardinal;
      Index : Integer;
      Buffer : String;
      InputStream : TStream;
      { true if the end of the input has been reached; note that there
        may still be some unread characters in the buffer even if this
        is true }
      FEof : Boolean;
      { indicates how many times eof has been true when calling a
        reading function }
      HitEof : Integer;
      
      procedure ReachedEof;
      
   public
      { provides information on the classes each character belongs to;
        may be changed by descendants }
      CharTable : TCharTable;

      constructor Create(astream : TStream);
      destructor Destroy; override;
      
      { skips whitespace; ends if a non-whitespace char is encountered
        or eof is reached }
      procedure SkipWhiteSpace;
      { reads characters till the end of the current line; discards
        leading and closing whitespace }
      function ReadLine : String;
      { reads everything until one of the characters from stop1 or a
        character that belongs to one of the classes in stop2 is
        encountered; does not skip whitespace or modify the input in
        any way; does not include the terminating character }
      function ReadUntil(stop1 : TCharSet;
                         stop2 : TCharTypeSet) : String; overload;
      { reads everything until a character from stop is encountered;
        does not include the terminating character }
      function ReadUntil(stop : TCharSet) : String; overload;
      { reads everything until a character that belongs to one of the
        classes in stop is encountered }
      function ReadUntilType(stop : TCharTypeSet) : String; overload;
      { reads a word; first, checks whether the end of buffer hasn't
        been reached; if no reads a word from the buffer; if it has
        calls FetchLine first; skips all whitespace before reading a
        word; a word is a sequence of characters composed entirely of
        alphanumeric characters; if there is no such sequence
        currently available on input (i.e. the next non-whitespace
        character is neither a digit nor a letter) then returns an
        empty string }
      function ReadWord : String;
      { the same as ReadWord, but reads only one character and does
        not skip whitespace }
      function ReadChar : Char;
      { returns the next char on input without removing it from input;
        equivalent to: Result := ReadChar; UnReadChar; }
      function PeekChar : Char;
      { 'unreads' a character; i.e. makes the last character read
        become available again for reading; this shouldn't be called
        more than once after reading a single character; }
      procedure UnReadChar;
      { returns true if the end of buffer (and hence the end of line)
        has been reached }
      function Eol : Boolean;
      { returns true if the end of file has been reached }
      function Eof : Boolean;
      { resets the eof indicator to false }
      procedure ResetEof;

      { fetches the next line into the buffer; discards anything which
        had previously been in the buffer; }
      procedure FetchLine; virtual;

      property LineNumber : Cardinal read linenum;
   end;
   
   TFileReader = class (TStreamReader)
   public
      constructor Create(afile : TFileStream);
   end;
   
   TStringReader = class (TStreamReader)
   public
      constructor Create(str : String);
   end;
   
   EReaderError = class (Exception)
   end;
   
const   
   DefaultCharTable : TCharTable = (
      [chSpace], [chSpace], [chSpace], [chSpace], [chSpace],
      [chSpace], [chSpace], [chSpace], [chSpace], [chSpace],
      [chSpace, chEol], [chSpace], [chSpace], [chSpace], [chSpace],
      [chSpace], [chSpace], [chSpace], [chSpace], [chSpace],
      [chSpace], [chSpace], [chSpace], [chSpace], [chSpace],
      [chSpace], [chSpace], [chSpace], [chSpace], [chSpace],
      [chSpace], [chSpace], [chSpace], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chDigit,chXDigit], [chDigit,chXDigit],
      [chDigit,chXDigit],[chDigit,chXDigit],[chDigit,chXDigit],[chDigit,chXDigit],
      [chDigit,chXDigit],
      [chDigit,chXDigit],[chDigit,chXDigit],[chDigit,chXDigit],[chPunct],[chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chAlpha,chXDigit],[chAlpha,chXDigit],[chAlpha,chXDigit],[chAlpha,chXDigit],
      [chAlpha,chXDigit],
      [chAlpha,chXDigit], [chAlpha], [chAlpha], [chAlpha], [chAlpha],
      [chAlpha], [chAlpha], [chAlpha], [chAlpha], [chAlpha],
      [chAlpha], [chAlpha], [chAlpha], [chAlpha], [chAlpha],
      [chAlpha], [chAlpha], [chAlpha], [chAlpha], [chAlpha],
      [chAlpha], [chPunct], [chPunct], [chPunct], [chPunct],
      [chAlpha],[chPunct],[chAlpha,chXDigit],[chAlpha,chXDigit],[chAlpha,chXDigit],
      [chAlpha,chXDigit],[chAlpha,chXDigit],[chAlpha,chXDigit],[chAlpha],[chAlpha],
      [chAlpha], [chAlpha], [chAlpha], [chAlpha], [chAlpha],
      [chAlpha], [chAlpha], [chAlpha], [chAlpha], [chAlpha],
      [chAlpha], [chAlpha], [chAlpha], [chAlpha], [chAlpha],
      [chAlpha], [chAlpha], [chAlpha], [chPunct], [chPunct],
      [chPunct], [chPunct], [chSpace],
      { and 128 upper ASCII codes ...  }
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct], [chPunct], [chPunct],
      [chPunct], [chPunct], [chPunct]
      );
   
   
implementation

const
   maxHitEof = 1000;

constructor TStreamReader.Create(astream : TStream);
begin
   inherited Create;
   InputStream := astream;
   Index := 1;
   Buffer := '';
   FEof := false;
   linenum := 0;
   CharTable := DefaultCharTable;
end;
  
destructor TStreamReader.Destroy; 
begin
   InputStream.Free;
end;

procedure TStreamReader.SkipWhiteSpace;
begin
   while (index > Length(buffer)) and not FEof do
      FetchLine;
   
   while not Eof and (chSpace in CharTable[buffer[index]]) do
   begin
      Inc(Index);
      while (Index > Length(Buffer)) and not FEof do
         FetchLine;
   end;
end;

procedure TStreamReader.ReachedEof;
begin
   Inc(HitEof);
   if HitEof > maxHitEof then
      raise EReaderError.Create('Fatal error: unexpected end of file.');
end;

procedure TStreamReader.FetchLine;
var
   c : Char;
   len : Integer;
begin
   Index := 1;
   SetLength(buffer, 50);
   len := 0;
   repeat
      if InputStream.Read(c, 1) = 0 then
      begin
         FEof := true;
         break;
      end;
      Inc(len);
      if len > Length(buffer) then
         SetLength(buffer, 2 * Length(buffer));
      buffer[len] := c;
   until chEol in CharTable[c];
   SetLength(buffer, len);
   Inc(linenum);
end;

function TStreamReader.ReadLine : String;
var
   len : Cardinal;
begin
   Result := '';
   if index <= Length(buffer) then
   begin
      len := 0;
      SetLength(Result, Length(buffer) - index + 2);
      
      { remove leading whitespace }
      while (index <= Length(buffer)) and
               (chSpace in CharTable[buffer[index]]) do
      begin
         Inc(index);
      end;
      
      { read characters till eol }
      while (index <= Length(buffer)) do
      begin
         Inc(len);
         Result[len] := buffer[index];
         Inc(index);
      end;
      
      { remove closing whitespace }
      while (len > 0) and (chSpace in CharTable[Result[len]]) do
      begin
         Dec(len);
      end;
      
      { add one space }
      Inc(len);
      Result[len] := ' ';
      
      SetLength(Result, len);
   end;
end;

function TStreamReader.ReadUntil(stop1 : TCharSet;
                                 stop2 : TCharTypeSet) : String;
begin
   while (index > Length(buffer)) and not FEof do
      FetchLine;
   
   Result := '';
   while not (Eof or (buffer[index] in stop1) or
                 (stop2 * CharTable[buffer[index]] <> [])) do
   begin
      Result := Result + buffer[index];
      Inc(Index);
      while (index > Length(buffer)) and not FEof do
         FetchLine;
   end;
end;

function TStreamReader.ReadUntil(stop : TCharSet) : String;
begin
   Result := ReadUntil(stop, []);
end;

function TStreamReader.ReadUntilType(stop : TCharTypeSet) : String;
begin
   Result := ReadUntil([], stop);
end;

function TStreamReader.ReadWord : String;
var
   len : Cardinal;
begin
   Result := '';
   SkipWhiteSpace;   
   
   if not Eof then
   begin
      SetLength(Result, Length(Buffer) - index + 1);
      len := 0;
      while (index <= Length(buffer)) and
               (([chAlpha, chDigit] * CharTable[buffer[index]]) <> []) do
      begin
         Inc(len);
         Result[len] := buffer[index];
         Inc(index);
      end;
      SetLength(Result, len);
   end else
   begin
      ReachedEof;
      Result := '';
   end;
end;

function TStreamReader.ReadChar : Char;
begin
   while (index > Length(buffer)) and not FEof do
      FetchLine;
   if (not Eof) then
   begin
      Result := buffer[index];
      Inc(index);
   end else
   begin
      ReachedEof;
      Result := #0;
   end;
end;

function TStreamReader.PeekChar : Char;
begin
   while (index > Length(buffer)) and not FEof do
      FetchLine;
   if not Eof then
   begin
      Result := buffer[index];
   end else
   begin
      ReachedEof;
      Result := #0;
   end;
end;

procedure TStreamReader.UnReadChar;
begin
   Dec(index);
end;

function TStreamReader.Eol : Boolean;
begin
   Result := Length(Buffer) < Index;
end;

function TStreamReader.Eof : Boolean;
begin
   Result := Eol and FEof;
end;

procedure TStreamReader.ResetEof;
begin
   FEof := false;
   HitEof := 0;
end;

{ ----------------------- TFileReader ---------------------- }

constructor TFileReader.Create(afile : TFileStream);
begin
   inherited Create(afile);
end;

{ ----------------------- TStringReader ---------------------- }

constructor TStringReader.Create(str : String);
begin
   inherited Create(TStringStream.Create(str));
end;

end.
