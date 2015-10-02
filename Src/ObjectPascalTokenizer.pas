(***************************************************************************

    Copyright 1998-2010, Christian Aymon (cyamon software, www.cyamon.com)

    This file is part of ''Discover''.

    ''Discover'' is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    ''Discover'' is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Discover.  If not, see <http://www.gnu.org/licenses/>.

 
***************************************************************************)
unit ObjectPascalTokenizer;

interface
  uses
    Classes;

  type
    TToken = (toNone, toParOpen, toParClose, toMult, toPlus, toComma, toMinus, toColon, toSemiColon,
      toEqual, toAt, toEndOfBuffer, toBraOpen, toBraClose, toDeref, toStringConstant,
      toPoint, toRange, toSlash, toAssignement, toLessEqual, toLess, toGreaterEqual,
      toGreater, toNumber, toIdentifier, toSegOffset,
       {ReserwedWords}
       toAnd, toArray, toAs, toAsm, toAssembler,
       toBegin,
       toCase, tocDecl, toClass, toConst, toConstructor,
       toDestructor, toDiv, toDo, toDownto,
       toElse, toEnd, toExcept, toExports, toExternal,
       toFile, toFinalization, toFinally, toFor, toForward, toFunction,
       toGoto,
       toIf, toImplementation, toIn, toInherited, toInitialization, toInline, toInterface, toIs,
       toLabel, toLibrary,
       toMod,
       toNil, toNot,
       toObject, toOf, toOn, toOr, {toOut,}
       toPacked, toPascal, toProcedure, toProgram, toProperty,
       toRaise, toRecord, toRegister, toRepeat, toResourcestring,
       toSafeCall, toSet, toShl, toShr, toStdCall, toString,
       toThen, toThreadVar, toTo, toTry, toType,
       toUnit, toUntil, toUses,
       toVar,
       toWhile, toWith,
       toXor
       );
    TDirective = (dirDEFINE, dirELSE, dirENDIF, dirI, dirIFDEF, dirIFNDEF,
       dirINCLUDE, dirUNDEF);
    TOnIfDirective = function (Directive : TDirective; const Param : string) : boolean of object;
    TOnDefUndefDirective = procedure (Directive : TDirective; const Param : string) of object;
    TOnIncludeDirective = procedure (const Param : string; var FileName : string) of object;
  const
    FirstReservedWord = toAnd;
    LastReservedWord = toXor;
    ConditionnalStackMaxDepth = 16;

  type
    TTokenSet = set of TToken;
    TTokenizerContext = record
      Token : TToken;
      LineNumber, LineIndex, Index, TokenIndex : integer;
    end;

    TConditionnalStackKind = (cosTrue, cosIgnore, cosFalse);
    TObjectPascalTokenizer = class(TObject)
    private
      FFileName : string;
      FLineNumber, FLineIndex : integer;
      FToken : TToken;
      Index, FTokenIndex : integer;
      Buffer : string;
      ConditionnalStackPtr : integer;
      ConditionnalStack : array [0..pred(ConditionnalStackMaxDepth)] of TConditionnalStackKind;
      Included : TObjectPascalTokenizer;
      procedure SetFile(const aFileName : string);
      function GetOffsetInLine : integer;
      procedure Illegal;
      procedure HexDigitSequence;
      procedure CharacterString;
      procedure ParOpen;
      procedure Point;
      procedure Slash;
      procedure DigitSequence;
      procedure Colon;
      procedure LessThan;
      procedure GreaterThan;
      procedure WhiteSpace;
      procedure NewLine;
      procedure CurlOpen;
      procedure QuotedString;
      procedure UnsignedNumber;
      procedure AsmIdentifier;
      function GetToken : TToken;
      function GetFileName : string;
      function GetLineNumber : integer;
    protected
      procedure UnsignedInteger; virtual;
      procedure Identifier; virtual;
    public
      EnableAsmIdentifiers, EnableDirectives : boolean;
      OnIfDirective : TOnIfDirective;
      OnDefUndefDirective : TOnDefUndefDirective;
      OnIncludeDirective : TOnIncludeDirective;
      HexValue : integer;
      property FileName : string read GetFileName write SetFile;
      property LineNumber : integer read GetLineNumber;
      property OffsetInLine : integer read GetOffsetInLine;
      property Token : TToken read GetToken;
      property TokenIndex : integer read FTokenIndex;
      procedure Next;
      function GetIdentifier : string;
      procedure GetContext(var aContext : TTokenizerContext);
      procedure SetContext(const aContext : TTokenizerContext);
      function SkipToLine(LineNr : integer) : boolean;
      procedure SkipToOffset(Offset : integer);
      function GotoLineContaining(const Text : string) : boolean;
      function PreviousIdentifier : string;
      function BufferLength : integer;
    end;

  function TokenString(aToken : TToken) : string;

implementation
  uses
    SysUtils, Exceptions, Globals;
 {$O-}
 const
   TokenStrings : array [Low(TToken)..High(TToken)] of string =
   (
     {toNone} '',
     {toParOpen} '"("',
     {toParClose} '")"',
     {toMult} '"*"',
     {toPlus} '"+"',
     {toComma} '","',
     {toMinus} '"-"',
     {toColon} '":"',
     {toSemiColon} '";"',
     {toEqual} '"="',
     {toAt} '"@"',
     {toEndOfBuffer} 'EndofBuffer',
     {toBraOpen} '"["',
     {toBraClose} '"]"',
     {toDeref} '"^"',
     {toStringConstant} 'StringConstant',
     {toPoint} '"."',
     {toRange} '".."',
     {toSlash} '"/"',
     {toAssignement} '":="',
     {toLessEqual} '"<="',
     {toLess} '"<"',
     {toGreaterEqual} '">="',
     {toGreater} '">"',
     {toNumber} 'Number',
     {toIdentifier} 'Identifier',
     {toSegOffset} 'SegOffset',
     'and', 'array', 'as', 'asm', 'assembler',
     'begin',
     'case', 'cdecl', 'class', 'const', 'constructor',
     'destructor', 'div', 'do', 'downto',
     'else', 'end', 'except', 'exports', 'external',
     'file', 'finalization', 'finally', 'for', 'forward', 'function',
     'goto',
     'if', 'implementation', 'in', 'inherited', 'initialization', 'inline', 'interface', 'is',
     'label', 'library',
     'mod',
     'nil', 'not',
     'object', 'of', 'on', 'or', {'out',}
     'packed', 'pascal', 'procedure', 'program', 'property',
     'raise', 'record', 'register', 'repeat',
     'safecall', 'set', 'shl', 'shr', 'stdcall', 'string', 'resourcestring',
     'then', 'threadVar', 'to', 'try', 'type',
     'unit', 'until', 'uses',
     'var',
     'while', 'with',
     'xor'
     );
   DirString : array [Low(TDirective)..High(TDirective)] of string =
     ('DEFINE',
      'ELSE',
      'ENDIF',
      'I',
      'IFDEF',
      'IFNDEF',
      'INCLUDE',
      'UNDEF');
(*
   NewLineChar = LF;
   NewLineCharCompanion = CR;
*)
   NewLineChar = CR;
   NewLineCharCompanion = LF;
var
  ReservedWordsList, DirectiveList : TStringList;
  t : TToken;
  d : TDirective;
{~t}
(***************)
(* TokenString *)
(***************)

function TokenString(aToken : TToken) : string;
begin
  Result := TokenStrings[aToken]
end {TokenString};


(****************************************)
(* TObjectPascalTokenizer.AsmIdentifier *)
(****************************************)

procedure TObjectPascalTokenizer.AsmIdentifier;
  var
    c : char;
    i : integer;
begin
  repeat
    inc(Index);
    c := Upcase(Buffer[Index]);
  until not (((c >= 'A') and (c <= 'Z')) or ((c >= '0') and (c <= '9'))
    or ( c = '_') or (c = '@'));
  i := ReservedWordsList.IndexOf(GetIdentifier);
  if i >= 0 then
    FToken := TToken(ord(FirstReservedWord) + i)
  else
    FToken := toIdentifier;
end {TObjectPascalTokenizer.AsmIdentifier};


(***************************************)
(* TObjectPascalTokenizer.BufferLength *)
(***************************************)

function TObjectPascalTokenizer.BufferLength;
begin
  Result := Length(Buffer);
end {TObjectPascalTokenizer.BufferLength};


(******************************************)
(* TObjectPascalTokenizer.CharacterString *)
(******************************************)

procedure TObjectPascalTokenizer.CharacterString;
  var
    Done : boolean;
begin
  Done := false;
  repeat
    if Buffer[Index] = '#' then begin
      inc(Index);
      UnsignedInteger;
    end else if Buffer [Index] = '''' then begin
      QuotedString;
    end else
      Done := true;
    until Done;
    FToken := toStringConstant;
end {TObjectPascalTokenizer.CharacterString};


(********************************)
(* TObjectPascalTokenizer.Colon *)
(********************************)

procedure TObjectPascalTokenizer.Colon;
begin
  inc(Index);
  if Buffer[Index] = '=' then begin
    FToken := toAssignement;
    inc(Index);
  end else
    FToken := toColon;
end {TObjectPascalTokenizer.Colon};


(***********************************)
(* TObjectPascalTokenizer.CurlOpen *)
(***********************************)

procedure TObjectPascalTokenizer.CurlOpen;
  var
    Done : boolean;

  procedure ToCurlClose;
  begin
    repeat
      Done := Buffer[Index] = '}';
      if Buffer[Index] = NewLineChar then
        NewLine
      else
        inc(Index);
    until Done or (Buffer[Index] = #0);
  end;

  procedure Directive;
    var
      n : integer;
      d : TDirective;
      StackTop : TConditionnalStackKind;
      s : string;
  begin
    FTokenIndex := Index;
    Identifier;
    n := DirectiveList.IndexOf(Uppercase(GetIdentifier));
    if n >= 0 then begin
      d := TDirective(n);
      StackTop := ConditionnalStack[ConditionnalStackPtr];
      case d of
        dirIFDEF, dirIFNDEF: begin
          inc(ConditionnalStackPtr);
          if StackTop = cosTrue then begin
            WhiteSpace;
            FTokenIndex := Index;
            Identifier;
            if OnIfDirective(d, GetIdentifier) then
              ConditionnalStack[ConditionnalStackPtr] := cosTrue
            else
              ConditionnalStack[ConditionnalStackPtr] := cosFalse
          end else
            ConditionnalStack[ConditionnalStackPtr] := cosIgnore
        end;

        dirDEFINE, dirUNDEF:
          if ConditionnalStack[ConditionnalStackPtr] = cosTrue then begin
            WhiteSpace;
            FTokenIndex := Index;
            Identifier;
            OnDefUndefDirective(d, GetIdentifier);
          end {if};

        dirELSE:
          case ConditionnalStack[ConditionnalStackPtr] of
            cosTrue:
              ConditionnalStack[ConditionnalStackPtr] := cosFalse;
            cosFalse:
              ConditionnalStack[ConditionnalStackPtr] := cosTrue;
          end {case};

        dirENDIF:
          if ConditionnalStackPtr > 0 then
            dec(ConditionnalStackPtr);
          // else do nothing, we have not seen the corresondig
          // if...

        dirI, dirINCLUDE:
          if ConditionnalStack[ConditionnalStackPtr] = cosTrue then begin
            WhiteSpace;
            FTokenIndex := Index;
            while Buffer[Index] <> '}' do
              inc(Index);
            if Assigned(OnIncludeDirective) then begin
              OnIncludeDirective(GetIdentifier, s);
              Included := TObjectPascalTokenizer.Create;
              Included.FileName := s;
              Included.EnableAsmIdentifiers := EnableAsmIdentifiers;
              Included.OnIfDirective := OnIfDirective;
              Included.OnDefUndefDirective := OnDefUndefDirective;
              Included.OnIncludeDirective := OnIncludeDirective;
            end {if};
          end {if};
      end {case};
      ToCurlClose;
    end else
      ToCurlClose
  end {Directive};

begin
  Done := false;
  Inc(Index);
  if (Buffer[Index] = '$') and EnableDirectives then begin
    inc(Index);
    Directive;
  end else
    ToCurlClose;
  FToken := toNone;
end {TObjectPascalTokenizer.CurlOpen};


(****************************************)
(* TObjectPascalTokenizer.DigitSequence *)
(****************************************)

procedure TObjectPascalTokenizer.DigitSequence;
begin
  repeat inc(Index) until (Buffer[Index] < '0') or (Buffer[Index] > '9');
end {TObjectPascalTokenizer.DigitSequence};


(*************************************)
(* TObjectPascalTokenizer.GetContext *)
(*************************************)

procedure TObjectPascalTokenizer.GetContext(var aContext : TTokenizerContext);
begin
  if Included <> nil then
    Included.GetContext(aContext)
  else begin
    aContext.Token := FToken;
    aContext.LineNumber := FLineNumber;
    aContext.LineIndex := FLineIndex;
    aContext.Index := Index;
    aContext.TokenIndex := FTokenIndex;
  end {if};
end {TObjectPascalTokenizer.GetContext};


(**************************************)
(* TObjectPascalTokenizer.GetFileName *)
(**************************************)

function TObjectPascalTokenizer.GetFileName : string;
begin
  if Included <> nil then
    Result := Included.Filename
  else
    Result := FFileName;
end {TObjectPascalTokenizer.GetFileName};


(****************************************)
(* TObjectPascalTokenizer.GetIdentifier *)
(****************************************)

function TObjectPascalTokenizer.GetIdentifier : string;
begin
  if Included <> nil then
    Result := Included.GetIdentifier
  else
    Result := Copy(Buffer, FTokenIndex, Index-FTokenIndex);
end {TObjectPascalTokenizer.GetIdentifier};


(****************************************)
(* TObjectPascalTokenizer.GetLineNumber *)
(****************************************)

function TObjectPascalTokenizer.GetLineNumber : integer;
begin
  if Included <> nil then
    Result := Included.LineNumber
  else
    Result := FLineNumber;
end {TObjectPascalTokenizer.GetLineNumber};


(******************************************)
(* TObjectPascalTokenizer.GetOffsetInLine *)
(******************************************)

function TObjectPascalTokenizer.GetOffsetInLine : integer;
begin
  if Included <> nil then
    Result := Included.OffsetInLine
  else
    Result := Index - FLineIndex;
end {TObjectPascalTokenizer.GetOffsetInLine};


(***********************************)
(* TObjectPascalTokenizer.GetToken *)
(***********************************)

function TObjectPascalTokenizer.GetToken;
begin
  if Included <> nil then
    Result := Included.Token
  else
    Result := FToken;
end {TObjectPascalTokenizer.GetToken};


(*********************************************)
(* TObjectPascalTokenizer.GotoLineContaining *)
(*********************************************)

function TObjectPascalTokenizer.GotoLineContaining;
  var
    StartIndex, i : integer;
begin
  i := Pos(Text, Buffer);
  if (i > 0) and (i > Index) then begin
    StartIndex := succ(Index);
    Index := i;
    while StartIndex < Index do begin
      if Buffer[StartIndex] = NewLineChar then begin
        inc(FLineNumber);
        FLineIndex := Index;
      end {if};
      inc(StartIndex);
    end {while};
    Result := true;
  end else
    Result := false;
end {TObjectPascalTokenizer.GotoLineContaining};


(**************************************)
(* TObjectPascalTokenizer.GreaterThan *)
(**************************************)

procedure TObjectPascalTokenizer.GreaterThan;
begin
  inc(Index);
  if Buffer[Index] = '=' then begin
    FToken := toGreaterEqual;
    inc(Index);
  end else
    FToken := toGreater;
end {TObjectPascalTokenizer.GreaterThan};


(*******************************************)
(* TObjectPascalTokenizer.HexDigitSequence *)
(*******************************************)

procedure TObjectPascalTokenizer.HexDigitSequence;
  var
    c : char;
begin
  HexValue := 0;
  c := Upcase(Buffer[Index]);
  while ((c >= '0') and (c <= '9')) or ((c >= 'A') and (c <= 'F')) do begin
    HexValue := HexValue  shl 4;
    if c >= 'A' then begin
      inc(HexValue,ord(c)-ord('A')+10);
    end else begin
      inc(HexValue,ord(c)-ord('0'));
    end {if};
    inc(Index);
    c := UpCase(Buffer[Index]);
  end {while};
end {TObjectPascalTokenizer.HexDigitSequence};


(*************************************)
(* TObjectPascalTokenizer.Identifier *)
(*************************************)

procedure TObjectPascalTokenizer.Identifier;
  var
    c : char;
    i : integer;
begin
  repeat
    inc(Index);
    c := Buffer[Index];
  until ((c < 'A') or (c >'Z')) and ((c <'a') or (c > 'z')) and
    ((c < '0') or (c > '9')) and (c <> '_');
  i := ReservedWordsList.IndexOf(GetIdentifier);
  if i >= 0 then
    FToken := TToken(ord(FirstReservedWord) + i)
  else
    FToken := toIdentifier;
end {TObjectPascalTokenizer.Identifier};


(**********************************)
(* TObjectPascalTokenizer.Illegal *)
(**********************************)

procedure TObjectPascalTokenizer.Illegal;
begin
  raise Exception.Create('Illegal character');
end {TObjectPascalTokenizer.Illegal};


(***********************************)
(* TObjectPascalTokenizer.LessThan *)
(***********************************)

procedure TObjectPascalTokenizer.LessThan;
begin
  inc(Index);
  if Buffer[Index] = '=' then begin
    FToken := toLessEqual;
    inc(Index);
  end else
    FToken := toLess;
end {TObjectPascalTokenizer.LessThan};


(**********************************)
(* TObjectPascalTokenizer.NewLine *)
(**********************************)

procedure TObjectPascalTokenizer.NewLine;
begin
  inc(Index);
  inc(FLineNumber);
  FLineIndex := Index;
end {TObjectPascalTokenizer.NewLine};


(*******************************)
(* TObjectPascalTokenizer.Next *)
(*******************************)

procedure TObjectPascalTokenizer.Next;
begin
  FToken := toNone;
  repeat
    if Included = nil then begin
      FTokenIndex := Index;
      case Buffer [Index] of
        #0: begin
          FToken := toEndOfBuffer;
          break;
        end;

        #1..#8:
          //Illegal;
          // Ignore
          inc(Index);

        TAB:
          WhiteSpace;

        NewLineChar:
          NewLine;

        #11..#12:
          // Illegal;
          // Ignore
          inc(Index);

        NewLineCharCompanion:
          // Ignore
          inc(Index);

        #14..#31:
          // Ignore
          inc(Index);

        ' ':
          WhiteSpace;

        '!', '"':
          // Illegal;
          // Ignore
          inc(Index);

        '#':
          CharacterString;

        '$':
          UnsignedInteger;

        '%', '&' :
          // Illegal;
          // Ignore
          inc(Index);

        '''' :
          CharacterString;

        '(' :
          ParOpen;

        ')': begin
          FToken := toParClose;
          inc(Index);
        end;

        '+' : begin
          FToken := toPlus;
          inc(Index);
        end;

        '*' : begin
          FToken := toMult;
          inc(Index);
        end;

        ',' : begin
          FToken := toComma;
          inc(Index);
        end;

        '-' : begin
          FToken := toMinus;
          inc(Index);
        end;

        '.' :
          Point;

        '/':
          Slash;

        '0'..'9':
          UnsignedInteger;

        ':' :
          Colon;

        ';' : begin
          FToken := toSemiColon;
          inc(Index);
        end;

        '<' :
          LessThan;

        '=' : begin
          FToken := toEqual;
          inc(Index);
        end;

        '>' :
          GreaterThan;

        '?' :
          //Illegal;
          // Ignore
          inc(Index);

        '@' : begin
          if EnableAsmIdentifiers then
            AsmIdentifier
          else begin
            FToken := toAt;
            inc(Index);
          end {if};
        end;

        'A'..'Z' :
          Identifier;

        '[': begin
          FToken := toBraOpen;
          inc(Index);
        end;

        '\' :
          //Illegal;
          // Ignore
          inc(Index);

        ']' : begin
          FToken := toBraClose;
          inc(Index);
        end;

        '^' : begin
          FToken := toDeref;
          inc(Index);
        end;

        '`':
          // Illegal;
          // Ignore
          inc(Index);

        '_' :
          Identifier;

        'a'..'z' :
          Identifier;

        '{':
          CurlOpen;
        else {case}
          // ignore
          inc(Index);
      end {case};
    end else begin
      Included.Next;
      FToken := Included.Token;
      if FToken = toEndOfBuffer then begin
        Included.Free;
        Included := nil;
        FToken := toNone;
      end {if};
    end {if};
  until (FToken <> toNone) and (ConditionnalStack[ConditionnalStackPtr] = cosTrue);
end {TObjectPascalTokenizer.Next};


(**********************************)
(* TObjectPascalTokenizer.ParOpen *)
(**********************************)

procedure TObjectPascalTokenizer.ParOpen;

  procedure Comment;
    var
      Done : boolean;
  begin
    Done := false;
    repeat
      Done := (Buffer[Index] = ')') and (Buffer[pred(Index)] = '*');
      if Buffer[Index] = NewLineChar then
        NewLine
      else
        inc(Index);
    until Done or (Buffer [Index] = #0);
  end {Comment};

begin
  inc(Index);
  if Buffer [Index] = '*' then begin
    inc(Index);
    Comment
  end else
    FToken := toParOpen;
end {TObjectPascalTokenizer.ParOpen};


(********************************)
(* TObjectPascalTokenizer.Point *)
(********************************)

procedure TObjectPascalTokenizer.Point;
begin
  inc(Index);
  if Buffer [Index] = '.' then begin
    inc(Index);
    FToken := toRange
  end else
    FToken := toPoint;
end {TObjectPascalTokenizer.Point};


(*********************************************)
(* TObjectPascalTokenizer.PreviousIdentifier *)
(*********************************************)

function TObjectPascalTokenizer.PreviousIdentifier : string;

  procedure PWhiteSpace;
  begin
    repeat
      dec(Index);
    until (Index < 1) or ((Buffer[Index] <> TAB) and (Buffer[Index] <> ' '));
  end {PWhiteSpace};

  procedure PComment;
  begin
    while (Index > 0) and (Buffer [Index] <> '{') do
      dec(Index);
  end;

  procedure PIdentifier;
    var
      e : integer;
  begin
    e := Index;
    while (Index > 1) and (Buffer[Index] in ['a'..'z','_','A'..'Z']) do
      dec(Index);
    Result := UpperCase(Copy(Buffer, succ(Index), e-Index));
    FToken := toIdentifier;
  end;

begin
  FToken := toNone;
  dec(Index);
  repeat
    case Buffer[index] of
      TAB, ' ' :
        PWhiteSpace;

      '}':
        PComment;

      'a'..'z','A'..'Z', '_':
        PIdentifier;

      NewLineChar: begin
        dec(FLineNumber);
        dec(Index);
      end;

      else
        dec(Index);
    end {case};
  until (index < 1) or (Token in [toIdentifier]);
end {TObjectPascalTokenizer.PreviousIdentifier};


(***************************************)
(* TObjectPascalTokenizer.QuotedString *)
(***************************************)

procedure TObjectPascalTokenizer.QuotedString;
  var
    Done : boolean;
begin
  Done := false;
  inc(Index);
  repeat
    if Buffer[Index] = '''' then begin
      inc(Index);
      if Buffer[Index] = '''' then
        inc(Index)
      else
        Done := true
    end else
      Inc(Index);
  until Done or (Buffer[Index] = #0);
end {TObjectPascalTokenizer.QuotedString};


(*************************************)
(* TObjectPascalTokenizer.SetContext *)
(*************************************)

procedure TObjectPascalTokenizer.SetContext(const aContext : TTokenizerContext);
begin
  if Included <> nil then
    Included.SetContext(aContext)
  else begin
    FToken := aContext.Token;
    FLineNumber := aContext.LineNumber;
    FLineIndex := aContext.LineIndex;
    Index := aContext.Index;
    FTokenIndex := aContext.TokenIndex;
  end {if};
end {TObjectPascalTokenizer.SetContext};


(**********************************)
(* TObjectPascalTokenizer.SetFile *)
(**********************************)

procedure TObjectPascalTokenizer.SetFile;
  var
    S : TFileStream;
    t : AnsiString;
begin
  S := TFileStream.Create(aFileName, fmOpenRead);
  try
    SetLength(t, S.Size);
    S.Read(t[1], S.Size);
    Buffer := t;  // Convert to WideString
    FFileName := aFileName;
    Index := 1;
    FLineNumber := 1;
    FLineIndex := 0;
    ConditionnalStackPtr := 0;
    ConditionnalStack[ConditionnalStackPtr] := cosTrue;
  finally
    S.Free;
  end {try};
end {TObjectPascalTokenizer.SetFile};


(*************************************)
(* TObjectPascalTokenizer.SkipToLine *)
(*************************************)

function TObjectPascalTokenizer.SkipToLine(LineNr : integer) : boolean;
// Returns true if the required line exists
begin
  Result := false;
  if FLineNumber < LineNr then
    while (Index <= Length(Buffer)) do begin
      if Buffer [Index] = NewLineChar then begin
        NewLine;
        if FLineNumber = LineNr then begin
          Result := true;
          break;
        end {if};
      end else
        inc(Index);
    end {while};
end {TObjectPascalTokenizer.SkipToLine};


(***************************************)
(* TObjectPascalTokenizer.SkipToOffset *)
(***************************************)

procedure TObjectPascalTokenizer.SkipToOffset;
begin
  inc(Index, Offset-OffsetInLine);
end {TObjectPascalTokenizer.SkipToOffset};


(********************************)
(* TObjectPascalTokenizer.Slash *)
(********************************)

procedure TObjectPascalTokenizer.Slash;
begin
  inc(Index);
  if Buffer [Index] = '/' then
    repeat inc(Index) until (Buffer[Index] = CR) or (Buffer[Index] = #0)
  else
    FToken := toSlash
end {TObjectPascalTokenizer.Slash};


(******************************************)
(* TObjectPascalTokenizer.UnsignedInteger *)
(******************************************)

procedure TObjectPascalTokenizer.UnsignedInteger;
begin
  if Buffer[Index] = '$' then begin
    inc(Index);
    HexDigitSequence;
  end else
    DigitSequence;
  FToken := toNumber;
end {TObjectPascalTokenizer.UnsignedInteger};


(*****************************************)
(* TObjectPascalTokenizer.UnsignedNumber *)
(*****************************************)

procedure TObjectPascalTokenizer.UnsignedNumber;

  procedure ScaleFactor;
  begin
    if (Buffer[Index] = 'e') or (Buffer[Index] = 'E') then begin
      inc(Index);
      if (Buffer[Index] = '-') or (Buffer[Index] = '+') then
        inc(Index);
      DigitSequence;
    end {if};
  end {ScaleFactor};

begin
  DigitSequence;
  if Buffer[Index] = '.' then begin
    inc(Index);
    DigitSequence;
    ScaleFactor;
  end else
    ScaleFactor;
  FToken := toNumber;
end {TObjectPascalTokenizer.UnsignedNumber};


(*************************************)
(* TObjectPascalTokenizer.WhiteSpace *)
(*************************************)

procedure TObjectPascalTokenizer.WhiteSpace;
begin
  repeat inc(Index) until (Buffer [Index] <> ' ') and (Buffer[Index] <> TAB);
end {TObjectPascalTokenizer.WhiteSpace};


{~b}
initialization
  ReservedWordsList := TStringList.Create;
  ReservedWordsList.Sorted := true;
  for t := FirstReservedWord to LastReservedWord do
    ReservedWordsList.Add(TokenString(t));
  DirectiveList := TStringList.Create;
  DirectiveList.Sorted := true;
  for d := Low(TDirective) to High(TDirective) do
    DirectiveList.Add(DirString[d]);
finalization
  ReservedWordsList.Free;
end.
