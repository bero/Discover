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
unit CRC32;

interface


function FileCRC32(const FileName : string) : LongInt;

implementation

uses
  SysUtils, Windows;


const
  CRC32Table: array[0..255] of integer = (
    integer($00000000), integer($77073096), integer($ee0e612c), integer($990951ba), integer($076dc419), integer($706af48f), integer($e963a535), integer($9e6495a3),
    integer($0edb8832), integer($79dcb8a4), integer($e0d5e91e), integer($97d2d988), integer($09b64c2b), integer($7eb17cbd), integer($e7b82d07), integer($90bf1d91),
    integer($1db71064), integer($6ab020f2), integer($f3b97148), integer($84be41de), integer($1adad47d), integer($6ddde4eb), integer($f4d4b551), integer($83d385c7),
    integer($136c9856), integer($646ba8c0), integer($fd62f97a), integer($8a65c9ec), integer($14015c4f), integer($63066cd9), integer($fa0f3d63), integer($8d080df5),
    integer($3b6e20c8), integer($4c69105e), integer($d56041e4), integer($a2677172), integer($3c03e4d1), integer($4b04d447), integer($d20d85fd), integer($a50ab56b),
    integer($35b5a8fa), integer($42b2986c), integer($dbbbc9d6), integer($acbcf940), integer($32d86ce3), integer($45df5c75), integer($dcd60dcf), integer($abd13d59),
    integer($26d930ac), integer($51de003a), integer($c8d75180), integer($bfd06116), integer($21b4f4b5), integer($56b3c423), integer($cfba9599), integer($b8bda50f),
    integer($2802b89e), integer($5f058808), integer($c60cd9b2), integer($b10be924), integer($2f6f7c87), integer($58684c11), integer($c1611dab), integer($b6662d3d),
    integer($76dc4190), integer($01db7106), integer($98d220bc), integer($efd5102a), integer($71b18589), integer($06b6b51f), integer($9fbfe4a5), integer($e8b8d433),
    integer($7807c9a2), integer($0f00f934), integer($9609a88e), integer($e10e9818), integer($7f6a0dbb), integer($086d3d2d), integer($91646c97), integer($e6635c01),
    integer($6b6b51f4), integer($1c6c6162), integer($856530d8), integer($f262004e), integer($6c0695ed), integer($1b01a57b), integer($8208f4c1), integer($f50fc457),
    integer($65b0d9c6), integer($12b7e950), integer($8bbeb8ea), integer($fcb9887c), integer($62dd1ddf), integer($15da2d49), integer($8cd37cf3), integer($fbd44c65),
    integer($4db26158), integer($3ab551ce), integer($a3bc0074), integer($d4bb30e2), integer($4adfa541), integer($3dd895d7), integer($a4d1c46d), integer($d3d6f4fb),
    integer($4369e96a), integer($346ed9fc), integer($ad678846), integer($da60b8d0), integer($44042d73), integer($33031de5), integer($aa0a4c5f), integer($dd0d7cc9),
    integer($5005713c), integer($270241aa), integer($be0b1010), integer($c90c2086), integer($5768b525), integer($206f85b3), integer($b966d409), integer($ce61e49f),
    integer($5edef90e), integer($29d9c998), integer($b0d09822), integer($c7d7a8b4), integer($59b33d17), integer($2eb40d81), integer($b7bd5c3b), integer($c0ba6cad),
    integer($edb88320), integer($9abfb3b6), integer($03b6e20c), integer($74b1d29a), integer($ead54739), integer($9dd277af), integer($04db2615), integer($73dc1683),
    integer($e3630b12), integer($94643b84), integer($0d6d6a3e), integer($7a6a5aa8), integer($e40ecf0b), integer($9309ff9d), integer($0a00ae27), integer($7d079eb1),
    integer($f00f9344), integer($8708a3d2), integer($1e01f268), integer($6906c2fe), integer($f762575d), integer($806567cb), integer($196c3671), integer($6e6b06e7),
    integer($fed41b76), integer($89d32be0), integer($10da7a5a), integer($67dd4acc), integer($f9b9df6f), integer($8ebeeff9), integer($17b7be43), integer($60b08ed5),
    integer($d6d6a3e8), integer($a1d1937e), integer($38d8c2c4), integer($4fdff252), integer($d1bb67f1), integer($a6bc5767), integer($3fb506dd), integer($48b2364b),
    integer($d80d2bda), integer($af0a1b4c), integer($36034af6), integer($41047a60), integer($df60efc3), integer($a867df55), integer($316e8eef), integer($4669be79),
    integer($cb61b38c), integer($bc66831a), integer($256fd2a0), integer($5268e236), integer($cc0c7795), integer($bb0b4703), integer($220216b9), integer($5505262f),
    integer($c5ba3bbe), integer($b2bd0b28), integer($2bb45a92), integer($5cb36a04), integer($c2d7ffa7), integer($b5d0cf31), integer($2cd99e8b), integer($5bdeae1d),
    integer($9b64c2b0), integer($ec63f226), integer($756aa39c), integer($026d930a), integer($9c0906a9), integer($eb0e363f), integer($72076785), integer($05005713),
    integer($95bf4a82), integer($e2b87a14), integer($7bb12bae), integer($0cb61b38), integer($92d28e9b), integer($e5d5be0d), integer($7cdcefb7), integer($0bdbdf21),
    integer($86d3d2d4), integer($f1d4e242), integer($68ddb3f8), integer($1fda836e), integer($81be16cd), integer($f6b9265b), integer($6fb077e1), integer($18b74777),
    integer($88085ae6), integer($ff0f6a70), integer($66063bca), integer($11010b5c), integer($8f659eff), integer($f862ae69), integer($616bffd3), integer($166ccf45),
    integer($a00ae278), integer($d70dd2ee), integer($4e048354), integer($3903b3c2), integer($a7672661), integer($d06016f7), integer($4969474d), integer($3e6e77db),
    integer($aed16a4a), integer($d9d65adc), integer($40df0b66), integer($37d83bf0), integer($a9bcae53), integer($debb9ec5), integer($47b2cf7f), integer($30b5ffe9),
    integer($bdbdf21c), integer($cabac28a), integer($53b39330), integer($24b4a3a6), integer($bad03605), integer($cdd70693), integer($54de5729), integer($23d967bf),
    integer($b3667a2e), integer($c4614ab8), integer($5d681b02), integer($2a6f2b94), integer($b40bbe37), integer($c30c8ea1), integer($5a05df1b), integer($2d02ef8d));

  type
    TLongIntRec = record
      case Byte of
        1: (Lo: Word;
            Hi: Word);
        2: (LoLo: Byte;
            LoHi: Byte;
            HiLo: Byte;
            HiHi: Byte);
  end;

procedure UpdateCRC32(var CRC : LongInt;  const Buf;  BufSize : LongInt); forward;

{~t}
(*************)
(* FileCRC32 *)
(*************)

function FileCRC32(const FileName : string) : LongInt;
var
  Fh      : THandle;
  FileMap : THandle;
  Size    : LongInt;
  Memory  : PByteArray;
  Buf     : array [0..MAX_PATH - 1] of char;
begin
  Result := -1;
  StrPLCopy(Buf, FileName, SizeOf(Buf)-1);
  Fh := CreateFile(Buf, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (Fh > -1) then begin
    Size := Windows.GetFileSize(Fh, nil);
    FileMap := CreateFileMapping(Fh, nil, PAGE_READONLY, 0, 0, nil);
    if (FileMap <> 0) then begin
      Memory := MapViewOfFile(FileMap, FILE_MAP_READ, 0, 0, 0);
      if (Memory <> nil) then begin
        Result := $FFF00FFF;  {special CRC init}
        UpdateCRC32(Result, Memory^, Size);
        UnmapViewOfFile(Memory);
      end;
      CloseHandle(FileMap);
    end;
    CloseHandle(Fh);
  end;
end {FileCRC32};


(***************)
(* UpdateCRC32 *)
(***************)

procedure UpdateCRC32(var CRC : LongInt;  const Buf;  BufSize : LongInt);
var
  Bytes : TByteArray absolute Buf;
  I     : LongInt;
  B     : Byte;
begin
  for I := 0 to BufSize - 1 do begin
    B := TLongIntRec(CRC).LoLo xor Bytes[I];
    CRC := (CRC shr 8) xor CRC32Table[B];
  end;
end {UpdateCRC32};


{~b}


end.
