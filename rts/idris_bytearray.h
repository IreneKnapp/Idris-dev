#ifndef _IDRISBYTEARRAY_H
#define _IDRISBYTEARRAY_H

VAL idris_emptyByteArray(VM *vm);
VAL idris_byteArrayLength(VM *vm, VAL byteArray);
VAL idris_byteArrayGetByte(VM *vm, VAL byteArray, VAL offset);
VAL idris_byteArrayReplaceByte(VM *vm, VAL byteArray, VAL offset, VAL byte);
VAL idris_byteArrayGetDataPiece(VM *vm, VAL byteArray, VAL offset, VAL length);
VAL idris_byteArrayReplaceDataPiece
  (VM *vm, VAL byteArray, VAL offset, VAL length, VAL piece);

#endif
