#ifndef _IDRISBLOB_H
#define _IDRISBLOB_H

VAL idris_emptyBlob(VM *vm);
VAL idris_blobLength(VM *vm, VAL blob);
VAL idris_blobGetByte(VM *vm, VAL blob, VAL offset);
VAL idris_blobReplaceByte(VM *vm, VAL blob, VAL offset, VAL byte);
VAL idris_blobGetDataPiece(VM *vm, VAL blob, VAL offset, VAL length);
VAL idris_blobReplaceDataPiece
  (VM *vm, VAL blob, VAL offset, VAL length, VAL piece);

#endif
