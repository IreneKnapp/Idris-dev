#ifndef _IDRISBYTEARRAY_H
#define _IDRISBYTEARRAY_H

VAL idris_newByteArray(VM *vm, VAL lengthIn);
VAL idris_byteArrayLength(VM *vm, VAL byteArrayIn);
VAL idris_byteArrayPeek(VM *vm, VAL byteArrayIn, VAL offsetIn);
void idris_byteArrayPoke(VM *vm, VAL byteArrayIn, VAL offsetIn, VAL byteIn);
void idris_byteArrayZeroPiece
    (VM *vm, VAL byteArrayIn, VAL offsetIn, VAL lengthIn);
void idris_byteArrayMovePiece
    (VM *vm,
     VAL destinationByteArrayIn, VAL destinationOffsetIn,
     VAL sourceByteArrayIn, VAL sourceOffsetIn,
     VAL lengthIn);
VAL idris_byteArrayCopyForGC(VM *vm, VAL byteArrayIn);

#endif
