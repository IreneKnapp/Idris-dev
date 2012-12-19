#include "idris_rts.h"
#include "idris_bytearray.h"
#include "idris_gmp.h"

VAL idris_newByteArray(VM *vm, VAL lengthIn) {
    size_t length = GETINT(lengthIn);
    
    Closure* closure = allocate(vm, sizeof(Closure) + GETINT(lengthIn), 0);
    SETTY(closure, BYTEARRAY);
    closure->info.byteArrayLength = length;
    
    return closure;
}

VAL idris_byteArrayLength(VM *vm, VAL byteArrayIn) {
    return MKBIGI(byteArrayIn->info.byteArrayLength);
}

VAL idris_byteArrayPeek(VM *vm, VAL byteArrayIn, VAL offsetIn) {
    size_t offset = GETINT(offsetIn);
    if((offset >= 0) && (offset < byteArrayIn->info.byteArrayLength)) {
        uint8_t *storage = (uint8_t *) (byteArrayIn + 1);
        return MKBIGI(storage[offset]);
    } else {
        fprintf(stderr, "Out-of-range ByteArray peek.");
        exit(-1);
    }
}

void idris_byteArrayPoke(VM *vm, VAL byteArrayIn, VAL offsetIn, VAL byteIn) {
    size_t offset = GETINT(offsetIn);
    if((offset >= 0) && (offset < byteArrayIn->info.byteArrayLength)) {
        uint8_t *storage = (uint8_t *) (byteArrayIn + 1);
        byteArray->storage[offset] = GETINT(byteIn);
    } else {
        fprintf(stderr, "Out-of-range ByteArray poke.");
        exit(-1);
    }
}

void idris_byteArrayZeroPiece
    (VM *vm, VAL byteArrayIn, VAL offsetIn, VAL lengthIn)
{
    size_t offset = GETINT(offsetIn);
    size_t length = GETINT(lengthIn);
    size_t end = offset + length;
    if((offset < 0)
       || (length < 0)
       || (end >= byteArrayIn->info.byteArrayLength))
    {
        fprintf(stderr, "Out-of-range ByteArray in zero-piece.");
        exit(-1);
    }
    
    uint8_t *storage = (uint8_t *) (byteArrayIn + 1);
    bzero(storage + offset, length);
}

void idris_byteArrayMovePiece
    (VM *vm,
     VAL destinationByteArrayIn, VAL destinationOffsetIn,
     VAL sourceByteArrayIn, VAL sourceOffsetIn,
     VAL lengthIn)
{
    size_t length = GETINT(lengthIn);
    
    size_t sourceOffset = GETINT(sourceOffsetIn);
    size_t sourceEnd = sourceOffset + length;
    if((sourceOffset < 0)
       || (sourceLength < 0)
       || (sourceEnd >= sourceByteArrayIn->info.byteArrayLength))
    {
        fprintf(stderr, "Out-of-range source ByteArray in move-piece.");
        exit(-1);
    }
    
    size_t destinationOffset = GETINT(destinationOffsetIn);
    size_t destinationEnd = destinationOffset + length;
    if((destinationOffset < 0)
       || (destinationLength < 0)
       || (destinationEnd >= destinationByteArrayIn->info.byteArrayLength))
    {
        fprintf(stderr, "Out-of-range destination ByteArray in move-piece.");
        exit(-1);
    }
    
    uint8_t *sourceStorage = (uint8_t *) (byteArrayIn + 1);
    uint8_t *source = source + sourceOffset;
    
    uint8_t *destinationStorage = (uint8_t *) (byteArrayIn + 1);
    uint8_t *destination = destination + destinationOffset;
    
    memmove(destination, source, GETINT(lengthIn));
}

VAL idris_byteArrayCopyForGC(VM *vm, VAL byteArrayIn) {
    Closure* resultClosure = allocate
        (vm, sizeof(Closure) + byteArrayIn->info.byteArrayLength, 1);
    SETTY(resultClosure, BYTEARRAY);
    
    resultClosure->info.byteArrayLength = byteArrayIn->info.byteArrayLength;
    
    uint8_t *storage = (uint8_t *) (byteArrayIn + 1);
    uint8_t *resultStorage = (uint8_t *) (resultClosure + 1);
    
    memcpy(resultStorage, storage, byteArrayIn->info.byteArrayLength);
    
    return resultClosure;
}

