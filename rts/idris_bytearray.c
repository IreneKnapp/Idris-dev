#include "idris_rts.h"
#include "idris_bytearray.h"
#include "idris_gmp.h"

VAL idris_zeroedByteArray(VM *vm, VAL lengthIn) {
    size_t length = GETINT(lengthIn);
    
    ClosureType* closure = allocate
        (vm, sizeof(ClosureType) + sizeof(ByteArray), 0);
    SETTY((Closure *) closure, BYTEARRAY);
    
    ByteArray *byteArray = (ByteArray *) (closure + 1);
    byteArray->storage = allocate(vm, length, 0);
    byteArray->offset = 0;
    byteArray->length = length;
    
    bzero(byteArray->storage, length);
    
    return (Closure *) closure;
}

VAL idris_byteArrayLength(VM *vm, VAL byteArrayIn) {
    ByteArray *byteArray = (ByteArray *) ((ClosureType *) byteArrayIn + 1);
    return MKBIGI(byteArray->length);
}

VAL idris_byteArrayPeek(VM *vm, VAL byteArrayIn, VAL offsetIn) {
    VAL byteArrayLength = idris_byteArrayLength(vm, byteArrayIn);
    int offsetInRange = GETINT(idris_bigLt(vm, offsetIn, byteArrayLength));
    if(offsetInRange) {
        ByteArray *byteArray = (ByteArray *) ((ClosureType *) byteArrayIn + 1);
        return MKBIGI(byteArray->storage[GETINT(offsetIn)]);
    } else {
        fprintf(stderr, "Out-of-range ByteArray peek.");
        exit(-1);
    }
}

void idris_byteArrayPoke(VM *vm, VAL byteArrayIn, VAL offsetIn, VAL byteIn) {
    VAL byteArrayLength = idris_byteArrayLength(vm, byteArrayIn);
    int offsetInRange = GETINT(idris_bigLt(vm, offsetIn, byteArrayLength));
    if(offsetInRange) {
        ByteArray *byteArray = (ByteArray *) ((ClosureType *) byteArrayIn + 1);
        
        byteArray->storage[byteArray->offset + GETINT(offsetIn)] =
            GETINT(byteIn);
    } else {
        fprintf(stderr, "Out-of-range ByteArray poke.");
        exit(-1);
    }
}

VAL idris_byteArrayCopy(VM *vm, VAL byteArrayIn) {
    ByteArray *byteArray = (ByteArray *) ((ClosureType *) byteArrayIn + 1);
    
    ClosureType* resultClosure = allocate
        (vm, sizeof(ClosureType) + sizeof(ByteArray), 0);
    SETTY((Closure *) resultClosure, BYTEARRAY);
    
    ByteArray *resultByteArray = (ByteArray *) (resultClosure + 1);
    resultByteArray->storage = allocate(vm, byteArray->length, 0);
    resultByteArray->offset = 0;
    resultByteArray->length = byteArray->length;
    
    memcpy(resultByteArray->storage, byteArray->storage, byteArray->length);
    
    return (Closure *) resultClosure;
}

void idris_byteArrayMovePiece
    (VM *vm,
     VAL destinationByteArrayIn, VAL destinationOffsetIn,
     VAL sourceByteArrayIn, VAL sourceOffsetIn,
     VAL lengthIn)
{
    VAL sourceEndIn = idris_bigPlus(vm, sourceOffsetIn, lengthIn);
    VAL sourceByteArrayLength = idris_byteArrayLength(vm, sourceByteArrayIn);
    if(!GETINT(idris_bigLe(vm, sourceEndIn, sourceByteArrayLength))) {
        fprintf(stderr, "Out-of-range source ByteArray in move-piece.");
        exit(-1);
    }
    
    VAL destinationEndIn = idris_bigPlus(vm, destinationOffsetIn, lengthIn);
    VAL destinationByteArrayLength = idris_byteArrayLength
        (vm, destinationByteArrayIn);
    if(!GETINT(idris_bigLe(vm, destinationEndIn, destinationByteArrayLength)))
    {
        fprintf(stderr, "Out-of-range destination ByteArray in move-piece.");
        exit(-1);
    }
    
    ByteArray *sourceByteArray =
        (ByteArray *) ((ClosureType *) sourceByteArrayIn + 1);
    uint8_t *source = sourceByteArray->storage + sourceByteArray->offset;
    
    ByteArray *destinationByteArray =
        (ByteArray *) ((ClosureType *) sourceByteArrayIn + 1);
    uint8_t *destination =
        destinationByteArray->storage + destinationByteArray->offset;
    
    memmove(destination, source, GETINT(lengthIn));
}
