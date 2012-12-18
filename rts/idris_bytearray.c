#include "idris_rts.h"
#include "idris_bytearray.h"
#include "idris_gmp.h"

VAL idris_emptyByteArray(VM *vm) {
    Closure* cl = allocate(vm, sizeof(Closure) + sizeof(uint8_t), 0);
    SETTY(cl, BYTEARRAY);
    cl->info.byteArray = (char*)cl + sizeof(Closure);
    
    *(uint8_t *) cl->info.byteArray = 0;

    return cl;
}

VAL idris_byteArrayLength(VM *vm, VAL byteArray) {
    return MKBIGI(*((uint8_t *) ((char *)byteArray + sizeof(Closure))));
}

VAL idris_byteArrayGetByte(VM *vm, VAL byteArray, VAL offset) {
    VAL byteArrayLength = idris_byteArrayLength(vm, byteArray);
    int offsetInRange = GETINT(idris_bigLt(vm, offset, byteArrayLength));
    if(offsetInRange) {
        uint8_t *buffer = (uint8_t*) (char*)byteArray->info.byteArray
                          + sizeof(uint8_t);
        return MKBIGI(buffer[GETINT(offset)]);
    } else {
        fprintf(stderr, "Out-of-range byteArray byte to get.");
        exit(-1);
    }
}

VAL idris_byteArrayReplaceByte(VM *vm, VAL byteArray, VAL offset, VAL byte) {
    VAL byteArrayLength = idris_byteArrayLength(vm, byteArray);
    int offsetInRange = GETINT(idris_bigLt(vm, offset, byteArrayLength));
    if(offsetInRange) {
		int byteArrayLengthI = GETINT(byteArrayLength);
        Closure* cl = allocate
            (vm, sizeof(Closure) + sizeof(uint8_t) + byteArrayLengthI, 0);
        SETTY(cl, BYTEARRAY);
        cl->info.byteArray = (char*)cl + sizeof(Closure);

        uint8_t *source = (uint8_t*)byteArray->info.byteArray;
        uint8_t *destination = (uint8_t*)cl + sizeof(Closure);
        memcpy(destination, source, sizeof(uint8_t) + byteArrayLengthI);
        
        *(destination + sizeof(uint8_t) + GETINT(offset)) = GETINT(byte);
        
        return cl;
    } else {
        fprintf(stderr, "Out-of-range byteArray byte to replace.");
        exit(-1);
    }
}

VAL idris_byteArrayGetDataPiece(VM *vm, VAL byteArray, VAL offset, VAL length) {
    VAL end = idris_bigPlus(vm, offset, length);
    VAL byteArrayLength = idris_byteArrayLength(vm, byteArray);
    int offsetInRange = GETINT(idris_bigLe(vm, end, byteArrayLength));
    if(offsetInRange) {
        int pieceLength = GETINT(length);
        Closure* cl = allocate
            (vm, sizeof(Closure) + sizeof(uint8_t) + pieceLength, 0);
        SETTY(cl, BYTEARRAY);
        cl->info.byteArray = (char*)cl + sizeof(Closure);

        *((uint8_t *) cl->info.byteArray) = pieceLength;

        uint8_t *source =
            (uint8_t*)byteArray->info.byteArray + sizeof(uint8_t)
            + GETINT(offset);
        uint8_t *destination =
            (uint8_t*)cl + sizeof(Closure) + sizeof(uint8_t);
        memcpy(destination, source, pieceLength);

        return cl;
    } else {
        fprintf(stderr, "Out-of-range byteArray piece to get.");
        exit(-1);
    }
}

VAL idris_byteArrayReplaceDataPiece
  (VM *vm, VAL byteArray, VAL offset, VAL length, VAL piece)
{
    VAL end = idris_bigPlus(vm, offset, length);
    VAL byteArrayLength = idris_byteArrayLength(vm, byteArray);
    int offsetInRange = GETINT(idris_bigLe(vm, end, byteArrayLength));
    if(offsetInRange) {
        int removedLength = GETINT(length);
        int replacedLength = GETINT(idris_byteArrayLength(vm, piece));
        int resultLength =
            GETINT(byteArrayLength) - removedLength + replacedLength;
        Closure* cl = allocate
            (vm, sizeof(Closure) + sizeof(uint8_t) + resultLength, 0);
        SETTY(cl, BYTEARRAY);
        cl->info.byteArray = (char*)cl + sizeof(Closure);

        *((uint8_t *) cl->info.byteArray) = resultLength;

        int offsetI = GETINT(offset);
        
        uint8_t *source1 = (uint8_t*)byteArray + sizeof(Closure)
            + sizeof(uint8_t) + offsetI;
        uint8_t *destination1 = (uint8_t*)cl->info.byteArray + sizeof(uint8_t);
        if(offsetI > 0) memcpy(destination1, source1, offsetI);
        
        uint8_t *source2 = (uint8_t *) piece->info.byteArray + sizeof(uint8_t);
        uint8_t *destination2 = destination1 + offsetI;
        if(replacedLength > 0) memcpy(destination2, source2, replacedLength);

        uint8_t *source3 = source1 + removedLength;
        uint8_t *destination3 = destination2 + replacedLength;

        int tailLength = GETINT(byteArrayLength) - GETINT(end);
        if(tailLength > 0) memcpy(destination3, source3, tailLength);

        return cl;
    } else {
        fprintf(stderr, "Out-of-range byteArray piece to replace.");
        exit(-1);
    }
}

