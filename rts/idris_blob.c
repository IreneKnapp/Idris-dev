#include "idris_rts.h"
#include "idris_blob.h"
#include "idris_gmp.h"

VAL idris_emptyBlob(VM *vm) {
    Closure* cl = allocate(vm, sizeof(Closure) + sizeof(uint64_t), 0);
    SETTY(cl, BLOB);
    cl->info.blob = (char*)cl + sizeof(Closure);
    
    *(uint64_t *) cl->info.blob = 0;

    return cl;
}

VAL idris_blobLength(VM *vm, VAL blob) {
    return MKBIGI(*((uint64_t *) ((char *)blob + sizeof(Closure))));
}

VAL idris_blobGetByte(VM *vm, VAL blob, VAL offset) {
    VAL blobLength = idris_blobLength(vm, blob);
    int offsetInRange = GETINT(idris_bigLt(vm, offset, blobLength));
    if(offsetInRange) {
        uint8_t *buffer = (uint8_t*) (char*)blob->info.blob + sizeof(uint64_t);
        return MKBIGI(buffer[GETINT(offset)]);
    } else {
        fprintf(stderr, "Out-of-range blob byte to get.");
        exit(-1);
    }
}

VAL idris_blobReplaceByte(VM *vm, VAL blob, VAL offset, VAL byte) {
    VAL blobLength = idris_blobLength(vm, blob);
    int offsetInRange = GETINT(idris_bigLt(vm, offset, blobLength));
    if(offsetInRange) {
		int blobLengthI = GETINT(blobLength);
        Closure* cl = allocate
            (vm, sizeof(Closure) + sizeof(uint64_t) + blobLengthI, 0);
        SETTY(cl, BLOB);
        cl->info.blob = (char*)cl + sizeof(Closure);

        uint8_t *source = (uint8_t*)blob->info.blob;
        uint8_t *destination = (uint8_t*)cl + sizeof(Closure);
        memcpy(destination, source, sizeof(uint64_t) + blobLengthI);
        
        *(destination + sizeof(uint64_t) + GETINT(offset)) = GETINT(byte);
        
        return cl;
    } else {
        fprintf(stderr, "Out-of-range blob byte to replace.");
        exit(-1);
    }
}

VAL idris_blobGetDataPiece(VM *vm, VAL blob, VAL offset, VAL length) {
    VAL end = idris_bigPlus(vm, offset, length);
    VAL blobLength = idris_blobLength(vm, blob);
    int offsetInRange = GETINT(idris_bigLe(vm, end, blobLength));
    if(offsetInRange) {
        int pieceLength = GETINT(length);
        Closure* cl = allocate
            (vm, sizeof(Closure) + sizeof(uint64_t) + pieceLength, 0);
        SETTY(cl, BLOB);
        cl->info.blob = (char*)cl + sizeof(Closure);

        *((uint64_t *) cl->info.blob) = pieceLength;

        uint8_t *source =
            (uint8_t*)blob->info.blob + sizeof(uint64_t) + GETINT(offset);
        uint8_t *destination = (uint8_t*)cl + sizeof(Closure) + sizeof(uint64_t);
        memcpy(destination, source, pieceLength);

        return cl;
    } else {
        fprintf(stderr, "Out-of-range blob piece to get.");
        exit(-1);
    }
}

VAL idris_blobReplaceDataPiece
  (VM *vm, VAL blob, VAL offset, VAL length, VAL piece)
{
    VAL end = idris_bigPlus(vm, offset, length);
    VAL blobLength = idris_blobLength(vm, blob);
    int offsetInRange = GETINT(idris_bigLe(vm, end, blobLength));
    if(offsetInRange) {
        int removedLength = GETINT(length);
        int replacedLength = GETINT(idris_blobLength(vm, piece));
        int resultLength =
            GETINT(blobLength) - removedLength + replacedLength;
        Closure* cl = allocate
            (vm, sizeof(Closure) + sizeof(uint64_t) + resultLength, 0);
        SETTY(cl, BLOB);
        cl->info.blob = (char*)cl + sizeof(Closure);

        *((uint64_t *) cl->info.blob) = resultLength;

        int offsetI = GETINT(offset);
        
        uint8_t *source1 = (uint8_t*)blob + sizeof(Closure)
            + sizeof(uint64_t) + offsetI;
        uint8_t *destination1 = (uint8_t*)cl->info.blob + sizeof(uint64_t);
        if(offsetI > 0) memcpy(destination1, source1, offsetI);
        
        uint8_t *source2 = (uint8_t *) piece->info.blob + sizeof(uint64_t);
        uint8_t *destination2 = destination1 + offsetI;
        if(replacedLength > 0) memcpy(destination2, source2, replacedLength);

        uint8_t *source3 = source1 + removedLength;
        uint8_t *destination3 = destination2 + replacedLength;

        int tailLength = GETINT(blobLength) - GETINT(end);
        if(tailLength > 0) memcpy(destination3, source3, tailLength);

        return cl;
    } else {
        fprintf(stderr, "Out-of-range blob piece to replace.");
        exit(-1);
    }
}

