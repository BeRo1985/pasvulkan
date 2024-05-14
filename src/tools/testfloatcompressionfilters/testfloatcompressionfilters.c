#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// Deflate compression
#include <zlib.h>

// Forward transform for 32-bit float data using value-wise delta encoding and order preservation
void ForwardTransform32BitFloatDataValueWiseDelta(const void *aInData, void *aOutData, size_t aDataSize) {
    size_t Count = aDataSize >> 2;
    uint32_t Previous = 0;
    uint32_t Value, Delta;

    for (size_t Index = 0; Index < Count; ++Index) {
        Value = ((uint32_t *)aInData)[Index];
        Value = Value ^ ((uint32_t)((int32_t)((uint32_t)(-(int32_t)(Value >> 31)))) | (uint32_t)0x80000000);
        Delta = Value - Previous;
        Previous = Value;
        ((uint8_t *)aOutData)[Index] = (Delta >> 24) & 0xff;
        ((uint8_t *)aOutData)[Index + Count] = (Delta >> 16) & 0xff;
        ((uint8_t *)aOutData)[Index + (Count * 2)] = (Delta >> 8) & 0xff;
        ((uint8_t *)aOutData)[Index + (Count * 3)] = (Delta >> 0) & 0xff;
    }
}

// Backward transform for 32-bit float data using value-wise delta encoding and order preservation
void BackwardTransform32BitFloatDataValueWiseDelta(const void *aInData, void *aOutData, size_t aDataSize) {
    size_t Count = aDataSize >> 2;
    uint32_t Value = 0;

    for (size_t Index = 0; Index < Count; ++Index) {
        Value += ((uint32_t)((uint8_t *)aInData)[Index] << 24) |
                 ((uint32_t)((uint8_t *)aInData)[Index + Count] << 16) |
                 ((uint32_t)((uint8_t *)aInData)[Index + (Count * 2)] << 8) |
                 ((uint32_t)((uint8_t *)aInData)[Index + (Count * 3)] << 0);
        ((uint32_t *)aOutData)[Index] = Value ^ ((uint32_t)((uint32_t)((Value >> 31) - 1) | (uint32_t)0x80000000));
    }
}

// Forward transform for 32-bit float data using byte-wise delta encoding and order preservation
void ForwardTransform32BitFloatDataBytewiseDelta(const void *aInData, void *aOutData, size_t aDataSize) {
    
    size_t Count = aDataSize >> 2;

    uint8_t PreviousA = 0, PreviousB = 0, PreviousC = 0, PreviousD = 0;   

    for (size_t Index = 0; Index < Count; ++Index) {
        
        uint32_t Value = ((uint32_t *)aInData)[Index];
        Value = Value ^ ((uint32_t)((int32_t)((uint32_t)(-(int32_t)(Value >> 31)))) | (uint32_t)0x80000000);

        uint8_t ValueA = (Value >> 24) & 0xff;
        uint8_t ValueB = (Value >> 16) & 0xff;
        uint8_t ValueC = (Value >> 8) & 0xff;
        uint8_t ValueD = (Value >> 0) & 0xff;

        uint8_t DeltaA = ValueA - PreviousA;
        uint8_t DeltaB = ValueB - PreviousB;
        uint8_t DeltaC = ValueC - PreviousC;
        uint8_t DeltaD = ValueD - PreviousD;

        PreviousA = ValueA;
        PreviousB = ValueB;
        PreviousC = ValueC;
        PreviousD = ValueD;

        ((uint8_t *)aOutData)[Index] = DeltaA;
        ((uint8_t *)aOutData)[Index + Count] = DeltaB;
        ((uint8_t *)aOutData)[Index + (Count * 2)] = DeltaC;
        ((uint8_t *)aOutData)[Index + (Count * 3)] = DeltaD;      

    }

}

// Backward transform for 32-bit float data using byte-wise delta encoding and order preservation
void BackwardTransform32BitFloatDataByteWiseDelta(const void *aInData, void *aOutData, size_t aDataSize) {
  
  size_t Count = aDataSize >> 2;

  uint8_t ValueA = 0, ValueB = 0, ValueC = 0, ValueD = 0;

  for (size_t Index = 0; Index < Count; ++Index) {

    ValueA += ((uint8_t *)aInData)[Index];
    ValueB += ((uint8_t *)aInData)[Index + Count];
    ValueC += ((uint8_t *)aInData)[Index + (Count * 2)];
    ValueD += ((uint8_t *)aInData)[Index + (Count * 3)];
    
    uint32_t Value = ((uint32_t)ValueA << 24) | ((uint32_t)ValueB << 16) | ((uint32_t)ValueC << 8) | ((uint32_t)ValueD << 0);

    ((uint32_t *)aOutData)[Index] = Value ^ ((uint32_t)((uint32_t)((Value >> 31) - 1) | (uint32_t)0x80000000));

  }

}
   
int32_t zlibCompress(const void *aInData, void *aOutData, size_t aInDataSize, size_t* aOutDataSize) {
  z_stream Stream;
  Stream.zalloc = Z_NULL;
  Stream.zfree = Z_NULL;
  Stream.opaque = Z_NULL;
  Stream.avail_in = aInDataSize;
  Stream.next_in = (Bytef *)aInData;
  Stream.avail_out = *aOutDataSize;
  Stream.next_out = (Bytef *)aOutData;
  deflateInit(&Stream, Z_DEFAULT_COMPRESSION);
  int32_t result = deflate(&Stream, Z_FINISH);
  *aOutDataSize = Stream.total_out;
  deflateEnd(&Stream);
  return result;
}

int32_t zlibDecompress(const void *aInData, void *aOutData, size_t aInDataSize, size_t aOutDataSize) {
  z_stream Stream;
  Stream.zalloc = Z_NULL;
  Stream.zfree = Z_NULL;
  Stream.opaque = Z_NULL;
  Stream.avail_in = aInDataSize;
  Stream.next_in = (Bytef *)aInData;
  Stream.avail_out = aOutDataSize;
  Stream.next_out = (Bytef *)aOutData;
  inflateInit(&Stream);
  int32_t result = inflate(&Stream, Z_FINISH);
  inflateEnd(&Stream);
  return result;
}

int32_t main(const int32_t aArgc, const char *aArgv[]) {

  void *Data = NULL;
  size_t DataSize = 0;

  // Check for data.bin
  FILE *File = fopen("data.bin", "rb");
  if (File != NULL) {
    
    // Load test data from data.bin 
    fprintf(stdout, "Loading test data from data.bin . . . ");
    FILE *File = fopen("data.bin", "rb");
    fseek(File, 0, SEEK_END);
    DataSize = ftell(File);
    fseek(File, 0, SEEK_SET);
    Data = malloc(DataSize);
    fread(Data, 1, DataSize, File);
    fclose(File);
    fprintf(stdout, "done!\n");

  }else{

    // Generate test data
    fprintf(stdout, "Generating test data . . . ");
    DataSize = 1024 * 1024;
    Data = malloc(DataSize);
    for (size_t Index = 0; Index < DataSize / 4; ++Index) {
      ((float *)Data)[Index] = sinf((float)Index / (float)(DataSize) * 2.0f * 3.14159265359f);
    }
    fprintf(stdout, "done!\n");

  }  

  // Prefilter data using value-wise delta encoding
  fprintf(stdout, "Prefiltering data using value-wise delta encoding . . . ");
  void *PrefilteredValueWiseData = malloc(DataSize);
  ForwardTransform32BitFloatDataValueWiseDelta(Data, PrefilteredValueWiseData, DataSize);
  fprintf(stdout, "done!\n");

  // Prefilter data using byte-wise delta encoding
  fprintf(stdout, "Prefiltering data using byte-wise delta encoding . . . ");
  void *PrefilteredByteWiseData = malloc(DataSize);
  ForwardTransform32BitFloatDataBytewiseDelta(Data, PrefilteredByteWiseData, DataSize);
  fprintf(stdout, "done!\n");

  // Compress unfiltered data using zlib
  fprintf(stdout, "Compressing unfiltered data using zlib . . . ");
  size_t CompressedDataSize = DataSize * 2;
  void *CompressedData = malloc(CompressedDataSize);
  zlibCompress(Data, CompressedData, DataSize, &CompressedDataSize);
  fprintf(stdout, "done!\n");

  // Compress prefiltered value-wise data using zlib
  fprintf(stdout, "Compressing prefiltered value-wise data using zlib . . . ");
  size_t CompressedValueWiseDataSize = DataSize * 2;
  void *CompressedValueWiseData = malloc(CompressedValueWiseDataSize);
  zlibCompress(PrefilteredValueWiseData, CompressedValueWiseData, DataSize, &CompressedValueWiseDataSize);
  fprintf(stdout, "done!\n");

  // Compress prefiltered byte-wise data using zlib
  fprintf(stdout, "Compressing prefiltered byte-wise data using zlib . . . ");
  size_t CompressedByteWiseDataSize = DataSize * 2;
  void *CompressedByteWiseData = malloc(CompressedByteWiseDataSize);
  zlibCompress(PrefilteredByteWiseData, CompressedByteWiseData, DataSize, &CompressedByteWiseDataSize);
  fprintf(stdout, "done!\n");

  // Print sizes
  fprintf(stdout, "Original data size: %zu bytes\n", DataSize);
  fprintf(stdout, "Compressed data size: %zu bytes\n", CompressedDataSize);
  fprintf(stdout, "Compressed value-wise data size: %zu bytes\n", CompressedValueWiseDataSize);
  fprintf(stdout, "Compressed byte-wise data size: %zu bytes\n", CompressedByteWiseDataSize);

  // Decompress compressed value-wise data using zlib
  fprintf(stdout, "Decompressing compressed value-wise data using zlib . . . ");
  void *DecompressedValueWiseData = malloc(DataSize);
  zlibDecompress(CompressedValueWiseData, DecompressedValueWiseData, CompressedValueWiseDataSize, DataSize);
  fprintf(stdout, "done!\n");

  // Decompress compressed byte-wise data using zlib
  fprintf(stdout, "Decompressing compressed byte-wise data using zlib . . . ");
  void *DecompressedByteWiseData = malloc(DataSize);
  zlibDecompress(CompressedByteWiseData, DecompressedByteWiseData, CompressedByteWiseDataSize, DataSize);
  fprintf(stdout, "done!\n");

  // Postfilter decompressed value-wise data
  fprintf(stdout, "Postfiltering decompressed value-wise data . . . ");
  void *PostfilteredValueWiseData = malloc(DataSize);
  BackwardTransform32BitFloatDataValueWiseDelta(DecompressedValueWiseData, PostfilteredValueWiseData, DataSize);
  fprintf(stdout, "done!\n");

  // Postfilter decompressed byte-wise data
  fprintf(stdout, "Postfiltering decompressed byte-wise data . . . ");
  void *PostfilteredByteWiseData = malloc(DataSize);
  BackwardTransform32BitFloatDataByteWiseDelta(DecompressedByteWiseData, PostfilteredByteWiseData, DataSize);
  fprintf(stdout, "done!\n");

  // Compare original data with postfiltered value-wise data
  fprintf(stdout, "Comparing original data with postfiltered value-wise data . . . ");
  int32_t ValueWiseDataComparison = memcmp(Data, PostfilteredValueWiseData, DataSize);
  fprintf(stdout, "done! (comparison %s)\n", ValueWiseDataComparison == 0 ? "succeeded" : "failed");

  // Compare original data with postfiltered byte-wise data
  fprintf(stdout, "Comparing original data with postfiltered byte-wise data . . . ");
  int32_t ByteWiseDataComparison = memcmp(Data, PostfilteredByteWiseData, DataSize);
  fprintf(stdout, "done! (comparison %s)\n", ByteWiseDataComparison == 0 ? "succeeded" : "failed");

  // Free memory
  free(Data);
  free(PrefilteredValueWiseData);
  free(PrefilteredByteWiseData);
  free(CompressedData);
  free(CompressedValueWiseData);
  free(CompressedByteWiseData);
  free(DecompressedValueWiseData);
  free(DecompressedByteWiseData);
  free(PostfilteredValueWiseData);
  free(PostfilteredByteWiseData);

  return 0;
}
