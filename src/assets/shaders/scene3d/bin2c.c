#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>

int main(int argc, char **argv) {
	FILE *inputFile, *outputFile;
  char* inputFileName = argv[1];
  char* variableName = argv[2];
  char* outputFileName = argv[3];
  if(argc != 4){
    fprintf(stderr, "Usage: %s <input file> <variable name> <output file>\n", argv[0]);
    return 1;
  }
  if(!strcmp(inputFileName, "-")){
    inputFile = stdin;
  }else{
    inputFile = fopen(inputFileName, "rb");
  }
  if(!inputFile){
    fprintf(stderr, "Can't open input file \"%s\"", inputFileName);
    return errno;
  }
  if(!strcmp(outputFileName, "-")){
    outputFile = stdout;
  }else{
    outputFile = fopen(outputFileName, "w");
  }
  if(!outputFile){
    fprintf(stderr, "Can't open output file \"%s\"", outputFileName);
    return errno;
  }
  fseek(inputFile, 0, SEEK_END);
  size_t inputFileSize = ftell(inputFile);
  fseek(inputFile, 0, SEEK_SET);
  size_t inputPosition = 0;
  fprintf(outputFile, "#include <stdint.h>\n");
  fprintf(outputFile, "const uint8_t %s_data[%zu] = {\n", variableName, inputFileSize);
  while(inputPosition < inputFileSize){
    uint8_t buffer[16];
    size_t readSize = fread(buffer, 1, sizeof(buffer), inputFile);
    if(!readSize){
      fprintf(stderr, "Can't read input file \"%s\"", inputFileName);
      return errno;
    }
    for(size_t i = 0; i < readSize; ++i){
      fprintf(outputFile, "0x%02x", buffer[i]);
      if((inputPosition + i + 1) < inputFileSize){
        fprintf(outputFile, ", ");
      }
    }
    fprintf(outputFile, "\n");
    inputPosition += readSize;
  }
  fprintf(outputFile, "};\n"); 
  fprintf(outputFile, "const uint32_t %s_size = %zu;\n", variableName, inputFileSize);
  fclose(inputFile);
  fclose(outputFile);
  return 0;
}
