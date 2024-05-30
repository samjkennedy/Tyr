#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
void print_bool(bool b) { b ? printf("true\n") : printf("false\n"); }
typedef enum {Mode_Read,
Mode_Write,
Mode_Append,
Mode_Read_Binary,
Mode_Write_Binary,
Mode_Append_Binary,
} Mode;static const char *Mode_Values[6] = {"Read",
"Write",
"Append",
"Read_Binary",
"Write_Binary",
"Append_Binary",
};typedef struct {unsigned long size;
} Metadata;typedef struct {FILE * handle;
Metadata metadata;
} File;unsigned long get_file_size(FILE * handle) {unsigned long size = 0;
fseek(handle, 0, SEEK_END);
size = ftell(handle);
fseek(handle, 0, SEEK_SET);
return size;
}
File open(char * path, Mode mode) {char * mode_string = "";
switch (mode) {case Mode_Read: {mode_string = "r";
; break; }case Mode_Write: {mode_string = "w";
; break; }case Mode_Append: {mode_string = "a";
; break; }case Mode_Read_Binary: {mode_string = "rb";
; break; }case Mode_Write_Binary: {mode_string = "wb";
; break; }case Mode_Append_Binary: {mode_string = "ab";
; break; }}FILE * handle = fopen(path, mode_string);
Metadata metadata = (Metadata){.size=get_file_size(handle)};
return (File){.handle=handle, .metadata=metadata};
}
void close(File file) {fclose(file.handle);
}
void write(File file, char * text) {fwrite(text, 1, strlen(text), file.handle);
}
void writeln(File file, char * text) {fwrite(text, 1, strlen(text), file.handle);
fwrite("\n", 1, 1, file.handle);
}
char * read_all(File file) {unsigned long n = file.metadata.size;
FILE * handle = file.handle;
char * buf = "";
buf = malloc(n);
fread(buf, 1, n, handle);
return buf;
}
int main() {File f = open("samples/quine.tyr", Mode_Read);
printf("%s\n", read_all(f));
return 0;
}
