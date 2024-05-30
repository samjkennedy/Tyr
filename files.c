#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
void print_bool(bool b) { b ? printf("true\n") : printf("false\n"); }
typedef enum {Mode_Read,
Mode_Write,
Mode_Append,
} Mode;static const char *Mode_Values[3] = {"Read",
"Write",
"Append",
};typedef struct {unsigned long size;
} Metadata;typedef struct {FILE * handle;
Metadata metadata;
} File;typedef struct {signed long offset;
signed long len;
char ** data;
} slice_string;unsigned long get_file_size(FILE * handle) {unsigned long size = 0;
fseek(handle, 0, SEEK_END);
size = ftell(handle);
fseek(handle, 0, SEEK_SET);
return size;
}
File open(char * path, Mode mode) {char * mode_string = "";
switch (mode) {case Mode_Read: {mode_string = "r";
; break; }case Mode_Write: {mode_string = "w";
; break; }case Mode_Append: {mode_string = "a";
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
int main(int argc, char **argv) {slice_string args = (slice_string){0, argc, argv};for (int it = args.offset; it < args.len; it++) {char * arg = args.data[it];{
printf("%s\n", arg);
}
}{
File f = open(args.data[1], Mode_Write);
writeln(f, "Hello from tyr!");
writeln(f, "Hello from tyr again!");
write(f, "Goodbye from tyr!");
close(f);
}
File f = open(args.data[1], Mode_Read);
printf("%s\n", read_all(f));
close(f);
return 0;
}
