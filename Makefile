TARGET					:=	asmaudio.com

SOURCE_FILES			:=	$(wildcard src/*.asm)

$(TARGET): $(SOURCE_FILES)
	nasm $(SOURCE_FILES) -fbin -o $@

run: $(TARGET)
	dosbox $(TARGET)
	
clean:
	rm -fr $(TARGET)

.PHONY: run clean
