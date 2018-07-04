TARGET					:=	speaker.bin

SOURCE_FILES			:=	$(wildcard src/speaker/*.asm)

$(TARGET): $(SOURCE_FILES)
	nasm $(SOURCE_FILES) -fbin -o $@

clean:
	rm -fr $(TARGET)

.PHONY: clean
