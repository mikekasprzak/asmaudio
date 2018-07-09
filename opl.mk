TARGET					:=	opl.drv

SOURCE_FILES			:=	$(wildcard src/opl/*.asm)

$(TARGET): $(SOURCE_FILES)
	nasm $(SOURCE_FILES) -fbin -o $@

clean:
	rm -fr $(TARGET)

.PHONY: clean
