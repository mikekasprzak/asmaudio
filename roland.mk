TARGET					:=	roland.drv

SOURCE_FILES			:=	$(wildcard src/roland/*.asm)

$(TARGET): $(SOURCE_FILES)
	nasm $(SOURCE_FILES) -fbin -o $@

clean:
	rm -fr $(TARGET)

.PHONY: clean
