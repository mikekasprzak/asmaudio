TARGET					:=	asmaudio.com

SOURCE_FILES			:=	$(wildcard src/*.asm)
DRIVER_FILES			:=	speaker.drv tandy.drv opl.drv roland.drv

$(TARGET): $(SOURCE_FILES) $(DRIVER_FILES)
	nasm $(SOURCE_FILES) -fbin -o $@

%.drv: %.mk always
	$(MAKE) -f $<

run: $(TARGET)
	dosbox $(TARGET)

clean:
	rm -fr $(TARGET) $(DRIVER_FILES)

.PHONY: run clean always
