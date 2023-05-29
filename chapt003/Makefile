# vpath search directories
VPATH			=	src:objects:disk

# target name
TARGET			=	usr

all: $(TARGET).bin
	openmsx -machine Sanyo_PHC-70FD -diska disk

$(TARGET).bin: $(TARGET).ihx
	swipl bin/ihx2bload.pl objects/$(TARGET).ihx -o disk/$(TARGET).bin

$(TARGET).ihx: src/main.asm
	swipl bin/asz80.pl src/main.asm > objects/$(TARGET).ihx
bin/dskmgr: bin/dskmgr.cpp bin/basic.hpp
	cd bin; g++ dskmgr.cpp -o dskmgr
dsk: $(TARGET).bin bin/dskmgr
	rm -f objects/usr.dsk
	./bin/dskmgr objects/usr.dsk create disk/*
rundsk: dsk
	openmsx -machine Sanyo_PHC-70FD -diska objects/usr.dsk

clean:
	rm -rf objects/*
