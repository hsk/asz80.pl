# assembler source files
ASSRCS			=	main.s

# object files
OBJS			=	$(ASSRCS:.s=.rel)

# vpath search directories
VPATH			=	src:objects:disk

# target name
TARGET			=	usr

all: $(TARGET).bin
	openmsx -machine Sanyo_PHC-70FD -diska disk

$(TARGET).bin: $(TARGET).ihx
	bin/ihx2bload objects/$(TARGET).ihx -o disk/$(TARGET).bin

$(TARGET).ihx: $(OBJS)
	sdcc -mz80 --no-std-crt0 --nostdinc --nostdlib $(foreach file,$(OBJS),objects/$(file)) -o objects/$(TARGET).ihx --code-loc 0xc000 --data-loc 0xc000

%.rel: %.s
	sdasz80 -o objects/$@ src/$*.s

bin/dskmgr: bin/dskmgr.cpp bin/basic.hpp
	cd bin; g++ dskmgr.cpp -o dskmgr
dsk: $(TARGET).bin bin/dskmgr
	rm -f objects/usr.dsk
	./bin/dskmgr objects/usr.dsk create disk/*
rundsk: dsk
	openmsx -machine Sanyo_PHC-70FD -diska objects/usr.dsk

clean:
	rm -rf objects/*
