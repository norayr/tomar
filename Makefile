# determine the architecture
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# set the ARCH variable based on the detected architecture
ifeq ($(UNAME_S),Linux)
	ifeq ($(UNAME_M),x86_64)
		ARCH := x86_64-linux
	else ifeq ($(UNAME_M),aarch64)
		ARCH := aarch64-linux
	else ifeq ($(UNAME_M),armv7l)
		ARCH := arm-linux
	else
		ARCH := unknown
	endif
else
	ARCH := unknown
endif

# print the detected architecture
$(info ARCH is set to $(ARCH))

# define the path to lazarus, this is where is it in case of chimaera
#LAZARUS = "/usr/lib/lazarus/2.0.10"
#LAZARUS = "/usr/lib/lazarus/2.2.6"
LAZARUS = "/usr/lib/lazarus/4.0"

# Define the PATH to include the necessary directories for fpc and fpcres
CUSTOM_PATH = "/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"
export PATH = $(CUSTOM_PATH)

# Define the project and output file
PROJECT = rssreader.lpr
OUTFILE = tomar
FPC ?= fpc
PREFIX ?= /usr
DESTDIR ?=

UNITDIR := build/units

# Define the FPC command
FPC = fpc

# Define the compilation parameters
PARAMS =  $(PROJECT) -FU/tmp -Xs -Xg -MObjFPC -Scgi -O1 -gl -vewnhi -l \
	-Fu$(LAZARUS)/components/lazutils \
	-Fu$(LAZARUS)/lcl/units/$(ARCH)/ \
	-Fu$(LAZARUS)/lcl/units/$(ARCH)/gtk2/ \
	-Fu$(LAZARUS)/packager/units/$(ARCH)/ \
	-Fu$(LAZARUS)/components/turbopower_ipro/ \
	-Fu. -o$(OUTFILE) -dLCL -dLCLgtk2

# Default target to build the project
all:
	$(FPC) $(OUTFILE)
$(OUTFILE): $(PROJECT) MainForm.pas
	mkdir -p $(UNITDIR)
	$(FPC) $(PARAMS)

clean:
	rm -rf build $(OUTFILE) *.o *.ppu lib/*.o lib/*.ppu

install: install_hildon

install_hildon:
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 0755 $(OUTFILE) $(DESTDIR)$(PREFIX)/bin/tomar

	install -d $(DESTDIR)$(PREFIX)/share/applications/hildon
	install -m 0644 tomar.desktop $(DESTDIR)$(PREFIX)/share/applications/hildon/tomar.desktop

	install -d $(DESTDIR)$(PREFIX)/share/pixmaps
	install -m 0644 icon/receiver_48x48.png $(DESTDIR)$(PREFIX)/share/pixmaps/tomar.png

