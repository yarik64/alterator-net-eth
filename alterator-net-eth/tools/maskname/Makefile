PARANOID_FLAGS=-Wunused-function -Wunused-label -Wunused-variable -Wunused-value -Wall -W -Wshadow \
	-Wpointer-arith -Wcast-align -Wconversion -Waggregate-return -Wstrict-prototypes \
	-Werror -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute \
	-Wredundant-decls -Wdisabled-optimization -Wmissing-noreturn -Wextra

CFLAGS=$(RPM_OPTFLAGS)

INSTALL=/usr/bin/install

all: maskname
clean:
	$(RM) maskname
install: all
	$(INSTALL) -pDm755 maskname $(bindir)/maskname
