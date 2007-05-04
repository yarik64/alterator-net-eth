#include <stdio.h>
#include <stdlib.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <errno.h>

static 
void
__attribute__ ((__noreturn__))
usage(void)
{
	puts("Usage:program mask-code");
	exit(EXIT_FAILURE);
}
	       
static
uint32_t
prefix2mask(uint32_t prefix)
{
	if (prefix)
    		return htonl((uint32_t)~((1 << (32 - prefix)) - 1));
    	else
		return 0;
}

static
unsigned int
str2int(const char *str)
{
	unsigned long code = 0;
	char   *p = NULL;

	errno = 0;
	code = strtoul(str, &p, 10);
	if (!*str || *p || errno || code > 32)
		usage();

	return code;
}

int main(int argc,char **argv)
{
	uint32_t code;
	struct in_addr netmask;

	if (argc <= 1) usage();

	code = str2int(argv[1]);
	netmask.s_addr = prefix2mask(code);
	puts(inet_ntoa(netmask));
	
	exit(EXIT_SUCCESS);
}
