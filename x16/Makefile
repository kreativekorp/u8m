all: U8MDEMO.PRG

clean:
	rm -f *.PRG *.sym

U8MDEMO.PRG: u8mdemo.s
	acme --cpu 65c02 -f cbm -o U8MDEMO.PRG -l u8mdemo.sym u8mdemo.s
