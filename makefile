MAKEFLAGS += --silent

modules = \
	MIPS.HazardUnit \
	MIPS.ControlUnit \
	MIPS.RegisterFile \
	MIPS.RAM \
	MIPS.DecodeModule \
	MIPS.WriteBackModule \
	MIPS.ForwardUnit \
	MIPS.InstrModule \
	MIPS.MemModule \
	MIPS.ArithModule

clash_src = $(wildcard src/MIPS/**/*.hs)
verilog_src = $(wildcard src/MIPS/**/*.v verilog/MIPS/**/*.v)

all: gen build

gen:
	echo [Gen] Generating MIPS files...
	echo -e ":verilog $(modules) \n :q" | stack repl --with-ghc clash

build:
	echo [Build] Building the project...
	iverilog -Wall -Winfloop $(verilog_src) -o MIPS_CPU
	echo [Build] The generated file is available at ./MIPS_CPU

run:
	echo [Run] Running...
	./MIPS_CPU

clean:
	echo [Clean] Cleaning...
	rm -rf verilog MIPS_CPU DATA_RAM.txt
	echo [Clean] Done

test:
	for no in $$(seq 1 9); do \
		echo [Test] "Test Case $$no Diff:"; \
		cp tests/machine_code$${no}.txt instructions.bin; \
		./MIPS_CPU > DATA_RAM.txt; \
		diff -BbI"^[^01]" DATA_RAM.txt tests/DATA_RAM$${no}.txt; \
	done
	echo [Test] Test complete.
