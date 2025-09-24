# for creating new projects with default contents 
MMDD_DATE := $(shell date "+%Y-%m-%d")
PROJECT ?= project_$(MMDD_DATE)

SAMPLE_PROJ ?= default.hs
CONSTRAINTS ?= icestick_constraints.pcf
OUTPUT_NAME ?= output

# create a new directory with Makefile and default contents in it
new: 
	@mkdir $(PROJECT)
	@echo "Creating new project directory: $(PROJECT)"
	@cp ./Makefile ./$(PROJECT)
	@mkdir ./$(PROJECT)/src
	@cp ./samples/$(SAMPLE_PROJ) ./$(PROJECT)/src/$(PROJECT).hs
	@mkdir ./$(PROJECT)/outputs
	@mkdir ./$(PROJECT)/constraint_files
	@cp ./samples/$(CONSTRAINTS) ./$(PROJECT)/constraint_files/constraints.pcf

#synthesize design with yosys for ice40
synth_ice40: 
	@echo "Synthesizing Design with Yosys"
	yosys -p "synth_ice40 -json ./outputs/$(OUTPUT_NAME).json" verilog/*/*.v 


# using final output.json, bring up yosys show feature map
show:
	$(MAKE) synth_ice40
	yosys ./outputs/$(OUTPUT_NAME).json -p "show" 

#run nextpnr for ice40
pnr: 
	@echo "Running Place and Route"
	nextpnr-ice40  --json ./outputs/$(OUTPUT_NAME).json --hx1k --package tq144 --pcf ./constraint_files/constraints.pcf --asc ./outputs/$(OUTPUT_NAME).asc

#generate binary bitstream
bitstream: 
	@echo "Generating Bitstream"
	icepack ./outputs/$(OUTPUT_NAME).asc ./outputs/$(OUTPUT_NAME).bin

#program binary onto device 
flash: 
	@echo "Flashing Device"
	sudo iceprog ./outputs/$(OUTPUT_NAME).bin

# build up to flashing 
build: 
	@echo "Building $(PROJECT):"
	$(MAKE) synth_ice40
	$(MAKE) pnr
	$(MAKE) bitstream
	
# synth, pnr, bitstream, and flash
all: 
	@echo "Building and flashing $(PROJECT):"
	$(MAKE) synth_ice40
	$(MAKE) pnr
	$(MAKE) bitstream
	$(MAKE) flash
