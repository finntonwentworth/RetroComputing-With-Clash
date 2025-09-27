# for creating new projects with default contents 
MMDD_DATE := $(shell date "+%Y-%m-%d")
PROJECT ?= project_$(MMDD_DATE)

SAMPLE_PROJ   ?= default.hs
SAMPLE_LIB    ?= Utils.hs
CONSTRAINTS   ?= icestick_constraints.pcf
OUTPUT_NAME   ?= output
MODULE_NAME   ?= Project

# create a new directory with Makefile and default contents in it
new: 
	@mkdir $(PROJECT)
	@echo "Creating new project directory: $(PROJECT)"
	@cp ./Makefile ./$(PROJECT)
	@mkdir ./$(PROJECT)/src
	@mkdir ./$(PROJECT)/src/RetroClash
	@cp ./samples/$(SAMPLE_PROJ) ./$(PROJECT)/src/$(PROJECT).hs
	@cp ./samples/Lib/Retroclash/$(SAMPLE_LIB) ./$(PROJECT)/src/RetroClash/$(SAMPLE_LIB)
	@mkdir ./$(PROJECT)/outputs
	@mkdir ./$(PROJECT)/constraint_files
	@cp ./samples/$(CONSTRAINTS) ./$(PROJECT)/constraint_files/constraints.pcf
	@cp ./samples/.clashi ./$(PROJECT)/.clashi

#synthesize design with yosys for ice40
# TODO: Multiple topEntity's for the bonus exercises. 
# if you create a new Project.hs with a top entity and compile it to verilog, 
# it will appear in /verilog/ as verilog/MODULE.topEntity/topEntity.v
synth_ice40: 
	@echo "Synthesizing Design: $(MODULE_NAME) with Yosys"
	@sleep 1
	yosys -p "synth_ice40 -json ./outputs/$(OUTPUT_NAME).json" verilog/$(MODULE_NAME).topEntity/*.v 


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
