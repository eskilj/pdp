#Compile Program

SRC_DIR 	= src
EX_DIR		= example
INC_DIR		= $(EX_DIR)/inc
OUT_DIR		= output

ACTOR_DIR	= actor
BUILD_DIR 	= build
MOD_DIR		= mod
SRC_DIR 	= src

EXEC 		= main

SRC 		= $(SRC_DIR)/pool.F90 $(SRC_DIR)/actor_comm.F90 $(SRC_DIR)/actor.F90 $(SRC_DIR)/director.F90
INC_SRC 	= $(INC_DIR)/*.F90
SIM_SRC		= $(EX_DIR)/param.F90 $(EX_DIR)/actors/*.F90 $(EX_DIR)/simulation.F90 $(EX_DIR)/main.F90

#COMPILE CIRRUS
LFLAGS		= -lm
CC			= mpif90

#COMPILE MAC:
INC_MAC 	= -J
INC_CIRRUS	= -module

all: 
	$(CC) $(SRC) $(INC_SRC) $(SIM_SRC) $< $(INC_CIRRUS) $(MOD_DIR) -o $(BUILD_DIR)/$(EXEC) $(LFLAGS)

mac:
	$(CC) $(SRC) $(INC_SRC) $(SIM_SRC) $< $(INC_MAC) $(MOD_DIR) -o $(BUILD_DIR)/$(EXEC) $(LFLAGS)

run:
	qsub run.pbs

out:
	cat $(OUT_DIR)/*.OU

clean:
	clear
	rm -rf main *.o *.mod $(BUILD_DIR)/* $(MOD_DIR)/* $(OUT_DIR)/*
