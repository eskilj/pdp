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

#PREC_SRC 	= src/precision.F90
#SRC 		= actor/pool.F90, actor/actor.F90 actor/director.F90
SRC 		= $(SRC_DIR)/pool.F90 $(SRC_DIR)/actor_comm.F90 $(SRC_DIR)/actor.F90 $(SRC_DIR)/factory.F90 $(SRC_DIR)/director.F90
INC_SRC 	= $(INC_DIR)/*.F90
SIM_SRC		= $(EX_DIR)/actors/*.F90 $(EX_DIR)/simulation.F90 $(EX_DIR)/main.F90
# INC_SRC 	= src/ran2.F90 src/squirrel-functions.F90 
# SIM_SRC 	= src/squirrel.F90 src/counter.F90 src/cell.F90 src/simulation.F90 main.F90

LFLAGS		= -lm
CC			= mpif90

#COMPILE MAC:
INC_MAC 	= -J
INC_CIRRUS	= -module

all: 
	$(CC) $(SRC) $(INC_SRC) $(SIM_SRC) $< $(INC_CIRRUS) $(MOD_DIR) -o $(BUILD_DIR)/$(EXEC) $(LFLAGS)

mac:
	$(CC) $(SRC) $(INC_SRC) $(SIM_SRC) $< $(INC_MAC) $(MOD_DIR) -o $(BUILD_DIR)/$(EXEC) $(LFLAGS)

out:
	cat $(OUT_DIR)/*.OU

clean:
	rm -rf main *.o *.mod $(BUILD_DIR)/* $(MOD_DIR)/* $(OUT_DIR)/*
