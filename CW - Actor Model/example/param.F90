module param_mod
  implicit none

  ! Actor state
  integer, parameter :: NEW_STATE = 0
  integer, parameter :: READY = 1
  integer, parameter :: DEAD_STATUS = 2
  

  ! Simulation specific parameters
  integer, parameter :: NUM_CELLS=16      ! Cells in environment
  integer, parameter :: MAX_SQUIRRELS=200  ! Max squirrels
  integer, parameter :: INIT_SQUIRRELS=34 ! Number of squirrels initially 
  integer, parameter :: INIT_INFECTED=4   ! Number of squirrels initially infected
  integer, parameter :: SIM_DURATION=24   ! Length of simulation, in months
  
  real, parameter :: MONTH_DURATION = 0.05   ! Length of a month, in seconds - for director


  ! Comm Tags
  integer, parameter :: START_TAG = 107
  integer, parameter :: KILL_TAG = 102

  integer, parameter :: MONTH_TAG = 104
  integer, parameter :: STEP_TAG = 109
  integer, parameter :: CELL_TAG = 110


  ! Squirrel life-cycle parameters
  integer, parameter :: BIRTH_INTERVAL = 50
  integer, parameter :: INFECTION_INTERVAL = 50
  integer, parameter :: MIN_INFECTED_STEPS = 50



end module param_mod