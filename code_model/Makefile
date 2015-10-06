##################################################################
##################################################################
####   MAKEFILE FOR TPHIV
##################################################################
##################################################################

# ==================================
#     === Compiling Variables ===
# ==================================

C_GPP := g++
C_CLANG := clang++

C_GPP_OPT := -Wall -O3 -std=c++11 -Wno-sign-compare -Wno-predefined-identifier-outside-function
C_CLANG_OPT := -Wall -O3 -Wno-predefined-identifier-outside-function
# -Wno-deprecated

# =================
# === Libraries ===
# =================

# GSL and OOL libraries needed for
# random and minimisation routines
LINK_LIB := -lgsl #-lool  # <---- For Apple Mac
LINK_LIB_GPP := -lgsl -lgslcblas # <--- For Earnservs

# ==================
# === FILE LISTS ===
# ==================

DIR_IN = ../inputs

SOURCE_LIST := dcMatrix.cpp dcTools.cpp RV.cpp \
		individuals.cpp population.cpp simulation.cpp \
		calibration.cpp STI.cpp check_tools.cpp \
		sensitivity.cpp network_analytics.cpp \
		code_check.cpp dcDataFrame.cpp globalVar.cpp \
		MCsimulation.cpp nursery.cpp intervention.cpp \
		compare_simulation.cpp\

OBJ_LIST2 := main.cpp $(SOURCE_LIST)

OBJ_LIST_U := main_jobUnit.cpp $(SOURCE_LIST)
OBJ_LIST_U_SCEN := main_jobUnit_SCEN.cpp $(SOURCE_LIST)
OBJ_LIST_SFW := main_SFW.cpp $(SOURCE_LIST)

OBJ_LIST_U_MC := main_jobUnit_MC.cpp $(SOURCE_LIST)
OBJ_LIST_SF_MC := main_SF_MC.cpp $(SOURCE_LIST)
OBJ_LIST_SF_MC_GPP := main_SF_MCgpp.cpp $(SOURCE_LIST)

OBJ_LIST_U_CALIB := main_jobUnit_Calibration.cpp $(SOURCE_LIST)
OBJ_LIST_SF_CALIB := main_SF_Calibration.cpp $(SOURCE_LIST)

OBJ_LIST_U_CALIB_GPP := main_jobUnit_Calibration.cpp $(SOURCE_LIST)
OBJ_LIST_SF_CALIB_GPP := main_SF_Calibration_gpp.cpp $(SOURCE_LIST)

PARAM_LIST := $(DIR_IN)/in_simulation.csv $(DIR_IN)/in_scenario.csv

# ===================
# === EXECUTABLES ===
# ===================

PROG_NAME := tphiv
PROG_NAME2 := tphiv2
PROG_NAME_U := tphiv_U

# single run monte-carlo:
PROG_NAME_U_MC := tphiv_U_MC
PROG_NAME_SF_MC := tphiv_SF_MC

# multiple scenario 
# (several intervention simulations MC)
PROG_NAME_U_SCEN := tphiv_U_SCEN
PROG_NAME_SF_SCEN := tphiv_SF_SCEN

# calibration:
PROG_NAME_U_CALIB := tphiv_U_CALIB
PROG_NAME_SF_CALIB := tphiv_SF_CALIB


# -- GPP compiler:

PROG_NAME_U_CALIB_GPP := tphiv_U_CALIB_GPP
PROG_NAME_SF_CALIB_GPP := tphiv_SF_CALIB_GPP
PROG_NAME_U_MC_GPP := tphiv_U_MC_GPP
PROG_NAME_SF_MC_GPP := tphiv_SF_MC_GPP
PROG_NAME_U_SCEN_GPP := tphiv_U_SCEN_GPP
PROG_NAME_SF_SCEN_GPP := tphiv_SF_SCEN_GPP


R_CMD := Rscript   #R CMD BATCH


# ======================
# ==== COMPILE ONLY ====
# ======================


# ==== CLANG COMPLIER ====

$(PROG_NAME2): $(OBJ_LIST2)
	$(C_CLANG) $(C_CLANG_OPT) $(OBJ_LIST2) $(LINK_LIB) -o $@

$(PROG_NAME_U): $(OBJ_LIST_U)
	$(C_CLANG) $(C_CLANG_OPT) $(OBJ_LIST_U) $(LINK_LIB) -o $@

$(PROG_NAME_SFW): $(OBJ_LIST_SFW)
	$(C_CLANG) $(C_CLANG_OPT) $(OBJ_LIST_SFW) $(LINK_LIB) -o $@


# ==== GPP COMPILER (FOR EARNSERV, SHARCNET) ====

$(PROG_NAME): $(OBJ_LIST2)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST2) $(LINK_LIB_GPP) -o $@

$(PROG_NAME_U_GPP): $(OBJ_LIST_U)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST_U) $(LINK_LIB_GPP) -o $@

$(PROG_NAME_SFW_GPP): $(OBJ_LIST_SFW)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST_SFW) $(LINK_LIB_GPP) -o $@


# ==================================
# === Monte Carlo Serial Farming ===
# ==================================

$(PROG_NAME_U_MC): $(OBJ_LIST_U_MC)
	$(C_CLANG) $(C_CLANG_OPT) $(OBJ_LIST_U_MC) $(LINK_LIB) -o $@

$(PROG_NAME_U_SCEN): $(OBJ_LIST_U_SCEN)
	$(C_CLANG) $(C_CLANG_OPT) $(OBJ_LIST_U_SCEN) $(LINK_LIB) -o $@

$(PROG_NAME_SF_MC): $(OBJ_LIST_SF_MC)
	$(C_CLANG) $(C_CLANG_OPT) $(OBJ_LIST_SF_MC) $(LINK_LIB) -o $@

# == GPP version:

$(PROG_NAME_U_MC_GPP): $(OBJ_LIST_U_MC)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST_U_MC) $(LINK_LIB_GPP) -o $@

$(PROG_NAME_SF_MC_GPP): $(OBJ_LIST_SF_MC_GPP)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST_SF_MC_GPP) $(LINK_LIB_GPP) -o $@

$(PROG_NAME_U_SCEN_GPP): $(OBJ_LIST_U_SCEN)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST_U_SCEN) $(LINK_LIB_GPP) -o $@

$(PROG_NAME_SF_SCEN_GPP): $(OBJ_LIST_SF_SCEN_GPP)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST_SF_SCEN_GPP) $(LINK_LIB_GPP) -o $@

# ========================================
# === Calibration (LHS) Serial Farming ===
# ========================================

$(PROG_NAME_U_CALIB): $(OBJ_LIST_U_CALIB)
	$(C_CLANG) $(C_CLANG_OPT) $(OBJ_LIST_U_CALIB) $(LINK_LIB) -o $@

$(PROG_NAME_SF_CALIB): $(OBJ_LIST_SF_CALIB)
	$(C_CLANG) $(C_CLANG_OPT) $(OBJ_LIST_SF_CALIB) $(LINK_LIB) -o $@

# == GPP version:

$(PROG_NAME_U_CALIB_GPP): $(OBJ_LIST_U_CALIB_GPP)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST_U_CALIB_GPP) $(LINK_LIB_GPP) -o $@

$(PROG_NAME_SF_CALIB_GPP): $(OBJ_LIST_SF_CALIB_GPP)
	$(C_GPP) $(C_GPP_OPT) $(OBJ_LIST_SF_CALIB_GPP) $(LINK_LIB_GPP) -o $@



# =======================
# === RUN SIMULATIONS ===
# =======================

startPopulation.csv: generateIndividuals.R in_populationFeatures.csv in_STIinit.csv
	$(R_CMD) $<

runSimulMC: $(PROG_NAME_SF_MC) $(PROG_NAME_U_MC) $(PARAM_LIST)
	./$< singlerun &

runSimulScen.log: $(PROG_NAME_SF_MC) $(PROG_NAME_U_SCEN) $(PARAM_LIST)
	./$< scenario > $@


# --- GPP based:

runSimulMCgpp.log: $(PROG_NAME_SF_MC_GPP) $(PROG_NAME_U_MC_GPP) $(PARAM_LIST)
	./$< singlerun local > $@

runSimulScenGPP.log: $(PROG_NAME_SF_MC_GPP) $(PROG_NAME_U_SCEN_GPP) $(PARAM_LIST)
	./$< scenario local > $@

runSimulgpp: $(PROG_NAME) 
	./$< &

runSimulgpp_full: $(PROG_NAME) ##startPopulation.csv
	./$<
	Rscript ./OUT/read_ALL.R &
	
runSimul: $(PROG_NAME2) ##startPopulation.csv
	./$< &
	
runSimul_full: $(PROG_NAME2) ##startPopulation.csv
	./$<
	Rscript ./OUT/read_ALL.R &


# - Sharcnet environment:

runSimulMCsharc.log: $(PROG_NAME_SF_MC_GPP) $(PROG_NAME_U_MC_GPP) $(PARAM_LIST)
	./$< singlerun sharcnet > $@


# ===================
# === CALIBRATION ===
# ===================

# ARG #1: LHS SAMPLING SIZE
# ARG #2: NUMBER OF JOBS TO BE RUN (MUST DIVIDE MC)
# ARG #3: MONTE CARLO RUNS (overall)

N_LHS := 4
N_JOBS := 6
N_MC := 6

calibration.out: $(PROG_NAME_SF_CALIB) $(PROG_NAME_U_CALIB)
	./$< $(N_LHS) $(N_JOBS) $(N_MC) > $@ &

calibration_gpp.out: $(PROG_NAME_SF_CALIB_GPP) $(PROG_NAME_U_CALIB_GPP)
	./$< $(N_LHS) $(N_JOBS) $(N_MC) > $@ &





# =================
# === CLEAN-UP ===
# =================

cleanout:
	rm -f *.out
	rm -f ./OUT/*.out
	rm -f ./CALIBRATION/*.out
	rm -f ./VisualizeInput/*.out

clean:
	rm -f *.o
	rm -f ./OUT/*.out
	rm -f ./CALIBRATION/*.out
	rm -f tphiv*
