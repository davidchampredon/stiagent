##########################################
####
####   Makefile for STIAGENT
####
##########################################


# test_baseline_prev.out = Test equilibrium prevalence 




### Tests for prevalence equilibrium
###

# In 'inputs', folders '_pop_A' etc. specify
# the model parameters for each population 
# (e.g. A, B and C)
# There are common parameter files (e.g. STI_features.csv)
# and specific ones. The specifics define
# the specificities of the given population
# and is generated by Excel 

INPUTS =  inputs_A/* inputs_B/* inputs_C/*
TBSCRIPTS = code_analysis_obj/*.* code_analysis_obj/test-baseline

test_baseline_prev.out: $(INPUTS) $(TBSCRIPTS) setup-param
	./setup-param
	./code_analysis_obj/test-baseline > $@



### Comparison of outcomes between scenarios
###

comp_scen.out: $(INPUTS) $(TBSCRIPTS)
	Rscript ./code_analysis_obj/main_comp.R