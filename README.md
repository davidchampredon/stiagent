# stiagent
Agent-based model for sexually transmitted infections.

====================================

(As of 2015-11-23)

**Directories**

```code_model```: the engine of the model, written in C++ and wrapped for R (using Rcpp). See ```Rwrap_stiagent.cpp``` for wrapping functions from C++ to R.

```Rlibrary```: execute ```build_library``` to compile the C++ code and create the R library that wraps it. 

```inputs```: all necessary inputs for the model. There are many, so an Excel spreadsheet helps to specify and visualize them all.

```code_analysis_obj```: where simulations are run and analyzed with Rscripts. 

The other directories are not actively used and should be cleaned-up once a stable version of this code runs with R wrapping.

====================================

**Simulation Analysis**

*1) Goal*

We want to compare various epidemiological outcomes under different intervention scenarios using an hypothetical syphilis vaccine. See ```goals.xlsx```. Three populations are considered, each representing a typical population with low/high Syphilis and HIV prevlences.

*2) Preparatory work*

a) Parameters:

 * i) parameters that are *common* across all populations (e.g. STI features) must be specified in the Excel spreadsheet `inputs/STIagent_front.xlsm`. Run the VBA macros to save the parameters in ad hoc csv files.
 * ii) parameters that are *specific* to a population (e.g. partnership fomation) must be specified in the Excel spreadsheet `inputs/_pop_X/_specific_PRMSET_pop_X.xlsm`. Run the VBA macros to save the parameters in ad hoc csv files.
 * iii) execute `setup-param` to copy the parameters files at the correct locations.

b) Equilibrium:

Run simulations with the baseline scenario on the 3 populations and check 
 * i) prevalence reached equilibrium from t=30y (when new interventions start) to 50y (horizon)
 * ii) population features and time series sensible

For now, these tests are done with `make test_baseline_prev.out`.

*3) Comparisons*

Run the simulations with 3 main interventions, using 20 years waning immunity as central assumption (do 5y and no waning in supplements, discuss the differences)

For now, this is done executing `./docompscen X`in the `code_analysis_obj` folder (where `X` is the population label, i.e., A,B or C).

Time benchmarks: 
 * with 6 MC iterations and 5 scenarios (including baseline), takes about 2 hours on 11 cpus (earnserv)
 * with 30 MC iterations and 5 scenarios (including baseline), takes about 4 hours on 11 cpus (earnserv)

