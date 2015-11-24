# stiagent
Agent-based model for sexually transmitted infections.

====================================

(As of 2015-11-23)

**Directories**

```code_model```: the engine of the model, written in C++ and wrapped for R (using Rcpp). See ```Rwrap_stiagent.cpp``` for wrapping functions from C++ to R.

```Rlibrary```: execute ```build_library``` to compile the C++ code and create the R library that wraps it. 

```inputs```: all necessary inputs for the model. There are many, so an Excel spreadsheet helps to specify and visualize them all.

```code_analysis_obj```: where simulations are run and analyzed with Rscripts. 
 * ```main_comp.R``` compares outcomes of several scenarios.
 * ```test.R``` performs various tests, mostly for debugging.


The other directories are not actively used and should be cleaned-up once a stable version of this code runs with R wrapping.

====================================

**Simulation Analysis**

*1) Goal*

We want to compare various epidemiological outcomes under different intervention scenarios using an hypothetical syphilis vaccine. See ```goals.xlsx```. Three populations are considered, each representing a typical population with low/high Syphilis and HIV prevlences.

*2) Preparatory work*

Run simulations with the baseline scenario on the 3 populations and check 
 * i) prevalence reached equilibrium from t=30y (when new interventions start) to 50y (horizon)
 * ii) population features and time series sensible

For now, these tests are done with `make test_baseline_prev.out`.

*3) Comparisons*

Run the simulations with 3 main interventions, using 20 years waning immunity as central assumption (do 5y and no waning in supplements, discuss the differences)

For now, this is done with ```main_comp.R```

