/// =================================================================
///
///   WRAPPING FOR R
///
///   Created 2015-10-06 by David Champredon
///
/// =================================================================

#include <random>
#include <Rcpp.h>
using namespace Rcpp;

#include "globalVar.h"
#include "dcTools.h"
#include "dcMatrix.h"
#include "RV.h"
#include "population.h"
#include "simulation.h"
#include "MCsimulation.h"
#include "compare_simulation.h"
#include "calibration.h"
#include "sensitivity.h"
#include "network_analytics.h"
#include "check_tools.h"
#include "code_check.h"



List dcDataFrameToRcppList(dcDataFrame df){
	
	/// Converts  dcDataFrame to a Rccp list
	
	unsigned long ncol = df.get_colname().size();
	
	// Translate the 'dcDataFrame' into a R list
	// (convert to data frame in R, I don't know how to do it here in Rcpp):
	Rcpp::List rcpplist;
	// each column of the dcDataFrame is a list:
	for(int j=0; j<ncol; j++)
		rcpplist.push_back(df.get_value().extractColumn(j));
	// set the associated name:
	rcpplist.attr("names") = df.get_colname();
	
	return rcpplist;
}


// commented line below is necessary to export the function defined just after.

// [[Rcpp::export]]
List stiagent_runsim(List params) {
	
	
	string _DIR_IN		= params["folder_inputs"]; //../inputs/";
	string _DIR_CALIB	= params["folder_calib"];
	
	
	// ======================
	// === Initialization ===
	// ======================
	
	// Initialize empty Population object
	Population P(0);
	
	bool debugInfo=true;
	
	P.setup_for_simulation(_DIR_IN + "startPopulation.csv",
						   _DIR_IN + "in_STI.csv",
						   _DIR_IN + "in_STI_SFincrease.csv",
						   _DIR_IN + "in_HIVrebound.csv",
						   _DIR_IN + "in_STItreatment.csv",
						   _DIR_IN + "in_STI_vaccine.csv",
						   debugInfo);
	
	
	// ======================
	// === Run simulation ===
	// ======================
	
	// retrieve parameters:
	double horizon			= getParameterFromFile("horizon_years", _DIR_IN + "in_simulation.csv");
	double timeStep			= getParameterFromFile("timestep_days", _DIR_IN + "in_simulation.csv")/365.0;
	double horizon_prtn		= getParameterFromFile("horizon_prtn_years", _DIR_IN + "in_simulation.csv");
	double timestep_prtn	= getParameterFromFile("timestep_prtn_days", _DIR_IN + "in_simulation.csv")/365.0;
	unsigned int iter_mc	= getParameterFromFile("MCiter", _DIR_IN + "in_simulation.csv");
	bool TraceNetwork		= false;
	
	int displayProgress = 11;
	
	string file_init_STI	= _DIR_IN + "in_STI_initial_prevalence.csv";
	
	vector<string> file_intervention;
	string file_interv_base =_DIR_IN + "in_interv_baseline_wrapper.csv";
	vectorFromCSVfile_string(file_intervention,file_interv_base.c_str(), 1);
	file_intervention = trim(file_intervention);
	
	Simulation Sobj = runSimulation_one_obj(P,
											file_init_STI,
											file_intervention,
											horizon_prtn,
											timestep_prtn,
											horizon, timeStep,
											TraceNetwork,
											displayProgress,
											iter_mc,
											_DIR_IN,
											_DIR_CALIB);
	dcDataFrame df_sim = Sobj.get_df_sim();
	
	Rcpp::List df_sim_R = dcDataFrameToRcppList(df_sim);
	
	// Outputs:
	double prevHIV = Sobj.get_population().STI_prevalence(HIV);
	
	return List::create(Named("prevHIV") = prevHIV,
						Named("df_sim") = df_sim_R);
}





// [[Rcpp::export]]
List stiagent_comp_interv(List params) {
	
	
	string _DIR_IN		= params["folder_inputs"]; //../inputs/";
	string _DIR_CALIB	= params["folder_calib"];
	int jobnum			= params["jobnum"];
	
	
	// ======================
	// === Initialization ===
	// ======================
	
	// Initialize empty Population object
	Population P(0);
	
	bool debugInfo=true;
	
	P.setup_for_simulation(_DIR_IN + "startPopulation.csv",
						   _DIR_IN + "in_STI.csv",
						   _DIR_IN + "in_STI_SFincrease.csv",
						   _DIR_IN + "in_HIVrebound.csv",
						   _DIR_IN + "in_STItreatment.csv",
						   _DIR_IN + "in_STI_vaccine.csv",
						   debugInfo);
	
	
	// Scenario files files
	string path_scenario = _DIR_IN + "in_scenario.csv";
	vector<string> file_scenario;
	vectorFromCSVfile_string(file_scenario, path_scenario.c_str(), 1);
	
	//	cout<<endl<<"Scenario files:";
	//	displayVector(file_scenario);
	
	// ======================
	// === Run simulation ===
	// ======================
	
	// retrieve parameters:
	double horizon			= getParameterFromFile("horizon_years", _DIR_IN + "in_simulation.csv");
	double timeStep			= getParameterFromFile("timestep_days", _DIR_IN + "in_simulation.csv")/365.0;
	double horizon_prtn		= getParameterFromFile("horizon_prtn_years", _DIR_IN + "in_simulation.csv");
	double timestep_prtn	= getParameterFromFile("timestep_prtn_days", _DIR_IN + "in_simulation.csv")/365.0;
	unsigned int iter_mc	= getParameterFromFile("MCiter", _DIR_IN + "in_simulation.csv");
	bool TraceNetwork		= false;
	
	int displayProgress = 11;
	
	string file_init_STI	= _DIR_IN + "in_STI_initial_prevalence.csv";
	
	vector<string> file_intervention;
	string file_interv_base =_DIR_IN + "in_interv_baseline_wrapper.csv";
	vectorFromCSVfile_string(file_intervention,file_interv_base.c_str(), 1);
	file_intervention = trim(file_intervention);
	
	vector<dcDataFrame> df_comp_interv;
	
	df_comp_interv = run_comp_interventions_obj(iter_mc,
												P,
												file_init_STI,
												file_scenario,
												horizon_prtn,
												timestep_prtn,
												horizon,
												timeStep,
												TraceNetwork,
												displayProgress,
												jobnum,
												_DIR_IN,
												_DIR_CALIB
												);
	
	unsigned long nsti = df_comp_interv.size();
	
	std::vector<string> stinames;
	for(int i=0; i<nsti; i++)
		stinames.push_back(STInameString(P.get_STI()[i].get_name()));
		
	// Returns a list of lists:
	Rcpp::List x;
	
	
	for(int i=0; i<nsti; i++)
		x.push_back(dcDataFrameToRcppList(df_comp_interv[i]));
	x.attr("names") = stinames;
	
	return(x);
}




















