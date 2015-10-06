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




// commented line below is necessary to export the function defined just after.

// [[Rcpp::export]]
List rcpp_stiagent(List params) {
	
	
	string _DIR_IN = params["folder_inputs"]; //../inputs/";
		
	
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
	
	double horizon	= getParameterFromFile("horizon_years", _DIR_IN + "in_simulation.csv");
	double timeStep	= getParameterFromFile("timestep_days", _DIR_IN + "in_simulation.csv")/365.0;
	
	
	double horizon_prtn = 20.0;
	double timestep_prtn = 1.0/12.0;
	bool TraceNetwork = false;
	unsigned int iter_mc = 1;
	int displayProgress = 11;
	
	string file_init_STI = _DIR_IN + "in_STI_initial_prevalence.csv";
	
	vector<string> file_intervention;
	string file_interv_base =_DIR_IN + "in_interv_baseline_wrapper.csv";
	vectorFromCSVfile_string(file_intervention,file_interv_base.c_str(), 1);
	
	file_intervention = trim(file_intervention);
	
	Simulation S = runSimulation_one(P,
									 file_init_STI,
									 file_intervention,
									 horizon_prtn, timestep_prtn,
									 horizon, timeStep,
									 TraceNetwork,
									 displayProgress,
									 iter_mc);
	
	return List::create(Named("prevHIV") = prevHIV
						);
	
}