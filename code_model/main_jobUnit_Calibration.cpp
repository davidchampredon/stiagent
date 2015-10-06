	//
//  main.cpp
//  Epidemic_Models
//
//  Created by David Champredon on 12-05-27.
//  Copyright (c) 2012-13. All rights reserved.
//

//#include <sys/time.h>

#include "globalVar.h"

#include "dcTools.h"
#include "dcMatrix.h"
//#include "dcDataFrame.h"

#include "population.h"
#include "simulation.h"
#include "MCsimulation.h"
#include "calibration.h"
#include "sensitivity.h"
#include "RV.h"
#include "network_analytics.h"

#include "check_tools.h"
#include "code_check.h"

#include <random>

int main(int argc, const char * argv[])
{
	
	/// UNIT JOB FOR CALIBRATION
	/// ========================
	///
	/// ARG #1: JOB NUMBER
	/// ARG #2: NUMBER OF LHS EXECUTIONS PER JOB
	/// ARG #3: MONTE CARLO RUNS (FOR EACH LHS)

	
	
	// For performance monitoring
	// - do not delete -
	timeval tim;
	gettimeofday(&tim, NULL);
	double t1=tim.tv_sec+(tim.tv_usec/1000000.0);
	// ---------------------------------------
	
	if (argc<3)
	{
		cout <<endl << " ERROR in Job Unit ('main_jobUnit_Calibration.cpp'): not enough arguments" << endl;
		exit(1);
	}

	
	cout << endl << " === JOB UNIT #"<<argv[1]
	<<" (nLHS="<< argv[2]
	<<" ; nMC=" <<argv[3]
	<<") ==="<<endl;
	
	
	// Parameters file name
	int i_job	= atoi(argv[1]);
	int nLHS	= atoi(argv[2]);
	int nMC		= atoi(argv[3]);
	
		

	// ======================
	// === Initialization ===
	// ======================
	
	
	// Generate founding individuals
	//system("Rscript generateIndividuals.R > generateIndividuals.out");
	
	
	// Initialize empty Population object
	Population P(0);
	bool debugInfo=true;
	
	P.setup_for_simulation("startPopulation.csv",
						   "in_STI.csv",
						   "in_STI_SFincrease.csv",
						   "in_HIVrebound.csv",
						   "in_STItreatment.csv",
						   debugInfo);
	
	// DEBUG
	cout << "Population set-up for job #"<<i_job<<endl;
	
	
	
	// Make sure the seed is different
	// across all jobs
	_RANDOM_GENERATOR.seed(123 + i_job*789);
	
	
	// =======================
	// === CALIBRATION LHS ===
	// =======================
	
	
	//file name (wrapper) containing file names of LHS limit
	string limit_LHS_file_wrapper	= _DIR_CALIB + "calib_lhs_prm_WRAPPER.csv";
	string file_init_STI			= "in_STI_initial_prevalence.csv";
	string file_interv_wrapper		= "in_intervention_wrapper.csv";
	
	// Explore the parameter space using Latin Hypercube Sampling
	// The data frame generated has the following format:
	//
	// Parameter name			|  param set 1  | param set 2	| ... | param set nLHS|
	// ________________________________________________________________________________
	// p1						|  [value]		|  [value]		| ... |  [value]
	// p2						|  [value]		|  [value]		| ... |  [value]
	// p3						|  [value]		|  [value]		| ... |  [value]
	// ...						|  ..			|  ...			| ... |  ...
	// pN						|  [value]		|  [value]		| ... |  [value]
	// mean(MC) dist from target|  [dist1]		|  [dist2]		| ... |  [distnLHS]
	// var(MC) dist from target |  [var1]		|  [var2]		| ... |  [varnLHS]
	
	// This data frame is analyzed outside C++ code (in R)
	// to determine the best (~closest to targets) parameter set
	
	dcDataFrame d = LHS_explore(P,
								limit_LHS_file_wrapper,
								file_init_STI,
								file_interv_wrapper,
								nLHS,
								nMC,
								i_job
								);
	
	// Save data frame to file (one per job, will be merged outside C++)
	
	string filename = _DIR_CALIB+"LHS_explore_job_" + to_string(i_job)+".out";
	
	d.saveToCSV(filename, true);

	
	
	
	
	// === OLD STUFF (commented on 2015-03-15) ===
	//file name (wrapper) containing file names of LHS limit
//	string limit_LHS_file_wrapper = _DIR_CALIB + "calib_lhs_prm_WRAPPER.csv";
//	string target_file_wrapper = _DIR_CALIB + "calibration_filename_wrapper_ZM4.csv";
//	
//	calibration_LHS_new(P,limit_LHS_file_wrapper,
//						target_file_wrapper,
//						nLHS, nMC,i_job);
// ====================================================
	
	
	
	
	// --------------------------------------------------------------
	// COMPUTER TIME MONITORING - do not delete!
	
	gettimeofday(&tim, NULL);
	double t2=tim.tv_sec+(tim.tv_usec/1000000.0);
	
	int		minutes	= (int)((t2-t1)/60.0);
	double	sec		= (t2-t1)-minutes*60.0;
	// --------------------------------------------------------------
	
	cout << endl << "~~~> JOB UNIT #"<<argv[1]<<" was completed in ";
	cout << minutes<<" min "<<sec<<" sec" << endl;
	
    return 0;
}

