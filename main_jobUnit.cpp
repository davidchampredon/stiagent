	//
//  main.cpp
//  Epidemic_Models
//
//  Created by David Champredon on 12-05-27.
//  Copyright (c) 2012-13. All rights reserved.
//

//#include <sys/time.h>

#include "dcTools.h"
#include "dcMatrix.h"
#include "population.h"
#include "simulation.h"
#include "calibration.h"
#include "globalVar.h"
#include "RV.h"

#include "check_tools.h"

int main(int argc, const char * argv[])
{
	
	// UNIT JOB FOR CALIBRATION
	// ======================
	//
	// ARG #1: JOB NUMBER
	// ARG #2: NUMBER OF LHS EXECUTIONS PER JOB
	// ARG #3: MONTE CARLO RUNS (FOR EACH LHS)

	
	
	// For performance monitoring
	// - do not delete -
	timeval tim;
	gettimeofday(&tim, NULL);
	double t1=tim.tv_sec+(tim.tv_usec/1000000.0);
	
	
	if (argc<3)
	{
		cout <<endl << " ERROR in Job Unit ('main_jobUnit.cpp'): not enough arguments" << endl;
		exit(1);
	}

	
	cout << endl << " === JOB UNIT #"<<argv[1]<<" (nrows="<< argv[2] <<") ==="<<endl;
	
	
	// Parameters file name
	string fname = _DIR_CALIB + "calib_LHS_SF_input_" ;

	int i_job = atoi(argv[1]);
	int nrows = atoi(argv[2]);
	int nMC_calibration = atoi(argv[3]);
	

		
	// === PRE CALIBRATION ===
	
	// Start from an unpartnered population
	
	// Initialize empty Population object
	Population P0(0);
	
	// Read starting population from a file
	// output of "generateIndividuals.R"
	P0.initFromFile("startPopulation.csv",
					"in_STI.csv",
					"in_STI_SFincrease.csv",
					"in_HIVrebound.csv");
	
	// Set all parameters
	P0.setAllParameters();
	
	// Set-up pre-calibration simulation
	double horizon_preCalib = 60.0;
	double timestep_preCalib = 0.25;
	Simulation S_preCalib(horizon_preCalib, timestep_preCalib, P0, 0);
	
	// Pre-calibration run to form partnerships first
	bool logIndivInfo = false;
	bool doSex = false;
	bool traceNetwork = false;
	bool displayProgress = 0;
	
	
	S_preCalib.runAllEvents_horizon(doSex,
									logIndivInfo,
									traceNetwork,
									displayProgress);
	
	// Seed STIs' initial prevalence
	S_preCalib.STI_set_initial_prevalence("in_STI_initial_prevalence.csv");
	coutline(80);
	cout << " PRE CALIBRATION POPULATION:";
	S_preCalib.get_population().displayInfo(false);
	
	Population P1 = S_preCalib.get_population();
	
	// == END PRE-CALIBRATION ==


	// =================
	
    // Horizon for the calibration
	
	string file_calibration = _DIR_CALIB + "in_calibration.csv";
	
	double horizon_calib	= getParameterFromFile("horizon_calib", file_calibration);
	double timeStep_calib	= getParameterFromFile("timeStep_calib", file_calibration);
	
	
	// Runs the calibration using the
	// parameters limits defined in the files defined by the job number
	
	calibration_LHS_fromFile(_DIR_CALIB + "calibration_filename_wrapper.csv",
							 // Parameters (pre-sampled)
							 fname + "DMG_"+int2string(i_job)+".csv",
							 fname + "FORM_"+int2string(i_job)+".csv",
							 fname + "SPOUSAL_"+int2string(i_job)+".csv",
							 fname + "DISSOL_"+int2string(i_job)+".csv",
							 fname + "SEXACT_"+int2string(i_job)+".csv",
							 fname + "STIFTR_V_"+int2string(i_job)+".csv",
							 fname + "STIFTR_B_"+int2string(i_job)+".csv",
							 fname + "STIFTR_P_"+int2string(i_job)+".csv",
							 fname + "STIFTR_F_"+int2string(i_job)+".csv",
							 
							 nrows, i_job,
							 P1, horizon_calib, timeStep_calib,
							 nMC_calibration);
	
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

