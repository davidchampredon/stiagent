//
//  main_jobUnit_MC.cpp
//  STIagent_AIR
//
//  Created by David CHAMPREDON on 2014-11-19.
//  Copyright (c) 2014 David CHAMPREDON. All rights reserved.
//

#include <stdio.h>

#include "globalVar.h"

#include "dcTools.h"
#include "dcMatrix.h"
#include "RV.h"
//#include "dcDataFrame.h"

#include "population.h"
#include "simulation.h"
#include "MCsimulation.h"

#include "compare_simulation.h"

#include "calibration.h"
#include "sensitivity.h"

#include "network_analytics.h"

#include "check_tools.h"
#include "code_check.h"

#include <random>

int main(int argc, const char * argv[])
{
	
	// =============================
	// UNIT JOB FOR MONTE-CARLO
	// =============================
	
	// ARG #1: JOB NUMBER
	// ARG #2: NUMBER OF RUNS PER JOBS

	if (argc<2)	{
		cout <<endl << " ERROR in Job Unit ('main_jobUnit_SCEN.cpp'): not enough arguments" << endl;
		exit(1);
	}
	
	// For performance monitoring ---------------
	// - do not delete -
	timeval tim;
	gettimeofday(&tim, NULL);
	double t1=tim.tv_sec+(tim.tv_usec/1000000.0);
	// ------------------------------------------
	
	// Retrieve execution parameters
	int jobnum	= atoi(argv[1]);
	int nMC		= atoi(argv[2]);

	// === Display progress ===
	coutline(80);
	cout << " === (TpHIV) JOB UNIT MC #"<<jobnum
	<<" (nMC="<< nMC <<") LAUNCHED ==="<<endl;
	
	
	// =========================
	// = Initialize Population =
	// =========================
	
	Population P(0);
	
	bool debugInfo = false;
	
	string file_startpop	= _DIR_IN + "startPopulation.csv";
	string file_STI			= _DIR_IN + "in_STI.csv";
	string file_STI_SFinc	= _DIR_IN + "in_STI_SFincrease.csv";
	string file_HIVreb		= _DIR_IN + "in_HIVrebound.csv";
	string file_STI_treat	= _DIR_IN + "in_STItreatment.csv";
	string file_STI_vacc	= _DIR_IN + "in_STI_vaccine.csv";
	string file_init_STI	= _DIR_IN + "in_STI_initial_prevalence.csv";
	// Simulation parameters
	string file_simul_param = _DIR_IN + "in_simulation.csv";
	
	P.setup_for_simulation(file_startpop,
						   file_STI,
						   file_STI_SFinc,
						   file_HIVreb,
						   file_STI_treat,
						   file_STI_vacc,
						   debugInfo);
	
	if (jobnum==1) {
		// Display parameter files used:
		coutline(80);
		cout<<"PARAMETER FILES USED:"<<endl;
		cout << file_simul_param <<endl;
		cout << file_startpop <<endl;
		cout << file_STI <<endl;
		cout << file_STI_SFinc <<endl;
		cout << file_HIVreb <<endl;
		cout << file_STI_treat <<endl;
		cout << file_STI_vacc <<endl;
		cout << file_init_STI <<endl;
		// Basic information on population just created:
		P.displayInfo(false);
	}

	
	// ======================
	// === Run simulation ===
	// ======================
	

	
	double horizon	= getParameterFromFile("horizon_years", file_simul_param);
	double timeStep	= getParameterFromFile("timestep_days", file_simul_param)/365.0;
	double horizon_prtn	= getParameterFromFile("horizon_prtn_years",file_simul_param);
	double timestep_prtn= getParameterFromFile("timestep_prtn_days", file_simul_param)/365.0;
	
	bool TraceNetwork	= false;
	int displayProgress	= 11;
	
	// Scenario files
	vector<string> file_scenario;
	string file_scen = _DIR_IN + "in_scenario.csv";
	vectorFromCSVfile_string(file_scenario,file_scen.c_str() , 1);
	
	// Run & compare outcomes of all scenarios:
	run_comp_interventions(nMC,
						   P,
						   file_init_STI,
						   file_scenario,
						   horizon_prtn,
						   timestep_prtn,
						   horizon,
						   timeStep,
						   TraceNetwork,
						   displayProgress,
						   jobnum);
	
	cout << " === JOB UNIT MC #"<<jobnum<<" COMPLETED ==="<<endl;
	coutline(80);
	
	// --------------------------------------------------------------
	// COMPUTER TIME MONITORING - do not delete!
	
	gettimeofday(&tim, NULL);
	double t2=tim.tv_sec+(tim.tv_usec/1000000.0);
	
	int minutes = (int)((t2-t1)/60.0);
	double sec = (t2-t1)-minutes*60.0;
	cout << endl << " - - - Computational time for MC job #" <<jobnum <<" : ";
	cout << minutes<<" min "<<sec<<" sec" << endl;
	
	// --------------------------------------------------------------
	
	return 0;
}


