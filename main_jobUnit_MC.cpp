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
	
	
	
	
	if (argc<2)
	{
		cout <<endl << " ERROR in Job Unit ('main_jobUnit_MC.cpp'): not enough arguments" << endl;
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
	
	
	// Simulate with several Monte Carlo iterations
	// (test environment: will be serial execution - for parallel, see Makefile)
	string file_simul_param = "in_simulation.csv";
	
	double horizon		= getParameterFromFile("horizon_years", file_simul_param);
	double timeStep		= getParameterFromFile("timestep_days", file_simul_param)/365.0;
	double horizon_prtn	= getParameterFromFile("horizon_prtn_years", file_simul_param);
	double timestep_prtn= getParameterFromFile("timestep_prtn_days", file_simul_param)/365.0;
	
	
	// Intervention specification files
	
	vector<string> file_intervention;
	vectorFromCSVfile_string(file_intervention,
							 "in_interv_baseline_wrapper.csv", 1);
	file_intervention = trim(file_intervention);
	
	bool		TraceNetwork	= false;
	int			displayProgress = 11;
	
	
	// Initialize empty Population object
	Population P(0);
	
	bool debugInfo=true;
	
	string file_startpop	= "startPopulation.csv";
	string file_STI			= "in_STI.csv";
	string file_STI_SFinc	= "in_STI_SFincrease.csv";
	string file_HIVreb		= "in_HIVrebound.csv";
	string file_STI_treat	= "in_STItreatment.csv";
	string file_STI_vacc	= "in_STI_vaccine.csv";
	string file_init_STI	= "in_STI_initial_prevalence.csv";
	
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
	
	// Run all MC iterations
	vector<Simulation> Smc = runSimulationMC(nMC,
											 P,
											 file_init_STI,
											 file_intervention,
											 horizon_prtn,
											 timestep_prtn,
											 horizon,
											 timeStep,
											 TraceNetwork,
											 displayProgress,
											 jobnum);
	
	//	 Save trace for ONLY ONE monte-carlo trial
	//	 (used for analysis outside c++)
	
	for(int j=0;j<Smc.size();j++)
		Smc[j].get_population().saveToCSVFile(_DIR_OUT + "./last_population_job_"+to_string(jobnum)+"_mc"+to_string(j) +".out");
	
	if(jobnum==1){
		Smc[0].get_population().saveToCSVFile(_DIR_OUT + "./last_population.out");
		vectorToFile(Smc[0].get_population().census_AgeGap(), _DIR_OUT + "agegaps.out");
	}
	
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


