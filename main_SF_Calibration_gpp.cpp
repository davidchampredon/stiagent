//
//  main_SFW.cpp
//
//
//  Created by David CHAMPREDON on 11/18/2013.
//
//

//
//  main.cpp
//  Epidemic_Models
//
//  Created by David Champredon on 12-05-27.
//  Copyright (c) 2012-13. All rights reserved.
//

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
	
	// SERIAL FARMING WRAPPER
	// ======================
	//
	// ARG #1: LHS SAMPLING SIZE
	// ARG #2: NUMBER OF JOBS TO BE RUN
	// ARG #3: MONTE CARLO RUNS (in total)
	//
	

	if (argc<3)
	{
		cout <<endl << " ERROR in Serial Farming Wrapper ('main_SF_Calibration.cpp'): not enough arguments" << endl;
		exit(1);
	}
	
	
	system("date");
	system("pwd");
	
	ofstream fstart("SF_Calibration_timestamp.txt");
	fstart<<"dummy"<<endl;

	
	// =============================
	// === C A L I B R A T I O N ===
	// === SERIAL FARMING		 ===
	// =============================
	
	
	// Parameter space sampling
	int nLHS = atoi(argv[1]);
	
	// Serial Farming
	int nJobs = atoi(argv[2]);
	
	// Monte Carlo runs for each LHS
	int nMC = atoi(argv[3]);

	
	string fname = _DIR_CALIB + "calib_LHS_SF_input_" ;
	
	
	// Healthy checks
	// nJobs MUST divide nMC
	if (! (nMC%nJobs==0))
	{
		cout << "ERROR main_SF_Calibration.cpp: nJobs("<<nJobs <<"MUST divide nMC("<<nMC<<")"<<endl;
		exit(1);
	}
	
	unsigned int nMCperJob = nMC/nJobs;
	
	// Generate founding individuals
	system("Rscript generateIndividuals.R > generateIndividuals.out");
	
	
	// Runs the calibration using the
	// parameters limits defined in the files above
	
	for (int i_job = 1; i_job <= nJobs; i_job++)
	{
		// Sharcnet command line
		string sharc_cmd = "sqsub -o alljobs.out -r 1d --mpp 1G ./stiagent_U " 
		+ int2string(i_job) + " " + int2string((nLHS))+ " " + int2string(nMCperJob);
		
		// Standard Unix-like command line
		string local_cmd = "./tphiv_U_CALIB_GPP " + int2string(i_job) + " " + int2string(nLHS)+ " " + int2string(nMCperJob) +" &";
		
		// Choose environment
		string cmd_line = local_cmd;
		
		// Execute
		system(cmd_line.c_str());
		
		cout << "Calibration Unit job " + int2string(i_job) + " launched" <<endl;
	}
	

    
    return 0;
}






