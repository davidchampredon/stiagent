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
		cout <<endl << " ERROR in Serial Farming Wrapper ('main_SFW.cpp'): not enough arguments" << endl;
		exit(1);
	}
	
	
	system("date");
	system("pwd");
	
	ofstream fstart("SFW_timestamp.txt");
	fstart<<"dummy"<<endl;
	
	// Clean up files that are appended at each run...
	system("rm -f sexReduction.out");
	system("rm -f death*.out");
	system("rm -f birth*.out");
	
	/*
	// Initialize empty Population object
	Population P(0);
	
	// Read starting population from a file
	// output of "generateIndividuals.R"
	P.initFromFile("startPopulation.csv", "in_STI.csv","in_STI_SFincrease.csv");
	
	// Set all parameters
	P.setAllParameters();
	
	P.displayInfo(false);
	*/
	
	
	// =============================
	// === C A L I B R A T I O N ===
	// === SERIAL FARMING		 ===
	// =============================
	
	
	// Parameter space sampling
	int nLHS = atoi(argv[1]);
	
	// Serial Farming
	int nJobs = atoi(argv[2]);
	
	// Monte Carlo runs for each LHS
	int nMC_calibration = atoi(argv[3]);

	
	string fname = _DIR_CALIB + "calib_LHS_SF_input_" ;
	
	
	// Healthy checks
	if (nLHS<nJobs)
	{
		cout << "ERROR main_SFW.cpp: Can't have nLHS("<<nLHS <<")<nJobs("<<nJobs<<")"<<endl;
		exit(1);
	}
	
	
// Store parameters for calibration in vectors
			vector<vector<double> > prm_lower = calib_get_parameters(_DIR_CALIB + "calib_param_DMG_lower.csv",
																	 _DIR_CALIB + "calib_param_FORM_lower.csv",
																	 _DIR_CALIB + "calib_param_SPOUSAL_lower.csv",
																	 _DIR_CALIB + "calib_param_DISSOL_lower.csv");
			
			vector<vector<double> > prm_upper = calib_get_parameters(_DIR_CALIB + "calib_param_DMG_upper.csv",
																	 _DIR_CALIB + "calib_param_FORM_upper.csv",
																	 _DIR_CALIB + "calib_param_SPOUSAL_upper.csv",
																	 _DIR_CALIB + "calib_param_DISSOL_upper.csv");
			
			vector<vector<double> > prm_sti_lower = calib_get_parameters_STI(_DIR_CALIB + "calib_param_SEXACT_lower.csv",
																			 _DIR_CALIB + "calib_param_STIFTR_V_lower.csv",
																			 _DIR_CALIB + "calib_param_STIFTR_B_lower.csv",
																			 _DIR_CALIB + "calib_param_STIFTR_P_lower.csv",
																			 _DIR_CALIB + "calib_param_STIFTR_F_lower.csv");
			
			vector<vector<double> > prm_sti_upper = calib_get_parameters_STI(_DIR_CALIB + "calib_param_SEXACT_upper.csv",
																			 _DIR_CALIB + "calib_param_STIFTR_V_upper.csv",
																			 _DIR_CALIB + "calib_param_STIFTR_B_upper.csv",
																			 _DIR_CALIB + "calib_param_STIFTR_P_upper.csv",
																			 _DIR_CALIB + "calib_param_STIFTR_F_upper.csv");
			
			
			// Define lower and upper limit from single values read above
			vector<double> param_DMG_LowerLim = prm_lower[0];
			vector<double> param_DMG_UpperLim = prm_upper[0];
			
			vector<double> param_FORM_LowerLim = prm_lower[1];
			vector<double> param_FORM_UpperLim = prm_upper[1];
			
			vector<double> param_SPOUSAL_LowerLim = prm_lower[2];
			vector<double> param_SPOUSAL_UpperLim = prm_upper[2];
			
			vector<double> param_DISSOL_LowerLim = prm_lower[3];
			vector<double> param_DISSOL_UpperLim = prm_upper[3];
			
			vector<double> param_SEXACT_LowerLim = prm_sti_lower[0];
			vector<double> param_SEXACT_UpperLim = prm_sti_upper[0];
			
			vector<double> param_STIFTR_V_LowerLim = prm_sti_lower[1];
			vector<double> param_STIFTR_V_UpperLim = prm_sti_upper[1];
			
			vector<double> param_STIFTR_B_LowerLim = prm_sti_lower[2];
			vector<double> param_STIFTR_B_UpperLim = prm_sti_upper[2];
			
			vector<double> param_STIFTR_P_LowerLim = prm_sti_lower[3];
			vector<double> param_STIFTR_P_UpperLim = prm_sti_upper[3];

			vector<double> param_STIFTR_F_LowerLim = prm_sti_lower[4];
			vector<double> param_STIFTR_F_UpperLim = prm_sti_upper[4];
    
	

	
	// Generates the parameters input files
	// based on nLHS and nJobs
	
	calib_LHS_SerialFarming_GenInputFiles(nLHS, nJobs,fname,
										  param_DMG_LowerLim,  param_DMG_UpperLim,
										  param_FORM_LowerLim,  param_FORM_UpperLim,
										  param_SPOUSAL_LowerLim,  param_SPOUSAL_UpperLim,
										  param_DISSOL_LowerLim,  param_DISSOL_UpperLim,
										  param_SEXACT_LowerLim,  param_SEXACT_UpperLim,
										  param_STIFTR_V_LowerLim,  param_STIFTR_V_UpperLim,
										  param_STIFTR_B_LowerLim,  param_STIFTR_B_UpperLim,
										  param_STIFTR_P_LowerLim,  param_STIFTR_P_UpperLim,
										  param_STIFTR_F_LowerLim,  param_STIFTR_F_UpperLim);
	
	
	int nrows = nLHS/nJobs;
	
	// Runs the calibration using the
	// parameters limits defined in the files above
	
	for (int i_job = 1; i_job <= nJobs; i_job++)
	{
		string sharc_cmd = "sqsub -o alljobs.out -r 1d --mpp 1G ./stiagent_U " 
		+ int2string(i_job) + " " + int2string(nrows)+ " " + int2string(nMC_calibration);
		
		string local_cmd = "./stiagent_U " + int2string(i_job) + " " + int2string(nrows)+ " " + int2string(nMC_calibration) +" &";
		
		string cmd_line = local_cmd;
		
		system(cmd_line.c_str());
		cout << "Unit job " + int2string(i_job) + " launched" <<endl;
	}
	

    
    return 0;
}






