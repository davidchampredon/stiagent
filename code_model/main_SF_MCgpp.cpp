//
//  main_SF_MC.cpp
//
//
//  Created by David CHAMPREDON on 2014-11-19.
//
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
	// ==============================================
	//  SERIAL FARMING WRAPPER FOR MONTE CARLO RUNS
	// ==============================================
	//
	
	
	// Integrity checks
	stopif(argc<=1,"Serial Farming: not enough arguments!");
	
	
	// retrieve the type of execution
	std::string arg1(argv[1]);
	bool doSingleRun	= (arg1=="singlerun"?true:false);
	bool doScenario		= (arg1=="scenario"?true:false);
	
	// Sharcnet or local
	bool sharcnet_env = false;
	if(argc>1){
		std::string arg2(argv[2]);
		if (arg2=="sharcnet") sharcnet_env = true;
	}

	
	stopif(!doSingleRun && !doScenario, "Serial Farming: unknown execution type!");
	
	system("date");
	system("pwd");
	
	// Clean all previous output files
	// (some files are incremental, so can get huge)
	system("./cleanout");
	
	ofstream fstart("SF_MC_timestamp.txt");
	fstart<<"dummy"<<endl;
	
	int nMCtotal	= getParameterFromFile("MCiter", "in_simulation.csv");
	int nCPU		= getParameterFromFile("nCPU", "in_simulation.csv");
	int nMCperCPU	= nMCtotal/nCPU;
	
	coutline(80);
	cout << "*** SINGLE SIMULATION WITH "<<nMCtotal<<" MC ITERATIONS ***"<<endl;
	
	// Mandatory code checks
	CODECHECK_mandatory();
	
	// Launch unit jobs (1 for each CPU)
	
	for (int i_job = 1; i_job <= nCPU; i_job++)
	{
		
		// Determine the type of execution
		string unit_name;
		if (doSingleRun)	unit_name = "./tphiv_U_MC_GPP ";
		if (doScenario)		unit_name = "./tphiv_U_SCEN_GPP ";
		
		// DEBUG
		if (doScenario) cout <<endl<<" SCENARIO MODE"<<endl;
		// -----
		
		// Execution
		
		// -- for sharcnet --
		string sharc_cmd = "sqsub -o alljobs.out -r 1d --mpp 1G " + unit_name
		+ int2string(i_job) + " " + int2string(nMCperCPU);
		
		string local_cmd = unit_name + int2string(i_job) + " " + int2string(nMCperCPU) + " &";
		string cmd_line = (sharcnet_env? sharc_cmd:local_cmd);
		system(cmd_line.c_str());
		
		
		cout << "Unit job " + int2string(i_job) + "/"<<nCPU<<" launched..." <<endl;
	}
	return 0;
}
