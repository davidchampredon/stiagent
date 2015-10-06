//
//  main.cpp
//  Epidemic_Models
//
//  Created by David Champredon
//  Copyright (c) 2012-14. All rights reserved.
//

//#include <sys/time.h>

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
	system("date");
	system("pwd");
	
	
	// For performance monitoring
	// - do not delete -
	timeval tim;
	gettimeofday(&tim, NULL);
	double t1=tim.tv_sec+(tim.tv_usec/1000000.0);
	// ------------------------------------------
	
	
	// STOPPED HERE (GLOBAL):
	
	// 2015-03-13:
	// ----------
	// calibration analysis in progress
	// see file "read_singleSimul_vs_target-NEW.R"
	
	
		
	string _DIR_IN = "../inputs/";
	
	
	// Clean all previous output files
	// (some files are incremental, so can get huge)
//	system("./cleanout");
	
	
	if(0) CODECHECK_mandatory();
	
	
	//	Intervention ii("in_treatment.csv");
	//	ii.displayInfo();
	//	exit(99);
	//
	
	// Switches to determine what will be done
	// for this program execution
	string main_switches_file = _DIR_IN + "main_switches.csv";
	
	bool doChecks		= (bool)(getParameterFromFile("doChecks", main_switches_file)); // "1" is Strongly advised
	bool doTest			= (bool)(getParameterFromFile("doTest", main_switches_file));
	bool doSingleRun	= (bool)(getParameterFromFile("doSingleRun", main_switches_file));
	bool doMCRun		= (bool)(getParameterFromFile("doMCRun", main_switches_file));
	bool doMultiScenario= (bool)(getParameterFromFile("doMultiScenario", main_switches_file));
	bool doCalibration	= (bool)(getParameterFromFile("doCalibration", main_switches_file)); // avoid this (for debug only) Rather use serial farming
	bool doSensi		= (bool)(getParameterFromFile("doSensi", main_switches_file));
	
	
	string calib_files = "calibration_filename_wrapper_ZM4.csv";
	int DHSphase = 4;
	
	coutline(80);
	cout<<"doChecks= "		<< doChecks<<endl;
	cout<<"doTest= "		<< doTest<<endl;
	cout<<"doSingleRun= "	<< doSingleRun<<endl;
	cout<<"doMCRun= "		<< doMCRun<<endl;
	cout<<"doCalibration= "	<< doCalibration<<endl;
	cout<<"doSensi= "		<< doSensi<<endl;
	coutline(80);
	
	// Mandatory Checks (some used in documentation)
	
	//if (doChecks) CODECHECK_mandatory();
	
	
	if (!doTest)
	{
		
		// Generate founding individuals
		system("Rscript generateIndividuals.R > generateIndividuals.out");
		
		
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
		
		
		if (doSingleRun)
		{
			double horizon_prtn = 20.0;
			double timestep_prtn = 1.0/12.0;
			bool TraceNetwork = false;
			unsigned int iter_mc = 1;
			int displayProgress = 11;
			
			string file_init_STI = _DIR_IN + "in_STI_initial_prevalence.csv";
			
			vector<string> file_intervention;
			string file_interv_base =_DIR_IN + "in_interv_baseline_wrapper.csv";
			vectorFromCSVfile_string(file_intervention,file_interv_base.c_str(), 1);
			
			displayVector(file_intervention);
			
			file_intervention = trim(file_intervention);
			
			Simulation S = runSimulation_one(P,
											 file_init_STI,
											 file_intervention,
											 horizon_prtn, timestep_prtn,
											 horizon, timeStep, 
											 TraceNetwork,
											 displayProgress,
											 iter_mc);
			S.displayInfo();
			
			//cout<<"GLOBAL DISTANCE FROM TARGETS:"<<S.calibration_distance_targets()<<endl;
			//displayVector(S.get_calibrationDistances());
			
			
			if (0)
			{
				cout<<endl<<"Setting targets...";
				
				// Set all targets for this simulation
				calibration_set_all_target_wrap(S, _DIR_CALIB + calib_files,DHSphase);
				
				// Save main response variables
				cout << endl << "Saving output files..."<<endl;
				S.save_outputs_demog(_DIR_OUT);
				S.save_outputs_prtnr(_DIR_OUT);
				S.save_outputs_sex(_DIR_OUT);
				S.save_outputs_epi(_DIR_OUT);
				
				// Check how far this simulation is from targets
				cout<<endl<<"Calculating distance from targets...";
				cout<<endl<<"Distance from all targets:"<<S.calc_distanceFromAllTargets()<<endl;
			}
			
			// Save output files
			S.get_population().saveToCSVFile(_DIR_OUT + "./last_population.out");
			vectorToFile(S.get_population().census_AgeGap(), _DIR_OUT + "agegaps.out");
			
			double prevHIV = S.get_population().STI_prevalence(HIV);
			cout << endl<<"HIV prevalence:"<<prevHIV<<endl;
			
			vector<double> agebreaks;
			for(int i= 11;i<=70;i++)
				agebreaks.push_back((double)(i));
		
			S.get_population().displayInfo(false);
			//get_nursery().census_infected(s)
			
			cout<<endl<<"MTCT DEBUG:"<<endl;
			cout<<"HIV->"<< S.get_nursery().census_infected(HIV)<<endl;
			cout<<"Tp->" << S.get_nursery().census_infected(Tp)<<endl;
			
		}
		
		
		
		// =============================
		// ===    MONTE CARLO RUN    ===
		// =============================
		
		if (doMCRun)
		{
			
			// Simulate with several Monte Carlo iterations
			// (test environment: will be serial execution - for parallel, see Makefile)
			
			unsigned int nMC = getParameterFromFile("MCiter", "in_simulation.csv");
			
			double		horizon_prtn	= getParameterFromFile("horizon_prtn_years", "in_simulation.csv");
			double		timestep_prtn	= getParameterFromFile("timestep_prtn_days", "in_simulation.csv")/365.0;
			
			
			// Intervention specification files
			
			vector<string> file_intervention;
			vectorFromCSVfile_string(file_intervention,
									 "in_intervention_wrapper.csv", 1);
			vector<string> file_intervention2;
			vectorFromCSVfile_string(file_intervention2,
									 "in_intervention_wrapper2.csv", 1);
			
			
			bool		TraceNetwork	= false;
			int			displayProgress = 11;
			
			string		file_init_STI	= "in_STI_initial_prevalence.csv";
			int			jobnum = 1;
			
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
			// Store results in object
			MCsimulation MCS(Smc);
		
			
			// Another simulation to compare with
			
			bool do_comparison = false;
			vector<Simulation> Smc2;
			
			if (do_comparison)
			{
				force_seed_reset();
				Smc2 = runSimulationMC(nMC,
									   P,
									   file_init_STI,
									   file_intervention2,
									   horizon_prtn,
									   timestep_prtn,
									   horizon,
									   timeStep,
									   TraceNetwork,
									   displayProgress,
									   jobnum);
			}
			
			//cout<<endl<<"DISTANCE FROM TARGETS:";
			//displayVector(MCS.distance_from_targets());
			
			
			
			// Compare simulations
			if(do_comparison)
			{
				MCsimulation MCS2(Smc2);
				
				double test_mtct = comp_simul_mean_MTCT(Tp, MCS, MCS2);
				double test_cuminc = comp_simul_mean_cumul_incidence(Tp, MCS, MCS2);
				
				cout << "DEBUG DIFF MTCT = "<< test_mtct <<endl;
				cout << "DEBUG DIFF CUM.INC = "<< test_cuminc <<endl;
			}
			
			// DEBUG
			MCS.displayInfo();
			displayVector(MCS.mean_prevalence_ts(HIV));
			displayVector(MCS.mean_prevalence_ts(Tp));
		}
		
		
		if (doMultiScenario){
			// Simulate with several Monte Carlo iterations
			// (test environment: will be serial execution - for parallel, see Makefile)
			
			unsigned int nMC		= getParameterFromFile("MCiter", "in_simulation.csv");
			double horizon_prtn		= getParameterFromFile("horizon_prtn_years", "in_simulation.csv");
			double timestep_prtn	= getParameterFromFile("timestep_prtn_days", "in_simulation.csv")/365.0;
			bool TraceNetwork		= false;
			int	displayProgress		= 11;
			string file_init_STI	= "in_STI_initial_prevalence.csv";
			int	jobnum				= 1;
			
			// Scenario files files
			
			vector<string> file_scenario;
			vectorFromCSVfile_string(file_scenario,
									 "in_scenario.csv", 1);
			
			cout<<endl<<"Scenario files:";
			displayVector(file_scenario);
			
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
		}
		
		
		
		
		// =============================
		// === C A L I B R A T I O N ===
		// =============================
		
		if (doCalibration)
		{
			//file name (wrapper) containing file names of LHS limit
			string limit_LHS_file_wrapper	= _DIR_CALIB + "calib_lhs_prm_WRAPPER.csv";
			string file_init_STI			= "in_STI_initial_prevalence.csv";
			string file_interv_wrapper		= "in_intervention_wrapper.csv";
			
			unsigned int nLHS	= 4;
			unsigned int nMC	= 2;
			unsigned int nJobs	= 1;
			
			
			dcDataFrame d = LHS_explore( P,
										limit_LHS_file_wrapper, //file name (wrapper) containing file names of LHS limit
										file_init_STI,
										file_interv_wrapper,
										nLHS,
										nMC,
										nJobs
										);
			
			d.saveToCSV(_DIR_CALIB+"LHS_explore-singlerun.csv", true);

			coutline(80);
			cout<<"DEBUG LHS_explore:";
			d.display();
		}
		
		
		
		// =============================
		// === S E N S I T I V I T Y ===
		// =============================
		
		if (doSensi)
		{
			// CALCULATE SENSITIVITIES TO ALL INPUT PARAMETERS
			// (baseline parameters define futher up in the code)
			
			
			coutline(40);
			cout<<" RUNNING  SENSITIVITIES ..."<<endl;
			
			double	s_horizon		= 4.0;
			double	s_timeStep		= 0.08;
			int		s_nMonteCarlo	= 3; // large value will reduce stochastic noise
			double	s_relativeBump	= 0.20;
			
			sensitivity_distance_calib_from_files(_DIR_CALIB + calib_files,
												  P,
												  s_horizon, s_timeStep,
												  s_nMonteCarlo, s_relativeBump,
												  DHSphase);
			
			cout<<" SENSITIVITIES DONE!"<<endl;
			coutline(40);
			
			//			sensitivity_distance_calib_from_files("calib_target_ageDistrib_Kenya_1980.csv",
			//												  "calib_target_ageGaps_KE5.csv",
			//												  "calib_target_singleRatio_Kenya.csv",
			//												  P,s_horizon,s_timeStep,s_nMonteCarlo,s_relativeBump);
		}
		
		
		
		
	} // ---- end of !doTest -----
	
	
	
	// --- TESTS ----------------------------
	
	if (doTest)
	{
		
		vector<double> a = vector_seq(1.0, 3.3, 4);
		vector<double> b = vector_seq_by(1.0, 3.7, 0.5);
		displayVector(a);
		displayVector(b);
	}
	
	// ---- END TESTS -----------------------
	
	
	
	
	// --------------------------------------------------------------
	// COMPUTER TIME MONITORING - do not delete!
	
	gettimeofday(&tim, NULL);
	double t2=tim.tv_sec+(tim.tv_usec/1000000.0);
	
	int minutes = (int)((t2-t1)/60.0);
	double sec = (t2-t1)-minutes*60.0;
	cout << endl << " - - - Computational time : ";
	cout << minutes<<" min "<<sec<<" sec" << endl;
	
	// --------------------------------------------------------------
	
	return 0;
}





