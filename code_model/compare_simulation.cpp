//
//  compare_simulation.cpp
//  TpHIV
//
//  Created by David CHAMPREDON on 2015-01-08.
//  Copyright (c) 2015 David CHAMPREDON. All rights reserved.
//

#include "compare_simulation.h"
#include "MCsimulation.h"
#include "dcTools.h"




double comp_simul_mean_MTCT(STIname stiname,
							MCsimulation S1,
							MCsimulation S2)
{
	/// Compares b/w 2 simulations
	/// incidence of mother-to-child transmission
	/// of a given STI.
	/// Returns mean(incidence_1 - incidence_2)
	
	
	double inc1 = S1.mean_cumul_incidence_MTCT(stiname);
	double inc2 = S2.mean_cumul_incidence_MTCT(stiname);
	
	return inc1-inc2;
}



double comp_simul_mean_cumul_incidence(STIname stiname,
									   MCsimulation S1,
									   MCsimulation S2)
{
	/// Compares b/w 2 simulations
	/// cumulative incidence at simulation horizon
	/// of a given STI.
	/// Returns mean(incidence_1 - incidence_2)
	
	vector<double> inc1 = S1.mean_incidence(stiname);
	vector<double> inc2 = S2.mean_incidence(stiname);
	
	return inc1[inc1.size()-1] - inc2[inc2.size()-1];
}



double	comp_simul_mean_prevalence(STIname stiname,
								   MCsimulation S1,
								   MCsimulation S2)
{
	/// Compares b/w 2 simulations
	/// prevalence at simulation horizon
	/// of a given STI.
	/// Returns mean(prev_1 - prev_2)
	
	double prev1 = S1.mean_prevalence_final(stiname);
	double prev2 = S2.mean_prevalence_final(stiname);
	
	return prev1-prev2;
}




void writeToFile_scenario_outcome(string filename,
								  STIname stiname,
								  vector<MCsimulation> S)
{
	/// Writes to a file the comparison of outcomes
	/// b/w different interventions scenario
	/// for a given STI
	
	
	ofstream f(filename);
	stopif(S.size()==0,"Vector of simulations empty!");
	vector<string> colname;
	
	// ==== Prevalence at horizon
	
	vector<double> prev0 = S[0].prevalence_final(stiname);
	colname.push_back("prev_final0");
	
	Matrix M(prev0);
	
	for (int i=1; i<S.size(); i++){
		vector<double> prev_i = S[i].prevalence_final(stiname);
		M.addColVector(prev_i);
		prev_i.clear();
		colname.push_back("prev_final"+int2string(i));
	}
	
	// ==== Cumulative incidence at horizon
	
	for (int i=0; i<S.size(); i++){
		vector<double> cum_inc_i = S[i].cumul_incidence_final(stiname);
		M.addColVector(cum_inc_i);
		cum_inc_i.clear();
		colname.push_back("cum_inc_final"+int2string(i));
	}
	
	// ==== Cumulative MTCT at horizon
	
	for (int i=0; i<S.size(); i++)	{
		vector<double> cum_inc_mtct_i = S[i].cumul_incidence_MTCT_final(stiname);
		M.addColVector(cum_inc_mtct_i);
		cum_inc_mtct_i.clear();
		colname.push_back("cum_inc_mtct_final"+int2string(i));
	}
	
	// ==== Population at horizon
	
	for (int i=0; i<S.size(); i++){
		vector<double> pop_final_i = S[i].population_final();
		M.addColVector(pop_final_i);
		pop_final_i.clear();
		colname.push_back("pop_final"+int2string(i));
	}
	
	// Save outcome to a file:
	dcDataFrame D(M);
	D.set_colname(colname);
	D.saveToCSV(filename, true);
	
	// Save scenario names to a file:
	string fname = _DIR_OUT + "scenario_names.out";
	ofstream g(fname.c_str());
	for (int i=0; i<S.size(); i++) g << i << "," << S[i].get_scenarioName() <<endl;
	
}




void	run_comp_interventions(unsigned int nMC,
							   Population P_init,
							   string filename_init_STI_prev,
							   vector<string> filename_intervention_wrapper,
							   double horizon_prtn,
							   double timestep_prtn,
							   double horizon,
							   double timestep,
							   bool TraceNetwork,
							   int displayProgress,
							   int jobnum)
{
	/// Run several scenario and compare their outcomes
	
	// deals with eol character issues
	filename_intervention_wrapper = trim(filename_intervention_wrapper);
	
	int n_scenario = filename_intervention_wrapper.size();
	
	vector<MCsimulation> scenario;
	
	for (int i=0; i<n_scenario; i++)
	{
		// retrieve relevant intervention wrapper
		vector<string> interv_wrap;
		
		vectorFromCSVfile_string(interv_wrap,
								 filename_intervention_wrapper[i].c_str(), 1);
		
		// DEBUG
		cout<<"Running scenario #"<<i<<endl;
		// -----
		
		// run the MC simulations
		MCsimulation tmp(nMC,
						 P_init,
						 filename_init_STI_prev,
						 interv_wrap,
						 horizon_prtn,
						 timestep_prtn,
						 horizon,
						 timestep,
						 TraceNetwork,
						 displayProgress,
						 jobnum);
		
		tmp.set_scenarioName(filename_intervention_wrapper[i].c_str());
		
		scenario.push_back(tmp);
		
		// DEBUG
		cout<<"prevalence Tp scenario#"<<i<<"= " <<tmp.mean_prevalence_final(Tp)<<endl;
		// -----
	}
	
	
	// ============
	// OUTPUT FILES
	// ============
	
	// file names for output
	int n_sti = scenario[0].get_simulation(0).get_population().get_nSTImodelled();
	for(int s=0; s<n_sti; s++){
		STIname stiname = scenario[0].get_simulation(0).get_population().get_STI()[s].get_name();
		string str_stiname = STInameString(stiname);
		string filename_comp_scen	= _DIR_OUT + "compare_scenario_"+ str_stiname +"_job"+int2string(jobnum)+".out";
		writeToFile_scenario_outcome(filename_comp_scen, stiname, scenario);
	}
	
	
}

