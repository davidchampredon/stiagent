//
//  MCsimulation.cpp
//  LocalSTI
//
//  Created by David CHAMPREDON on 2014-11-17.
//  Copyright (c) 2014 David CHAMPREDON. All rights reserved.
//

#include "MCsimulation.h"
#include "globalVar.h"




MCsimulation::MCsimulation(vector<Simulation> S)
{
	_nMC = S.size();
	stopif(_nMC==0,"Simulation vector is empty!");
	_simulation = S;
	
	_horizon	= S[0].get_horizon();
	_timeStep	= S[0].get_timeStep();
	_schedule	= S[0].get_schedule();
}



MCsimulation::MCsimulation(unsigned int nMC,
						   Population P_init,
						   string filename_init_STI_prev,
						   vector<string> filename_interventions,
						   double horizon_prtn,
						   double timestep_prtn,
						   double horizon,
						   double timestep,
						   bool TraceNetwork,
						   int displayProgress,
						   int jobnum)
{
	vector<Simulation> S = runSimulationMC(nMC,
										   P_init,
										   filename_init_STI_prev,
										   filename_interventions,
										   horizon_prtn,
										   timestep_prtn,
										   horizon,
										   timestep,
										   TraceNetwork,
										   displayProgress,
										   jobnum);
	
	_nMC = S.size();
	stopif(_nMC==0,"Simulation vector is empty!");
	_simulation = S;
	
	_horizon	= S[0].get_horizon();
	_timeStep	= S[0].get_timeStep();
	_schedule	= S[0].get_schedule();
}






// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================


void MCsimulation::displayInfo()
{
	coutline(80);
	cout << endl << " ===== MC SIMULATION ======" <<endl<<endl;
	cout << " MC iterations: "<<_nMC <<endl;
	cout << " Horizon: "<<_horizon <<endl;
	cout << " Time Step: "<<_timeStep <<endl;
	coutline(80);
}



vector<double> MCsimulation::mean_prevalence_ts(STIname s)
{
	/// Returns the time series of mean prevalence for that STI
	
	vector<double> prev;
	
	int n = _schedule.size();
	int pos_sti = positionSTIinVector(s, _simulation[0].get_population().get_STI());
	
	for (int i=0; i<n; i++)
	{
		double mean = 0.0;
		
		for(int j=0;j<_nMC; j++){
			mean += _simulation[j].get_STI_prevalence()(i,pos_sti);
		}
		mean = mean/_nMC;
		prev.push_back(mean);
	}
	return prev;
}


double MCsimulation::mean_prevalence_final(STIname s)
{
	/// Returns the mean prevalence at horizon for that STI
	
	vector<double> prev = mean_prevalence_ts(s);
	return prev[prev.size()-1];
}






vector<double> MCsimulation::mean_incidence(STIname s)
{
	/// Returns the time series of mean incidence for that STI
	
	vector<double> incid;
	
	int n = _schedule.size();
	int pos_sti = positionSTIinVector(s, _simulation[0].get_population().get_STI());
	
	for (int i=0; i<n; i++)
	{
		double mean = 0.0;
		
		for(int j=0;j<_nMC; j++){
			mean += _simulation[j].get_STI_incidence()(i,pos_sti);
		}
		mean = mean/_nMC;
		incid.push_back(mean);
	}
	return incid;
}





double MCsimulation::mean_cumul_incidence_MTCT(STIname stiname)
{
	/// Return the mean incidence at _time horizon_ of
	/// mother-to-child transmission for a given STI
	
	double mean = 0.0;
	
	for (int k=0; k<_nMC; k++)
	{
		mean += _simulation[k].get_nursery().census_infected(stiname);
	}
	
	return mean/_nMC;
}




vector<double> MCsimulation::prevalence_final(STIname s)
{
	/// Returns all MC trials of prevalence at horizon for that STI
	
	vector<double> res;
	
	int n = _schedule.size();
	int pos_sti = positionSTIinVector(s, _simulation[0].get_population().get_STI());
	
	for(int j=0;j<_nMC; j++)
	{
		res.push_back(_simulation[j].get_STI_prevalence()(n-1,pos_sti));
	}
	
	return res;
}

vector<double> MCsimulation::cumul_incidence_final(STIname s)
{
	/// Returns all MC trials of cumulative incidence at horizon for that STI
	
	vector<double> res;
	int n = _schedule.size();
	
	for(int j=0;j<_nMC; j++)
	{
		res.push_back(_simulation[j].STI_cumul_incidence(s,n-1));
	}
	return res;
}


vector<double> MCsimulation::cumul_incidence_MTCT_final(STIname s)
{
	/// Return the incidence at _time horizon_ of
	/// mother-to-child transmission for a given STI
	
	vector<double> res;
	
	for(int k=0;k<_nMC; k++)
	{
		// The Nursery grows with simulation time,
		// hence counting infected infants is equivalent to
		// count cumulative incidence
		res.push_back( _simulation[k].get_nursery().census_infected(s));
	}
	return res;
}


vector<double> MCsimulation::population_final(){
	
	/// Returns final population of all MC trials
	vector<double> res;
	for(int k=0; k<_nMC; k++)
		res.push_back(_simulation[k].get_population().get_size());
	return res;
}


vector<double> MCsimulation::distance_from_targets()
{
	/// Calculates _mean_ and _variance_ distance from calibration targets
	
	vector<double> x;
	
	for(int i=0;i<_simulation.size();i++)
		x.push_back(_simulation[i].calibration_distance_targets());
	
	vector<double> res;
	
	res.push_back(meanElements(x));
	res.push_back(varianceElements(x));
	res.push_back(minElementVector(x));
	res.push_back(maxElementVector(x));
	
	return res;
}


// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================


Simulation	runSimulation_one(Population P_init,
							  string filename_init_STI_prev,
							  vector<string> filename_interventions,
							  double horizon_prtn,
							  double timestep_prtn,
							  double horizon,
							  double timestep,
							  bool TraceNetwork,
							  int displayProgress,
							  unsigned int iter_mc // <- this is a _unique_ number (across all jobs)
)
{
	/// RUNS A SINGLE ITERATION FOR A MC SIMULATION
	/// The random generator seed is reset in this function
	/// with a unique seed value (iter_mc=[jobnum-1]+iMC)
	
	
	// Create Simulation object
	Simulation S(horizon, timestep, P_init, 0);
	S.set_MC_trial_iter(iter_mc);
	
	bool saveflag = (bool)(getParameterFromFile("save_trace_files",
												_DIR_IN + "in_simulation.csv"));
	S.set_save_trace_files(saveflag);
	
	// Calibration
	// (file names are hard coded for now...)
	string file_calib_times		= "calibration_times.csv";
	string file_calib_targets	= "calibration_targets.csv";
	string file_calib_weights	= "calibration_weights.csv";
	
	if(iter_mc==1){
		coutline(80);
		cout<<"FILES FOR CALIBRATION:"<<endl;
		cout << "Calibration times: " << file_calib_times << endl;
		cout << "Calibration targets: " << file_calib_targets << endl;
		cout << "Calibration weights: " << file_calib_weights << endl;
	}
	
	S.set_calibration_schedule(_DIR_CALIB+file_calib_times,
							   _DIR_CALIB+file_calib_targets,
							   _DIR_CALIB+file_calib_weights);
	
	// ======================
	// ==== Intervention ====
	// ======================
	
	filename_interventions = trim(filename_interventions);
	vector<Intervention> I;
	for (int i=0; i<filename_interventions.size(); i++){
		Intervention tmp(_DIR_IN+filename_interventions[i]);
		I.push_back(tmp);
	}
	S.set_intervention(I);
	
	
	// Switches for trace files
	bool logIndivInfo = false;
	int displayProgress_prtn = 0;
	
	
	// ========================================================
	// === Pre-run to form partnerships first (no sex acts) ===
	// ========================================================
	
	// Temporary set horizon and timestep
	// for very long run to form couples only
	
	S.set_horizon(horizon_prtn); // long enough such that initial indiv are all > agemax (e.g. 80)
	S.set_timeStep(timestep_prtn);
	
	bool doSex = false;
	bool traceNetwork_prtn = false;
	
	// DEBUG ---
	cout << endl<< "runSimulation_one ["<< iter_mc <<"]";
	cout << endl<< "Forming partnerships only (no sex) during ";
	cout << horizon_prtn <<" years..."<<endl;
	// ---------
	
	
	// IMPORTANT: iter_mc = fct(jobnum, MC trial)
	// Makes sure the seed is different
	// for each MC trial.
	// However, the seed will be the same for a given
	// Job and MC trial when 2 simulations are run
	// (this is what we want, for example comparing 2
	// intervention scenario with the _same_ seed)
	force_seed_reset(iter_mc);
	S.runAllEvents_horizon(doSex,logIndivInfo,
						   traceNetwork_prtn,displayProgress_prtn,
						   iter_mc);
	
	cout << " ... Partnerships formed [iter MC #"<<iter_mc<<"]"<<endl;
	
	
	// Reset to original parameters
	S.set_horizon(horizon);
	S.set_timeStep(timestep);
	
	
	// =================================
	// Seed STIs' initial prevalence
	// =================================
	
	S.STI_set_initial_prevalence(filename_init_STI_prev);
	cout << endl<<"Simulation has STI prevalence initialized from this file: ";
	cout <<filename_init_STI_prev<<endl;
	
	
	// =================================
	// === STIs epidemics start ===
	// =================================
	
	doSex = true;
	S.runAllEvents_horizon(doSex,
						   logIndivInfo,
						   TraceNetwork,
						   displayProgress,
						   iter_mc);
	
	S.save_incidence(iter_mc);
	S.save_prevalence(iter_mc);
	
	cout<<"DEBUG: runSimulation_one ["<< iter_mc<<"] COMPLETED!"<<endl;
	
	return S;
}



Simulation	runSimulation_one_obj(Population P_init,
								  string filename_init_STI_prev,
								  vector<string> filename_interventions,
								  double horizon_prtn,
								  double timestep_prtn,
								  double horizon,
								  double timestep,
								  bool TraceNetwork,
								  int displayProgress,
								  unsigned int iter_mc, // <- this is a _unique_ number (across all jobs)
								  string folder_inputs,
								  string folder_calib
								  )
{
	/// RUNS A SINGLE ITERATION FOR A MC SIMULATION
	/// The random generator seed is reset in this function
	/// with a unique seed value (iter_mc=[jobnum-1]+iMC)
	
	
	// Create Simulation object
	Simulation S(horizon, timestep, P_init, 0);
	S.set_MC_trial_iter(iter_mc);
	S.set_save_trace_files(false);
	
	// Calibration
	// (file names are hard coded for now...)
	string file_calib_times		= "calibration_times.csv";
	string file_calib_targets	= "calibration_targets.csv";
	string file_calib_weights	= "calibration_weights.csv";
	
	S.set_calibration_schedule(folder_calib+file_calib_times,
							   folder_calib+file_calib_targets,
							   folder_calib+file_calib_weights);
	
	// ======================
	// ==== Intervention ====
	// ======================
	
	filename_interventions = trim(filename_interventions);
	vector<Intervention> I;
	for (int i=0; i<filename_interventions.size(); i++){
		Intervention tmp(folder_inputs+filename_interventions[i]);
		I.push_back(tmp);
	}
	S.set_intervention(I);
	
	
	// Switches for trace files
	bool logIndivInfo = false;
	int displayProgress_prtn = 0;
	
	
	// ========================================================
	// === Pre-run to form partnerships first (no sex acts) ===
	// ========================================================
	
	// Temporary set horizon and timestep
	// for very long run to form couples only
	
	S.set_horizon(horizon_prtn); // long enough such that initial indiv are all > agemax (e.g. 80)
	S.set_timeStep(timestep_prtn);
	
	bool doSex = false;
	bool traceNetwork_prtn = false;
	
	// DEBUG ---
	cout << endl<< "runSimulation_one ["<< iter_mc <<"]";
	cout << endl<< "Forming partnerships only (no sex) during ";
	cout << horizon_prtn <<" years..."<<endl;
	// ---------
	
	
	// IMPORTANT: iter_mc = fct(jobnum, MC trial)
	// Makes sure the seed is different
	// for each MC trial.
	// However, the seed will be the same for a given
	// Job and MC trial when 2 simulations are run
	// (this is what we want, for example comparing 2
	// intervention scenario with the _same_ seed)
	force_seed_reset(iter_mc);
	S.runAllEvents_horizon_obj(doSex,
							   logIndivInfo,
							   traceNetwork_prtn,
							   displayProgress_prtn,
							   iter_mc);
	
	cout << " ... Partnerships formed [iter MC #"<<iter_mc<<"]"<<endl;
	
	
	// Reset to original parameters
	S.set_horizon(horizon);
	S.set_timeStep(timestep);
	
	
	// =================================
	// Seed STIs' initial prevalence
	// =================================
	
	S.STI_set_initial_prevalence(filename_init_STI_prev);
	cout << endl<<"Simulation has STI prevalence initialized from this file: ";
	cout <<filename_init_STI_prev<<endl;
	
	
	// =================================
	// === STIs epidemics start ===
	// =================================
	
	doSex = true;
	S.runAllEvents_horizon_obj(doSex,
							   logIndivInfo,
							   TraceNetwork,
							   displayProgress,
							   iter_mc);
	
	//S.save_incidence(iter_mc);
	//S.save_prevalence(iter_mc);
	cout<<"DEBUG: runSimulation_one ["<< iter_mc<<"] COMPLETED!"<<endl;
	
	return S;
}




vector<Simulation>	runSimulationMC(unsigned int nMC,
									Population P_init,
									string filename_init_STI_prev,
									vector<string> filename_interventions,
									double horizon_prtn,
									double timestep_prtn,
									double horizon,
									double timestep,
									bool TraceNetwork,
									int displayProgress,
									int jobnum)
{
	/// Run multiple iterations of a simulation (Monte Carlo)
	
	
	// Display interventions files used:
	if(jobnum==1){
		cout<<endl<<"Intervention file(s) used: "<<endl;
		for (int i=0; i<filename_interventions.size(); i++)
			cout<<"file "<<i<<":"<<filename_interventions[i]<<endl;
	}
	
	vector<Simulation> res;
	
	for(unsigned int i=1; i<= nMC; i++){
		res.push_back(runSimulation_one(P_init,
										filename_init_STI_prev,
										filename_interventions,
										horizon_prtn,
										timestep_prtn,
										horizon,
										timestep,
										TraceNetwork,
										displayProgress,
										(jobnum-1)*nMC+i  // <- this is a _unique_ number (across all jobs)
										)
					  );
	}
	return(res);
}




vector<Simulation>	runSimulationMC_obj(unsigned int nMC,
										Population P_init,
										string filename_init_STI_prev,
										vector<string> filename_interventions,
										double horizon_prtn,
										double timestep_prtn,
										double horizon,
										double timestep,
										bool TraceNetwork,
										int displayProgress,
										string folder_inputs,
										string folder_calib,
										int jobnum)
{
	/// Run multiple iterations of a simulation (Monte Carlo)
	
	
	// Display interventions files used:
	if(jobnum==1){
		cout<<endl<<"Intervention file(s) used: "<<endl;
		for (int i=0; i<filename_interventions.size(); i++)
			cout<<"file "<<i<<":"<<filename_interventions[i]<<endl;
	}
	
	vector<Simulation> res;
	
	for(unsigned int i=1; i<= nMC; i++){
		res.push_back(runSimulation_one_obj(P_init,
											filename_init_STI_prev,
											filename_interventions,
											horizon_prtn,
											timestep_prtn,
											horizon,
											timestep,
											TraceNetwork,
											displayProgress,
											(jobnum-1)*nMC+i ,
											folder_inputs,
											folder_calib// <- this is a _unique_ number (across all jobs)
											)
					  );
	}
	return(res);
}






double	average_distance_target(vector<Simulation> Simul)
{
	/// Calculates _average_ distance from calibration targets
	/// Typically used for MC
	
	double s = 0.0;
	
	for (int i=0; i<Simul.size(); i++)
	{
		//s += Simul[i].calc_distanceFromAllTargets(); // <-- OLDER STUFF
		s += Simul[i].calibration_distance_targets();
	}
	
	return s/Simul.size();
}


