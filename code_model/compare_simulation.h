//
//  compare_simulation.h
//  TpHIV
//
//  Created by David CHAMPREDON on 2015-01-08.
//  Copyright (c) 2015 David CHAMPREDON. All rights reserved.
//

#ifndef __TpHIV__compare_simulation__
#define __TpHIV__compare_simulation__

#include "dcTools.h"
//#include "simulation.h"
#include "MCsimulation.h"


double		comp_simul_mean_MTCT(STIname stiname, MCsimulation S1, MCsimulation S2);

double		comp_simul_mean_cumul_incidence(STIname stiname, MCsimulation S1, MCsimulation S2);

double		comp_simul_mean_prevalence(STIname stiname, MCsimulation S1, MCsimulation S2);


void writeToFile_scenario_outcome(string filename,
							STIname stiname,
							vector<MCsimulation> S);


void		run_comp_interventions(unsigned int nMC,
								   Population P_init,
								   string filename_init_STI_prev,
								   vector<string> filename_intervention_wrapper,
								   double horizon_prtn,
								   double timestep_prtn,
								   double horizon,
								   double timestep,
								   bool TraceNetwork,
								   int displayProgress,
								   int jobnum);



#endif /* defined(__TpHIV__compare_simulation__) */
