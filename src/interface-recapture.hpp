#ifndef INTERFACE_RECAPTURE_H
#define INTERFACE_RECAPTURE_H

#include <RcppArmadillo.h>
#include <recapture.hpp>


// State 
RcppExport SEXP load_recaptures(SEXP x);  // Constructor...
RcppExport SEXP get_N(SEXP xp);
RcppExport SEXP get_K(SEXP xp);
RcppExport SEXP get_surveys(SEXP xp);
RcppExport SEXP get_recaptures(SEXP xp);
RcppExport SEXP get_births(SEXP xp);
RcppExport SEXP get_deaths(SEXP xp);
RcppExport SEXP get_first_obs(SEXP xp);
RcppExport SEXP get_last_obs(SEXP xp);
RcppExport SEXP get_known_deaths(SEXP xp);
RcppExport SEXP set_deaths(SEXP xp, SEXP td_); // Setter

// Parameters
RcppExport SEXP init_recapture_parameters(SEXP xp, SEXP scale);
RcppExport SEXP get_PHI(SEXP xp);
RcppExport SEXP set_PHI(SEXP xp, SEXP phi);
RcppExport SEXP get_P(SEXP xp);
RcppExport SEXP set_PHI(SEXP xp, SEXP p);

// Likelihood
RcppExport SEXP init_recapture_likelihood(SEXP xp_state, SEXP xp_par);
RcppExport SEXP get_likelihood(SEXP xp, SEXP log);

// td_Posterior
RcppExport SEXP init_recapture_td_posterior(SEXP xp_state, SEXP xp_par, SEXP xp_rng);
RcppExport SEXP draw_td(SEXP xp);
RcppExport SEXP get_td_PMF(SEXP xp);

#endif
