#ifndef INTERFACE_LOCATIONS_H
#define INTERFACE_LOCATIONS_H


#include <RcppArmadillo.h>
#include <locations.hpp>

RcppExport SEXP locations_init(
		SEXP locations_, 	SEXP drift_, 
		SEXP tails_, 	SEXP scales_, 
		SEXP obs_scales_,
		SEXP minima_, SEXP maxima_, SEXP draws_,
		SEXP xp_rng);
RcppExport SEXP locations_state(SEXP xp);

RcppExport SEXP set_map(SEXP xp, SEXP s, SEXP x);
RcppExport SEXP get_map(SEXP xp, SEXP s);

RcppExport SEXP locations_bind_constant(SEXP xp, SEXP which);
RcppExport SEXP locations_bind_uniform(SEXP xp, SEXP which, SEXP xp_rng);
RcppExport SEXP locations_bind_ordered_uniform(SEXP xp, SEXP which, SEXP xp_rng);
RcppExport SEXP locations_bind_normal(SEXP xp, SEXP which, SEXP xp_rng);
RcppExport SEXP locations_bind_t_walk(SEXP xp, SEXP which, SEXP xp_rng);
RcppExport SEXP locations_bind_t_walk_open(SEXP xp, SEXP which, SEXP xp_rng);
RcppExport SEXP locations_bind_t_walk_open_reverse(SEXP xp, SEXP which, SEXP xp_rng);
RcppExport SEXP locations_bind_t_walk_observed_normal(SEXP xp, SEXP which, SEXP xp_rng);
RcppExport SEXP locations_bind_t_walk_observed_interval(SEXP xp, SEXP which, SEXP xp_rng);

RcppExport SEXP locations_drop_distribution(SEXP xp, SEXP which);
RcppExport SEXP locations_draw(SEXP xp);
RcppExport SEXP locations_lpdfs(SEXP xp, SEXP X);


#endif
