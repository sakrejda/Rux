#ifndef INTERFACE_LOCATIONS_H
#define INTERFACE_LOCATIONS_H


#include <RcppArmadillo.h>
#include <time_series.hpp>

RcppExport SEXP time_series_init(
		SEXP times_, SEXP y_at_times_, 
		SEXP minima_at_times_, SEXP maxima_at_times_,
		SEXP x_at_times_, SEXP drift_,
		SEXP tails_, SEXP scales_, 
		SEXP obs_scales_,
		SEXP xp_rng
);


RcppExport SEXP bind_constant_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP bind_uniform_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP bind_ordered_uniform_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP bind_normal_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP bind_t_walk_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP bind_t_walk_open_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP bind_t_walk_open_reverse_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP bind_t_walk_observed_normal_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP bind_t_walk_observed_interval_distribution(SEXP tsp_xp, SEXP which);

RcppExport SEXP drop_distribution(SEXP tsp_xp, SEXP which);
RcppExport SEXP posterior_draw(SEXP tsp_xp);
RcppExport SEXP posterior_lpdfs(SEXP tsp_xp, SEXP X);


RcppExport SEXP set_map(SEXP xp, SEXP s, SEXP x);
RcppExport SEXP get_map(SEXP xp, SEXP s);

#endif
