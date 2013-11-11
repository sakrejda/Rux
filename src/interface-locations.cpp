#include "interface-locations.hpp"
#include "interface-trng.hpp"
#include <locations.hpp>
#include <random.hpp>
#include <trng/yarn2.hpp>
#include <RcppArmadillo.h>


RcppExport SEXP locations_init(
		SEXP locations_, SEXP drift_,
		SEXP tails_, SEXP scales_, 
		SEXP obs_scales_,
		SEXP minima_, SEXP maxima_, SEXP draws_, 
		SEXP xp_rng
) {
	BEGIN_RCPP
	Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
	Rcpp::NumericVector * R_vec = new Rcpp::NumericVector(locations_);
	Rcpp::NumericVector * R_drift = new Rcpp::NumericVector(drift_);
	Rcpp::NumericVector * R_tails = new Rcpp::NumericVector(tails_);
	Rcpp::NumericVector * R_scales = new Rcpp::NumericVector(scales_);
	Rcpp::NumericVector * R_obs_scales = new Rcpp::NumericVector(obs_scales_);
	Rcpp::NumericVector * R_minima = new Rcpp::NumericVector(minima_);
	Rcpp::NumericVector * R_maxima = new Rcpp::NumericVector(maxima_);
	Rcpp::NumericVector * R_draws = new Rcpp::NumericVector(draws_);
	arma::vec * vec 		= new arma::vec((*R_vec).begin(), (*R_vec).size(), false); 
	arma::vec * drift 	= new arma::vec((*R_drift).begin(), (*R_drift).size(), false); 
	arma::vec * tails 	= new arma::vec((*R_tails).begin(), (*R_tails).size(), false); 
	arma::vec * scales 	= new arma::vec((*R_scales).begin(), (*R_scales).size(), false); 
	arma::vec * obs_scales 	= new arma::vec((*R_obs_scales).begin(), (*R_obs_scales).size(), false); 
	arma::vec * minima 	= new arma::vec((*R_minima).begin(), (*R_minima).size(), false); 
	arma::vec * maxima 	= new arma::vec((*R_maxima).begin(), (*R_maxima).size(), false); 
	arma::vec * draws 	= new arma::vec((*R_draws).begin(), (*R_draws).size(), false); 
	Locations * locations_ptr = 
		new Locations(*vec, *drift, *tails, *scales, *obs_scales, *minima, *maxima, *draws, *R_ptr_rng);
	Rcpp::XPtr<Locations> R_ptr(locations_ptr, true);
	return R_ptr; 
	// When this returns, all these temporary pointers to vectors are orphaned.  Fix this, maybe by returning a list and then
	// delete them in some finalizer?

	END_RCPP
}

RcppExport SEXP locations_state(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	arma::vec locs;
	locs = locations->state();
	return Rcpp::wrap(locs);
	END_RCPP
}

RcppExport SEXP locations_bind_constant(SEXP xp, SEXP which) {
	BEGIN_RCPP
		Rcpp::XPtr<Locations> locations(xp);
		unsigned int i = Rcpp::as<unsigned int>(which);
		locations->bind_constant_distribution(i);
		return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_bind_uniform(SEXP xp, SEXP which, SEXP xp_rng) {
	BEGIN_RCPP
		Rcpp::XPtr<Locations> locations(xp);
		Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
		unsigned int i = Rcpp::as<unsigned int>(which);
		locations->bind_uniform_distribution(i, *R_ptr_rng);
		return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_bind_ordered_uniform(SEXP xp, SEXP which, SEXP xp_rng) {
	BEGIN_RCPP
		Rcpp::XPtr<Locations> locations(xp);
		Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
		unsigned int i = Rcpp::as<unsigned int>(which);
		locations->bind_ordered_uniform_distribution(i, *R_ptr_rng);
		return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_bind_normal(SEXP xp, SEXP which, SEXP xp_rng) {
	BEGIN_RCPP
		Rcpp::XPtr<Locations> locations(xp);
		Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
		unsigned int i = Rcpp::as<unsigned int>(which);
		locations->bind_normal_distribution(i, *R_ptr_rng);
		return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_bind_t_walk(SEXP xp, SEXP which, SEXP xp_rng) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
	unsigned int i = Rcpp::as<unsigned int>(which);
	locations->bind_t_walk_distribution(i, *R_ptr_rng);
	return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_bind_t_walk_open(SEXP xp, SEXP which, SEXP xp_rng) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
	unsigned int i = Rcpp::as<unsigned int>(which);
	locations->bind_t_walk_distribution_open(i, *R_ptr_rng);
	return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_bind_t_walk_observed_normal(SEXP xp, SEXP which, SEXP xp_rng) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
	unsigned int i = Rcpp::as<unsigned int>(which);
	locations->bind_t_walk_observed_normal_distribution(i, *R_ptr_rng);
	return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_bind_t_walk_observed_interval(SEXP xp, SEXP which, SEXP xp_rng) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
	unsigned int i = Rcpp::as<unsigned int>(which);
	locations->bind_t_walk_observed_interval_distribution(i, *R_ptr_rng);
	return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_drop_distribution(SEXP xp, SEXP which) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	unsigned int i = Rcpp::as<unsigned int>(which);
	locations->drop_distribution(i);
	return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_draw(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	locations->draw();
	return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP locations_lpdfs(SEXP xp, SEXP X) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	arma::vec x = Rcpp::as<arma::vec>(X);
	x = locations->lpdf(x);
	return Rcpp::wrap(x);
	END_RCPP
}
