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
	arma::vec * vec = new arma::vec((*R_vec).begin(), (*R_vec).size(), false); 
	arma::vec * drift = new arma::vec((*R_drift).begin(), (*R_drift).size(), false); 
	arma::vec * tails = new arma::vec((*R_tails).begin(), (*R_tails).size(), false); 
	arma::vec * scales = new arma::vec((*R_scales).begin(), (*R_scales).size(), false); 
	arma::vec * obs_scales = new arma::vec((*R_obs_scales).begin(), (*R_obs_scales).size(), false); 
	arma::vec * minima = new arma::vec((*R_minima).begin(), (*R_minima).size(), false);
	arma::vec * maxima = new arma::vec((*R_maxima).begin(), (*R_maxima).size(), false); 
	arma::vec * draws = new arma::vec((*R_draws).begin(), (*R_draws).size(), false); 
	Locations * locations = new Locations(*vec, *drift, *tails, *scales, *obs_scales, *minima, *maxima, *draws, *R_ptr_rng);

	Rcpp::XPtr<Locations> R_ptr(locations, true);

	Rcpp::XPtr<Rcpp::NumericVector> R_vec_ptr(R_vec, true);
	Rcpp::XPtr<Rcpp::NumericVector> R_drift_ptr(R_drift, true);
	Rcpp::XPtr<Rcpp::NumericVector> R_tails_ptr(R_tails, true);
	Rcpp::XPtr<Rcpp::NumericVector> R_scales_ptr(R_scales, true);
	Rcpp::XPtr<Rcpp::NumericVector> R_obs_scales_ptr(R_obs_scales, true);
	Rcpp::XPtr<Rcpp::NumericVector> R_minima_ptr(R_minima, true);
	Rcpp::XPtr<Rcpp::NumericVector> R_maxima_ptr(R_maxima, true);
	Rcpp::XPtr<Rcpp::NumericVector> R_draws_ptr(R_draws, true);

	Rcpp::XPtr<arma::vec> R_vec_arma_ptr(vec, true);
	Rcpp::XPtr<arma::vec> R_drift_arma_ptr(drift, true);
	Rcpp::XPtr<arma::vec> R_tails_arma_ptr(tails, true);
	Rcpp::XPtr<arma::vec> R_scales_arma_ptr(scales, true);
	Rcpp::XPtr<arma::vec> R_obs_scales_arma_ptr(obs_scales, true);
	Rcpp::XPtr<arma::vec> R_minima_arma_ptr(minima, true);
	Rcpp::XPtr<arma::vec> R_maxima_arma_ptr(maxima, true);
	Rcpp::XPtr<arma::vec> R_draws_arma_ptr(draws, true);

	// The pointers which we return will be delted when the R-level object
	// is erased!  Each XPtr is returned to R, on deletion (of the
	// containing object), each XPtr calls its finalizer which calls
	// delete on its pointer.  The resources of the R ptrs are the 
	// thin-wrapper numeric vectors and the resources of the arma::vec's
	// are also thin wrappers of the contents of hte numeric vector.
	// Once that's all done, the R-level memory can be freed. Yikes!
	// This also gives me write access at the C++ level which does
	// not trigger memory reallocation.
	return Rcpp::List::create(
			Rcpp::Named("sampler_ptr")	=	R_ptr,

			Rcpp::Named("R_observations_ptr") = R_vec_ptr,
			Rcpp::Named("R_drift_ptr") = R_drift_ptr,
			Rcpp::Named("R_tails_ptr") = R_tails_ptr,
			Rcpp::Named("R_scales_ptr") = R_scales_ptr,
			Rcpp::Named("R_obs_scales_ptr") = R_obs_scales_ptr,
			Rcpp::Named("R_minima_ptr") = R_minima_ptr,
			Rcpp::Named("R_maxima_ptr") = R_maxima_ptr,
			Rcpp::Named("R_draws_ptr") = R_draws_ptr,

			Rcpp::Named("arma_observations_ptr") = R_vec_arma_ptr,
			Rcpp::Named("arma_drift_ptr") = R_drift_arma_ptr,
			Rcpp::Named("arma_tails_ptr") = R_tails_arma_ptr,
			Rcpp::Named("arma_scales_ptr") = R_scales_arma_ptr,
			Rcpp::Named("arma_obs_scales_ptr") = R_obs_scales_arma_ptr,
			Rcpp::Named("arma_minima_ptr") = R_minima_arma_ptr,
			Rcpp::Named("arma_maxima_ptr") = R_maxima_arma_ptr,
			Rcpp::Named("arma_draws_ptr") = R_draws_arma_ptr
	);

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

RcppExport SEXP locations_bind_t_walk_open_reverse(SEXP xp, SEXP which, SEXP xp_rng) {
	BEGIN_RCPP
	Rcpp::XPtr<Locations> locations(xp);
	Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
	unsigned int i = Rcpp::as<unsigned int>(which);
	locations->bind_t_walk_distribution_open_reverse(i, *R_ptr_rng);
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
