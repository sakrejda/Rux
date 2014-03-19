#include "interface-locations.hpp"
#include "interface-trng.hpp"
#include <locations.hpp>
#include <random.hpp>
#include <trng/yarn2.hpp>
#include <RcppArmadillo.h>
#include <map>

RcppExport SEXP locations_init(
		SEXP locations_, SEXP drift_,
		SEXP tails_, SEXP scales_, 
		SEXP obs_scales_,
		SEXP minima_, SEXP maxima_, SEXP draws_, 
		SEXP xp_rng
) {
	BEGIN_RCPP
	Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);

	auto data = new std::map<std::string, arma::vec>();
	(*data)["observed"] 		= Rcpp::as<arma::vec>(locations_);
	(*data)["minima"]   		= Rcpp::as<arma::vec>(minima_);
	(*data)["maxima"]   		= Rcpp::as<arma::vec>(maxima_);
	Rcpp::XPtr<std::map<std::string, arma::vec> > R_data(data, true);

	auto theta = new std::map<std::string, arma::vec>();
	(*theta)["obs_scales"]	=	Rcpp::as<arma::vec>(obs_scales_); 
	(*theta)["drift"]				= Rcpp::as<arma::vec>(drift_);
	(*theta)["tails"]				= Rcpp::as<arma::vec>(tails_);
	(*theta)["scales"]				= Rcpp::as<arma::vec>(scales_);
	(*theta)["draws"]				= Rcpp::as<arma::vec>(draws_);
	Rcpp::XPtr<std::map<std::string, arma::vec> > R_theta(theta, true);
	
	Locations * locations = new Locations(
		(*data)["observed"], 
		(*theta)["drift"], 
		(*theta)["tails"], 
		(*theta)["scales"], 
		(*theta)["obs_scales"], 
		(*data)["minima"], 
		(*data)["maxima"], 
		(*theta)["draws"], 
		*R_ptr_rng
	);
	Rcpp::XPtr<Locations> R_ptr(locations, true);
	

//	Rcpp::NumericVector * R_vec = new Rcpp::NumericVector(locations_);
//	Rcpp::NumericVector * R_drift = new Rcpp::NumericVector(drift_);
//	Rcpp::NumericVector * R_tails = new Rcpp::NumericVector(tails_);
//	Rcpp::NumericVector * R_scales = new Rcpp::NumericVector(scales_);
//	Rcpp::NumericVector * R_obs_scales = new Rcpp::NumericVector(obs_scales_);
//	Rcpp::NumericVector * R_minima = new Rcpp::NumericVector(minima_);
//	Rcpp::NumericVector * R_maxima = new Rcpp::NumericVector(maxima_);
//	Rcpp::NumericVector * R_draws = new Rcpp::NumericVector(draws_);
//	arma::vec * vec = new arma::vec((*R_vec).begin(), (*R_vec).size(), false); 
//	arma::vec * drift = new arma::vec((*R_drift).begin(), (*R_drift).size(), false); 
//	arma::vec * tails = new arma::vec((*R_tails).begin(), (*R_tails).size(), false); 
//	arma::vec * scales = new arma::vec((*R_scales).begin(), (*R_scales).size(), false); 
//	arma::vec * obs_scales = new arma::vec((*R_obs_scales).begin(), (*R_obs_scales).size(), false); 
//	arma::vec * minima = new arma::vec((*R_minima).begin(), (*R_minima).size(), false);
//	arma::vec * maxima = new arma::vec((*R_maxima).begin(), (*R_maxima).size(), false); 
//	arma::vec * draws = new arma::vec((*R_draws).begin(), (*R_draws).size(), false); 

	// Handing memory magement for the sampler and map objects to R.
	return Rcpp::List::create(
			Rcpp::Named("sampler_ptr")	=	R_ptr,
			Rcpp::Named("data_ptr")     = R_data,
			Rcpp::Named("theta_ptr")    = R_theta
	);

	END_RCPP
}

RcppExport SEXP set_map(SEXP map_xp, SEXP s, SEXP x) {
	BEGIN_RCPP
	Rcpp::XPtr<std::map<std::string, arma::vec> > map(map_xp);
	std::string ss = Rcpp::as<std::string>(s);
	arma::vec xx = Rcpp::as<arma::vec>(x);
	(*map)[ss] = xx;
	return Rcpp::wrap((*map)[ss]);
	END_RCPP
}

RcppExport SEXP get_map(SEXP map_xp, SEXP s) {
	BEGIN_RCPP
	Rcpp::XPtr<std::map<std::string, arma::vec> > map(map_xp);
	std::string ss = Rcpp::as<std::string>(s);
	arma::vec xx = (*map)[ss];
	return Rcpp::wrap(xx);
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
