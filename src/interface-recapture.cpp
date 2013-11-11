#include "interface-recapture.hpp"
#include "interface-trng.hpp"
#include <recapture.hpp>
#include <trng/yarn2.hpp>



// Code for Recapture_State
RcppExport SEXP load_recaptures(SEXP x) {
	BEGIN_RCPP
	Rcpp::List rparam(x);
	std::vector<int> tos = Rcpp::as<std::vector<int> >(rparam["times_of_surveys"]);
	std::vector<std::vector<int> > tor;
	std::vector<int> tod = Rcpp::as<std::vector<int> >(rparam["times_of_deaths"]);
	std::vector<bool> kds = Rcpp::as<std::vector<bool> >(rparam["known_deaths"]);

// Could follow Dirk's instructions for making an
// "Rcpp::as<std::vector<std::vector<T_A> >(T_B);" for nested lists...
	Rcpp::List r_tor = rparam["times_of_recaptures"];
	for (Rcpp::List::iterator i=r_tor.begin(); i != r_tor.end(); ++i) {
		tor.push_back(Rcpp::as<std::vector<int> >(*i));
  }
	Recapture_State* state_ptr = new Recapture_State(tos, tor, tod, kds);
	Rcpp::XPtr<Recapture_State> R_ptr( state_ptr, true );
	return R_ptr;
	END_RCPP
}

RcppExport SEXP get_N(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	int N;
	N = R_ptr->get_N();
	return Rcpp::wrap(N);
	END_RCPP
}

RcppExport SEXP get_K(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	int K;
	K = R_ptr->get_K();
	return Rcpp::wrap(K);
	END_RCPP
}

RcppExport SEXP get_surveys(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Col<int> surveys(R_ptr->get_surveys());
	return Rcpp::wrap(surveys);
	END_RCPP
}

RcppExport SEXP get_recaptures(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Mat<int> recaptures(R_ptr->get_N(), R_ptr->get_K());
	recaptures = R_ptr->get_recaptures();
	return Rcpp::wrap(recaptures);
	END_RCPP
}

RcppExport SEXP get_births(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Col<int> births;
	births = R_ptr->get_births();
	return Rcpp::wrap(births);
	END_RCPP
}

RcppExport SEXP get_deaths(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Col<int> deaths;
	deaths = R_ptr->get_deaths();
	return Rcpp::wrap(deaths);
	END_RCPP
}

RcppExport SEXP get_first_obs(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Col<int> first_obs;
	first_obs = R_ptr->get_first_obs();
	return Rcpp::wrap(first_obs);
	END_RCPP
}

RcppExport SEXP get_last_obs(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Col<int> last_obs;
	last_obs = R_ptr->get_last_obs();
	return Rcpp::wrap(last_obs);
	END_RCPP
}

RcppExport SEXP get_known_deaths(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Col<int> known_deaths;
	known_deaths = R_ptr->get_known_deaths();
	return Rcpp::wrap(known_deaths);
	END_RCPP
}

RcppExport SEXP set_deaths(SEXP xp, SEXP td_) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Col<int> td = Rcpp::as<arma::Col<int> >(td_);
	R_ptr->set_td(td);
	arma::Col<int> deaths;
	deaths = R_ptr->get_deaths();
	return Rcpp::wrap(deaths);
	END_RCPP
}


// Code for Recapture_Parameters
RcppExport SEXP init_recapture_parameters(SEXP xp, SEXP scale) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr(xp);
	arma::Col<unsigned int> s = Rcpp::as<arma::Col<unsigned int> >(scale);
	Recapture_Parameters * parameter_ptr = new Recapture_Parameters(*R_ptr, s(0));
	Rcpp::XPtr<Recapture_Parameters> ptr(parameter_ptr, true);
	return ptr;
	END_RCPP
}

RcppExport SEXP get_PHI(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Parameters> R_ptr(xp);
	arma::Mat<double> PHI = R_ptr->get_PHI();
	return Rcpp::wrap(PHI);	
	END_RCPP
}

RcppExport SEXP set_PHI(SEXP xp, SEXP phi) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Parameters> R_ptr(xp);
	R_ptr->set_PHI(Rcpp::as<arma::Mat<double> >(phi));
	return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP get_P(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Parameters> R_ptr(xp);
	arma::Mat<double> P = R_ptr->get_P();
	return Rcpp::wrap(P);	
	END_RCPP
}

RcppExport SEXP set_P(SEXP xp, SEXP p) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Parameters> R_ptr(xp);
	R_ptr->set_P(Rcpp::as<arma::Mat<double> >(p));
	return Rcpp::wrap(0);
	END_RCPP
}

// Code for Recapture_Likelihood;

RcppExport SEXP init_recapture_likelihood(SEXP xp_state, SEXP xp_par) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr_state(xp_state);
	Rcpp::XPtr<Recapture_Parameters> R_ptr_par(xp_par);
	Recapture_Likelihood * likelihood_ptr = 
		new Recapture_Likelihood(*R_ptr_state, *R_ptr_par);
	Rcpp::XPtr<Recapture_Likelihood> ptr(likelihood_ptr, true);
	return ptr;
	END_RCPP
}

RcppExport SEXP get_likelihood(SEXP xp, SEXP log) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_Likelihood> R_ptr(xp);
	bool log_ = Rcpp::as<bool>(log);
	double likelihood = R_ptr->get_likelihood(log_);
	return Rcpp::wrap(likelihood);	
	END_RCPP
}

// Code for Recapture_td_Posterior;

RcppExport SEXP init_recapture_td_posterior(SEXP xp_state, SEXP xp_par, SEXP xp_rng) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_State> R_ptr_state(xp_state);
	Rcpp::XPtr<Recapture_Parameters> R_ptr_par(xp_par);
	Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);
	Recapture_td_Posterior * td_posterior_ptr = 
		new Recapture_td_Posterior(*R_ptr_state, *R_ptr_par, *R_ptr_rng);
	Rcpp::XPtr<Recapture_td_Posterior> ptr(td_posterior_ptr, true);
	return ptr;
	END_RCPP
}

RcppExport SEXP draw_td(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_td_Posterior> R_ptr(xp);
	arma::Col<int> td = R_ptr->draw();
	return Rcpp::wrap(td);
	END_RCPP
}

RcppExport SEXP get_td_PMF(SEXP xp) {
	BEGIN_RCPP
	Rcpp::XPtr<Recapture_td_Posterior> R_ptr(xp);
	arma::field<arma::Row<double> > td_PMF = R_ptr->calc_log_mass_function();
	int N = td_PMF.n_elem;
	int K = td_PMF(0).n_elem;
	arma::Mat<double> td_PMF_mat(N,K);
	for ( unsigned int i=0; i < N; ++i ) {
		for ( unsigned int j=0; j < K; ++j ) {
				td_PMF_mat(i,j) = td_PMF(i)(j) ;
		}
	}
	return Rcpp::wrap(td_PMF_mat);
	END_RCPP
}
