#include "interface-time_series.hpp"
#include "interface-trng.hpp"
#include <time_series.hpp>
#include <random.hpp>
#include <trng/yarn2.hpp>
#include <RcppArmadillo.h>
#include <map>

RcppExport SEXP time_series_init(
        SEXP times_, SEXP y_at_times_,
        SEXP minima_at_times_, SEXP maxima_at_times_,
        SEXP x_at_times_, SEXP drift_,
        SEXP tails_, SEXP scales_,
        SEXP obs_scales_,
        SEXP xp_rng
) {
    BEGIN_RCPP
    Rcpp::XPtr<trng::yarn2> R_ptr_rng(xp_rng);

    std::map<std::string, std::vector<double> > data;
    data["times"]                                   = Rcpp::as<std::vector<double> >(times_);
    data["y_at_times"]                          = Rcpp::as<std::vector<double> >(y_at_times_);
    data["minima_at_times"]         = Rcpp::as<std::vector<double> >(minima_at_times_);
    data["maxima_at_times"]         = Rcpp::as<std::vector<double> >(maxima_at_times_);

    std::map<std::string, std::vector<double> > theta;
    theta["x_at_times"]             = Rcpp::as<std::vector<double> >(x_at_times_);
    theta["drift"]                      = Rcpp::as<std::vector<double> >(drift_);
    theta["tails"]                      = Rcpp::as<std::vector<double> >(tails_);
    theta["scales"]                     = Rcpp::as<std::vector<double> >(scales_);
    theta["obs_scales"]             =   Rcpp::as<std::vector<double> >(obs_scales_);

    Time_Series_Data * time_series_data = new Time_Series_Data(
        data["times"],
        data["y_at_times"],
        data["minima_at_times"],
        data["maxima_at_times"]
    );

    Time_Series_Parameters * time_series_parameters = new Time_Series_Parameters(
        *time_series_data,
        theta["x_at_times"],
        theta["drift"],
        theta["scales"],
        theta["tails"],
        theta["obs_scales"]
    );

    Time_Series_Posterior * time_series_posterior = new Time_Series_Posterior(
        *time_series_data,
        *time_series_parameters,
        *R_ptr_rng
    );

    Rcpp::XPtr<Time_Series_Data> R_data_ptr(time_series_data, true);
    Rcpp::XPtr<Time_Series_Parameters> R_parameters_ptr(time_series_parameters, true);
    Rcpp::XPtr<Time_Series_Posterior> R_posterior_ptr(time_series_posterior, true);

    // Handing responsability for triggering delete to Rcpp/R.
    return Rcpp::List::create(
            Rcpp::Named("data_ptr") = R_data_ptr,
            Rcpp::Named("parameters_ptr") = R_parameters_ptr,
            Rcpp::Named("posterior_ptr") = R_posterior_ptr
    );

    END_RCPP
}


RcppExport SEXP bind_constant_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
        Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
        int i = Rcpp::as<int>(which);
        time_series_posterior->bind_constant_distribution(i);
        return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP bind_uniform_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
        Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
        int i = Rcpp::as<int>(which);
        time_series_posterior->bind_uniform_distribution(i);
        return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP bind_ordered_uniform_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
        Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
        int i = Rcpp::as<int>(which);
        time_series_posterior->bind_ordered_uniform_distribution(i);
        return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP bind_normal_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
        Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
        int i = Rcpp::as<int>(which);
        time_series_posterior->bind_normal_distribution(i);
        return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP bind_t_walk_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
    Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
    int i = Rcpp::as<int>(which);
    time_series_posterior->bind_t_walk_distribution(i);
    return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP bind_t_walk_open_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
    Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
    int i = Rcpp::as<int>(which);
    time_series_posterior->bind_t_walk_distribution_open(i);
    return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP bind_t_walk_open_reverse_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
    Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
    int i = Rcpp::as<int>(which);
    time_series_posterior->bind_t_walk_distribution_open_reverse(i);
    return Rcpp::wrap(0);
    END_RCPP
}


RcppExport SEXP bind_t_walk_observed_normal_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
    Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
    int i = Rcpp::as<int>(which);
    time_series_posterior->bind_t_walk_observed_normal_distribution(i);
    return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP bind_t_walk_observed_interval_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
    Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
    int i = Rcpp::as<int>(which);
    time_series_posterior->bind_t_walk_observed_interval_distribution(i);
    return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP drop_distribution(SEXP tsp_xp, SEXP which) {
    BEGIN_RCPP
    Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
    int i = Rcpp::as<int>(which);
    time_series_posterior->drop_distribution(i);
    return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP posterior_draw(SEXP tsp_xp) {
    BEGIN_RCPP
    Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
    time_series_posterior->draw();
    return Rcpp::wrap(0);
    END_RCPP
}

RcppExport SEXP posterior_lpdfs(SEXP tsp_xp, SEXP X) {
    BEGIN_RCPP
    Rcpp::XPtr<Time_Series_Posterior> time_series_posterior(tsp_xp);
    arma::vec x = Rcpp::as<arma::vec>(X);
    x = time_series_posterior->lpdf(x);
    return Rcpp::wrap(x);
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

