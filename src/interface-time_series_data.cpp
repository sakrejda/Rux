#include "interface-time_series_data.hpp"

RcppExport SEXP time_series_data_get_times(SEXP data_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Data> time_series_data(data_ptr);
		arma::Col<double> times;
		times = time_series_data->get_times();
		return Rcpp::wrap(times);
	END_RCPP
}

RcppExport SEXP time_series_data_get_y_at_times(SEXP data_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Data> time_series_data(data_ptr);
		arma::Col<double> y_at_times;
		y_at_times = time_series_data->get_y_at_times();
		return Rcpp::wrap(y_at_times);
	END_RCPP
}

RcppExport SEXP time_series_data_get_minima_at_times(SEXP data_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Data> time_series_data(data_ptr);
		arma::Col<double> minima_at_times;
		minima_at_times = time_series_data->get_minima_at_times();
		return Rcpp::wrap(minima_at_times);
	END_RCPP
}

RcppExport SEXP time_series_data_get_maxima_at_times(SEXP data_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Data> time_series_data(data_ptr);
		arma::Col<double> maxima_at_times;
		maxima_at_times = time_series_data->get_maxima_at_times();
		return Rcpp::wrap(maxima_at_times);
	END_RCPP
}

RcppExport SEXP time_series_parameters_get_x_at_times(SEXP parameters_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> x_at_times;
		x_at_times = time_series_parameters->get_x_at_times();
		return Rcpp::wrap(x_at_times);
	END_RCPP
}

RcppExport SEXP time_series_parameters_get_drift(SEXP parameters_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> drift;
		drift = time_series_parameters->get_drift();
		return Rcpp::wrap(drift);
	END_RCPP
}

RcppExport SEXP time_series_parameters_get_tails(SEXP parameters_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> tails;
		tails = time_series_parameters->get_tails();
		return Rcpp::wrap(tails);
	END_RCPP
}


RcppExport SEXP time_series_parameters_get_scales(SEXP parameters_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> scales;
		scales = time_series_parameters->get_scales();
		return Rcpp::wrap(scales);
	END_RCPP
}

RcppExport SEXP time_series_parameters_get_obs_scales(SEXP parameters_ptr) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> obs_scales;
		obs_scales = time_series_parameters->get_obs_scales();
		return Rcpp::wrap(obs_scales);
	END_RCPP
}

RcppExport SEXP time_series_parameters_set_x_at_times(SEXP parameters_ptr, SEXP x) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> x_at_times = Rcpp::as<arma::Col<double> >(x);
		time_series_parameters->set_x_at_times(x_at_times);
		return Rcpp::wrap(0);
	END_RCPP
}


RcppExport SEXP time_series_parameters_set_drift(SEXP parameters_ptr, x=x) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> drift = Rcpp::as<arma::Col<double> >(x);
		time_series_parameters->set_drift(drift);
		return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP time_series_parameters_set_tails(SEXP parameters_ptr, x=x) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> tails = Rcpp::as<arma::Col<double> >(x);
		time_series_parameters->set_tails(tails);
		return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP time_series_parameters_set_scales(SEXP parameters_ptr, x=x) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> scales = Rcpp::as<arma::Col<double> >(x);
		time_series_parameters->set_scales(scales);
		return Rcpp::wrap(0);
	END_RCPP
}

RcppExport SEXP time_series_parameters_set_obs_scales(SEXP parameters_ptr, x=x) {
	BEGIN_RCPP
		Rcpp::XPtr<Time_Series_Parameters> time_series_parameters(parameters_ptr);
		arma::Col<double> obs_scales = Rcpp::as<arma::Col<double> >(x);
		time_series_parameters->set_obs_scales(obs_scales);
		return Rcpp::wrap(0);
	END_RCPP
}

