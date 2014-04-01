#ifndef INTERFACE_TIME_SERIES_DATA_H
#define INTERFACE_TIME_SERIES_DATA_H

#include <Rcpp.h>
#include <time_series.hpp>

RcppExport SEXP time_series_data_get_times(SEXP data_ptr);
RcppExport SEXP time_series_data_get_y_at_times(SEXP data_ptr);
RcppExport SEXP time_series_data_get_minima_at_times(SEXP data_ptr);
RcppExport SEXP time_series_data_get_maxima_at_times(SEXP data_ptr);

RcppExport SEXP time_series_parameters_get_x_at_times(SEXP parameters_ptr);
RcppExport SEXP time_series_parameters_get_drift(SEXP parameters_ptr);
RcppExport SEXP time_series_parameters_get_tails(SEXP parameters_ptr);
RcppExport SEXP time_series_parameters_get_scales(SEXP parameters_ptr);
RcppExport SEXP time_series_parameters_get_obs_scales(SEXP parameters_ptr);

RcppExport SEXP time_series_parameters_set_x_at_times(SEXP parameters_ptr, x=x);
RcppExport SEXP time_series_parameters_set_drift(SEXP parameters_ptr, x=x);
RcppExport SEXP time_series_parameters_set_tails(SEXP parameters_ptr, x=x);
RcppExport SEXP time_series_parameters_set_scales(SEXP parameters_ptr, x=x);
RcppExport SEXP time_series_parameters_set_obs_scales(SEXP parameters_ptr, x=x);


#endif
