#ifndef INTERFACE_RNG_H
#define INTERFACE_RNG_H

#include <Rcpp.h>

RcppExport SEXP get_RNG(SEXP seed_);

RcppExport SEXP broken();

#endif
