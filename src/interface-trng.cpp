// RNG
//
#include "interface-trng.hpp"
#include <trng/yarn2.hpp>

RcppExport SEXP get_RNG(SEXP seed_) {
	BEGIN_RCPP
	unsigned long seed = Rcpp::as<unsigned long>(seed_);
	trng::yarn2 * RNG_ptr = new trng::yarn2(seed);
	Rcpp::XPtr<trng::yarn2> R_ptr(RNG_ptr, true);
	return R_ptr;
	END_RCPP
}

