setRefClass(Class = "recapture_handle",
	fields = list(
		rng_ptr = "externalptr",
		state_ptr = "externalptr",
		par_ptr = "externalptr",
		like_ptr = "externalptr",
		td_posterior_ptr = "externalptr",
		N = function(N=NULL) {
			if (!is.null(N)) warning("'N' is read-only after construction.")
			return(get_N())
		},
		K = function(K=NULL) {
			if (!is.null(K)) warning("'K' is read-only after construction.")
			return(get_K())
		},
		recaptures = function(recaptures=NULL) {
			if (!is.null(recaptures)) warning("'recaptures' is read-only after construction.")
			return(get_recaptures())
		},
		surveys = function(surveys=NULL) {
			if (!is.null(surveys)) warning("'surveys' is read-only after construction.")
			return(get_surveys())
		},
		births = function(births=NULL) {
			if (!is.null(births)) warning("'births' is read-only after construction.")
			return(get_births())
		},
		deaths = function(deaths=NULL) {
			if (!is.null(deaths)) { 
				set_deaths(deaths)
			} else {
				get_deaths()
			}
		},
		PHI = function(PHI=NULL) {
			if (!is.null(PHI)) {
				set_PHI(PHI)
			} else {
				get_PHI()
			}
		},
		P = function(P=NULL) {
			if (!is.null(P)) {
				set_P(P)
			} else {
				get_P()
			}
		},
		log_likelihood = function(log_likelihood=NULL) {
			if (!is.null(log_likelihood)) {
				warning("'log_likelihood' is read-only.")
			} else {
				return(get_likelihood(TRUE))
			}
		},
		likelihood = function(likelihood=NULL) {
			if (!is.null(likelihood)) {
				warning("'likelihood' is read-only.")
			} else {
				return(get_likelihood(FALSE))
			}
		},
		td = function(td=NULL) {
			if (!is.null(td)) {
				warning("'draw_td' is read-only.")
			} else {
				return(draw_td())
			}
		}
	),
	methods = list(
		initialize = function(
			times_of_surveys, times_of_recaptures, 
			times_of_deaths, known_deaths,
			PHI, P,
			survival_scale = 5, seed=sample(x=1:10^3, size=1)
		) {
	  	if (	!is.vector(times_of_surveys) || 
						!is.integer(as.integer(times_of_surveys))) {
    		stop("Times of surveys must be convertible to a vector of integers.")
	  	}
  		if (	!is.list(times_of_recaptures) || 
						!all(sapply(times_of_recaptures, is.vector)) ||
    	   		!all(sapply(times_of_recaptures, function(x) {is.integer(as.integer(x))}))
  		) {
    		stop("Times of recaptures must be a list of vectors of integers.")
  		}
	  	if ( 	!is.vector(times_of_deaths) || 
						!is.integer(as.integer(times_of_deaths))) {
    		stop("Times of deaths must be convertible to a vector of integers.")
  		}
	  	if ( 	!is.vector(known_deaths) || 
						!is.logical(as.logical(known_deaths))) {
    		stop("Times of deaths must be convertible to a vector of integers.")
  		}
	  	x <- list(  ## "-1" shifts to C/C++ indexing.
	    	times_of_surveys = as.integer(times_of_surveys) - 1,
	    	times_of_recaptures = lapply(
	      	X = times_of_recaptures,
	      	FUN = function(x) { return(as.integer(x) - 1) }),
	    	times_of_deaths = as.integer(times_of_deaths) - 1,
	    	known_deaths = as.logical(known_deaths) 
	  	)
	  	state_ptr <<- .Call("load_recaptures", 
				x=x, PACKAGE="Rux")
			## Calling these makes their other versions available using object$XXX syntax.
			get_N(); get_K(); get_recaptures(); get_surveys(); get_births();
			get_deaths(); set_deaths(get_deaths()); 

			par_ptr <<- .Call("init_recapture_parameters", 
				xp=state_ptr, scale=survival_scale, PACKAGE="Rux")
			## Calling these makes their other versions available using object$XXX syntax.
			get_PHI(); get_P(); set_PHI(get_PHI()+PHI); set_P(get_P()+P); 


			like_ptr <<- .Call("init_recapture_likelihood", 
				xp_state=state_ptr, xp_par=par_ptr, PACKAGE="Rux")
			## Calling these makes their other versions available using object$XXX syntax.
			get_likelihood(TRUE);

			rng_ptr <<- .Call("get_RNG", seed_=seed, PACKAGE="Rux")

			td_posterior_ptr <<- .Call("init_recapture_td_posterior", 
				xp_state=state_ptr, xp_par=par_ptr, xp_rng=rng_ptr,
				PACKAGE="Rux")
			draw_td();
			## Calling these makes their other versions available using object$XXX syntax.


  		return(.self)
		},
		get_N = function() {.Call("get_N", xp=state_ptr, PACKAGE="Rux")},
		get_K = function() {.Call("get_K", xp=state_ptr, PACKAGE="Rux")},
		get_surveys = function() {
			.Call("get_surveys", xp=state_ptr, PACKAGE="Rux")+1}, 
		get_recaptures = function() {
			.Call("get_recaptures", xp=state_ptr, PACKAGE="Rux")},
		get_births = function() {
			.Call("get_births", xp=state_ptr, PACKAGE="Rux")+1},
		get_deaths = function() {
			.Call("get_deaths", xp=state_ptr, PACKAGE="Rux")+1},
		get_first_obs = function() {
			.Call("get_first_obs", xp=state_ptr, PACKAGE="Rux")+1},
		get_last_obs = function() {
			.Call("get_last_obs", xp=state_ptr, PACKAGE="Rux")+1},
		get_known_deaths = function() {
			.Call("get_known_deaths", xp=state_ptr, PACKAGE="Rux")},
		set_deaths = function(td) {
			.Call("set_deaths", xp=state_ptr, td_=td-1, PACKAGE="Rux")+1},
		get_PHI = function() {
			.Call("get_PHI", xp=par_ptr)},
		set_PHI = function(PHI) {
			.Call("set_PHI", xp=par_ptr, phi=PHI)},
		get_P = function() {
			.Call("get_P", xp=par_ptr)},
		set_P = function(P) {
			.Call("set_P", xp=par_ptr, p=P)},
		get_likelihood = function(log) {
			.Call("get_likelihood", xp=like_ptr, log=log)
		},
		draw_td = function() {
			.Call("draw_td", xp=td_posterior_ptr)+1
		},
		td_PMF = function(log) {
			o <- .Call("get_td_PMF", xp=td_posterior_ptr)
			if (log) return(o) else return(exp(o))
		},
		draw_td_R = function() {
			pmf <- td_PMF(FALSE)
			o <- apply(pmf, 1, function(x) {sample(x=1:length(x), size=1,
																				replace=FALSE, prob=x)})
			return(o)
		}
	)
)
			
