time_series_handle <- setRefClass(
	Class = "time_series_handle",
	fields = list(
		times_ = "numeric",
    y_ = "numeric",
    mins_ = "numeric",
    maxs_ = "numeric",
    x_ = "numeric",
    drift_ = "numeric",
		tails_ = "numeric",
		scales_ = "numeric",
		obs_scales_ = "numeric",
		distr_ = "character",
		distribution_type = "list",
		data_ptr = "externalptr",
		parameters_ptr = "externalptr",
		posterior_ptr = "externalptr",
		rng_ptr = "externalptr",
    times = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_data_get_times", data_ptr=data_ptr, PACKAGE="Rux")))
			else 
				stop("Member 'times' can only be set at initialization.")
    },
    y = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_data_get_y_at_times", data_ptr=data_ptr, PACKAGE="Rux")))
			else 
				stop("Member 'y_at_times' can only be set at initialization.")
    }
    minima = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_data_get_minima_at_times", data_ptr=data_ptr, PACKAGE="Rux")))
			else 
				stop("Member 'minima_at_times' can only be set at initialization.")
    },
    maxima = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_data_get_maxima_at_times", data_ptr=data_ptr, PACKAGE="Rux")))
			else 
				stop("Member 'maxima_at_times' can only be set at initialization.")
    },
    x = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_parameters_get_x_at_times", parameters_ptr=parameters_ptr, PACKAGE="Rux")))
			else 
				return(c(.Call("time_series_parameters_set_x_at_times", parameters_ptr=parameters_ptr, x=x, PACKAGE="Rux")))
    },
    drift = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_parameters_get_drift", parameters_ptr=parameters_ptr, PACKAGE="Rux")))
			else 
				return(c(.Call("time_series_parameters_set_drift", parameters_ptr=parameters_ptr, x=x, PACKAGE="Rux")))
    },
    tails = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_parameters_get_tails", parameters_ptr=parameters_ptr, PACKAGE="Rux")))
			else 
				return(c(.Call("time_series_parameters_set_tails", parameters_ptr=parameters_ptr, x=x, PACKAGE="Rux")))
    },
    scales = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_parameters_get_scales", parameters_ptr=parameters_ptr, PACKAGE="Rux")))
			else 
				return(c(.Call("time_series_parameters_set_scales", parameters_ptr=parameters_ptr, x=x,PACKAGE="Rux")))
    },
    obs_scales = function(x=NULL) {
			if (is.null(x)) 
				return(c(.Call("time_series_parameters_get_obs_scales", parameters_ptr=parameters_ptr, PACKAGE="Rux")))
			else 
				return(c(.Call("time_series_parameters_set_obs_scales", parameters_ptr=parameters_ptr, x=x, PACKAGE="Rux")))
    }
	),
  methods = list(
		initialize = function(
			y_at_times = NA,
			minima_at_times = rep(NA, length(y_at_times)),
			maxima_at_times = rep(NA, length(y_at_times)),
			times = 1:length(y_at_times),
			x_at_times = y_at_times,
			drift = rep(0, length(y_at_times)),
			tails = rep(NA, length(y_at_times)),
			scales = rep(NA, length(y_at_times)),
			obs_scales = rep(NA, length(y_at_times)),
			distributions = NULL,
			RNG=NULL,
			seed=sample(x=1:10^3, size=1)  ## If 'ya wants random, 'ya needs a REAL seed! (not this one!)
    ) {
      # Checks:
      competingData <- !is.na(y_at_times) & (!is.na(minima_at_times) | !is.na(maxima_at_times))
      if (any(competingData)) {
  	    whichCompetingData <- which(competingData)
        msg <- paste(
     	   "Multiple data types specified for certain time points.  ",
         "Problems for: \n",
          paste("\t", whichCompetingData, "\n", sep='', collapse=""))
         stop(msg)
       }

			# Sometimes it's just easier this way:
			n_times <- length(times)
			if (length(y_at_times) != n_times) stop("Must have one measurement per time point.")
			if (length(minima) != n_times)     stop("Must have one lower bound per time point.")
			if (length(maxima) != n_times)     stop("Must have one upper bound per time point.")
			if (length(x_at_times) != n_times) stop("Must have one initial value for state ('x') per time point.")
			if (length(drift) != n_times)      stop("Must have one drift value per time point.")
			if (length(tails) != n_times)      stop("Must have one tail value per time point.")
			if (length(scales) != n_times)     stop("Must have one scale value per time point.")
			if (length(obs_scales) != n_times) stop("Must have one observation scale value per time point.")




			# Distributions:
			if (is.null(distributions)) {
				distr_ <<- c(
				'constant', 'uniform', 'ordered_uniform', 'normal', 't_walk',
				't_walk_open', 't_walk_open_reverse',
				't_walk_observed_normal', 't_walk_observed_interval'
				)
			} else {
				distr_ <<- distributions
			}
			
			# RNG:
			if (is.null(RNG)) {
				rng_ptr <<- .Call("get_RNG", seed_=seed, PACKAGE="Rux")
			} else {
				rng_ptr <<- RNG
			}
			
			# Tracking C++ level types...
			distribution_type <<- list()
			
			# Set vars.
			times_ <<- times
			y_ <<- y_at_times   ## Used to be locs_ (observed)
			mins_ <<- minima_at_times
			maxs_ <<- maxima_at_times
			x_ <<- x_at_times   ## Used to be draws_ (state)
			drift_ <<- drift
			tails_ <<- tails
			scales_ <<- scales
			obs_scales_ <<- obs_scales
			
			
			# Make C++ level objects, R objects are copied
			# and should now ONLY be accessed through handles.
			manage_these_ptrs <<- .Call("time_series_init",
				times_= times_,
				y_at_times_ = y_,
				minima_at_times_ = mins_,
				maxima_at_times_ = maxs_,
				x_at_times = x_,
				drift_ = drift_,
				tails_ = tails_,
				scales_ = scales_,
				obs_scales_ = obs_scales_,
				xp_rng=rng_ptr,
			PACKAGE="Rux")
			
			data_ptr <<- manage_these_ptrs[['data_ptr']]
			parameters_ptr <<- manage_these_ptrs[['parameters_ptr']]
			posterior_ptr <<- manage_these_ptrs[['posterior_ptr']]
			
			# Return
			return(.self)
		},
    check_distribution_binding = function(types=NULL) {
	    if(is.null(types)) types <- distribution_type
      if(!all(types) %in% distr_)
  	    stop("Some distributions not of an allowed type.")
      if(length(type) != length(times_))
        stop("A distribution type must be assigned to each time point.")
       return(TRUE)
		},
    add_distribution = function(type=NULL, which=NULL, ...) {
      if (is.null(type) || !(type %in% distr_)) {
          msg <- paste(
              "The 'type' parameter must be one of:\n", paste(distr_,collapse=', '),
              "\n")
          stop(msg)
      }
      if ((which < 1) || (which > length(locs_))) {
          msg <- paste(
              "The 'which' parameter must be an integer in the range
              [1,",length(locs_), "].\n", sep='')
          stop(msg)
      }

			target_function <- paste('bind', type, 'distribution', sep='_')
			
			#
			.Call(target_function, tsp_xp=posterior_ptr, which=which-1) 
    	distribution_type[[as.character(which)]] <<- type
    	return(0)
  	},
		add_distributions = function(types=NULL) {
			if (is.null(types)) stop("'types' must be a character vector of distribution types.")
			if (length(types) != length(y_)) 
				stop("'types' must contain one distribution type per time point.")
			for (type in types) add_distribution(type)
		},
    drop_distribution = function(which=NULL) {
   		if ((which < 1) || (which > length(locs_))) {
      	msg <- paste(
        	"The 'which' parameter must be an integer in the range
          [1,",length(locs_), "].\n", sep='')
        stop(msg)
      } else {
        .Call("drop_distribution", tsp_xp=posterior_ptr, which=which-1, PACKAGE="Rux")
      }
      return(0)
    },
    drop_all = function(which=NULL) {
    	for ( i in 1:length(locs_)) { drop_distribution(i) }
    },
    lpdf = function(x) c(.Call("posterior_lpdfs", tsp_xp=posterior_ptr, X=x, PACKAGE="Rux")),
    draw = function() {
    	.Call("posterior_draw", tsp_xp=locations_ptr, PACKAGE="Rux")
    }
  )
)

locations_handle$lock('times_')
locations_handle$lock('y_')
locations_handle$lock('mins_')
locations_handle$lock('maxs_')
#locations_handle$lock('drift_')
#locations_handle$lock('tails_')
#locations_handle$lock('scales_')
#locations_handle$lock('obs_scales_')


