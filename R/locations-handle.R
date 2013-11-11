locations_handle <- setRefClass(Class = "locations_handle",
	fields = list(
		locs_ = "numeric",
		drift_ = "numeric",
		draws_ = "numeric",
		tails_ = "numeric",
		scales_ = "numeric",
		obs_scales_ = "numeric",
		mins_ = "numeric",
		maxs_ = "numeric",
		distr_ = "character",
		distribution_type = "list",
		locations = function(x=NULL) {
			if (is.null(x))
				return(locs_)
			else
				locs_ <<- x
		},
		draws = function(x=NULL) {
			if (is.null(x)) {
				.self$draw()
				return(draws_)
			} else 
				draws_ <<- x
		},
		locations_ptr = "externalptr",
		rng_ptr = "externalptr"
	),
	methods = list(
		initialize = function(
			locations = NA, 
			initials = NULL,
			drift = rep(0, length(locations)),
			tails = rep(NA, length(locations)),
			scales = rep(NA, length(locations)),
			obs_scales = rep(NA, length(locations)),
			minima = rep(NA, length(locations)),
			maxima = rep(NA, length(locations)),
			distributions = NULL,
			RNG=NULL, seed=sample(x=1:10^3, size=1)
		) {
			# Checks:
			competingData <- !is.na(locations) & (!is.na(minima) | !is.na(maxima))
			if (any(competingData)) {
				whichCompetingData <- which(competingData)
				msg <- paste("Multiple data types specified for certain locations.  Problems for: \n",
										 paste("\t", whichCompetingData, "\n", sep='', collapse="")
										 )
				stop(msg)
			}

			if (is.null(initials)) stop("Provide initial values.")


			# Distributions:
			if (is.null(distributions)) {
				distr_ <<- c(
					'constant', 'uniform', 'ordered_uniform', 'normal', 't_walk',
					't_walk_open', 't_walk_observed_normal', 't_walk_observed_interval'
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
			locs_ <<- locations
			drift_ <<- drift
			tails_ <<- tails
			scales_ <<- scales
			obs_scales_ <<- obs_scales
			mins_ <<- minima
			maxs_ <<- maxima
			draws_ <<- initials


			# Pass down to C++
			locations_ptr <<- .Call("locations_init", 
				locations_=locs_, drift_ = drift_, 
				tails_=tails_, scales_=scales_,
				obs_scales_ = obs_scales_,
				minima_=mins_, maxima_=maxs_, draws_ = draws_, 
				xp_rng=rng_ptr, PACKAGE="Rux")

			# Initialize (this is no longer quite right, resolve?)
			# Much more ambiguous now how to do by default, it will have to be
			# part of initalization, maybe just a loop of distribution names
			# to add... plus checks.
			for ( i in which(!is.na(locations) &  is.na(obs_scales) ) ) add_distribution(type='constant', which=i)
#			for ( i in which(!is.na(locations) & !is.na(obs_scales) ) ) add_distribution(type='normal', which=i)
#			for ( i in which(!is.na(minima) & !is.na(maxima)) ) add_distribution(type='uniform', which=i)

			# Return
			return(.self)
		},
		lpdf = function(x) c(.Call("locations_lpdfs", xp=locations_ptr, X=x, PACKAGE="Rux")),
		state = function() c(.Call("locations_state", xp=locations_ptr, PACKAGE="Rux")),
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

			switch(EXPR=type,	
				constant = {
					.Call("locations_bind_constant", 	xp=locations_ptr, 
						which=which-1, PACKAGE="Rux")
				},
				uniform = {
					.Call("locations_bind_uniform", 	xp=locations_ptr,
						which=which-1, xp_rng=rng_ptr, PACKAGE="Rux")
				},
				ordered_uniform = {
					.Call("locations_bind_ordered_uniform", 	xp=locations_ptr,
						which=which-1, xp_rng=rng_ptr, PACKAGE="Rux")
				},
				normal = {
					.Call("locations_bind_normal", xp=locations_ptr,
						which=which-1, xp_rng=rng_ptr, PACKAGE="Rux")
				},
				t_walk = {
					.Call("locations_bind_t_walk", 		xp=locations_ptr, 
						which=which-1, xp_rng=rng_ptr, PACKAGE="Rux")
				},
				t_walk_open = {
					.Call("locations_bind_t_walk_open", 		xp=locations_ptr, 
						which=which-1, xp_rng=rng_ptr, PACKAGE="Rux")
				},
				t_walk_observed_normal = {
					.Call("locations_bind_t_walk_observed_normal", xp=locations_ptr,
						which=which-1, xp_rng=rng_ptr, PACKAGE="Rux")
				},
				t_walk_observed_interval = {
					.Call("locations_bind_t_walk_observed_interval", xp=locations_ptr,
						which=which-1, xp_rng=rng_ptr, PACKAGE="Rux")
				}
			)
			distribution_type[[as.character(which)]] <<- type
			return(0)
		},
		drop_distribution = function(which=NULL) {
			if ((which < 1) || (which > length(locs_))) {
				msg <- paste(
					"The 'which' parameter must be an integer in the range
					[1,",length(locs_), "].\n", sep='')
				stop(msg)
			} else {
				.Call("locations_drop_distribution", xp=locations_ptr,
							which=which-1, PACKAGE="Rux")
			}
			return(0)
		},
		drop_all = function(which=NULL) {
			for ( i in 1:length(locs_)) { drop_distribution(i) }
		},
		draw = function() {
			.Call("locations_draw", xp=locations_ptr, PACKAGE="Rux")
		}
	)
)

locations_handle$lock('locs_')
locations_handle$lock('drift_')
locations_handle$lock('tails_')
locations_handle$lock('scales_')
locations_handle$lock('obs_scales_')
locations_handle$lock('mins_')
locations_handle$lock('maxs_')
locations_handle$lock('distr_')



