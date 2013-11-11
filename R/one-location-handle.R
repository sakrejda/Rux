setRefClass(Class = "one_missing_location_handle",
	fields = list(
		rng_ptr = "externalptr",
		location_ptr = "externalptr",
		x1 = function(x1=NULL) {
			if (!is.null(x1)) warning("'x1' is read-only after construction.")
			return(get_state(s='x1'))
		},
		x2 = function(x2=NULL) {
			if (!is.null(x2)) {
				jump(x2=x2)
			} else { 
				return(get_state(s='x2'))
			}
		},
		x3 = function(x3=NULL) {
			if (!is.null(x3)) warning("'x3' is read-only after construction.")
			return(get_state(s='x3'))
		},
		p1 = function(p1=NULL) {
			if (!is.null(p1)) {
				warning("'p1' is read-only after construction.")
			} else {
				return(get_state(s='p1'))
			}
		},
		p2 = function(p2=NULL) {
			if (!is.null(p2)) {
				warning("'p2' is read-only after construction.")
			} else {
				return(get_state(s='p2'))
			}
		},
		s1 = function(s1=NULL) {
			if (!is.null(s1)) {
				warning("'s1' is read-only after construction.")
			} else {
				return(get_state(s='s1'))
			}
		},
		s2 = function(s2=NULL) {
			if (!is.null(s2)) {
				warning("'s2' is read-only after construction.")
			} else {
				return(get_state(s='s2'))
			}
		},
		vec = "numeric"
	),
	methods = list(
		initialize = function(x1, x2, x3, p1, p2, s1, s2, 
													seed=sample(x=1:10^3, size=1)) {
			rng_ptr <<- .Call("get_RNG", seed_=seed, PACKAGE="Rux")
			vec <<- c(x1=x1, x2=x2, x3=x3, p1=p1, p2=p2, s1=s1, s2=s2)
			location_ptr <<- .Call('t_walk_init',
				x = vec,
				xp_rng = rng_ptr,
				PACKAGE="Rux"
			)
			get_state()

			return(.self)
		},
		get_state = function(s=NULL) {
			state <- .Call("t_walk_state", xp=location_ptr, PACKAGE="Rux")
			if (is.null(s)) {
				return(state)
			} else {
				return(state[s])
			}
		},
		jump = function(x2) {
			.Call("t_walk_jump", xp=location_ptr, x2=x2, PACKAGE="Rux")
			return(get_state('x2'))
		},
		lpdf = function(x2) {
			.Call("t_walk_lpdf", xp=location_ptr, x2=x2, PACKAGE="Rux")
		}
	)
)


setRefClass(Class = "location",
	fields = list(
		is.observed = function(x=NULL) {
			if (!is.null(x)) {
				stop("Observation status can only be set at initialization.")
			} else {
				return(obs)
			}
		},
		obs = "logical"
	),
	methods = list(
		initialize = function(observed=FALSE) {
			obs <<- observed
		}
	)
)
getRefClass('location')$lock('obs')

setMethod(f="is.na", signature=signature(x="location"), function(x) !x$is.observed)

setRefClass(Class = "point_location",
	fields = list(
		x = function(x=NULL) {
			if (!is.null(x)) location <<- x else return(location)
		},
		draw = function(x=NULL) {
				return(location)
		},
		location = "numeric"
	),
	methods = list(
		initialize = function(x) { 
			location <<- x
			return(.self)
		}
	),
	contains = 'location'
)


setRefClass(Class = "interval_location",
	fields = list(
		minimum = "numeric",
		maximum = "numeric",
		draw = function(x=NULL) {
			if (!is.null(x)) 
				return(x)
			else 
				return(runif(n=1, min=minimum, max=maximum))
		}
	),
	contains = 'location',
	methods = list(
		initialize = function(min=NULL, max=NULL, ...) {
			if (is.null(min) || is.null(max)) { stop("min/max must be provided.") }
			minimum <<- min
			maximum <<- max
			callSuper(observed=TRUE, ...)
		}
	)
)

setRefClass(Class = "location_vector",
	fields = list(
		locations = "list", # of locations
		observed = function(x=NULL) {
			if (!is.null(x)) {
				stop("Observation status can only be set at initialization.")
			} else {
				return(sapply(X=locations, FUN=function(x) x$is.observed))
			}
		}
	),
	methods = list()
)
	

setRefClass(Class = "location_handle",
	fields = list(
		rng_ptr = "externalptr",
		location_ptr = "list",   #"externalptr",
		locations = "list",  # of "locations"
		tails = "numeric",
		scales = "numeric"
	),
	methods = list(
		initialize = function(
				locations,
				tails,
				scales,
				rng_handle = NULL,
				seed=sample(x=1:10^3, size=1)
			) {
			locations <<- locations
			tails <<- tails
			scales <<- scales
			if (is.null(rng_ptr)) {
				rng_ptr <<- .Call("get_RNG", seed_=seed, PACKAGE="Rux")
			}
			

			return(.self)
		}
	)
)


