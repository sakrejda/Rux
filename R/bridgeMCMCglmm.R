LuxToMCMCglmmBridge <- function(
	tags, tb, td, tt=NULL, recaptures,   ## tt: time of tagging
	times, surveys, 
	CJS=TRUE, COV=NULL
) {
	## CJS or not?
	if (!CJS && is.null(tt)) 
		stop("Must provide time of tagging for non-CJS.")
	else { tt <- tb } ## CJS assumption

	dat <- mapply(
		FUN = function(i, tb, td, tt, recaptures, times, surveys, CJS) {
			dat <- data.frame(
				survive=NA, recapture=NA, alive=0, tag=i, time=tb:td)

			## Alive:
			dat[['alive']][ dat[['time']] %in% tb:(td-1) ] <- 1

			## Survive:
			if( (td-1) > tb ) {
				dat[['survive']][ dat[['time']] %in% (tb:(td-2)) ] <- 1
			}
			dat[['survive']][ dat[['time']] %in% (td-1)      ] <- 0

			## Recapture:
			available <- tt:(min(td-1,surveys['stop']))
			dat[['recapture']][ dat[['time']] %in% available ] <- 0
			dat[['recapture']][ dat[['time']] %in% recaptures ] <- 1
			if (CJS) dat[['recapture']][ dat[['time']] == tb ] <- NA  ## CJS assumption.

			return(dat)
		}, i=tags, tb=tb, td=td, tt=tt, recaptures=recaptures, 
		MoreArgs = list(times=times, surveys=surveys, CJS=CJS),
		SIMPLIFY=FALSE
	)
	if (!is.data.frame(dat)) {dat <- do.call(what=rbind, args=dat)}
	if (!is.null(COV)) { dat <- merge(x=dat, y=COV, by=c('tag','time'), all.x=TRUE) }
	return(dat)

}

#LuxToMCMCglmmBridge(
#	tags = c(1,2), tb=c(3,5), td=c(10,10), 
#	recaptures=list('1'=c(3,5,6), '2'=c(5,6)),
#	times=c(start=1, stop=15), surveys=c(start=3, stop=6), CJS=TRUE)


#LuxToMCMCglmmBridge(
#	tags = c(1,2), tb=c(3,5), td=c(10,10), tt=c(3,5),
#	recaptures=list('1'=c(3,5,6), '2'=c(5,6)),
#	times=c(start=1, stop=15), surveys=c(start=3, stop=6), CJS=FALSE)


