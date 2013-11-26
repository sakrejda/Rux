# The function:
logt <- function(x, mu, delta, p, sigma) {
	pp1 <- (p+1)/2
	pp2 <- (p/2)
	a1 <- lgamma(pp1) - lgamma(pp2) - 0.5*log(p*pi*sigma^2)
	a2 <- - pp1 * log(p*sigma^2 + (x-(mu+delta))^2 ) 
	a3 <-   pp1 * log(p*sigma^2)
	return(a1 + a2 + a3)
}

logt_two_step <- function(
	xt__, 			xtm1, 			xtp1, 
	deltat__, 	deltatm1,
	pt__, 			ptm1,
	sigmat__, 	sigmatm1
) {
	logt1 <- logt(xt__, xtm1, deltatm1, ptm1, sigmatm1)
	logt2 <- logt(xtp1, xt__, deltat__, pt__, sigmat__)
	return(logt1+logt2) 
}


logt_two_step_obs <- function(
	xt__, 			xtm1, 			xtp1, 
	deltat__, 	deltatm1,
	pt__, 			ptm1,
	sigmat__, 	sigmatm1,
	xtobs, 			sigmaobs
) {
	logt1 <- logt(xt__, xtm1, deltatm1, ptm1, sigmatm1)
	logt2 <- logt(xtp1, xt__, deltat__, pt__, sigmat__)
	logtobs <- dnorm(x=xtobs, mean=xt__, sd=sigmaobs, log=TRUE)
	return(logt1+logt2+logtobs) 
}


logt_two_step_gradient_coeffs <- function( 
	xt__, 			xtm1, 			xtp1, 
	deltat__, 	deltatm1,
	pt__, 			ptm1,
	sigmat__, 	sigmatm1,
	xtobs, 			sigmaobs
) {
	pow <- `^`

	B1 <- ( -1*(ptm1+1) - (pt__+1) )
	B2 <- ( (ptm1+1)*(xtm1+deltatm1+2*xtp1-2*deltat__) +
				  (pt__+1)*(xtp1-deltat__+2*xtm1+2*deltatm1)	)
	B3 <- ( -1*(ptm1+1)*(pt__*pow(sigmat__,2) + pow(xtp1-deltat__,2) + 2*(xtm1+deltatm1)*(xtp1-deltat__))
			    -1*(pt__+1)*(ptm1*pow(sigmatm1,2) + pow(xtm1+deltatm1,2) + 2*(xtp1-deltat__)*(xtm1+deltatm1))	)
	B4 <- (
				 (ptm1+1)*(xtm1+deltatm1)*(pt__*pow(sigmat__,2)+pow(xtp1-deltat__,2)) +
				 (pt__+1)*(xtp1-deltat__)*(ptm1*pow(sigmatm1,2)+pow(xtm1+deltatm1,2)) )
	return(list(B1=B1, B2=B2, B3=B3, B4=B4))
}

logt_two_step_gradient_obs_coeffs <- function( 
	xt__, 			xtm1, 			xtp1, 
	deltat__, 	deltatm1,
	pt__, 			ptm1,
	sigmat__, 	sigmatm1,
	xtobs, 			sigmaobs
) {
	pow <- `^`

	A1 <- 2*( (deltat__-xtp1) - (deltatm1+xtm1) )
	A2 <-   ( pow(xtp1-deltat__,2) + pt__*pow(sigmat__,2) +
 					  pow(xtm1+deltatm1,2) + ptm1*pow(sigmatm1,2) +
					 -4*(deltatm1+xtm1)*(deltat__-xtp1) )
	A3 <- 2*( (deltat__-xtp1)*(pow(xtm1+deltatm1,2)+ptm1*pow(sigmatm1,2)) - 
					  (deltatm1+xtm1)*(pow(xtp1-deltat__,2)+pt__*pow(sigmat__,2)) ) 
	A4 <-   ( (pow(xtp1-deltat__,2) + pt__*pow(sigmat__,2)) * 
					  (pow(xtm1+deltatm1,2) + ptm1*pow(sigmatm1,2)) )
	Bs <- logt_two_step_gradient_coeffs(
					xt__, xtm1, xtp1, deltat__, deltatm1, pt__, ptm1, sigmat__, sigmatm1)
	return(c(list(A1=A1, A2=A2, A3=A3, A4=A4),Bs))
}


logt_two_step_gradient <- function( 
	xt__, 			xtm1, 			xtp1, 
	deltat__, 	deltatm1,
	pt__, 			ptm1,
	sigmat__, 	sigmatm1
) {
	o <- with(
		data = logt_two_step_gradient_coeffs(
							xt__, xtm1, xtp1, deltat__, deltatm1, pt__, ptm1,
							sigmat__, sigmatm1),
		expr = {
				B1 *	xt__^3 	+ B2 *	xt__^2 	+ B3 *	xt__ 		+ B4
		}
	)
	return(o)
}

logt_two_step_companion <- function( 
	xt__, 			xtm1, 			xtp1, 
	deltat__, 	deltatm1,
	pt__, 			ptm1,
	sigmat__, 	sigmatm1
) {
	o <- with(
		data = logt_two_step_gradient_coeffs(
							xt__, xtm1, xtp1, deltat__, deltatm1, pt__, ptm1,
							sigmat__, sigmatm1),
		expr = {
			o <- matrix(data=0, nrow=3, ncol=3)
			o[2,1] <- 1
			o[3,2] <- 1
			o[3,3] <- -B2/B1
			o[2,3] <- -B3/B1
			o[1,3] <- -B4/B1
			return(o)
		}
	)
	return(o)
}

logt_two_step_gradient_obs <- function( 
	xt__, 			xtm1, 			xtp1, 
	deltat__, 	deltatm1,
	pt__, 			ptm1,
	sigmat__, 	sigmatm1,
	xtobs, 			sigmaobs
) {
	o <- with(
		data = logt_two_step_gradient_obs_coeffs(
							xt__, xtm1, xtp1, deltat__, deltatm1, pt__, ptm1,
							sigmat__, sigmatm1, xtobs, sigmaobs),
		expr = {
			  -1															*	xt__^5 	+ 
				(		xtobs	- A1								)	*	xt__^4 	+ 
				(A1*xtobs - A2 + B1*sigmaobs^2)	*	xt__^3 	+
 	      (A2*xtobs - A3 + B2*sigmaobs^2)	*	xt__^2 	+ 
				(A3*xtobs - A4 + B3*sigmaobs^2)	*	xt__ 		+ 
			  (A4*xtobs 		 + B4*sigmaobs^2)
		}
	)
	return(o)
}

logt_two_step_obs_companion <- function( 
	xt__, 			xtm1, 			xtp1, 
	deltat__, 	deltatm1,
	pt__, 			ptm1,
	sigmat__, 	sigmatm1,
	xtobs,			sigmaobs
) {
	o <- with(
		data = logt_two_step_gradient_obs_coeffs(
							xt__, xtm1, xtp1, deltat__, deltatm1, pt__, ptm1,
							sigmat__, sigmatm1, xtobs, sigmaobs),
		expr = {
			o <- matrix(data=0, nrow=5, ncol=5)
			o[2,1] <- 1
			o[3,2] <- 1
			o[4,3] <- 1
			o[5,4] <- 1
			o[5,5] <- (		xtobs	- A1								)
			o[4,5] <- (A1*xtobs - A2 + B1*sigmaobs^2)
      o[3,5] <- (A2*xtobs - A3 + B2*sigmaobs^2)
			o[2,5] <- (A3*xtobs - A4 + B3*sigmaobs^2)
		  o[1,5] <- (A4*xtobs 		 + B4*sigmaobs^2)

			return(o)
		}
	)
	return(o)
}






