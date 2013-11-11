## Krzysztof Sakrejda
## In this example, we simulate simple mark-recapture data, and then
## estimate the phi/p parameters by using the discrete slice sampler
## for the state and the algorithm from the MCMCglmm package for
## phi/p conditional on survival.

library(gaga)
library(MASS)
phi <- 0.8
p <- 0.7
tos <- 1:10
N <- 100
tbs <- rbinom(n=N, size=5, prob=.3)+1
toaM <- matrix(0,nrow=N, ncol=length(tos))
tor <- matrix(0,nrow=N, ncol=length(tos))
for ( i in 1:N ) {
  toaM[i,tbs[i]] <- 1
  tor[i,tbs[i]] <- 1
	phi_local = phi
  for (j in (tbs[i]+1):max(tos) ) {
		phi_local <- phi_local * 0.8
    toaM[i,j] <- toaM[i,j-1] *
      sample(x=c(0,1),size=1,replace=TRUE, prob=c(1-phi, phi))
    tor[i,j] <- toaM[i,j] *
      sample(x=c(0,1),size=1,replace=TRUE, prob=c(1-p, p))

  }

}
tor <- apply(X=tor, MARGIN=1, FUN=function(x) {which(x==1)})
tod <- apply(X=toaM, MARGIN=1, FUN=function(x) {max(which(x==1))+1})
cat("Done simulating data.\n\n")



xpp <- posterior_and_proposal_wrapper(tos, tor, tod, rep(FALSE, length(tod)))
o <- set_PHI(xpp, get_PHI(xpp)+0.5)
o <- set_P(xpp, get_P(xpp)+0.6)

N_it <- 1000

o_accept <- vector(mode="numeric", length=N_it)
o_PHI <- vector(mode="numeric", length=N_it)
o_P <- vector(mode="numeric", length=N_it)
o_lpd <- vector(mode="numeric", length=N_it)

system.time({
for ( it in 1:N_it ) {
	new_deaths = propose_deaths(xpp)
	set_deaths(xpp, 1:N, new_deaths)

	## Steps:
	# 1) Transform PHI/P/deaths into data/covariates...
	#			data is going to be, for each i/t combo, live or
	#			die (0/1), the covariates are going to be i, t,
	#			and any i/t-dependent covariates.
	# 2) Decide on priors and model formula.
	# 3) Estimate the model with MCMCglmm, get posterior sample
	#			(just one, uncorrelated) for PHI/P
	# 4) Use the sample to calculate PHI/P @ i/t (make the matrix, 
	#			efficiently, one hopes).



	o_PHI[it] = NA
	o_P[it]   = NA
	o_lpd[it] = get_log_posterior(xpp)
}
})

plot(o_PHI, pch='*')
plot(o_lpd, pch='*')
plot(o_PHI, o_lpd, pch = '*')
plot(o_PHI, o_P, pch = '*")


