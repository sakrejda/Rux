library(gaga)
#tos <- c(1,2,3,5,6,7,8)
#tor <- list(c(1,3,5),c(3,5),c(3,7))

library(gaga)
phi <- 0.8
p <- 0.7
tos <- 1:20
N <- 10
tbs <- rbinom(n=N, size=5, prob=.3)+1
toa <- matrix(0,nrow=N, ncol=length(tos))
tor <- matrix(0,nrow=N, ncol=length(tos))
for ( i in 1:N ) {
  toa[i,tbs[i]] <- 1
  tor[i,tbs[i]] <- 1
	phi_local = phi
  for (j in (tbs[i]+1):20 ) {
		phi_local <- phi_local * 0.8
    toa[i,j] <- toa[i,j-1] *
      sample(x=c(0,1),size=1,replace=TRUE, prob=c(1-phi, phi))
    tor[i,j] <- toa[i,j] *
      sample(x=c(0,1),size=1,replace=TRUE, prob=c(1-p, p))

  }

}
tor <- apply(X=tor, MARGIN=1, FUN=function(x) {which(x==1)})
tos <- 1:20
tod <- apply(X=toa, MARGIN=1, FUN=function(x) {max(which(x==1))+1})
cat("Done simulating data.\n\n")



xpd <- data_wrapper(tos, tor)
cat(rep("\n",5))

test <- compare_K(xpd)
test <- compare_N(xpd)
test <- compare_recaptures(xpd)
test <- compare_surveys(xpd)
test <- compare_births(xpd)
test <- compare_first_obs(xpd)
test <- compare_last_obs(xpd)
cat("Done testing data object.\n\n")

xps <- state_wrapper(tos, tor, tod, rep(FALSE,length(tod)))

cat(rep("\n",5))

test <- compare_K(xps)
test <- compare_N(xps)
test <- compare_recaptures(xps)
test <- compare_surveys(xps)
test <- compare_births(xps)
test <- compare_deaths(xps)
test <- compare_first_obs(xps)
test <- compare_last_obs(xps)
test <- compare_set_deaths(xps, tod+3)
cat("Done testing state object.\n\n")

xpl <- likelihood_wrapper(tos, tor, tod, rep(FALSE, length(tod)))
cat(rep("\n",5))

test <- compare_K(xpl)
test <- compare_N(xpl)
test <- compare_recaptures(xpl)
test <- compare_surveys(xpl)
test <- compare_births(xpl)
test <- compare_deaths(xpl)
test <- compare_first_obs(xpl)
test <- compare_last_obs(xpl)
test <- compare_ll_phi_components(xpl,get_PHI(xpl)+phi)
test <- compare_ll_p_components(xpl,get_P(xpl)+p)
test <- compare_ll_phi_getters(xpl)
test <- compare_ll_p_getters(xpl)
cat("Done testing likelihood object.\n\n")


#xpp <- proposal_wrapper(tos, tor, tod, rep(FALSE, length(tod)))
#set_PHI(xpp, get_PHI(xpp)+0.8)
#set_P(xpp, get_P(xpp)+0.72)
#propose_deaths(xpp)


xpp <- posterior_and_proposal_wrapper(tos, tor, tod, rep(FALSE, length(tod)))
PHI <- set_PHI(xpp, get_PHI(xpp)+0.7)

N_rep <- 1000
o <- matrix(NA, nrow=get_N(xpp), ncol=N_rep)
for ( i in 1:N_rep ) {
	o[,i] <- propose_deaths(xpp)
}



