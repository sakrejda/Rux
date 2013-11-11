library(gamlss)

f <- function(x1, x2, x3) {
	o <- dTF(x=x2, mu=x1, sigma=4, nu=1, log=TRUE) + 
			 dTF(x=x3, mu=x2, sigma=4, nu=1, log=TRUE)
	o <- exp(o)
	return(o)
}

x2 <- seq(-20,120,0.01)
x1 <- rep(0,length(x2))
x3 <- rep(100,length(x2))
dat <- data.frame(x1=x1, x2=x2, x3=x3)
dat[['f']] <- f(dat$x1, dat$x2, dat$x3)
dat[['d1']] <- dat$x2-dat$x1
dat[['d2']] <- dat$x3-dat$x2

library(ggplot2)
ggplot(data=dat, aes(x=x2, y=f)) + geom_line() + ylim(0,max(dat$f)*1.1)

#g <- function(x1, x2, x3) {
#	o <- dnorm(x=x2-x1, mean=0, sd=1, log=TRUE) + 
#			 dnorm(x=x3-x2, mean=0, sd=1, log=TRUE)
#	return(exp(o))
#}
#
#dat[['g']] <- g(dat$x1, dat$x2, dat$x3)
#ggplot(data=dat, aes(x=x2, y=g)) + geom_line()


g <- function(x) { f(0,x,150) }

N_itt <- 1000
w <- 20 
m <- 20
x <- vector(mode='numeric', length=N_itt+1)
y <- vector(mode='numeric', length=N_itt+1)
for ( i in 2:(N_itt+1) ) {
	y[i] <- runif(1) * g(x[i-1])
	u <- runif(1)
	L <- x[i-1] - w * u
	R <- L + w
	V <- runif(1)
	J <- floor(m * V)
	K <- (m-1) - J
	while(J > 0 && y[i] < g(L)) {
		L <- L - w
		J <- J - 1
	}
	while(K > 0 && y[i] < g(R)) {
		R <- R + w
		K <- K - 1
	}
	while(TRUE) {
		U <- runif(1)
		x[i] <- L + U * (R-L)
		if (y[i] < g(x[i])) break
		if (x[i] < x[i-1]) L <- x[i] else R <- x[i]
	}
}
	
plot(x=x, y=y, type='l')


ha <- function(x1, x3, nu) { .5*( 1*sqrt((x1-x3)^2-4*nu)+x1+x3) }
hb <- function(x1, x3, nu) { .5*(-1*sqrt((x1-x3)^2-4*nu)+x1+x3) }

curve(from=0.1, to=6000, expr=ha(0,150,x), ylim=c(0,150))
curve(from=0.1, to=6000, expr=hb(0,150,x), add=TRUE)

