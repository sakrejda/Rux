library(gamlss)

f <- function(x1, x2, x3, x4) {
	o <- dTF(x=x2, mu=x1, sigma=4, nu=1, log=TRUE) + 
			 dTF(x=x3, mu=x2, sigma=4, nu=1, log=TRUE) +
			 dTF(x=x4, mu=x4, sigma=4, nu=1, log=TRUE)
	o <- exp(o)
	return(o)
}

x2 <- seq(-20,120,0.1)
x1 <- rep(0,length(x2))
x3 <- seq(-20,120,0.1)
x4 <- rep(120,length(x2))


dat <- data.frame(x1=x1, x2=x2, x3=x3)
dat[['f']] <- f(dat$x1, dat$x2, dat$x3)
dat[['d1']] <- dat$x2-dat$x1
dat[['d2']] <- dat$x3-dat$x2


g <- function(x) { f(0,x,y,300) }
library(ggplot2)
ggplot(data=dat, aes(x=x2, y=f)) + geom_line() + ylim(0,max(dat$f)*1.1)

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



