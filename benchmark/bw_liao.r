#' Liao bandwith selector
#'
#' This function implements the two bandwidths, h2 and h3, in Liao (2010).
#' The h2 and h3 are very close. If you have to choose, we suggest you use h2
#' for sample size <= 400 and use h3 for sample size larger.
#'
#' @param x Sample, vector of numeric data points.
#' @param List with the two bandwidths.
#' @examples
#' x <- c(rnorm(400), rnorm(400)+3)
#' bw <- bw.liao(x)
#' plot(density(x, bw=bw$h3))
#' @author Jason Liao
#' @references Liao, J.G., Wu, Y.J. and Lin, Y. (2010) \emph{Improving Sheather
#'   and Jones bandwidth selector for difficult densities in kernel density
#'   estimation.} Journal of Nonparametric Statistics 22, 105-114.
#' @export
bw.liao <- function(x) {
    n.exact <- 400
    if(length(x) <= n.exact) {
        h2.value <- bw.liao.exact(x)
    } else {
        h2.value <- bw.liao.approx(x)
    }
    list(h2 = h2.value, h3 = min(h2.value, bw.SJ(x)))
}


#---------------------------------------------------------[ Internal functions ]

library(compiler)
SDh <- function(x, h, n, d) .Call(stats:::C_bw_phi4, n, d, x, h)
#Phi4  x will be replaced by a processed version cnt
TDh <- function(x, h, n, d) .Call(stats:::C_bw_phi6, n, d, x, h)
#Phi6  x will be replaced by a processed version cnt


#-------------------------------------------------------------------------------

h <- cmpfun(function(u, k) {
    if(k>=2) u2 <- u*u
    if(k>=4) u4 <- u2*u2
    if(k>=6) u6 <- u2*u4
    if(k>=8) u8 <- u4*u4
    if(k>=10) u10 <- u8*u2

    if(k==0) 1
    else if(k==1) u
    else if(k==2) u2 - 1
    else if(k==4) u4 - 6*u2 + 3
    else if(k==6) u6 - 15*u4 + 45*u2 - 15
    else if(k==8) u8 - 28*u6 + 210*u4 - 420*u2 + 105
    else if(k==10) u10 - 45*u8 + 630*u6 - 3150*u4 + 4725*u2 - 945
})

K.deriv <- function(u, k) h(u, k)*dnorm(u)

phi <- cmpfun(function(x, h1, k) {
    n <- length(x)
    x <- x/h1
    temp <- outer(x, x, "-")
    n/(n-1)*mean(K.deriv(temp, k))/h1^(k+1)
})

#g4 as obtained from SJ bandwidth, h0 is bw.SJ
g4.initial <- cmpfun(function(x) {
    n <- length(x)
    nb <- 4000 #number of bins

    z <- .Call(stats:::C_bw_den, nb, x)
    d <- z[[1L]]      #needed for the faster functions
    cnt <- z[[2L]]    #this will be a r

    scale <- min(sd(x), IQR(x)/1.349)
    a <- 1.24 * scale * n^(-1/7)
    b <- 1.23 * scale * n^(-1/9)

    alph2 <- 1.357 * (SDh(cnt, a, n, d)/-TDh(cnt, b, n, d))^(1/7)
    alph2 * (bw.SJ(x))^(5/7) #output g4 as used in bw.SJ
})


#-------------------------------------------------------------------------------

V.deriv.func <- cmpfun(function(h, s, q, w, mu, sigma2, equal.w = FALSE) {
    #sigma.local <- sqrt( outer(sigma2, sigma2, "+") + q*h*h )    #general form for sigma2  that can differ from element to element
    sigma.local <- sqrt( sigma2[1] + sigma2[1] + q*h*h ) # applies when all elements of sigma2 are the same

    diff1 <- outer(mu, mu, "-")/sigma.local

    term1 <- K.deriv(diff1, 2*s)/(sigma.local^(2*s+1))
    term2 <- K.deriv(diff1, 2*s+2)/(sigma.local^(2*s+3))

    if(equal.w == TRUE) {
        term1 <- 2*s*h^(2*s-1)*sum(term1)
        term2 <- q*h^(2*s+1)*sum(term2)
        (term1+term2)/length(w)^2
    } else {
        ww <- outer(w, w, "*")
        term1 <- 2*s*h^(2*s-1)*sum(term1*ww)
        term2 <- q*h^(2*s+1)*sum(term2*ww)
        term1+term2
    }
})


#-------------------------------------------------------------[ Exact solution ]

MISE.deriv.cal.exact <- cmpfun(function(h, n, w, mu, sigma2, equal.w = FALSE) {
   term1 <- -1/(2*sqrt(pi))/(n*h*h)
   term2 <- (1-1/n)*V.deriv.func(h, 0, 2, w, mu, sigma2, equal.w)
   term3 <- -2*V.deriv.func(h, 0, 1, w, mu, sigma2, equal.w)
   term1 + term2 + term3
})

bw.liao.exact <- cmpfun(function(x) {
    n <- length(x)
    rk <- 1/2/sqrt(pi)
    temp <- 2*K.deriv(0, 4)/rk

    h1 <- NULL
    func1 <- function(log.g4) {
        #g4 is the bandwidth to estimate r2 from sample x_1,...,x_n
        g4 <- exp(log.g4)
        g6 <- .992*g4*n^(1/7-1/9)

        ratio <- -phi(x, g6, 4)/phi(x, g6, 6)
        thefactor <- (temp*ratio)^(1/7)      #reference distribution with r2
        h1 <<- (g4/thefactor)^(7/5)

        w <- rep(1/n, n)
        sigma2 <- rep(g4*g4/2, length(w))  #g4/sqrt(2) is the bandwidth, for using wand and marron's exact formula in theorem 5.1

        MISE.deriv.cal.exact(h1, n, w, x, sigma2, equal.w=TRUE)
    }

    g4 <- g4.initial(x)
    interval <- log(c(g4/10, g4*10))
    value1 <- func1(interval[1])
    value2 <- func1(interval[2])
    if(value1*value2 > 0) {
        bw.SJ(x)
    } else {
        uniroot(func1, interval, f.lower=value1, f.upper=value2)
        h1
    }
})


#-------------------------------------------------------[ Approximate solution ]

#this is a simplified form as in Hall (1991)
MISE.deriv.cal.approx <- cmpfun(function(h1, n, I2, I3, RK) {
    h2 <- (RK/I2/n)^.2
    ratio2 <- I2/I3
    J2 <- 3/20/ratio2
    h1 - (h2 + J2*h2^3)
})

#' @param nb The number of bins
bw.liao.approx <- cmpfun(function (x, nb = 4000, SJ=F) {
    n <- length(x)

    Z <- .Call(stats:::C_bw_den, nb, x)
    d <- Z[[1L]]      #needed for the faster functions
    cnt <- as.integer(Z[[2L]])  #this will be a replacement for x in  SDh and TDh

    RK <- 1/2/sqrt(pi)
    temp <- 2*K.deriv(0, 4)/RK

    h1 <- NULL
    func1 <- function(log.g4) {
        g4 <- exp(log.g4)
        g6 <- .992*g4*n^(1/7-1/9)

        ratio <- -SDh(cnt, g6, n, d)/TDh(cnt, g6, n, d)
        theFactor <- (temp*ratio)^(1/7)
        h1 <<- (g4/theFactor)^(7/5)

        I2 <- SDh(cnt, g4, n, d)
        I3 <- -TDh(cnt, g4, n, d)
        MISE.deriv.cal.approx(h1, n, I2, I3, RK)
    }

    g4 <- g4.initial(x)
    interval <- log(c(g4/10, g4*10))
    value1 <- func1(interval[1])
    value2 <- func1(interval[2])

    if(value1*value2 > 0) {
        bw.SJ(x)
    } else {
        uniroot(func1, interval, f.lower=value1, f.upper=value2)
        h1
    }
})

