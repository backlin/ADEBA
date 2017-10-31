library(adeba)

post_mean <- function(post){
    requireNamespace("mixtools")
    
    x <- cbind(log_alpha=log(post$alpha), beta=post$beta)
    post_cov <- cov.wt(x = x, wt = post$posterior)
    
    fit_posterior <- mixtools::dmvnorm(x, post_cov$center, post_cov$cov)
    post$fit_posterior <- fit_posterior/sum(fit_posterior)

    return(post)
}
