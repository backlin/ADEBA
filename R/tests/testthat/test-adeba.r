context("Algorithm")

test_that("Expected behaviour", {
    x <- matrix(rnorm(50, mean=3, sd=2), nrow=25, ncol=2)
    f <- adeba(x, adaptive=TRUE, beta=0:4/2)

    # Check parameter estimation
    expect_less_than(abs(sum(f$parameters$posterior) - 1), 1e-12)

    # Check that pdf volume ~= 1
    # A small amount of probability mass will inevitably be located outside the
    # grid area causing the volume to be slightly less than 1.
    f <- render(f)
    dg <- prod(sapply(f$grid, diff)[1,])
    volume <- sum(dg*f$posterior)
    expect_less_than(volume, 1)
    expect_more_than(volume, .97) # Some of the volume is outside of the grid

    # Parallelization
    require(parallel)
    options(mc.cores = 2)
    f.parallel <- render(adeba(x, parallel=TRUE))
    expect_identical(f.parallel$parameters, f$parameters)
})

test_that("Posterior calculation", {
    x <- rnorm(20)

    obj1 <- make.adeba(x)
    obj1 <- iterate(obj1)

    obj2 <- list(data = matrix(x),
                 distance = as.matrix(dist(x)),
                 parallel = FALSE,
                 pilot = rep(1, 20))
    parameters <- get_posterior(obj2, beta=0)
    expect_is(parameters, "data.frame")
    expect_true(all(c("alpha", "beta", "log.posterior") %in% names(parameters)))

    parameters <- normalize_posterior(list(parameters))
    expect_true(sum(parameters$posterior) == 1)

    expect_equal(parameters, obj1$parameters)
})

test_that("Cumulative distribution function", {
    x <- c(rnorm(20), rnorm(20, mean=2, sd=.5))
    den <- adeba(x)
    q <- seq(-5, 5, length.out = 1000)
    dq <- dadeba(q, object = den)
    cdq <- cumsum(dq)/sum(dq)
    pq <- padeba(q, object = den)
    expect_true(all(abs(pq - cdq) < .01))
    #plot(q, pq, type="l")
    #lines(q, cdq, col="red")
    #plot(q, cdq - pq, type="l")
})

# library(ggplot2)
# obj1 <- adeba(x, adaptive=FALSE, beta=0:4/2)
# obj1$pilot <- predict(obj1)
# p <- lapply(obj1$beta, function(b) adeba:::get_posterior(obj1, beta=b))
# p <- do.call(rbind, p)
# ggplot(p, aes(x = alpha, y = log.posterior, colour=factor(beta))) +
#     geom_line()
# 
# obj1 <- iterate(obj1)
# 
#     obj2 <- list(data = matrix(x),
#                  distance = as.matrix(dist(x)),
#                  parallel = FALSE,
#                  beta = 0:4/2,
#                  pilot = rep(1, 20))
#     parameters <- lapply(obj2$beta, function(b) adeba:::get_posterior(obj2, beta=b))
