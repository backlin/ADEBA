
#---------------------------------------------------------------------[ Config ]

priors <- c("simpleuniform", "simplejeffreys", "uniform", "empiricalgaussian")
n_replicates <- 400
sample_sizes <- unique(round(exp(seq(log(2), log(100), length.out = 22))))
distribution_indexes <- c(1, 3, 12)
results_file <- "comparison.Rdata"


#------------------------------------------------------------------[ Internals ]

library(adeba)
library(data.table)

setwd("~/devel/adeba_public/benchmark/")
source("functions.r")
load("distributions/distributions.Rdata")
setwd("~/devel/adeba_public/prior-comparison")
truth <- Map(ddist, distributions[1:15], grids[1:15])

ise_estimates <- function(distribution_index, sample_size){
    x <- rdist(distributions[[distribution_index]], sample_size)
    g <- grids[[distribution_index]]
    t <- truth[[distribution_index]]
  
    # Uniform prior and fixed beta
    f <- adeba(x, beta=.5, log_prior=uniform_log_prior)
    ise_simpleuniform <- ise_univariate(predict(f, g), t, g)

    # Jeffreys prior on only alpha, with fixed beta
    f <- adeba(x, beta=.5, log_prior=jeffreys_log_prior)
    ise_simplejeffreys <- ise_univariate(predict(f, g), t, g)

    # Uniform prior and free beta
    f <- adeba(x, beta=0:8/2, log_prior=uniform_log_prior)
    ise_uniform <- ise_univariate(predict(f, g), t, g)
     
    # empirical Bayes through Gaussian assumption
    f <- adeba(x, beta=0:8/2, log_prior=empirical_gaussian_log_prior)
    ise_empiricalgaussian <- ise_univariate(predict(f, g), t, g)
    
    c(
        ise_simpleuniform,
        ise_simplejeffreys,
        ise_uniform,
        ise_empiricalgaussian
    )
}


#--------------------------------------------------------------------[ Compute ]

library(futile.logger)
library(parallel)
library(doParallel)
registerDoParallel(detectCores())


if(file.exists(results_file)){
  load(results_file)
} else {
  ise <- structure(
    vector("list", length(distribution_indexes) * length(sample_sizes)),
    dim = c(length(distribution_indexes), length(sample_sizes)),
    dimnames = list(names(distributions)[distribution_indexes],
                    sprintf("n=%d", sample_sizes))
  )
}

i <- 2
j <- 18
set.seed(1124371)
for(i in seq_along(distribution_indexes)){
    distribution_index <- distribution_indexes[i]
    flog.info("working on Marron-Wand #%d (%d of %d)", distribution_index, i,
              length(distribution_indexes))
    for(j in seq_along(sample_sizes)){
        sample_size <- sample_sizes[j]
        flog.info("n = %d (%d of %d)", sample_size, j, length(sample_sizes))
        if(is.null(ise[[i, j]])){
          res <- foreach(r = seq_len(n_replicates), .combine = "rbind", .inorder=TRUE) %dopar% {
              ise_estimates(distribution_index, sample_size)
          }
          colnames(res) <- priors
          ise[[i, j]] <- data.frame(
            distribution = names(distributions)[distribution_index],
            sample_size = sample_size,
            res
          )
          save(ise, file=results_file)
        }
    }
}
