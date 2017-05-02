#===============================================================================
#   Setup
#-------------------------------------------------------------------------------

# Dependencies
if(!require(adeba)){
    stop("You must install adeba!")
}
if(!require(mixtools)){
    install.packages("mixtools")
    require(mixtools)
}
source("functions.r")
require(data.table)
require(parallel)

#' @param ... Sent to \code{\link{gmm}}
evaluate_estimators <- function(data, truth, x, time.limit=3600, ...){
    if(is.vector(data) || ncol(data) == 1){
        estimators <- list(AB = bayes, SJ = sj, Liao = liao)
        ise.fun <- function(estimate) ise_univariate(estimate, truth, x)
    } else {
        estimators <- list(AB = bayes, GMM2 = function(...) gmm(..., k=2),
            GMM3 = function(...) gmm(..., k=3), GMM5  = function(...) gmm(..., k=5),
            GMM8 = function(...) gmm(..., k=8))
        dx <- prod(sapply(x, function(x) x[2]-x[1]))
        x <- as.matrix(do.call(expand.grid, x))
        ise.fun <- function(estimate) sum(abs(estimate - truth)*dx)
    }

    n.rep <- dim(data)[3]
    duration <- matrix(NA, n.rep, length(estimators),
                        dimnames=list(NULL, names(estimators)))
    ise <- matrix(NA, n.rep, length(estimators)+1,
                  dimnames=list(NULL, c("FB", "AB", names(estimators)[-1])))
    for(est in names(estimators)){
        try({
            setTimeLimit(elapsed = time.limit, transient = TRUE)
            for(i in 1:n.rep){
                st <- system.time({
                    estimate <- estimators[[est]](data[,,i], x)
                })
                duration[i, est] <- summary(st)["user"]
                if(!identical(estimate, NA)){
                    if(est == "AB") ise[i, 1:2] <- sapply(estimate, ise.fun)
                    else ise[i, est] <- ise.fun(estimate)
                }
            }
        }, silent=TRUE)
        setTimeLimit(elapsed = Inf, transient=TRUE)
    }
    list(duration = duration, ise = ise)
}

# Input/output
dir.create("partial/univariate", recursive=TRUE, showWarnings=FALSE)
dir.create("partial/multivariate", showWarnings=FALSE)
dir.create("plot", showWarnings=FALSE)
load("distributions/distributions.Rdata")

sample.sizes <- as.integer(25*2^(0:6))
n.replicates <- 100
runs <- as.data.table(expand.grid(
    Distribution = names(distributions),
    n = sample.sizes
))
runs[, Multivariate := grepl("[23]d", Distribution)]
runs[, Filename := sprintf("partial/%s/%s_n%i.Rdata", 
    ifelse(Multivariate, "multivariate", "univariate"),
    Distribution, n)]
runs[, Seed := 1:nrow(runs)]


#===============================================================================
#   Run
#-------------------------------------------------------------------------------

if(Sys.info()["nodename"] == "backlin-laptop"){
    n.cores <- 2
    time.limit <- 120
} else {
    n.cores <- 16
    time.limit <- 3600
}
options(mc.cores = n.cores)

runs[!file.exists(Filename), mcmapply(function(dn, n, f, s){
    tryCatch({
        d <- distributions[[dn]]
        g <- grids[[dn]]
        set.seed(s)
        my_data <- rdist(d, n, n.replicates)
        part <- evaluate_estimators(
            data = my_data,
            truth = ddist(d, g),
            x = g
        )
        save(part, dn, n, file=f)
        cat(sprintf("%s: Completed.\n", f))
    }, error = function(err){
        save(err, file=f)
        cat(sprintf("%s: %s\n", f, err))
    })
    return(TRUE)
}, as.character(Distribution), n, Filename, Seed)]


#===============================================================================
#   Assemble results
#-------------------------------------------------------------------------------

library(dplyr)
runs[, Completed := file.exists(Filename)]
table(runs$Completed)
runs[Completed == FALSE]

univariate <- lapply(dir("partial/univariate", full.names = TRUE), function(f){
    load(f)
    cbind(
        data.table(File = f) %>%
            extract(File, c("Distribution", "n"),
                "partial/univariate/(.*)_n(\\d+)\\.Rdata") %>%
            mutate(
                Distribution = factor(Distribution,
                    runs[Multivariate == FALSE, levels(factor(Distribution))]),
                n = as.integer(n)),
        merge(by = c("Replicate", "Method"), all=TRUE,
            part$ise %>%
                tbl_dt %>%
                mutate(Replicate = seq_along(AB)) %>%
                gather(Method, ISE, FB:Liao),
            part$duration %>%
                tbl_dt %>%
                mutate(Replicate = seq_along(AB)) %>%
                gather(Method, Duration, AB:Liao)
        )
    )
}) %>% bind_rows
multivariate <- lapply(dir("partial/multivariate", full.names = TRUE), function(f){
    cat(f, "\n")
    load(f)
    colnames(part$ise) <- c("FB", "AB", sprintf("GMM%i", c(2,3,5,8)))
    colnames(part$duration) <- c("AB", sprintf("GMM%i", c(2,3,5,8)))
    cbind(
        data.table(File = f) %>%
            extract(File, c("Distribution", "n"),
                "partial/multivariate/(.*)_n(\\d+)\\.Rdata") %>%
            mutate(
                Distribution = factor(Distribution,
                    runs[Multivariate == TRUE, levels(factor(Distribution))]),
                n = as.integer(n)),
        merge(by = c("Replicate", "Method"), all=TRUE,
            part$ise %>%
                tbl_dt %>%
                mutate(Replicate = seq_along(AB)) %>%
                gather(Method, ISE, FB:GMM8),
            part$duration %>%
                tbl_dt %>%
                mutate(Replicate = seq_along(AB)) %>%
                gather(Method, Duration, AB:GMM8)
        )
    )
}) %>% bind_rows

save(runs, sample.sizes, n.replicates, univariate, multivariate,
     file = "benchmark.Rdata")

