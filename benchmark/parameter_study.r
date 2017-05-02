#===============================================================================
#   Setup
#-------------------------------------------------------------------------------

# Dependencies
if(!require(adeba)){
    stop("You must install adeba!")
}
# !!!
source.all("../R/R")
library(pdist)
# ^^^

source("functions.r")
require(data.table)
require(parallel)
dir.create("partial/parameters", recursive=TRUE, showWarnings=FALSE)
dir.create("plot", showWarnings=FALSE)
load("distributions/distributions.Rdata")
distributions <- distributions[1:15]


evaluate_estimators <- function(x, truth, g){
    square.root.law <- adeba(x, beta=.5, parallel=FALSE)
    thumbrule.bw <- exp(mean(log(square.root.law$pilot)))
    silverman1 <- adeba(x, adaptive=TRUE, beta=.5, alpha=thumbrule.bw, parallel=FALSE)
    silverman2 <- adeba(x, adaptive=TRUE, beta=.5, alpha=thumbrule.bw^.5, parallel=FALSE)
    fb <- adeba(x, adaptive=FALSE, beta = seq(0, 2, length.out=5), parallel=FALSE)
    ab <- iterate(fb)
    cat(".")

    list(best.srl.alpha = with(square.root.law$parameters, alpha[which.max(posterior)]),
         silverman.alpha = silverman$parameters$alpha,
         ise.srl = ise_univariate(predict(square.root.law, g), truth, g),
         ise.silverman1 = ise_univariate(predict(silverman1, g), truth, g),
         ise.silverman2 = ise_univariate(predict(silverman2, g), truth, g),
         ise.fb = ise_univariate(predict(fb, g), truth, g),
         ise.ab = ise_univariate(predict(ab, g), truth, g),
         ab.parameters = ab$parameters)
}


#===============================================================================
#   Run
#-------------------------------------------------------------------------------

if(Sys.info()["nodename"] == "backlin-laptop"){
    n.cores <- 2
} else {
    n.cores <- 16
}
options(mc.cores = n.cores)

sample.sizes <- c(50, 100, 200, 400)
n.replicates <- 100
for(ni in seq_along(sample.sizes)){
    n <- sample.sizes[ni]
    for(dist.name in names(distributions)){
        out.file <- sprintf("partial/parameters/%s n%i.Rdata", dist.name, n)
        if(!file.exists(out.file)){
            trace_msg("%s, n=%i ", dist.name, n)
            d <- distributions[[dist.name]]
            g <- grids[[dist.name]]
            truth <- ddist(d, g)

            xx <- rdist(d, n, replicates=n.replicates)
            xx <- as.data.frame(xx[,1,])

            # Execute
            res <- mclapply(xx, evaluate_estimators, truth, g)
            save(res, file=out.file)
            cat("done!\n")
        }
    }
}
system("scancelsleep")
stop()

