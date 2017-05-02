source("bw_liao.r")

#' Calculate integrated square error
#' 
#' @param estimate Density function estimate, supplied as a numeric vector
#'   containing the function value at the points defined by \code{x}.
#' @param truth True density function, supplied as a numeric vector similar to
#'   \code{estimate}.
#' @param x The points on the x-axis where the functions were evaluated.
#' @return The ISE as a scalar number.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
ise_univariate <- function(estimate, truth, x){
    f <- estimate - truth

    # Find intersection points
    dx <- diff(x)
    m <- f[-length(x)]
    k <- diff(f)
    intrsc <- m*(m+k) < 0
    a <- (-m/k*dx + x[-length(x)])[intrsc]

    # Append intersection points
    x <- c(x,a)
    f <- c(f, rep(0, length(a)))[order(x)]
    x <- sort(x)

    # Sum up
    dx <- diff(x)
    m <- f[-length(x)]
    k <- diff(f)
    sum(dx*(m^2+m*k+k^2/3))
}

#' Prints a nicely formatted log message
#' 
#' @param ... Sent to \code{\link{sprintf}}.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
trace_msg <- function(...)
    message(format(Sys.time(), "%d %b %H:%M  "),
            sprintf(...))


#' Different density estimators
#' 
#' @param data Sample of data to estimate density from.
#' @param x Grid points in which to evaluate the estimate.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @alias estimators
#' @export
sj <- function(data, x){
    bw <- density(data, bw="SJ", n=0)$bw
    apply(matrix(dnorm(rep(x, each=length(data)), mean=data, sd=bw),
                 length(data), length(x)),
          2, mean)
}
#' @rdname estimators
liao <- function(data, x){
    bw <- bw.liao(as.vector(data))
    bw <- if(length(data) <= 400) bw$h2 else bw$h3
    apply(matrix(dnorm(rep(x, each=length(data)), mean=data, sd=bw),
                 length(data), length(x)),
          2, mean)
}
#' @param parallel Whether to execute in parallel.
#' @param ... Sent to \code{\link{adeba}}.
#' @rdname estimators
bayes <- function(data, x, parallel=FALSE, ...){
    f <- adeba(data, adaptive=FALSE, beta=1, parallel=parallel, ...)
    y <- predict(f, as.matrix(x))
    f <- iterate(f)
    list(fb = y, ab = predict(f, x))
}
#' @param k Number of Gaussians.
#' @param nSuccess Number of successful estimates to require.
#' @param nRestart Number of times you are allowed to fail and restard before
#'   considering the estimation as a failure.
#' @param ... Sent to \code{\link{mvnormalmixEM}}.
#' @param .verbose Whether to print a log on how the estimation progresses.
#' @rdname estimators
gmm <- function(data, x, k, nSuccess=3, nRestart=5, ..., .verbose){
    if(missing(.verbose)) .verbose <- getOption("mc.cores", 1) == 1
    if(.verbose){
        muffler <- identity
        cat(sprintf("GMM with n=%i and k=%i\n", nrow(data), k))
    } else {
        muffler <- capture.output
        cat <- function(...) invisible()
    }
    # If the number of observations in the dataset is smaller than twice
    # the number of GMM-parameters, we won't even try to fit a model.
    p <- ncol(data)
    if(nrow(data) < 2*k*(p + (p^2-p)/2 + p)){
        cat("Too little data\n")
        return(NA)
    }
    best <- list(loglik=-Inf)
    restarts <- 0
    successes <- 0
    while(successes < nSuccess && restarts < nRestart){
        tryCatch({
            muffler(f <- mvnormalmixEM(data, k=k, ..., verb=.verbose > 1))
            successes <- successes + 1
            if(f$loglik > best$loglik) best <- f
            cat("Success", successes, "\n")
        }, error = function(err){
            if(err$message == "reached elapsed time limit"){
                stop(err)
            } else {
                restarts <<- restarts + 1
                cat("Restart", restarts, "\n")
            }
        })
    }
    if(successes == 0){
        NA
    } else {
        Reduce("+", Map(function(m, s, w) w * dmvnorm(x, mu=m, sigma=s),
                        best$mu, best$sigma, best$lambda))
    }
}


#' Gather ragged column into key-value pairs
#' 
#' @param data Data frame.
#' @param column Column containing named vectors.
#' @param key Name of key column to be created.
#' @param value Name of value column to be created.
#' @example
#' make.vector <- function(length.out){
#'     x <- sample(9, length.out)
#'     names(x) <- switch(length.out,
#'         "Alice",
#'         c("Bob", "Charlie"),
#'         c("Dave", "Erin", "Frank"),
#'         c("Gwen", "Harold", "Inez", "James"))
#'     x
#' }
#' mydf <- data.frame(Game = gl(3, 3, labels=LETTERS[1:3]),
#'                    Set = rep(1:3, 3),
#'                    Score = I(lapply(rep(2:4, each=3), make.vector)))
#' gather_ragged(mydf, "Score", "Player", "Score")
#' @seealso gather
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
gather_ragged <- function(data, column, key, value){
    if(is.character(column)){
        i <- which(colnames(data) %in% column)
    } else if(is.logical(column)){
        i <- which(column)
    } else {
        i <- column
    }
    stopifnot(is.numeric(i), length(i) == 1)
    if(is.data.table(data)){
        newcols <- data.table(
            akey = unlist(lapply(data[[column]], names)), # `key` is a reserved word
            value = unlist(data[[column]])
        )
        setnames(newcols, c("akey", "value"), c(key, value))
        cbind(
            data[rep(1:nrow(data), sapply(data[[column]], length)), -i, with=FALSE],
            newcols
        )
    } else if(is.data.frame(data)){
        newcols <- data.frame(
            key = unlist(lapply(data[[column]], names)),
            value = unlist(data[[column]])
        )
        names(newcols) <- c(key, value)
        cbind(
            data[rep(1:nrow(data), sapply(data[[column]], length)), -i],
            newcols
        )
    } else {
        stop("Invalid data.")
    }
}

