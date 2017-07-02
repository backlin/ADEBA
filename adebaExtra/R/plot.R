#' Plot the parameters and posterior
#' 
#' \code{plot_beta} plots the total posterior mass for each value of the beta
#' parameter (integrated over alpha). \code{plot_alpha} plots the normalized
#' posterior of alpha given different beta values. The normalization adjusts the
#' posterior based on how densely the alpha values were sampled for each beta
#' value. This is a requirement for the integral approximation to hold.
#' 
#' @param object ADEBA estimate.
#' @param ... Ignored, kept for S3 consistency.
#' @examples
#' x <- rnorm(50)
#' f <- adeba(x)
#' plot_alpha(f)
#' plot_beta(f)
#' @importFrom ggplot2 ggplot aes_string geom_line
#' @export
plot_alpha <- function(object, ...){
    ggplot(object$parameters, aes_string(x = "alpha", y = "posterior", colour = "factor(beta)")) +
        geom_line()
}

#' @rdname plot_alpha
#' @importFrom ggplot2 geom_bar
#' @export
plot_beta <- function(object, ...){
    p <- do.call(rbind, lapply(split(object$parameters, object$parameters$beta), function(x){
        data.frame(beta = x$beta[1], posterior = sum(x$posterior))
    }))
    ggplot(p, aes_string(x = "beta", y = "posterior")) + geom_bar(stat="identity")
}

#' Plot adeba estimate in 3d
#'
#' Using OpenGL. Re-exported from the \code{rgl} package, so please see
#' \code{\link[rgl]{plot3d}} for details.
#'
#' @method plot3d adeba
#' @param x Object to plot.
#' @param ... Sent to \code{\link[rgl]{plot3d}} of the \code{rgl} package.
#' @examples
#' x.train <- 3 + sweep(matrix(rnorm(60), 30), 2, 1:2, "*") %*% matrix(c(1, .4, .4, 1), 2)
#' f <- adeba(x.train, adaptive=FALSE)
#' 
#' library(rgl)
#' plot3d(f)
#' @importFrom adeba dimension is.rendered render
#' @importFrom rgl plot3d open3d surface3d decorate3d
#' @rdname plot3d
#' @export
plot3d.adeba <- function(x, ...){
    if(!is.rendered(x)){
        if(dimension(x) > 3)
            stop("Estimates of dimension 4 or higher must be manually sliced before plotting.")
        if(dimension(x) < 2)
            stop("Univariate estimates cannot be plotted with `plot3d.adeba`.")
        x <- render(x)
    }
    open3d()
    if(dimension(x) == 2){
        with(x, surface3d(grid[[1]], grid[[2]], 100*posterior, col="skyblue3"))
        decorate3d()
    } else if(dimension(x) == 3){
        stop("Cloud plots are not yet included in the package.")
        #requireNamespace(png)
        # This was done manually in the original paper
    }
}


