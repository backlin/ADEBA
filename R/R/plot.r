#' Plot density estimate
#' 
#' @param x Density estimate.
#' @param y Ignored, kept for S3 consistency.
#' @param type What type of plot to draw.
#' @param ... Sent to \code{\link{plot}}, \code{\link{image}} or some other
#'   base plotting function, depending on \code{type} and dimension of the
#'   estimate.
#' @param add Whether to start a new plot (\code{FALSE}) or add to an existing
#'   (\code{TRUE}).
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
plot.adeba <- function(x, y, type=c("estimate", "data", "both"), ..., add=FALSE){
    if(!is.rendered(x)){
        if(dimension(x) > 2)
            stop("Estimates of dimension 3 or higher must be manually sliced before plotting.")
        x <- render(x)
    }
    slice <- which(sapply(x$grid, length) > 1)
    if(length(slice) > 2)
        stop("Only slices of dimension 1 or 2 can be plotted")
    type <- match.arg(type)
    if(type %in% c("estimate", "both")){
        if(length(slice) == 1){
            if(add) lines(x$grid[[1]], x$posterior, ...)
            else plot(x$grid[[1]], x$posterior, type="l", ...)
        } else {
            image(x$grid[[1]], x$grid[[2]], x$posterior, ...)
        }
    }
    add <- add || type %in% "both"
    if(type %in% c("data", "both")){
        data <- x$retransform(x$data)
        if(length(slice) == 1){
            if(add) rug(data[,slice], ...)
            else plot(data[,slice], ...)
        } else {
            if(add) points(data[,slice[1]], data[,slice[2]], ...)
            else plot(data[,slice[1]], data[,slice[2]], ...)
        }
    }
}

#' @rdname plot.adeba
#' @export
lines.adeba <- function(x, ...){
    if(!is.rendered(x)){
        if(dimension(x) > 2)
            stop("Estimates of dimension 3 or higher must be manually sliced before plotting.")
        x <- render(x)
    }
    lines(x = x$grid[[1]], y = x$posterior, ...)
}

#' @rdname plot.adeba
#' @export
points.adeba <- function(x, ...){
    if(!is.rendered(x)){
        if(dimension(x) > 2)
            stop("Estimates of dimension 3 or higher must be manually sliced before plotting.")
        x <- render(x)
    }
    data <- x$retransform(x$data)
    if(dimension(x) == 1){
        points(approx(x$grid[[1]], x$posterior, data), ...)
    } else if(dimension(x) == 2){
        points(x = data[,1], y = data[,2], ...)
    } else {
        stop("Only estimates of dimension 1 and 2 can be plotted with `points`.")
    }
}

#' Plot the parameters and posterior
#' 
#' \code{plot_beta} plots the total posterior mass for each value of the beta
#' parameter (integrated over alpha). \code{plot_alpha} plots the normalized
#' posterior of alpha given different beta values. The normalization adjusts the
#' posterior based on how densely the alpha values were sampled for each beta
#' value. This is a requirement for the integral approximation to hold.
#' 
#' @param x ADEBA estimate.
#' @param ... Ignored, kept for S3 consistency.
#' @importFrom ggplot2 ggplot aes_string geom_line
#' @export
plot_alpha <- function(x, ...){
    ggplot(x$parameters, aes_string(x = "alpha", y = "posterior", colour = "factor(beta)")) +
        geom_line()
}
#' @rdname plot_alpha
#' @importFrom ggplot2 geom_bar
#' @export
plot_beta <- function(x, ...){
    p <- do.call(rbind, lapply(split(x$parameters, x$parameters$beta), function(x){
        data.frame(beta = x$beta[1], posterior = sum(x$posterior))
    }))
    ggplot(p, aes_string(x = "beta", y = "posterior")) + geom_bar(stat="identity")
}


#' Plot adeba estimate in 3d
#'
#' Using OpenGL. Re-exported from the \code{rgl} package, so please see
#' \code{\link[rgl]{plot3d}} for details.
#'
#' @name plot3d
#' @rdname plot3d
#' @param x Object to plot.
#' @param ... Sent to \code{\link[rgl]{plot3d}} of the \code{rgl} package.
#' @importFrom rgl plot3d
#' @export
#' @usage plot3d(x, ...)
NULL

#' @importFrom rgl open3d surface3d decorate3d
#' @rdname plot.adeba
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

#' @rdname plot.adeba
#' @export
contour.adeba <- function(x, ...){
    if(!is.rendered(x)){
        if(dimension(x) > 2)
            stop("Estimates of dimension 3 or higher must be manually sliced before plotting.")
        x <- render(x)
    }
    contour(x$grid[[1]], x$grid[[2]], x$posterior, ...)
}

