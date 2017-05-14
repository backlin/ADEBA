#' Image blending
#' 
#' @param a png-array, as produced by \code{\link[png]{readPNG}}.
#' @param b png-array.
#' @param mode Blend mode.
#' @return A blended png-array.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso mono2png
#' @importFrom png readPNG
#' @export
blend <- function(a, b, mode=c("screen", "multiply")){
    switch(match.arg(mode),
        screen = 1-(1-a)*(1-b),
        multiply = a*b)
}

#' Convert monochrome matrix to png array
#' 
#' @param x Monochrome matrix (numeric with all elements in [0,1]).
#' @param pal Color palette.
#' @param alpha Whether to also use the monochrome information as alpha channel.
#' @return An RGB or RGBA-array.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso blend, mono2hex
#' @importFrom abind abind
#' @importFrom grDevices colorRampPalette col2rgb
#' @export
mono2png <- function(x, pal=c("#ff9933", "#ffff66"), alpha=TRUE){
    pal <- colorRampPalette(pal)(100)
    xcol <- pal[matrix(findInterval(x, seq(0, 1+1e-12, length.out = length(pal)+1)), nrow(x))]
    xcol <- array(t(col2rgb(xcol)/255), c(dim(x), 3))
    if(alpha) abind(xcol, x, along=3) else xcol
}

#' Convert monochrome matrix to hex color matrix
#' 
#' @param x Monochrome matrix (numeric with all elements in [0,1]).
#' @param pal Color palette.
#' @return A character matrix of hexadecimal color codes.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso mono2png
#' @export
mono2hex <- function(x, pal=c("#559988", "#ff9933")){
    matrix(colorRampPalette(pal)(1000)[
        findInterval(x, seq(min(x), max(x)+1e-12, length.out=1001))
    ], nrow(x), ncol(x))
}
