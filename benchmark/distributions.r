require(data.table)

trips <- cbind(x=sin(2/3*1:3*pi), y=cos(2/3*1:3*pi), z=0)
theta <- c(1.9, 0.5, 2.1)
R <- list(cbind(x = c(1,0,0),
                y = c(0, cos(theta[1]), -sin(theta[1])),
                z = c(0, sin(theta[1]), cos(theta[1]))),
          cbind(x = c(cos(theta[2]), 0, sin(theta[2])), 
                y = c(0,1,0),
                z = c(-sin(theta[2]), 0, cos(theta[2]))),
          cbind(x = c(cos(theta[3]), -sin(theta[3]), 0),
                y = c(sin(theta[3]), cos(theta[3]), 0),
                z = c(0,0,1)))
blob <- list(
    rbind(c(94, 253), c(-17, -214), c(-58, 104), c(133, -12),
          c(-197, 204), c(-93, 6), c(68, 76), c(-136, -3)),
    rbind(c(53, 357, -13), c(183, 276, 148), c(-226, -135, -140), c(86, 303, 141),
          c(31, 72, 67), c(-131, -7, -121), c(-44, 71, 71), c(34, -21, 163))
)
colnames(blob[[1]]) <- c("x", "y")
colnames(blob[[2]]) <- c("x", "y", "z")

multivariate <- list(
    `Trips 2d` = list(m = trips[,1:2], s = rep(.3, 3)),
    `Trips 3d` = list(m = trips %*% Reduce("%*%", R), s = rep(.3, 3)),
    `Blob 2d` = list(m = scale(blob[[1]]), s = rep(.5, 8)),
    `Blob 3d` = list(m = scale(blob[[2]]), s = c(.9, .76, 1.32, 1.31, .54, .97, .84, 1.63))
)

distributions <- list(
    data.table(Name="Gaussian", s=1, w=1, x=0),
    data.table(Name="Skewed", s=c(1, 2/3, 5/9), w=c(1, 1, 3)/5, x=c(0, 1/2, 13/12)),
    data.table(Name="Strongly skewed", s=(2/3)^seq(0:7), w=rep(1/8, 8), x=3*((2/3)^seq(0,7) - 1)),
    data.table(Name="Kurtotic", s=c(1, 1/10), w=c(2, 1)/3, x=c(0, 0)),
    data.table(Name="Outliers", s=c(1, 1/10), w=c(1, 9)/10, x=c(0, 0)),

    data.table(Name="Bimodal", s=rep(2/3, 2), w=c(.5, .5), x=c(-1, 1)),
    data.table(Name="Separated", s=c(.5, .5), w=c(.5, .5), x=c(-1.5, 1.5)),
    data.table(Name="Asm. Bimodal", s=c(1, 1/3), w=c(.75, .25), x=c(0, 1.5)),
    data.table(Name="Trimodal", s=c(.6, .6, .25), w=c(9, 9, 2)/20, x=c(-1.2, 1.2, 0)),
    data.table(Name="Claw", s=c(1, rep(.1, 5)), w=c(.5, rep(.1, 5)), x=c(0, seq(0,4)/2 - 1)),

    data.table(Name="Double Claw", s=c(2/3, 2/3, rep(.01, 7)), w=c(.49, .49, rep(1/350, 7)), x=c(-1, 1, seq(-1.5, 1.5, .5))),
    data.table(Name="Asymmetric Claw", s=c(1, 2^seq(2, -2)/10), w=c(1/2, 2^seq(3,-1)/31), x=c(0, seq(-1.5, 2.5))),
    data.table(Name="Asm. Dbl. Claw", s=c(2/3, 2/3, rep(.01,3), rep(.07,3)),
        w=c(.46, .46, rep(1/300,3), rep(7/300,3)), x=c(-1, 1, -1/2, -1, -3/2, 1/2, 1, 3/2)),
    data.table(Name="Smooth Comb", s=(32/63)/2^seq(0,5), w=2^seq(5,0)/63, x=c(65-96*.5^seq(0,5))/21),
    data.table(Name="Discrete Comb", s=c(rep(2/7, 3), rep(1/21, 3)), w=c(rep(2/7, 3), rep(1/21, 3)), x=c(12*seq(0,2)-15, 2*seq(8,10))/7),

    data.table(Name="Trips 2d", s = .3, w=1/3, trips[,1:2]),
    data.table(Name="Trips 3d", s = .3, w=1/3, trips %*% Reduce("%*%", R)),
    data.table(Name="Blob 2d", s = .5, w=1/8, scale(blob[[1]])),
    data.table(Name="Blob 3d", s = c(.9, .76, 1.32, 1.31, .54, .97, .84, 1.63), w=1/8, scale(blob[[2]])),
    {
        x <- seq(-.5, .5, length.out=100)
        data.table(Name="Bend 2d", s=(x^2+.2)/3, w=1/length(x), x=x, y=6*cos(x)-5.6)
    },
    data.table(Name="Bend 3d", s=(x^2+.2)/3, w=.01, x=x, y=6*cos(x)-5.6, z=x^2),
    {
        x <- seq(-.75, .75, length.out=100)
        data.table(Name="Wave 2d", s=.4, w=1/length(x), x = ifelse(x < 0, cos(pi*(x-.25))-.5*sqrt(2), cos(pi*x)+.5*sqrt(2)),
                          y = ifelse(x < 0, sin(pi*(x-.25))+.5*sqrt(2), sin(pi*x)-.5*sqrt(2)))
    },
    {
        x <- seq(0, 2, length.out=100)
        data.table(Name="Spiral 3d", s=.3, w=1/length(x), x=x, y=sin(pi*x), z=cos(pi*x))
    },
    {
        x <- seq(0, 1, length.out=25)
        i <- rep(1:4, each=length(x))
        m <- rbind(c(1,1,1), c(-1,-1,1), c(-1,1,-1), c(1,-1,-1))
        colnames(m) <- c("x", "y", "z")
        data.table(Name="Tetra 3d", s=1-.8*x, w=(1-.8*x)/4/sum(1-.8*x), sweep(m[i,], 1, x, "*"))
    }
)
names(distributions) <- sapply(distributions, function(d) d$Name[1])

ddist <- function(d, x){
    if(is.list(x) && !is.data.frame(x)){
        dimx <- sapply(x, length)
        x <- do.call(expand.grid, x)
    } else {
        dimx <- NULL
        x <- as.data.frame(x)
    }

    f <- Reduce("+", do.call(Map, c(list(function(Name, s, w, ...){
        m <- c(...)
        w*apply(mapply(function(m, x) dnorm(x, mean=m, sd=s), m, x), 1, prod)
    }), as.data.frame(d))))

    if(is.null(dimx)) f else array(f, dimx)
}
rdist <- function(d, n, replicates=1){
    if("package:data.table" %in% search()){
        stop("Please load the data.table package to sample from distributions")
    }
    dims <- intersect(c("x", "y", "z"), names(d))
    i <- sample(nrow(d), replicates*n, replace=TRUE, prob=d$w)
    aperm(perm=c(2,3,1), array(
        rnorm(replicates*n*length(dims), mean=unlist(d[i,dims,with=FALSE]), sd=d[i,s]),
        dim=c(replicates, n, length(dims)), dimnames=list(Replicates=NULL, Examples=NULL, Dimensions=NULL)
    ))
}

xi <- seq(-3, 3, length.out=1000)
grids <- lapply(distributions, function(d){
    if("y" %in% names(d)){
        fun <- function(x, res) seq(min(x-3*d$s), max(x+3*d$s), length.out=res)
        if("z" %in% names(d)){
            lapply(d[,list(x,y,z)], fun, 20)
        } else {
            lapply(d[,list(x,y)], fun, 80)
        }
    } else {
        nn <- pmax(11, ceiling(d$w*400/2)*2+1)
        g <- sort(unique(unlist(
            Map(function(m, s, n) seq(m-4*s, m+4*s, length.out=n), d$x, d$s, nn)
        )))
        gtail <- seq(0, .2*diff(range(g)), length=51)[-1]
        unique(c(head(g, 1) - rev(gtail), g, tail(g, 1) + gtail))
    }
})
# Plots were made with only 2s from each mean
save(distributions, ddist, rdist, grids, file="distributions/distributions.Rdata")


#-----------------------------------------------------------------------[ Plot ]

require(adeba) # `surf` and `cloud` should go here too eventually
res <- 201
major <- 0:20*10+1
pal <- c("#559988", "#ff9933", "#ffff99")

#' Open or reset rgl device
#' 
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @noRd
open.rgl <- function(windowRect = c(0,0,800,800)){
    require(rgl)
    if(rgl.cur() == 0){
        open3d(windowRect=windowRect)
    } else {
        clear3d("shapes")
    }
}

surf <- function(g, z, lwd=2, lwd.alpha=.2, alpha=.8, col, ...){
    open.rgl()
    persp3d(g$x, g$y, z, col=mono2hex(z),
            xlab="x", ylab="y", zlab="p(x,y)", box=TRUE, alpha=alpha, ...)
    ep <- diff(range(z))*1e-3
    if(lwd) for(i in major){
        lines3d(g$x[i], g$y, z[i,]+ep, lwd=lwd, alpha=lwd.alpha)
        lines3d(g$x, g$y[i], z[,i]+ep, lwd=lwd, alpha=lwd.alpha)
    }
}
cloud <- function(d, file, pre.alpha=.1){
    if(!require(png)){
        ans <- readline("The `png` package is required to make a cloud plot, would you like to install it? (Y/n)")
        if(tolower(ans) %in% c("y", "yes")){
            install.packages("png")
            require(png)
        } else {
            stop("Aborting")
        }
    }
    if(missing(file)) stop("You must specify a filename.")

    # Calculate the quantile radii
    r <- seq(0, 4, length.out=1e4)
    mr <- r[-1] - .5*diff(r)
    cum.vol <- cumsum(diff(4/3*pi*r^3) * (2*pi)^(-2/2)*dnorm(mr))
    np <- pmax(2, ceiling(500/nrow(d)))
    p <- approx(cum.vol, mr, 1:np/(np+1))

    # Expand the distribution object and plot sloppily
    dd <- d[,list(x=x, y=y, z=z, r=p$y*s, w=w), by=1:nrow(d)]
    open.rgl()
    with(dd, plot3d(x, y, z, type="s", radius=r, box=TRUE, alpha=pmin(1, pre.alpha*w), lit=FALSE, antialias=FALSE))
    with(d, points3d(x, y, z))
    readline("Rotate the image as you like it and press enter to render")
    b <- par3d("bbox")

    # Draw the box
    with(dd, plot3d(x, y, z, type="s", radius=r, lit=FALSE, alpha=0,
                       box=FALSE, xlim=b[1:2], ylim=b[3:4], zlim=b[5:6],
                       xlab="", ylab="", zlab="", expand=1))
    rgl.snapshot(sprintf("%s/lines.png", tempdir()))
    rgl.bbox(color=c(pal[1], "white"), lit=FALSE)
    rgl.snapshot(sprintf("%s/fill.png", tempdir()))

    # Draw the centers
    with(d, plot3d(x, y, z, xlim=b[1:2], ylim=b[3:4], zlim=b[5:6],
                   xlab="", ylab="", zlab=""))
    rgl.bbox(color=c("white", "white"), lit=FALSE)
    rgl.snapshot(sprintf("%s/centers.png", tempdir()))

    # Draw the partial images
    file.remove(dir(tempdir(), "sphere", full.names=TRUE))

    cat("Writing partial images...\n")
    pb <- txtProgressBar(max=nrow(dd), style=3)
    for(i in 1:nrow(dd)){
        with(dd[i], plot3d(x, y, z, type="s", radius=r, lit=FALSE, col="red",
                           box=FALSE, xlim=b[1:2], ylim=b[3:4], zlim=b[5:6],
                           xlab="", ylab="", zlab=""))
        rgl.bbox(color=c("white", "white"), lit=FALSE)
        rgl.snapshot(sprintf("%s/sphere%06i.png", tmp.dir, i))
        pb$up(pb$getVal()+1)
    }
    pb$kill()

    # Collapse the partial volumes to one density image
    cat("Merging partial images...\n")
    pb <- txtProgressBar(max=nrow(dd), style=3)
    ff <- dir(tmp.dir, "^sphere.*png$", full.names=TRUE)
    pb$up(1)
    volume <- readPNG(ff[1])[,,2]
    for(f in ff[-1]){
        volume <- volume + readPNG(f)[,,2]
        pb$up(pb$getVal()+1)
    }
    pb$kill()
    volume <- (volume-min(volume))/diff(range(volume))
    writePNG(mono2png(1-volume), sprintf("%s/density.png", tempdir()))

    # Merge all images
    f <- c("lines", "fill", "density", "centers")
    img <- lapply(sprintf("%s/%s.png", tempdir(), f), readPNG)
    names(img) <- f
    img$bg <- blend(img$lines, img$fill, "multiply")
    img$fg <- blend(img$density[,,-4], img$bg, "screen")
    img$alpha <- img$density[,,4]
    writePNG(with(img, blend(
        img$centers,
        sweep(fg, 1:2, alpha, "*") + sweep(bg, 1:2, 1-alpha, "*"),
        "multiply"
    )), file)
}

for(n in grep("2d", names(distributions), value=TRUE)){
    d <- distributions[[n]]
    g <- grids[[n]]
    gg <- lapply(g, function(x) seq(head(x,1), tail(x, 1), length.out=res))
    z <- matrix(distfun(d, do.call(expand.grid, gg)), res, res)

    rgl.viewpoint(0, -45, zoom=.75)
    rgl.snapshot(sprintf("distributions/%s.png", n))
}
for(n in grep("3d", names(distributions), value=TRUE)){
    spheres(distributions[[n]], sprintf("distributions/%s.png", n))
}

