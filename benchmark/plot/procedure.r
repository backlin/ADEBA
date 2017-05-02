pdf(
  "plot/fig1_procedure.pdf",
  width = 9/cm(1), height = 9/cm(1),
  family = "ArialMT",
  fonts = "ArialMT"
)

my_grey <- "grey77"

my_axis <- function(..., lwd=par("lwd")) emil:::nice_axis(..., lwd=lwd, col=my_grey, col.axis=my_grey)
horizontal_ylab <- function(label) mtext(label, 2, 1, at = mean(par("usr")[3:4]), las = 1)
poly.plot <- function(x, y, line=TRUE, ..., xlab="x", ylab, axes=TRUE){
    plot(0, 0, type="n", xlim=c(-4, 4), ylim=c(0, .4), ..., axes=FALSE, xlab="", ylab="")
    mtext(xlab, 1, .5)
    horizontal_ylab(ylab)
    if(axes){
        my_axis(2, at = c(0, .4))
        my_axis(1, at = c(-4, 4), mgp=par("mgp") * c(1, 0, 1))
    }
    polygon(c(head(x, 1), x, tail(x, 1)), c(0, y, 0), col="grey90", border=NA)
    if(line) lines(x, y)
}
library(adeba)
set.seed(17)
d <- distributions[[8]]
x <- rdist(d, 30)
g <- grids[[8]]
layout(cbind(1:3,4:6))
par(ps=7, cex=1, las=1, mar=c(2,3.5,1.4,.2), mgp=c(1.5,.5,0), tcl=-.15, font.main=1)

# # Truth
# poly.plot(g, ddist(d, g), main="True PDF", ylab=expression(p(x)))
# rug(x)

# Pilot 1
poly.plot(c(-5, -4, -4, 4, 4, 5), rep(c(0, 1/8, 0), c(2, 2, 2)),
    main="First pilot", ylab=expression(hat(p)["0,1"](x)))
points(x, rep(1/8, length(x)), cex=.4)
rug(x)
curve(ddist(d, x), lty=2, add=TRUE)

# Parameters
alpha <- seq(0, 1, length.out=42)[-1] # alpha = 0 is invalid
beta <- seq(0, 2, length.out=41)
fb <- adeba(x, adaptive=FALSE, alpha=alpha*3, beta=beta)
with(fb$parameters, {
    plot(alpha, posterior, type="n", main="Parameter posterior",
         xlab="", ylab="", axes=FALSE)
    horizontal_ylab(expression(p(alpha~"|"~D)))
    mtext(expression(alpha), 1, .5)
    segments(alpha, 0, alpha, posterior, lend=1, lwd=1)
    my_axis(2, at = c(0, .08))
    my_axis(1, at = c(0, 3), mgp=par("mgp")*c(1,0,1))
})

# Fixed estimate
poly.plot(g, predict(fb, g), main="DEBA estimate",
          ylab=expression(hat(p)[1](x~"|"~D)))
curve(ddist(d, x), lty=2, add=TRUE)
rug(x)

# Pilot 2
poly.plot(g, predict(fb, g), main="Second pilot",
          ylab=expression(hat(p)["0,2"](x)))
points(x, predict(fb), cex=.4)
rug(x)
curve(ddist(d, x), lty=2, add=TRUE)

# Parameters
fb$alpha <- alpha
ab <- iterate(fb)
z <- ab$parameters %>% spread(beta, posterior) %>% select(-alpha) %>% as.matrix
image(alpha, beta, z, xlab="", ylab="",
      col=grey(100:0/100), bty="n", axes=FALSE, main="Parameter posterior",
      useRaster=TRUE)
horizontal_ylab(expression(beta))
my_axis(1, at = par("usr")[1:2], labels = 0:1, mgp=par("mgp")*c(1,0,1))
my_axis(2, at = c(0, 2))
emil:::nice_box(col=my_grey)
mtext(expression(alpha), 1, .5)

# Adaptive estimate
poly.plot(g, predict(ab, g), main="ADEBA-1 estimate",
          xlab="x", ylab=expression(hat(p)[2](x~"|"~D)))
curve(ddist(d, x), lty=2, add=TRUE)
rug(x)


dev.off()