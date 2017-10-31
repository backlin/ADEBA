library(adeba)
library(ggplot2)
library(gridExtra)
library(treesmapstheorems)

source("lib.R")
source("../benchmark/plot_functions.r")
load("../benchmark/distributions/distributions.Rdata")


plot_fit_posterior <- function(post){
    post <- mutate(post, rel_err = (fit_posterior - posterior)/max(posterior))
    ggplot(post, aes(x=log(alpha), y=beta, fill=rel_err)) +
        geom_tile() +
        scale_fill_gradientn(colours = grey(0:100/100))
}

setups <- data.frame(
    distribution = c(1, 3, 12),
    n = 100,
    log_alpha_low = -7,
    log_alpha_high = 0,
    beta_low = 0,
    beta_high = 4
)


i <- 2
resolution <- 101
panels <- structure(vector("list", 2*nrow(setups)), dim=c(nrow(setups), 2))

left_data_list <- vector("list", nrow(setups))
right_data <- vector("list", nrow(setups))
set.seed(123)
for(i in 1:nrow(setups)){
    d <- distributions[[setups$distribution[i]]]
    x <- rdist(d, setups$n[i])
    f <- adeba(x, adaptive = FALSE)
    f$pilot <- predict(f)
    alpha <- exp(seq(setups$log_alpha_low[i], setups$log_alpha_high[i], length.out = resolution))
    beta <- seq(setups$beta_low[i], setups$beta_high[i], length.out = resolution)
    log_post <- lapply(beta, function(b) adeba:::get_posterior(object = f, alpha, b))
    norm_post <- adeba:::normalize_posterior(log_post)
    
    left_data_list[[i]] <- post_mean(norm_post)
    left_data_list[[i]]$Distribution <- d$Name[1]
}

left_data <- do.call(rbind, left_data_list)
left_data$Distribution <- factor(
    left_data$Distribution,
    unique(left_data$Distribution),
    sprintf("#%d %s", setups$distribution, unique(left_data$Distribution))
)

rtop <- with(left_data, range(posterior))
top <- ggplot(left_data, aes(x=log(alpha), y=beta, fill=posterior)) +
    facet_grid(. ~ Distribution) +
    geom_raster() +
    scale_x_continuous(NULL, expand=c(0,0), breaks = c(-7, 0)) + 
    scale_y_continuous(expression(beta), expand=c(0,0), breaks = c(0, 4)) + 
    scale_fill_gradient(expression(p(log(alpha),beta)), low="white", high="black",
                        breaks=c(rtop, 0), labels=function(x) sprintf("%.4f", x)) +
    theme_minimal(base_size = 8) +
    theme(
        panel.border = element_rect(colour="grey60", fill=NA),
        legend.key.height = unit(3, "mm"),
        legend.key.width = unit(3, "mm")
    )
    

rbottom <- with(left_data, range((fit_posterior - posterior)/max(posterior)))
bottom <- ggplot(left_data, aes(x=log(alpha), y=beta, fill=(fit_posterior - posterior)/max(posterior))) +
    facet_grid(. ~ Distribution) +
    geom_raster() +
    scale_fill_gradient("Relative error", low="black", high="white",
                        breaks=c(rbottom, 0), labels=function(x) sprintf("%.3f", x)) +
    scale_x_continuous(expression(log(alpha)), expand=c(0,0), breaks = c(-7, 0)) + 
    scale_y_continuous(expression(beta), expand=c(0,0), breaks = c(0, 4)) + 
    theme_minimal(base_size = 8) +
    theme(
        panel.border = element_rect(colour="grey60", fill=NA),
        legend.key.height = unit(3, "mm"),
        legend.key.width = unit(3, "mm")
    )

plot <- arrangeGrob(top, bottom, ncol=1, heights = c(6, 7))
my_save("gaussian_assumption.pdf", plot, 10, 7)

