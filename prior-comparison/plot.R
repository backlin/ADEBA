library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(treesmapstheorems)

source("../benchmark/plot_functions.r")
source("comparison.R")

load("comparison.Rdata")

prior_labels <- list(
  simpleuniform = expression(alpha~"constant"),
  simplejeffreys = expression(alpha~"Jeffreys"),
  uniform = expression(alpha~beta~"constant"),
  empiricalgaussian = expression(alpha~beta~"Gaussian")
)

ratio_labels <- list(
  simplejeffreys = expression(frac("ISE("*alpha~"Jeffreys)", "ISE("*alpha~"constant)")),
  empiricalgaussian = expression(frac("ISE("*alpha~beta~"Gaussian)", "ISE("*alpha~beta~"constant)"))
)

prior_colour <- c(
  simpleuniform = "black",
  simplejeffreys = "black",
  uniform = "dodgerblue3",
  empiricalgaussian = "dodgerblue3"
)

prior_linetypes <- c(
  simpleuniform = "solid",
  simplejeffreys = "dashed",
  uniform = "twodash",
  empiricalgaussian = "81"
)

theme_palette <- get_palette()
theme_palette["annotation_dark"] <- "black"
theme_palette["annotation_light"] <- "grey60"
set_palette(theme_palette)


plot_data <- as_data_frame(do.call(rbind, ise))
factorize <- function(plot_data){
  plot_data %>%
    mutate(
      prior = factor(prior, names(prior_labels)),
      distribution = factor(
        distribution,
        names(distributions)[distribution_indexes],
        sprintf("#%d %s", distribution_indexes, names(distributions)[distribution_indexes])
      )
    )
}
na_count <- plot_data %>%
  gather_("prior", "ISE", priors) %>%
  filter(is.na(ISE)) %>%
  count(distribution, sample_size, prior)
if(any(na_count$n > 1) || any(na_count$prior != "empiricalgaussian")){
  stop("Results contain suspicious NAs, please fix.")
}



mean_ise <- plot_data %>%
  gather_("prior", "ISE", priors) %>%
  group_by(distribution, sample_size, prior) %>%
  summarise(mean_ise = mean(ISE), sd_ise = sd(ISE)) %>%
  ungroup %>%
  factorize

yb <- outer(1:9, 10^(-2:2), "*") 
top_row <- mean_ise %>%
  ggplot(aes(x = sample_size, y = mean_ise, colour = prior, linetype = prior)) +
    facet_wrap(~distribution) +
    geom_line() +
    scale_x_log10(
      expression("Sample size, "*italic(n)),
      breaks = yb,
      labels = function(x) ifelse(x %in% c(2, 10^(-8:8)), x, "")
    ) +
    scale_y_log10(
      "Mean\nISE",
      breaks = yb,
      labels = function(x) ifelse(x %in% 10^(-8:8), x, "")
    ) +
    scale_colour_manual(
      "Prior",
      values = prior_colour,
      labels = prior_labels
    ) +
    scale_linetype_manual(
      "Prior",
      values = prior_linetypes,
      labels = prior_labels
    ) +
    tmt_theme(base_size = 8, panel.border = TRUE) +
    theme(legend.key.width = unit(6, "mm"))

dir.create("plot", showWarnings = FALSE)
my_save("plot/comparison_mean_ise.pdf", top_row, 13, 4)


#----------------------------------------------------[ Head to head comparsion ]

head2head <- plot_data %>%
  mutate(
    simplejeffreys = log2(simplejeffreys / simpleuniform),
    empiricalgaussian = log2(empiricalgaussian / uniform)
  ) %>%
  select(-simpleuniform, -uniform) %>%
  gather_("prior", "log_rel_ise", priors[c(2,4)]) %>%
  group_by(distribution, sample_size, prior) %>%
  summarise(
    y = mean(log_rel_ise),
    ymin = mean(log_rel_ise) - sd(log_rel_ise),
    ymax = mean(log_rel_ise) + sd(log_rel_ise)
  ) %>%
  ungroup %>%
  factorize


yb <- with(head2head, c(0, range(y), min(ymin), max(ymax)))
p2 <- head2head %>%
  ggplot(aes(x = sample_size)) +
    facet_wrap(~distribution) +
    geom_hline(yintercept = 0, colour=colour("annotation_light")) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = prior, alpha=prior)) +
    geom_line(aes(y = y, colour = prior, linetype=prior)) +
    scale_x_log10(
      expression("Sample size, "*italic(n)),
      breaks = c(2, 5, 10, 20, 50, 100)
    ) +
    scale_y_continuous(
      expression(log[2]*"(ISE ratio)"),
      breaks=yb,
      labels = numeric_label
    ) +
    scale_colour_manual(
      expression("Mean" %+-% "std.dev."),
      values = prior_colour,
      labels = ratio_labels
    ) +
    scale_fill_manual(values = prior_colour) +
    scale_alpha_manual(values = c(.15, .25)) +
    guides(
      colour = guide_legend(
        override.aes = list(linetype=prior_linetypes[2:3])
      ),
      fill = FALSE,
      alpha = FALSE,
      linetype = FALSE
    ) +
    scale_linetype_manual(
      "Prior",
      values = prior_linetypes,
      labels = prior_labels
    ) +
    tmt_theme(base_size = 8, axis.title = "XY", axis.text = "xy", panel.border = TRUE) +
    theme(
      legend.key.width = unit(6, "mm"),
      legend.key.height = unit(12, "mm")
    )

my_save("plot/head2head.pdf", p2, 13, 4)

