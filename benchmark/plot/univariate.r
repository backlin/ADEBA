highlight_dists <- c("#7 Separated", "#4 Kurtotic", "#10 Claw", "#13 Asm. Dbl. Claw")
stopifnot(all(highlight_dists %in% levels(number_distributions(univariate$Distribution))))

ise <- univariate %>%
  mutate(
    replicate=Replicate,
    Replicate=NULL,
    Method = factor(as.character(Method), c("FB", "AB", "SJ", "Liao"), c("DEBA", "ADEBA-a", "SJ", "Liao")),
    Distribution = number_distributions(Distribution)
  )
mise <- ise %>%
  group_by(Distribution, Method, n) %>%
  summarise(MISE = mean(ISE, na.rm=TRUE), SD_ISE=sd(ISE, na.rm=TRUE)) %>%
  ungroup

axis_limits <- mise %>%
  group_by(Distribution) %>%
  do(with(., data.frame(
    n = range(n),
    MISE = c(10^floor(log10(min(MISE, na.rm=TRUE))), min(2, 10^ceiling(log10(max(MISE, na.rm=TRUE)))))
  ))) %>%
  ungroup

#------------------------------------------------------------------[ Figure S3 ]

figS3 <- mise %>%
  ggplot(aes(x = n, y = MISE)) +
    facet_wrap(~Distribution, scales = "free_y") +
    geom_blank(data = axis_limits) +
    geom_line(aes(colour = Method, linetype=Method)) +
    xlab("\nSample size") +
    ylab("Mean ISE\n(log scale)") + 
    scale_x_log10(
      breaks = 25*2^(0:6),
      expand = c(.02, 0),
      labels = extreme_labels
    ) +
    scale_y_log10(
      breaks = 10^-(4:0),
      expand = c(.02, 0),
      labels = extreme_labels
    ) +
    scale_linetype_manual(NULL, values = method_linetypes, labels = method_labels) +
    scale_colour_manual(
      NULL,
      values = method_palette,
      labels = method_labels
    ) +
    theme(axis.ticks.x = element_line(colour = "grey80", size = .3)) +
    corner_legend
my_save("plot/figS3_univariate_mise.pdf", figS3, 14, 12)


#------------------------------------------------------------------[ Figure 4A ]

highlight_mise <- mise %>%
  mutate(Distribution = factor(as.character(Distribution), highlight_dists)) %>%
  filter(complete.cases(.))

my_axis_limits <- axis_limits %>%
  mutate(Distribution = factor(as.character(Distribution), highlight_dists)) %>%
  filter(complete.cases(.))

fig4labels <- highlight_mise %>%
  ggplot(aes(x = n, y = MISE)) +
    geom_blank(data = my_axis_limits) +
    facet_grid(
      Distribution ~ .,
      scales = "free_y",
      labeller = distribution_labeller
    ) +
    xlab("\n") +
    ylab(NULL) +
    ggtitle("") +
    scale_x_log10(
      expand = c(.02, 0),
      labels = function(x) rep("", length(x))
    ) +
    scale_y_log10(
      expand = c(.02, 0),
      labels = function(x) rep("", length(x))
    ) +
    theme(panel.border = element_blank())

fig4A <- highlight_mise %>%
  ggplot(aes(x = n, y = MISE)) +
    geom_blank(data = my_axis_limits) +
    geom_vline(xintercept = 200, size = .3, colour = "grey90") +
    geom_line(aes(colour = Method, linetype=Method)) +
    facet_grid(Distribution ~ ., scales = "free_y") +
    xlab("\nn (log scale)") +
    ylab(NULL) +
    ggtitle("Mean ISE (log scale)\n") + 
    scale_x_log10(
      breaks = 25*2^(0:6),
      expand = c(.02, 0),
      labels = function(x) extreme_labels(x, extra = 200)
    ) +
    scale_y_log10(
      breaks = 10^-(4:0),
      expand = c(.02, 0),
      labels = extreme_labels
    ) +
    scale_size_manual(values=c(.1,.6)) + guides(size=FALSE) + 
    scale_linetype_manual(NULL, values = method_linetypes) +
    scale_colour_manual(NULL, values = method_palette) +
    theme(
      plot.title = element_text(hjust=.5),
      strip.text.y = element_blank()
    ) +
    no_legend


#------------------------------------------------------------------[ Figure 4B ]

source("functions.r")

make.estimates <- function(di, n=100, seed=123){
  di_stripped <- sub("^#\\d+ ", "", di)
  d <- distributions[[di_stripped]]
  g <- grids[[di_stripped]]
  set.seed(seed)
  x <- rdist(d, n)
  fbab <- bayes(x, g)
  data.frame(
    Distribution = di,
    x = g,
    Truth = ddist(d, g),
    DEBA = fbab$fb,
    `ADEBA-a` = fbab$ab,
    SJ = sj(x, g),
    Liao = liao(x, g),
    check.names = FALSE
  ) %>% tbl_df
}

estimates <- highlight_dists %>%
  lapply(make.estimates, n = 200) %>%
  bind_rows %>%
  gather(Method, f, Truth:Liao) %>%
  mutate(
    Method = factor(Method, c("Truth", "DEBA", "ADEBA-a", "SJ", "Liao")),
    Distribution = factor(Distribution, highlight_dists),
    Offset = 100*as.integer(Distribution)
  )

axis_limits <- estimates %>%
  group_by(Distribution, Offset) %>%
  do(with(., data.frame(
    x = c(-4, 4),
    f = c(0, ceiling(10*1.02*max(f))/10)
  )))

fig4B <- estimates %>%
  ggplot(aes(x = x, y = f + Offset)) +
    geom_blank(data = axis_limits) +
    geom_line(aes(colour = Method, linetype=Method), size = .5) +
    facet_grid(Distribution ~ ., scales="free_y") +
    scale_linetype_manual(NULL, values = method_linetypes, labels=method_labels) +
    scale_colour_manual(NULL, values = method_palette, labels=method_labels) +
    xlab("\nx") +
    ylab(NULL) +
    scale_x_continuous(
      breaks = c(-4, 4),
      limits = c(-4, 4),
      expand = c(.02, 0)
    ) +
    scale_y_continuous(
      breaks = with(axis_limits, f + Offset),
      expand = c(.02, 0),
      labels = extreme_labels_with_offset
    ) +
    ggtitle("Examples of estimates (n = 200)\n") +
    theme(
      plot.title = element_text(hjust=.5),
      strip.text.y = element_blank(),
      legend.position = c(1.26, .78),
      legend.justification = c(1, 1),
      legend.key.size = unit(c(5, 1), "mm")
    )

fig4 <- arrangeGrob(fig4labels, fig4A, fig4B,
             nrow = 1, widths = c(10, 23, 38))
my_save("plot/fig4_univariate.pdf", fig4, 9, 12)


#--------------------------------------------------------[ Export tabular data ]

dir.create("tables", showWarnings = FALSE)

ise %>%
  mutate(distribution_id = as.integer(Distribution)) %>%
  select(distribution_id, n, replicate, method=Method, ise=ISE, computation_time=Duration) %>%
  arrange(distribution_id, n, replicate, method, ise) %>%
  write_csv(path="tables/univariate_ise.csv")

mise %>%
  mutate(distribution_id = as.integer(Distribution)) %>%
  select(distribution_id, n, method=Method, mean_ise=MISE, sd_ise=SD_ISE) %>%
  arrange(distribution_id, n, method, mean_ise, sd_ise) %>%
  write_csv(path="tables/univariate_mean_ise.csv")

ise %>%
  filter(as.integer(Distribution) <= 8) %>%
  select(-Duration) %>%
  make_table(caption="Data underlying Figure~4 and Supplementary Figure~2. The best performance of each row is written in bold font. This table continues on the next page.") %>%
  cat(file="tables/univariate_mise_1.tex")
ise %>%
  filter(as.integer(Distribution) >= 9) %>%
  select(-Duration) %>%
  make_table(caption="Data underlying Figure~4 and Supplementary Figure~2. The best performance of each row is written in bold font. This table is a continuation of the table on the previous page.") %>%
  cat(file="tables/univariate_mise_2.tex")


