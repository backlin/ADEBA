# Distributions that will be show in the main figure
#highlight_dists <- c("#1 Gaussian", "#2 Skewed", "#5 Outliers", "#15 Discrete Comb")
highlight_dists <- c("#1 Gaussian", "#2 Skewed", "#5 Outliers", "#12 Asymmetric Claw")

pretty_methods <- function(x){
  drop_levels(factor(x,
    c("Truth", "DEBA", "ADEBA1", "SRL", "RoT"),
    c("Truth", "DEBA", "ADEBA-1", "BMA-SRL", "RoT-SRL")
  ))
}

#-----------------------------------------------------[ Get simulation results ]

files <- dir("partial/parameters", full.names=TRUE)
ise <- lapply(files, function(f){
    load(f)
    data.frame(
        Distribution = sub("^.*/(.*) n\\d+\\.Rdata$", "\\1", f),
        n = as.integer(sub("^.*n(\\d+).*$", "\\1", f)),
        replicate = 1:100,
        DEBA = sapply(res, "[[", "ise.fb"),
        ADEBA1 = sapply(res, "[[", "ise.ab"),
        SRL = sapply(res, "[[", "ise.srl"),
        RoT = sapply(res, "[[", "ise.silverman1")
    )
}) %>%
  bind_rows %>%
  tbl_df %>%
  gather(Method, ISE, DEBA:RoT) %>%
  mutate(
    Method = pretty_methods(Method),
    Distribution = number_distributions(Distribution)
  )


#-----------------------------------------------------[ Supplementary figure 1 ]

mise <- ise %>%
  group_by(Distribution, n, Method) %>%
  summarise(MISE = mean(ISE)) %>%
  ungroup
axis_limtis <- mise %>%
  group_by(Distribution) %>%
  do(with(., data.frame(
    n = range(n),
    MISE = c(10^floor(log10(min(MISE))), min(2, 10^ceiling(log10(max(MISE)))))
  ))) %>%
  ungroup

mise %>%
  filter(
    Method == "RoT-SRL",
    as.integer(Distribution) %in% 3:5
  )

figS1 <- mise %>%
  ggplot(aes(x = n, y = MISE)) +
    geom_blank(data = axis_limtis) +
    geom_line(aes(colour = Method)) +
    facet_wrap(~Distribution, scales = "free_y") +
    xlab("\nSample size") +
    ylab("Mean ISE\n(log scale)") + 
    scale_x_log10(
      breaks = 50*2^(0:3),
      expand = c(.02, 0)
    ) +
    scale_y_log10(
      breaks = 10^-(3:0),
      expand = c(.02, 0),
      labels = extreme_labels
    ) +
    scale_size_manual(values=c(.1,.6)) + guides(size=FALSE) + 
    scale_colour_manual(NULL, values = method_palette) +
    corner_legend
my_save("plot/figS1_traditional_AKDE.pdf", figS1, 14, 12)



#-------------------------------------------------------------------[ Figure 3 ]

highlight_mise <- mise %>%
  filter(Distribution %in% highlight_dists) %>%
  mutate(Distribution = drop_levels(Distribution))

highlight_axis_limits <- axis_limtis %>%
  filter(Distribution %in% highlight_dists) %>%
  mutate(Distribution = drop_levels(Distribution))

fig3labels <- highlight_axis_limits %>%
  mutate(Distribution = abbreviate_distributions(Distribution)) %>%
  ggplot(aes(x = n, y = MISE)) +
    geom_blank() +
    facet_grid(Distribution ~ ., scales = "free_y") +
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

fig3A <- highlight_mise %>%
  ggplot(aes(x = n, y = MISE)) +
    geom_blank(data = highlight_axis_limits) +
    geom_line(aes(colour = Method)) +
    facet_grid(Distribution ~ ., scales = "free_y") +
    xlab("\nn (log scale)") +
    ylab(NULL) +
    ggtitle("Mean ISE (log scale)\n") + 
    scale_x_log10(
      breaks = 50*2^(0:3),
      expand = c(.02, 0)
    ) +
    scale_y_log10(
      breaks = 10^-(3:0),
      expand = c(.02, 0),
      labels = extreme_labels
    ) +
    scale_size_manual(values=c(.1,.6)) + guides(size=FALSE) + 
    scale_colour_manual(NULL, values = method_palette) +
    theme(strip.text.y = element_blank()) +
    no_legend


#-----------------------------------------------------------[ Actual estimates ]

estimates <- lapply(sub("^#\\d+ ", "", highlight_dists), function(dn){
    d <- distributions[[dn]]
    g <- grids[[dn]]
    set.seed(100)
    x <- rdist(d, n=200)
    
    square.root.law <- silverman <- fb <- ab <- adeba(x, adaptive=FALSE)
    square.root.law <- iterate(square.root.law)
    silverman$alpha <- exp(mean(log(predict(silverman))))
    silverman <- iterate(silverman)
    ab$beta <- seq(0, 2, length.out=5)
    ab <- iterate(ab)
    
    data.frame(
        Distribution = dn,
        x = g,
        Truth = ddist(d, g),
        DEBA = predict(fb, g),
        ADEBA1 = predict(ab, g),
        SRL = predict(square.root.law, g),
        RoT = predict(silverman, g)
    )
}) %>% bind_rows

estimates <- estimates %>%
    gather(Method, f, Truth:RoT) %>%
    mutate(
      Distribution = number_distributions(Distribution),
      Method = pretty_methods(Method),
      Offset = 100*as.integer(Distribution)
    )

axis_limits <- estimates %>%
  group_by(Distribution, Offset) %>%
  do(with(., data.frame(
    x = c(-4, 4),
    f = c(0, ceiling(10*1.02*max(f))/10)
  )))

fig3B <- estimates %>%
  ggplot(aes(x = x, y = f + Offset)) +
    geom_blank(data = axis_limits) +
    geom_line(aes(colour = Method), size = .5) +
    facet_grid(Distribution ~ ., scales="free_y") +
    scale_colour_manual(NULL, values = method_palette) +
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
      strip.text.y = element_blank(),
      legend.position = c(1, 1),
      legend.justification = c(.87, .76),
      legend.key.size = unit(3, "mm")
    )

fig3 <- arrangeGrob(fig3labels, fig3A, fig3B,
             nrow = 1, widths = c(10, 23, 38))

my_save("plot/fig3_traditional_akde.pdf", fig3, 9, 12)
