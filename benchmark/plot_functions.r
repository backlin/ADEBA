library(extrafont)

# https://coolors.co/app/3a3a39-d14d4b-ffb942-258fbc-70c1b3
# https://coolors.co/app/3a3a39-258fbc-873496-e866ab-ffb942
# https://coolors.co/70c1b3-fbc45d-ff6446-a21b1b-3a3a39
p <- list(
  black = "#3a3a39", blue = "#258fbc", red = "#d14d4b", orange = "#ffb942", green = "#70c1b3",
  pink = "#e866ab", purple = "#911fa8", grey = "grey70",
  yellow = "#fbc45d", orange2 = "#ff6446", dark_red = "#a21b1b"
)
method_palette <- c(
    Truth = p$grey,
    DEBA = p$blue, `ADEBA-1` = p$black, `ADEBA*` = p$black, `ADEBA-a` = p$black,
    #DEBA = p$black, `ADEBA-1` = p$blue, `ADEBA*` = p$blue, `ADEBA-a` = p$blue,
    `BMA-SRL` = p$red, `RoT-SRL`= p$orange,
    SJ = p$purple, Liao = p$pink,
    GMM2=p$green, GMM3=p$yellow, GMM5=p$orange2, GMM8=p$dark_red
)
method_labels <- list(
  SJ = "SJ", Liao = "Liao",
  DEBA="DEBA", `ADEBA-a`=expression("ADEBA-"*alpha), `ADEBA*`="ADEBA*",
  GMM2="GMM2", GMM3="GMM3", GMM5="GMM5", GMM8="GMM8"
)
method_linetypes <- c(
  Truth="solid",
  DEBA="solid", `ADEBA-1`="twodash", `ADEBA-a`="twodash", `ADEBA*`="twodash",
  `BMA-SRL`="81", `RoT-SRL`="dashed",
  SJ = "dashed", Liao = "81",
  GMM2="81", GMM3="61", GMM5="41", GMM8="21"
)

text_colour <- "grey50"
theme_set(
  theme_light(
    base_size=7,
    base_family="ArialMT"
  ) +
  theme(
    line = element_line(colour = "grey80", size = .3),
    axis.text = element_text(colour = "grey70"),
    axis.ticks.y = element_blank(),
    axis.title = element_text(colour = text_colour),
    axis.title.y = element_text(angle = 0),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = text_colour),
    panel.border = element_rect(colour = "grey80", size = .3),
    panel.grid = element_blank(),
    panel.margin = unit(4, "mm"),
    plot.margin = unit(c(2,2,1,1), "mm"),
    strip.background = element_blank(),
    strip.text.x = element_text(colour = text_colour),
    strip.text.y = element_text(colour = text_colour, angle = 0),
    title = element_text(colour = text_colour, face = "plain", size = 6)
  )
)
corner_legend <- theme(legend.position = c(7/8, .8/8), legend.key.height=unit(3, "mm"))
bottom_legend <- theme(legend.position = "bottom", legend.margin=unit(-0.6,"cm"))
no_legend <- theme(legend.position = "none")

my_save <- function(filename, x, width, height, units = "cm", ...){
  ggsave(filename, x, width=width, height=height, units = "cm", ..., device=pdf)
  embed_fonts(filename, outfile=filename)
}
number_distributions <- function(x){
    x <- factor(x, names(distributions), 
        sprintf("#%i %s", seq_along(distributions), names(distributions)))
}
#' @param x Factor of distribution names
abbreviate_distributions <- function(x){
  factor(as.character(x), levels(x), gsub("Asymmetric ", "Asym.\n", levels(x)))
}
distribution_labeller <- function(x, multi_line = TRUE){
  labels <- lapply(x, function(x) gsub("Asm.", "Asym.\n", as.character(x)))
  if (multi_line) {
    labels
  } else {
    ggplot2:::collapse_labels_lines(labels)
  }
}
method_labeller <- function(x, multi_line = TRUE){
  labels <- lapply(x, function(x) method_labels[as.character(x)])
  if (multi_line) {
    labels
  } else {
    ggplot2:::collapse_labels_lines(labels)
  }
}

label_alpha <- function(variable, value){
    ifelse(value == "ADEBA-a", expression("ADEBA-"~alpha), as.character(value))
}

extreme_labels <- function(x, extra = NULL){
  xr <- range(x, na.rm = TRUE)
  log_round <- if(all(xr - round(xr) < 1e-6)){
    # We're dealing with integers on a log axis
    round
  } else {
    # We're not, don't round
    identity
  }
  ifelse(log_round(x) %in% log_round(c(xr, extra)), x, "")
}
extreme_labels_with_offset <- function(x, offset = 100){
  ifelse(x %in% range(x, na.rm = TRUE), signif(x %% offset, 4), "")
}
drop_levels <- function(x) factor(x, levels(x)[table(x) > 0])

complete_frame <- function(x){
  x %>%
    dplyr::filter(n_ISE >= 10)
}
incomplete_frame <- function(x){
  x %>%
    mutate(n_ISE_lead = lead(n_ISE, default = 0)) %>%
    dplyr::filter(n_ISE_lead < 10)
}
