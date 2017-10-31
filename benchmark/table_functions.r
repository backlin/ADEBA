emphasise_min <- function(x){
  ifelse(x == min(x, na.rm=TRUE),
         sprintf("\\textbf{%.3f}", x),
         sprintf("%.3f", x))
}

make_table <- function(df, position="p", caption){
  if(missing(caption)) stop("You must set a caption.")
  alignment <- paste(rep(c("l", "r", "l"), c(1, 1, 2*nlevels(df$Method))), collapse="")
  sub_header <- paste(rep(c("", "Mean & Std.Dev."), c(2, nlevels(df$Method))), collapse = " & ")
  df <- prepare_table(df)
  fields <- gsub("\\ba\\b", "$\\\\alpha$", gsub("\\bn\\b", "$n$", names(df)))
  header <- sprintf("\\begin{table}[%s]
  \\small
  \\centering
  \\begin{tabular}{%s}
    \\hline
    %s \\\\
    %s \\\\
    \\hline", position, alignment, paste(fields, collapse=" & "), sub_header)
  footer <- sprintf("  \\end{tabular}
  \\caption{%s}
\\end{table}", caption)
  rows <- apply(df, 1, paste, collapse = " & ")
  n_replicates <- length(unique(df$n))
  n_distributions <- sum(df$Distribution != "")
  hlines <- rep(rep(c("", " \\hline"), c(n_replicates-1, 1)), n_distributions)
  paste(header, paste("    ", rows, " \\\\", hlines, sep="", collapse = "\n"), footer, sep="\n")
}

prepare_table <- function(df){
  df %>%
    group_by_(.dots=setdiff(names(df), c("replicate", "ISE"))) %>%
    summarise(m = mean(ISE, na.rm=TRUE), s = sd(ISE, na.rm=TRUE)) %>%
    group_by_("Distribution", "n") %>%
    mutate(
      Method = sprintf("\\multicolumn{2}{c}{%s}", Method),
      Method = factor(Method, unique(Method)),
      m_s=sprintf("%s & (%s)", emphasise_min(m), emphasise_min(s))
    ) %>%
    select(-m, -s) %>%
    spread(Method, m_s) %>%
    ungroup %>%
    mutate(
      Distribution = sprintf("%s", Distribution),
      Distribution = ifelse(Distribution == lag(Distribution, 1, ""), "", Distribution)
    )
}
