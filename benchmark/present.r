setwd("~/devel/adeba/benchmark/")
require(data.table)
require(ggplot2)
require(grid)
require(gridExtra)
require(dplyr)
require(tidyr)
require(extrafont)
require(adeba)

options(stringsAsFactors = FALSE)
if(FALSE){
  # Run manually only once to setup the font database of the current R installation
  font_import()
  loadfonts()
}

load("distributions/distributions.Rdata")
load("benchmark.Rdata")

source("functions.r")
source("plot_functions.r")

local(source("plot/fig1_procedure.r"))
local(source("plot/fig2_distributions_1d.r"))
local(source("plot/fig3_traditional_akde.r"))
local(source("plot/fig4_univariate.r"))
local(source("plot/fig5_custom_pilot.r"))
local(source("plot/fig6_multivariate.r"))
