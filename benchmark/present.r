setwd("~/devel/adeba_public/benchmark/")
require(data.table)
require(ggplot2)
require(grid)
require(gridExtra)
require(dplyr)
require(tidyr)
require(readr)
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
source("table_functions.r")

local(source("plot/procedure.r"))
local(source("plot/distributions_1d.r"))
local(source("plot/traditional_akde.r"))
local(source("plot/univariate.r"))
local(source("plot/custom_pilot.r"))
local(source("plot/multivariate.r"))
