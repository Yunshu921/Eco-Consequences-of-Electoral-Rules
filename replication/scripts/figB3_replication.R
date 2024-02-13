#### Preamble ####
# Purpose: Figure B3 replication
# Author: Yunshu Zhang
# Date: 13 February 2022
# Contact: yunshu.zhang@mail.utoronto.ca

#### Workspace setup ####
# Load libraries
library(tidyverse)

# Load the RDS file into R
elections <- readRDS('inputs/data/elections.rds')

# Load the helper functions and regression specifications
source("replication/helper_functions/brazil_tworound_functions.R")
source("replication/helper_functions/regression_specifications.R")


# Make plot
#### ** Figure B3 ####
#### treatment status in last election
figureb3 <- plotRD(elections,
                   "tworound_l1", "Prev. election two round", "tse_code", spec = spec_no_controls_yr,
                   file = "replication/plots/figureb3.png", w = fw, h = fh)

output <- estRD(
   list(elections), 
   lhs = c("tworound_l1"),
   spec = spec_no_controls_yr,
   bandwidths = bw
)
summary(output$tworound_l1_bw_1)