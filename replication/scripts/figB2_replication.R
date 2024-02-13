#### Preamble ####
# Purpose: Figure B2 replication
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
#### ** Figure B2 ####
#### other cutoffs

# Panel A: Probability of being above/below 300,000 resident cutoff
figureb2a <- plotRD(elections[elect_year >= 2000], 
                    "tworound_pop300k", "Above 300k resident \nthreshold", "tse_code", spec = spec_no_controls_yr,
                    file = "replication/plots/figureb2a.png", w = fw, h = fh)

# Panel B: Probability of being above/below 285,714 resident cutoff
figureb2b <- plotRD(elections[elect_year >= 2004], 
                    "tworound_pop285k", "Above 285k resident \nthreshold", "tse_code", spec = spec_no_controls_yr,
                    file = "replication/plots/figureb2b.png", w = fw, h = fh)

figureb2 <- grid.arrange(
   figureb2a + labs(caption = "(a) Threshold: 300,000 inhabitants"),
   figureb2b + labs(caption = "(b) Threshold: 285,714 inhabitants"),
   ncol = 2)
ggsave(figureb2, file = "replication/plots/figureb2.png", width = 10, height = 2, units = "in")

