#### Preamble ####
# Purpose: Figure 1 replication
# Author: Yunshu Zhang
# Date: 12 February 2022
# Contact: yunshu.zhang@mail.utoronto.ca

## Data Sources:

# TSE’s electoral data respository (Tribunal Superior Eleitoral, 1996-2016): While
# the repository has been discontinued, the data can be freely downloaded at
# the TSE’s Portal de Dados Abertos at https://dadosabertos.tse.jus.br/.
# The datasets used are: Resultados, Candidatos, and Prestação de contas. Files are
# either in .txt or .csv format.

# IBGE’s Demographic Censuses (Instituto Brasileiro de Geografia e Estatística,1980, 1991, 2000, 2010): 
# For 1980, the census 5% sample was accessed using IPUMS.1 For 1991, 2000, and 2010, the municipality 
# aggregates and setor censitário aggregates can be freely downloaded from the IBGE server and 
# municipality maps can be freely downloaded from the IBGE map portal. Aggregates
# are either in .txt, .csv, or .xls format. Maps are in .shp format.
 

# I crosschecked the data from these 2  data sources with the data in elections.csv provided by 
# the authors of the paper M. Chin, “When do politicians appeal broadly?: the economic consequences
# of electoral rules in Brazil,” American economic journal. Applied economics, vol. 15, no. 3, 
# pp. 183–209, 2023, doi: 10.1257/app.20210529.
# and it is the same.


#### Workspace setup ####
# Load libraries
library(tidyverse)


# Load the RDS file into R
raw_data <- readRDS("Cloud/lib/brazil/elections.rds")

# Rewrite the data into a CSV file
write.csv(raw_data, "outputs/data/elections.csv", row.names = FALSE)

fig1_data <- read_csv("outputs/data/elections.csv")

# Make plot
#### ** Figure 1 ####
#### residualized RD plots, concentration indices

# Panel A: Coefficient of variation
figure1a <- plotRD(elections,
                   "s", "Coeff. of Variation", "tse_code", spec = spec_baseline,
                   file = "figure1a.pdf", w = fw, h = fh)

# Panel B: Fractionalization index
figure1b <- plotRD(elections, 
                   "hih", "Fractionalization", "tse_code", spec = spec_baseline,
                   file = "figure1b.pdf", w = fw, h = fh)

# Panel C: Entropy index
figure1c <- plotRD(elections, 
                   "h", "Entropy", "tse_code", spec = spec_baseline,
                   file = "figure1c.pdf", w = fw, h = fh)

#### residualized RD plots, standard deviation in votes

# Panel D: 1st place candidate
figure1d <- plotRD(elections,
                   "vote_share_sec_std_c1", "Stdev in votes, 1st place", "tse_code", spec = spec_baseline,
                   file = "figure1d.pdf", w = fw, h = fh)

# Panel E: 2nd place candidate
figure1e <- plotRD(elections,
                   "vote_share_sec_std_c2", "Stdev in votes, 2nd place", "tse_code", spec = spec_baseline,
                   file = "figure1e.pdf", w = fw, h = fh)

# Panel F: 3rd place candidate
figure1f <- plotRD(elections,
                   "vote_share_sec_std_c3", "Stdev in votes, 3rd place", "tse_code", spec = spec_baseline,
                   file = "figure1f.pdf", w = fw, h = fh)

# Panel G: 4th place candidate
figure1g <- plotRD(elections,
                   "vote_share_sec_std_c4", "Stdev in votes, 4th place", "tse_code", spec = spec_baseline,
                   file = "figure1g.pdf", w = fw, h = fh)

figure1 <- grid.arrange(
   figure1a + labs(caption = "(a) Coefficient of variation"),
   figure1b + labs(caption = "(b) Fractionalization"),
   figure1c + labs(caption = "(c) Entropy"),
   figure1d + labs(caption = "(d) Standard deviation in votes, 1st place candidate"),
   figure1e + labs(caption = "(e) Standard deviation in votes, 2nd place candidate"),
   figure1f + labs(caption = "(f) Standard deviation in votes, 3rd place candidate"),
   figure1g + labs(caption = "(g) Standard deviation in votes, 4th place candidate"),
   ncol = 2)
ggsave(figure1, file = "figure1.pdf", width = 10, height = 8, units = "in")


