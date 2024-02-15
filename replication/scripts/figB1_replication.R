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
# Change the working directory to the path where your RDS file is located
setwd('/cloud/project')

# Load the RDS file into R
elections <- readRDS('inputs/data/elections.rds') 

# Load the helper functions and regression specifications
source("replication/helper_functions/brazil_tworound_functions.R")
source("replication/helper_functions/regression_specifications.R")



# Make plot
#### ** Figure B1 ####
#### density of elections

# For the McCrary test, exclude cities above 99.9 percentile of voters
mccrary = DCdensity(
   elections[elig_voters <= quantile(elections$elig_voters, 0.999), elig_voters], 
   cutpoint = 250000, ext.out = TRUE)

# Plot only includes elections between 50k-400k voters
plotdt <- elections[elig_voters >= 45000 & elig_voters <= 500000]

figureb1 <- ggplot(data = plotdt, aes(x = elig_voters)) +
   theme_bw(base_size = 8) +
   xlab("Number of registered voters") +
   ylab("# of elections \n(or municipality-election years)") +
   scale_x_continuous(labels = scales::comma) +
   geom_histogram(
      binwidth = 3500, boundary = 0, closed = "left",
      color = "slategray", fill = lightmaroon) +
   geom_vline(aes(xintercept = 250000), linetype = 2) +
   annotate("text", 
            label = paste0("Size: ", format2(mccrary$theta), " (p-value = ", format2(mccrary$p), ")"), 
            x = 270000, y = 50, size = 2.5)
figureb1
ggsave(figureb1, file = "replication/plots/figureb1.png", width = fw, height = fh, units = "in")


