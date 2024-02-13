
rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("brazil_tworound_functions.R")

#***************************************************************************************************
# READ IN FILES HERE ####
#***************************************************************************************************

elections <- readRDS("elections.rds")
spend <- readRDS("spending.rds")
schools <- readRDS("schools.rds")
lights <- readRDS("lights.rds")
simulation <- readRDS("simulation.rds")

#***************************************************************************************************
# REGRESSION SPECIFICATIONS ####
#***************************************************************************************************

# regression specifications
bw = 50000
spec_baseline =
  "tworound + cut_dist + pop_den_pre + tworound:cut_dist + tworound:pop_den_pre + factor(elect_year)|0|0|tse_code"
spec_no_controls = 
  "tworound + cut_dist + tworound:cut_dist|0|0|tse_code"
spec_no_controls_yr = 
  "tworound + cut_dist + tworound:cut_dist + factor(elect_year)|0|0|tse_code"
spec_controls = 
  "tworound + cut_dist + pop_den_pre + tworound:cut_dist + tworound:pop_den_pre + area_chg_T0 + pop_growth_T0 + pop_0_15_T0 + illit_T0 + h_inc_T0 + h_dem_T0 + inc_T0 + inc_0_50_T0 + unempl_T0 + gini_T0 + factor(elect_year)|0|0|tse_code"
spec_quadratic = 
  "tworound + cut_dist + cut_dist^2 + pop_den_pre + tworound:cut_dist + tworound:cut_dist^2 + tworound:pop_den_pre + factor(elect_year)|0|0|tse_code"
spec_num_cand =
  "tworound + cut_dist + pop_den_pre + tworound:cut_dist + tworound:pop_den_pre + num_cand + factor(elect_year)|0|0|tse_code"

# color and size formatting for plots
lightmaroon = "#BF7C7C"
maroon = "#800000"
blue = "#354B99"
lightblue = "#98CAE0"

fw = 5
fh = 2

#***************************************************************************************************
# TABLES AND FIGURES ####
#***************************************************************************************************

#### ** Figure B1 ####
#### density of elections

# For the McCrary test, exclude cities above 99.9 percentile of voters
mccrary = DCdensity(
  elections[elig_voters <= quantile(elections$elig_voters, 0.999), elig_voters], 
  cutpoint = 200000, ext.out = TRUE)

# Plot only includes elections between 50k-400k voters
plotdt <- elections[elig_voters >= 50000 & elig_voters <= 400000]

figureb1 <- ggplot(data = plotdt, aes(x = elig_voters)) +
  theme_bw(base_size = 8) +
  xlab("Number of registered voters") +
  ylab("# of elections \n(or municipality-election years)") +
  scale_x_continuous(labels = scales::comma) +
  geom_histogram(
    binwidth = 5000, boundary = 0, closed = "left",
    color = "slategray", fill = lightmaroon) +
  geom_vline(aes(xintercept = 200000), linetype = 2) +
  annotate("text", 
    label = paste0("Size: ", format2(mccrary$theta), " (p-value = ", format2(mccrary$p), ")"), 
    x = 270000, y = 50, size = 2.5)
figureb1
ggsave(figureb1, file = "figureb1.pdf", width = fw, height = fh, units = "in")

#### ** Table 1 ####
#### pre-characteristics

# Panel A: characteristics measured in 1980 census
output <- estRD(
  list(elections), 
  lhs = c("illit_80", "inc_0_50_80", "unempl_80", "pop_den_80"),
  spec = spec_no_controls_yr,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table1_panela.tex")

# Panel B: characteristics measured prior to most recent single round
output <- estRD(
  list(elections), 
  lhs = c("illit_T0", "inc_T0", "inc_0_50_T0", "unempl_T0"),
  spec = spec_no_controls_yr,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table1_panelb1.tex")

output <- estRD(
  list(elections), 
  lhs = c("area_chg_T0", "pop_growth_T0", "pop_den_T0", "pop_0_15_T0"),
  spec = spec_no_controls_yr,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table1_panelb2.tex")

output <- estRD(
  list(elections), 
  lhs = c("h_inc_T0", "h_dem_T0", "gini_T0"),
  spec = spec_no_controls_yr,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table1_panelb3.tex")

# Calculate the F-stat of joint significance of regressions in Panel B, using a seemingly unrelated
# regression
lhs = c(
  "area_chg_T0", "pop_growth_T0", "pop_den_T0", "pop_0_15_T0", "illit_T0",
  "h_inc_T0", "h_dem_T0", "inc_T0", "inc_0_50_T0", "unempl_T0", "gini_T0"
)

elections_sur <- melt(
  elections[, .SD, .SDcols = c("elect_year", "tworound", "cut_dist", "cut_dist_abs", "tse_code", lhs)], 
  id.vars = c("elect_year", "tworound", "cut_dist", "cut_dist_abs", "tse_code"),
  measure.vars = lhs, variable.factor = FALSE, value.factor = FALSE
)
elections_sur[, paste0("ind_", 1:length(lhs)) := lapply(lhs, function(x) 1*(variable == x))]

output <- felm(
  as.formula(paste0(
    "value ~ ",
    paste(
      paste0("ind_", 1:length(lhs)),
      paste0("tworound:ind_", 1:length(lhs)), 
      paste0("cut_dist:ind_", 1:length(lhs)),
      paste0("tworound:cut_dist:ind_", 1:length(lhs)),
      paste0("factor(elect_year):ind_", 1:length(lhs)),
      collapse = " + ", sep = " + "
  ), "|0|0|tse_code")),
  data = elections_sur, subset = elections_sur[,cut_dist_abs <= bw]
)

lfe::waldtest(output,
  R = ~`ind_1:tworound`|`tworound:ind_2`|`tworound:ind_3`|`tworound:ind_4`|`tworound:ind_5`|
    `tworound:ind_6`|`tworound:ind_7`|`tworound:ind_8`|`tworound:ind_9`|`tworound:ind_10`|
    `tworound:ind_11`,
  type = "cluster")

rm(elections_sur)

#### ** Figure B2 ####
#### other cutoffs

# Panel A: Probability of being above/below 300,000 resident cutoff
figureb2a <- plotRD(elections[elect_year >= 2000], 
  "tworound_pop300k", "Above 300k resident \nthreshold", "tse_code", spec = spec_no_controls_yr,
  file = "figureb2a.pdf", w = fw, h = fh)

# Panel B: Probability of being above/below 285,714 resident cutoff
figureb2b <- plotRD(elections[elect_year >= 2004], 
  "tworound_pop285k", "Above 285k resident \nthreshold", "tse_code", spec = spec_no_controls_yr,
  file = "figureb2b.pdf", w = fw, h = fh)

figureb2 <- grid.arrange(
  figureb2a + labs(caption = "(a) Threshold: 300,000 inhabitants"),
  figureb2b + labs(caption = "(b) Threshold: 285,714 inhabitants"),
  ncol = 2)
ggsave(figureb2, file = "figureb2.pdf", width = 10, height = 2, units = "in")


#### ** Figure B3 ####
#### treatment status in last election
figureb3 <- plotRD(elections,
  "tworound_l1", "Prev. election two round", "tse_code", spec = spec_no_controls_yr,
  file = "figureb3.pdf", w = fw, h = fh)

output <- estRD(
  list(elections), 
  lhs = c("tworound_l1"),
  spec = spec_no_controls_yr,
  bandwidths = bw
)
summary(output$tworound_l1_bw_1)

#### ** Figure B4 ####

# Figure B4 Panel A: Measured in 1980
figureb4a <- plotRD(elections, 
  "pop_den_80", "Population density", "tse_code", spec = spec_no_controls_yr,
  run_low = 100000, run_high = 300000,
  file = "figureb4a.pdf", w = fw, h = fh)

# Figure B4 Panel B: Measured prior to the most recent single-round election
figureb4b <- plotRD(elections, 
  "pop_den_T0", "Population density", "tse_code", spec = spec_no_controls_yr,
  run_low = 100000, run_high = 300000,
  file = "figureb4b.pdf", w = fw, h = fh)

# Figure B3 Panel C: Measured in 1980
output <- estRD(
  list(elections), 
  lhs = c("pop_den_80", "pop_den_T0"),
  spec = spec_no_controls_yr,
  bandwidths = c(bw, seq(5000, 100000, by = 2500))
)

figureb4c <- plotBWs(
  output, "pop_den_80", c("bw_1"),
  file = "figureb4c.pdf", w = fw, h = fh)

# Figure B4 Panel D: Measured prior to the most recent single-round election
figureb4d <- plotBWs(
  output, "pop_den_T0", c("bw_1"),
  file = "figureb4d.pdf", w = fw, h = fh)

# Results on population density after dropping outlier
output <- estRD(
  list(elections[!tse_code %in% c(63134)]), 
  lhs = c("pop_den_T0"),
  spec = spec_no_controls_yr,
  bandwidths = bw, disp_bw = FALSE
)
summary(output$pop_den_T0_bw_1)



figureb4 <- grid.arrange(
  figureb4a + labs(caption = "(a) RD plot, measured prior to the 1988 \nConstitution"),
  figureb4b + labs(caption = "(b) RD plot, measured prior to the most \nrecent single-round"),
  figureb4c + labs(caption = "(c) RD estimates at different bandwidths, \nmeasured prior to the 1988 Constitution"),
  figureb4d + labs(caption = "(d) RD estimates at different bandwidths, \nmeasured prior to the most recent single-round"),
  ncol = 2)
ggsave(figureb4, file = "figureb4.pdf", width = 10, height = 4, units = "in")

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

#### ** Figure B5 ####
#### residualized RD plots, vote shares from top 2 candidates

# Panel A: Coefficient of variation
figureb5a <- plotRD(elections,
  "s_top2", "Coeff. of Variation", "tse_code", spec = spec_baseline,
  file = "figureb5a.pdf", w = fw, h = fh)

# Panel B: Fractionalization index
figureb5b <- plotRD(elections, 
  "hih_top2", "Fractionalization", "tse_code", spec = spec_baseline,
  file = "figureb5b.pdf", w = fw, h = fh)

# Panel C: Entropy index
figureb5c <- plotRD(elections, 
  "h_top2", "Entropy", "tse_code", spec = spec_baseline,
  file = "figureb5c.pdf", w = fw, h = fh)

# Panel D: 1st place candidate
figureb5d <- plotRD(elections, 
  "vote_share_sec_std_top2_c1", "Stddev in votes, 1st place", "tse_code", spec = spec_baseline,
  file = "figureb5d.pdf", w = fw, h = fh)


figureb5 <- grid.arrange(
  figureb5a + labs(caption = "(a) Coefficient of variation"),
  figureb5b + labs(caption = "(b) Fractionalization"),
  figureb5c + labs(caption = "(c) Entropy"),
  figureb5d + labs(caption = "(d) Standard deviation of 1st place candidate"),
  ncol = 2)
ggsave(figureb5, file = "figureb5.pdf", width = 10, height = 4, units = "in")


#### ** Table 2, Panel A ####
#### RD estimates on concentration indices

output <- estRD(
  list(elections), 
  lhs = c("s", "hih", "h"),
  spec = spec_baseline,
  bandwidths = c(bw, seq(5000, 150000, by = 5000)), ik = TRUE, mserd = TRUE,
  lines = list(c(
    "Potential bias", 
    format2(unlist(simulation[fake_vote_share == 0.015, c("s_mean", "hih_mean", "h_mean")]), digits = 4)))
)
cat(output$output_table_bw_1, sep = "\n", file = "table2a.tex")


#### ** Figure B8 ####
#### bandwidth robustness, concentration indices

# Panel A: Coefficient of variation
figureb8a <- plotBWs(
  output, "s", color = TRUE,
  file = "figureb8a.pdf", w = fw, h = fh)

# Panel B: Fractionalization index
figureb8b <- plotBWs(
  output, "hih", color = TRUE,
  file = "figureb8b.pdf", w = fw, h = fh)

# Panel C: Entropy index
figureb8c <- plotBWs(
  output, "h", color = TRUE,
  file = "figureb8c.pdf", w = fw, h = fh)

#### ** Table 2, Panel B ####
#### RD estimates on standard deviation in votes

output <- estRD(
  list(elections), 
  lhs = c(
    "vote_share_sec_std_c1", "vote_share_sec_std_c2", 
    "vote_share_sec_std_c3", "vote_share_sec_std_c4"),
  spec = spec_baseline,
  bandwidths = c(bw, seq(5000, 150000, by = 5000)), ik = TRUE, mserd = TRUE,
  lines = list(c("Potential bias", 
    format2(unlist(simulation[fake_vote_share == 0.015, 
    c("vote_share_sec_std1_mean", "vote_share_sec_std2_mean",
      "vote_share_sec_std3_mean", "vote_share_sec_std4_mean")]),
    digits = 4)))
)
cat(output$output_table_bw_1, sep = "\n", file = "table2b.tex")


#### ** Figure B8 ####
#### bandwidth robustness, standard deviation in votes

# Panel A: 1st place candidate
figureb8d <- plotBWs(
  output, "vote_share_sec_std_c1", color = TRUE,
  file = "figureb8d.pdf", w = fw, h = fh)

# Panel B: 2nd place candidate
figureb8e <- plotBWs(
  output, "vote_share_sec_std_c2", color = TRUE,
  file = "figureb8e.pdf", w = fw, h = fh)


figureb8 <- grid.arrange(
  figureb8a + labs(caption = "(a) Coefficient of variation"),
  figureb8b + labs(caption = "(b) Fractionalization"),
  figureb8c + labs(caption = "(c) Entropy"),
  figureb8d + labs(caption = "(d) Standard deviation of 1st place candidate"),
  figureb8e + labs(caption = "(e) Standard deviation of 2nd place candidate"),
  ncol = 2)
ggsave(figureb8, file = "figureb8.pdf", width = 10, height = 6, units = "in")




#### ** Table 2, Panel C ####
#### using votes from top 2 candidates

output <- estRD(
  list(elections), 
  lhs = c("s_top2", "hih_top2", "h_top2", "vote_share_sec_std_top2_c1"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table2c.tex")



#### ** Table B1 ####
#### using votes from the final round

output <- estRD(
  list(elections), 
  lhs = c(
    "s_top2_finalturn", "hih_top2_finalturn", "h_top2_finalturn", 
    "vote_share_sec_std_top2_c1_finalturn"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb1.tex")

#### ** Table 3 ####
#### other electoral outcomes

output <- estRD(
  list(elections), 
  lhs = c("turnout", "void", "num_cand"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table3.tex")

#### ** Figure B6 ####
#### residualized RD plots, other electoral outcomes

# Panel A: turnout
figureb6a <- plotRD(elections,
  "turnout", "Turnout", "tse_code", spec = spec_baseline,
  file = "figureb6a.pdf", w = fw, h = fh)

# Panel B: blank/invalid votes
figureb6b <- plotRD(elections,
  "void", "Blank/invalid votes", "tse_code", spec = spec_baseline,
  file = "figureb6b.pdf", w = fw, h = fh)

# Panel C: turnout
figureb6c <- plotRD(elections,
  "num_cand", "Number of candidates", "tse_code", spec = spec_baseline,
  file = "figureb6c.pdf", w = fw, h = fh)


figureb6 <- grid.arrange(
  figureb6a + labs(caption = "(a) Turnout"),
  figureb6b + labs(caption = "(b) Blank/invalid ballots"),
  figureb6c + labs(caption = "(c) Number of candidates"),
  ncol = 2)
ggsave(figureb6, file = "figureb6.pdf", width = 10, height = 4, units = "in")

#### ** Table 10 ####
#### across rounds in two-round

# Reshape dataset so we can use the round # as a regressor
elections2 <- elections[tworound == TRUE & num_rounds == 2, .SD,
  .SDcols = c(
    "elect_year", "tse_code", "cut_dist_abs",
    "s_top2", "s_top2_turn2",
    "hih_top2", "hih_top2_turn2",
    "h_top2", "h_top2_turn2",
    "vote_share_sec_std_top2_c1", "vote_share_sec_std_top2_c1_turn2"
  )]
elections2 <- melt(elections2,
  id.vars = c("elect_year", "tse_code", "cut_dist_abs"),
  measure.vars = patterns(
    s_top2 = "^s_top2", hih_top2 = "^hih_top2", h_top2 = "^h_top2", 
    vote_share_sec_std_top2_c1 = "^vote_share_sec_std_top2_c1"),
  value.name = "vote_share_sec_std_top2_c1", value.factor = FALSE,
  variable.name = "vname", variable.factor = FALSE, na.rm = TRUE
)

# Keep only elections with data for both rounds
elections2[, n := .N, by = c("elect_year", "tse_code", "cut_dist_abs")]
elections2 <- elections2[n == 2]

elections2[vname == "1", tworound := 0]
elections2[vname == "2", tworound := 1]

output <- estRD(
  list(elections2),
  lhs = c("s_top2", "hih_top2", "h_top2", "vote_share_sec_std_top2_c1"),
  spec = "tworound|factor(elect_year):factor(tse_code)|0|tse_code",
  keep = "tworound", covariate.labels = "2ndRound",
  bandwidths = 1000000000,
  mean_text = "First round mean"
)
cat(output$output_table_bw_1, sep = "\n", file = "table10.tex")

rm(elections2)

#### ** Table B2 ####
#### number of candidates as control

# Panel A: concentration indices
output <- estRD(
  list(elections), 
  lhs = c("s", "hih", "h"),
  spec = spec_num_cand,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb2a.tex")

# Panel B: standard deviation in vote shares
output <- estRD(
  list(elections), 
  lhs = c(
    "vote_share_sec_std_c1", "vote_share_sec_std_c2", 
    "vote_share_sec_std_c3", "vote_share_sec_std_c4"), 
  spec = spec_num_cand,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb2b.tex")



#### ** Table B4 ####
#### different specifications for concentration indices

# Panel A: no controls
output <- estRD(
  list(elections), 
  lhs = c("s", "hih", "h"),
  spec = spec_no_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb4a.tex")

# Panel B: election-year fixed effects
output <- estRD(
  list(elections), 
  lhs = c("s", "hih", "h"),
  spec = spec_no_controls_yr,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb4b.tex")

# Panel C: baseline specification with pre-treatment controls
output <- estRD(
  list(elections), 
  lhs = c("s", "hih", "h"),
  spec = spec_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb4c.tex")

# Panel D: quadratic specification
output <- estRD(
  list(elections), 
  lhs = c("s", "hih", "h"),
  spec = spec_quadratic,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb4d.tex")


#### ** Table B5 ####
#### different specifications for standard deviation in vote shares

# Panel A: no controls
output <- estRD(
  list(elections), 
  lhs = c(
    "vote_share_sec_std_c1", "vote_share_sec_std_c2", 
    "vote_share_sec_std_c3", "vote_share_sec_std_c4"), 
  spec = spec_no_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb5a.tex")

# Panel B: election-year fixed effects
output <- estRD(
  list(elections), 
  lhs = c(
    "vote_share_sec_std_c1", "vote_share_sec_std_c2", 
    "vote_share_sec_std_c3", "vote_share_sec_std_c4"), 
  spec = spec_no_controls_yr,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb5b.tex")

# Panel C: baseline specification with pre-treatment controls
output <- estRD(
  list(elections), 
  lhs = c(
    "vote_share_sec_std_c1", "vote_share_sec_std_c2", 
    "vote_share_sec_std_c3", "vote_share_sec_std_c4"), 
  spec = spec_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb5c.tex")

# Panel D: quadratic specification
output <- estRD(
  list(elections), 
  lhs = c(
    "vote_share_sec_std_c1", "vote_share_sec_std_c2", 
    "vote_share_sec_std_c3", "vote_share_sec_std_c4"), 
  spec = spec_quadratic,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb5d.tex")


#### ** Table B6 ####
#### different specifications for vote shares from top 2 candidates

# Panel A: no controls
output <- estRD(
  list(elections), 
  lhs = c("s_top2", "hih_top2", "h_top2", "vote_share_sec_std_top2_c1"),
  spec = spec_no_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb6a.tex")

# Panel B: election-year fixed effects
output <- estRD(
  list(elections), 
  lhs = c("s_top2", "hih_top2", "h_top2", "vote_share_sec_std_top2_c1"),
  spec = spec_no_controls_yr,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb6b.tex")

# Panel C: baseline specification with pre-treatment controls
output <- estRD(
  list(elections), 
  lhs = c("s_top2", "hih_top2", "h_top2", "vote_share_sec_std_top2_c1"),
  spec = spec_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb6c.tex")

# Panel D: quadratic specification
output <- estRD(
  list(elections), 
  lhs = c("s_top2", "hih_top2", "h_top2", "vote_share_sec_std_top2_c1"),
  spec = spec_quadratic,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb6d.tex")


#### ** Table B9 ####
#### placebo at 285,714 inhabitants

# Panel A: concentration indices
output <- estRD(
  list(elections[elect_year >= 2004]), 
  lhs = c("s", "hih", "h"),
  spec = 
    "tworound_pop285k + pop_dist285k + pop_den_pre + tworound_pop285k*pop_dist285k + tworound_pop285k*pop_den_pre + factor(elect_year)|0|0|tse_code",
  keep = "tworound_pop285k", cut = "pop_dist285k", bandwidths = 125000
  )
cat(output$output_table_bw_1, sep = "\n", file = "tableb9a.tex")

# Panel B: standard deviation in votes
output <- estRD(
  list(elections[elect_year >= 2004]), 
  lhs = c(
    "vote_share_sec_std_c1", "vote_share_sec_std_c2", 
    "vote_share_sec_std_c3", "vote_share_sec_std_c4"), 
  spec = 
    "tworound_pop285k + pop_dist285k + pop_den_pre + tworound_pop285k*pop_dist285k + tworound_pop285k*pop_den_pre + factor(elect_year)|0|0|tse_code",
  keep = "tworound_pop285k", cut = "pop_dist285k", bandwidths = 125000
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb9b.tex")


#### ** Table B10 ####
#### placebo at 300,000 inhabitants

# Panel A: concentration indices
output <- estRD(
  list(elections[elect_year >= 2000]), 
  lhs = c("s", "hih", "h"),
  spec = 
    "tworound_pop300k + pop_dist300k + pop_den_pre + tworound_pop300k*pop_dist300k + tworound_pop300k*pop_den_pre + factor(elect_year)|0|0|tse_code",
  keep = "tworound_pop300k", cut = "pop_dist300k", bandwidths = 125000
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb10a.tex")

# Panel B: standard deviation in votes
output <- estRD(
  list(elections[elect_year >= 2000]), 
  lhs = c(
    "vote_share_sec_std_c1", "vote_share_sec_std_c2", 
    "vote_share_sec_std_c3", "vote_share_sec_std_c4"), 
  spec = 
    "tworound_pop300k + pop_dist300k + pop_den_pre + tworound_pop300k*pop_dist300k + tworound_pop300k*pop_den_pre + factor(elect_year)|0|0|tse_code",
  keep = "tworound_pop300k", cut = "pop_dist300k", bandwidths = 125000
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb10b.tex")

#### ** Figure B11 ####
#### placebo thresholds, concentration indices

# Panel A: coefficient of variation
figureb11a <- placebos(
  elections, "s", spec_baseline, bw = bw,
  file = "figureb11a.pdf", w = fw, h = fh)

# Panel B: fractionalization index
figureb11b <- placebos(
  elections, "hih", spec_baseline, bw = bw,
  file = "figureb11b.pdf", w = fw, h = fh)

# Panel C: entropy index
figureb11c <- placebos(
  elections, "h", spec_baseline, bw = bw,
  file = "figureb11c.pdf", w = fw, h = fh)

#### placebo thresholds, standard deviation in votes

# Panel A: standard deviation in votes for 1st place candidate
figureb11d <- placebos(
  elections, "vote_share_sec_std_c1", spec_baseline, bw = bw,
  file = "figureb11d.pdf", w = fw, h = fh)

# Panel B: standard deviation in votes for 2nd place candidate
figureb11e <- placebos(
  elections, "vote_share_sec_std_c2", spec_baseline, bw = bw,
  file = "figureb11e.pdf", w = fw, h = fh)


figureb11 <- grid.arrange(
  figureb11a + labs(caption = "(a) Coefficient of variation"),
  figureb11b + labs(caption = "(b) Fractionalization"),
  figureb11c + labs(caption = "(c) Entropy"),
  figureb11d + labs(caption = "(d) Standard deviation of 1st place candidate"),
  figureb11e + labs(caption = "(e) Standard deviation of 2nd place candidate"),
  ncol = 2)
ggsave(figureb11, file = "figureb11.pdf", width = 10, height = 6, units = "in")


#### ** Characteristics of polling stations ####
elections[cut_dist_abs <= bw, mean(num_sec, na.rm = TRUE)]
elections[cut_dist_abs <= bw, mean(elig_voters_sec_m, na.rm = TRUE)]

output <- estRD(
  list(elections), 
  lhs = c("elig_voters_sec_m", "votes_valid_sec_m"),
  spec = spec_no_controls,
  bandwidths = bw
)
summary(output$elig_voters_sec_m_bw_1)
summary(output$votes_valid_sec_m_bw_1)

#### ** Figure 2 ####
#### residualized RD plots, mean level of resources

# Panel A: equipment
figure2a <- plotRD(schools, 
  "eq_pca_ptile_mean", "Equipment, mean", "tse_code", spec = spec_baseline,
  file = "figure2a.pdf", w = fw, h = fh)

# Panel B: infrastructure
figure2b <- plotRD(schools, 
  "dep_pca_ptile_mean", "Infrastructure, mean", "tse_code", spec = spec_baseline,
  file = "figure2b.pdf", w = fw, h = fh)

#### residualized RD plots, standard deviation in resources

# Panel A: equipment
figure2c <- plotRD(schools, 
  "eq_pca_ptile_sd", "Equipment, stdev", "tse_code", spec = spec_baseline,
  file = "figure2c.pdf", w = fw, h = fh)

# Panel B: infrastructure
figure2d <- plotRD(schools, 
  "dep_pca_ptile_sd", "Infrastructure, stdev", "tse_code", spec = spec_baseline,
  file = "figure2d.pdf", w = fw, h = fh)

figure2 <- grid.arrange(
  figure2a + labs(caption = "(a) Equipment, mean level of resources"),
  figure2b + labs(caption = "(b) Infrastructure, mean level of resources"),
  figure2c + labs(caption = "(c) Equipment, standard deviation in resources"),
  figure2d + labs(caption = "(d) Infrastructure, standard deviation in resources"),
  ncol = 2)
ggsave(figure2, file = "figure2.pdf", width = 10, height = 4, units = "in")


#### ** Table 4 ####
#### RD estimates on school resources
output <- estRD(
  list(schools), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_baseline,
  bandwidths = c(bw, seq(5000, 150000, by = 5000)), ik = TRUE, mserd = TRUE
)
cat(output$output_table_bw_1, sep = "\n", file = "table4.tex")

#### ** Figure B9 ####
#### bandwidth robustness, mean level of resources

# Panel A: equipment
figureb9a <- plotBWs(
  output, "eq_pca_ptile_mean", color = TRUE,
  file = "figureb9a.pdf", w = fw, h = fh)

# Panel B: infrastructure
figureb9b <- plotBWs(
  output, "dep_pca_ptile_mean", color = TRUE,
  file = "figureb9b.pdf", w = fw, h = fh)

#### bandwidth robustness, stadard deviation in resources

# Panel C: equipment
figureb9c <- plotBWs(
  output, "eq_pca_ptile_sd", color = TRUE,
  file = "figureb9c.pdf", w = fw, h = fh)

# Panel D: infrastructure
figureb9d <- plotBWs(
  output, "dep_pca_ptile_sd", color = TRUE,
  file = "figureb9d.pdf", w = fw, h = fh)


figureb9 <- grid.arrange(
  figureb9a + labs(caption = "(a) Equipment, mean level of resources"),
  figureb9b + labs(caption = "(b) Infrastructure, mean level of resources"),
  figureb9c + labs(caption = "(c) Equipment, standard deviation in resources"),
  figureb9d + labs(caption = "(d) Infrastructure, standard deviation in resources"),
  ncol = 2)
ggsave(figureb9, file = "figureb9.pdf", width = 10, height = 4, units = "in")


#### ** Table 5 ####
#### RD estimates on mean level of resources in schools at different parts of the municipality
#### distribution

# Panel A: equipment
output <- estRD(
  list(schools), 
  lhs = c("eq_pca_ptile_q1", "eq_pca_ptile_q2", "eq_pca_ptile_q3", "eq_pca_ptile_q4"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table5a.tex")

# Panel B: infrastructure
output <- estRD(
  list(schools), 
  lhs = c("dep_pca_ptile_q1", "dep_pca_ptile_q2", "dep_pca_ptile_q3", "dep_pca_ptile_q4"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table5b.tex")

#### ** Table B3 ####
#### using zscores

output <- estRD(
  list(schools), 
  lhs = c("eq_zsc_ptile_mean", "dep_zsc_ptile_mean", "eq_zsc_ptile_sd", "dep_zsc_ptile_sd"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb3.tex")

#### ** Table B7 ####
#### different specifications

# Panel A: no controls
output <- estRD(
  list(schools), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_no_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb7a.tex")

# Panel B: election-year fixed effects
output <- estRD(
  list(schools), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_no_controls_yr,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb7b.tex")

# Panel C: baseline specification with pre-treatment controls
output <- estRD(
  list(schools), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb7c.tex")

# Panel D: quadratic specification
output <- estRD(
  list(schools), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_quadratic,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb7d.tex")


#### ** Figure B12 ####
#### placebo thresholds, mean level of resources

# Panel A: equipment
figureb12a <- placebos(
  schools, "eq_pca_ptile_mean", spec_baseline, bw = bw,
  file = "figureb12a.pdf", w = fw, h = fh)

# Panel B: infrastructure
figureb12b <- placebos(
  schools, "dep_pca_ptile_mean", spec_baseline, bw = bw,
  file = "figureb12b.pdf", w = fw, h = fh)

#### placebo thresholds, standard deviation in resources

# Panel C: equipment
figureb12c <- placebos(
  schools, "eq_pca_ptile_sd", spec_baseline, bw = bw,
  file = "figureb12c.pdf", w = fw, h = fh)

# Panel D: infrastructure
figureb12d <- placebos(
  schools, "dep_pca_ptile_sd", spec_baseline, bw = bw,
  file = "figureb12d.pdf", w = fw, h = fh)


figureb12 <- grid.arrange(
  figureb12a + labs(caption = "(a) Equipment, mean level of resources"),
  figureb12b + labs(caption = "(b) Infrastructure, mean level of resources"),
  figureb12c + labs(caption = "(c) Equipment, standard deviation in resources"),
  figureb12d + labs(caption = "(d) Infrastructure, standard deviation in resources"),
  ncol = 2)
ggsave(figureb12, file = "figureb12.pdf", width = 10, height = 4, units = "in")


#### ** Table B11 ####
#### treatment effects by incumbency status

output <- estRD(
  list(schools), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = paste0("tworound:winner_l1_cwin + ", spec_baseline),
  keep = c("tworound", "tworound:winner_l1_cwin"),
  covariate.labels = c("TwoRound", "TwoRound * Incumbent"),
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb11.tex")


#### ** School resources by year in term ####

# 1st year in term
output <- estRD(
  list(schools[elect_term_yr == 1]), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_baseline,
  bandwidths = bw
)
output$output_table_bw_1

# 2nd year in term
output <- estRD(
  list(schools[elect_term_yr == 2]), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_baseline,
  bandwidths = bw
)
output$output_table_bw_1

# 3rd year in term
output <- estRD(
  list(schools[elect_term_yr == 3]), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_baseline,
  bandwidths = bw
)
output$output_table_bw_1

# 4th year in term
output <- estRD(
  list(schools[elect_term_yr == 4]), 
  lhs = c("eq_pca_ptile_mean", "dep_pca_ptile_mean", "eq_pca_ptile_sd", "dep_pca_ptile_sd"),
  spec = spec_baseline,
  bandwidths = bw
)
output$output_table_bw_1


#### ** Figure B7 ####
#### residualized RD plots, downstream outcomes

# Panel A: drop-out rate
figureb7a <- plotRD(schools,
  "MAT_DROP_mean", "Drop-out rate", "tse_code", spec = spec_baseline,
  file = "figureb7a.pdf", w = fw, h = fh)

# Panel B: failing rate
figureb7b <- plotRD(schools,
  "MAT_FAIL_mean", "Failing rate", "tse_code", spec = spec_baseline,
  file = "figureb7b.pdf", w = fw, h = fh)

# Panel C: passing rate
figureb7c <- plotRD(schools,
  "MAT_PASS_mean", "Passing rate", "tse_code", spec = spec_baseline,
  file = "figureb7c.pdf", w = fw, h = fh)

# Panel D: elementary literacy rate
figureb7d <- plotRD(elections,
  "elem_lit", "Elementary literacy rate", "tse_code", spec = spec_baseline,
  file = "figureb7d.pdf", w = fw, h = fh)

# Panel E: low income rate
figureb7e <- plotRD(elections,
  "inc_0_50_post", "Low income rate", "tse_code", spec = spec_baseline,
  file = "figureb7e.pdf", w = fw, h = fh)

# Panel F: income per capita
figureb7f <- plotRD(elections,
  "inc_post", "Income per capita", "tse_code", spec = spec_baseline,
  file = "figureb7f.pdf", w = fw, h = fh)

# Panel G: unemployment rate
figureb7g <- plotRD(elections,
  "unempl_post", "Unemployment rate", "tse_code", spec = spec_baseline,
  file = "figureb7g.pdf", w = fw, h = fh)

# Panel H: night lights
figureb7h <- plotRD(lights,
  "mean", "Night lights", "tse_code", spec = spec_baseline,
  file = "figureb7h.pdf", w = fw, h = fh)


figureb7 <- grid.arrange(
  figureb7a + labs(caption = "(a) Drop-out rate"),
  figureb7b + labs(caption = "(b) Failing rate"),
  figureb7c + labs(caption = "(c) Passing rate"),
  figureb7d + labs(caption = "(d) Elementary literacy rate"),
  figureb7e + labs(caption = "(e) Low income rate"),
  figureb7f + labs(caption = "(f) Income per capita"),
  figureb7g + labs(caption = "(g) Unemployment rate"),
  figureb7h + labs(caption = "(h) Night lights"),
  ncol = 2)
ggsave(figureb7, file = "figureb7.pdf", width = 10, height = 8, units = "in")


#### ** Table 6, Panel A ####
#### RD estimates on education outcomes

output <- estRD(
  list(schools, schools, schools, elections), 
  lhs = c("MAT_DROP_mean", "MAT_FAIL_mean", "MAT_PASS_mean", "elem_lit"),
  spec = spec_baseline,
  bandwidths = c(bw, seq(5000, 150000, by = 5000)), mserd = TRUE, ik = TRUE
)
cat(output$output_table_bw_1, sep = "\n", file = "table6a.tex")

#### ** Figure B10 ####
#### bandwidth robustness, education outcomes

# Panel A: drop-out rate
figureb10a <- plotBWs(
  output, "MAT_DROP_mean", color = TRUE,
  file = "figureb10a.pdf", w = fw, h = fh)

# Panel B: elementary literacy rate
# IK and MSERD not plotted for elem_lit because the chosen bandwidth was larger than the support
figureb10b <- plotBWs(
  output, "elem_lit", omit_bw = c("bw_mserd", "bw_ik"), color = TRUE,
  file = "figureb10b.pdf", w = fw, h = fh)


figureb10 <- grid.arrange(
  figureb10a + labs(caption = "(a) Drop-out rate"),
  figureb10b + labs(caption = "(b) Elementary literacy rate"),
  ncol = 2)
ggsave(figureb10, file = "figureb10.pdf", width = 10, height = 2, units = "in")


#### ** Table 6, Panel B ####
#### RD estimates on economic outcomes

output <- estRD(
  list(elections, elections, elections, lights), 
  lhs = c("inc_0_50_post", "inc_post", "unempl_post", "mean"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table6b.tex")


#### ** Table B8 ####
#### different specifications

# Panel A: no controls
output <- estRD(
  list(schools, schools, schools, elections), 
  lhs = c("MAT_DROP_mean", "MAT_FAIL_mean", "MAT_PASS_mean", "elem_lit"),
  spec = spec_no_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb8a.tex")

# Panel B: election-year fixed effects
output <- estRD(
  list(schools, schools, schools, elections), 
  lhs = c("MAT_DROP_mean", "MAT_FAIL_mean", "MAT_PASS_mean", "elem_lit"),
  spec = spec_no_controls_yr,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb8b.tex")

# Panel C: baseline specification with pre-treatment controls
output <- estRD(
  list(schools, schools, schools, elections), 
  lhs = c("MAT_DROP_mean", "MAT_FAIL_mean", "MAT_PASS_mean", "elem_lit"),
  spec = spec_controls,
  bandwidths = bw, disp_mean = FALSE
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb8c.tex")

# Panel D: quadratic specification
output <- estRD(
  list(schools, schools, schools, elections), 
  lhs = c("MAT_DROP_mean", "MAT_FAIL_mean", "MAT_PASS_mean", "elem_lit"),
  spec = spec_quadratic,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "tableb8d.tex")


#### ** Figure B13 ####
#### placebo thresholds, education outcomes

# Panel A: drop-out rate
figureb13a <- placebos(
  schools, "MAT_DROP_mean", spec_baseline, bw = bw,
  file = "figureb13a.pdf", w = fw, h = fh)

# Panel B: elementary literacy rate
figureb13b <- placebos(
  elections, "elem_lit", spec_baseline, bw = bw,
  file = "figureb13b.pdf", w = fw, h = fh)



figureb13 <- grid.arrange(
  figureb13a + labs(caption = "(a) Drop-out rate"),
  figureb13b + labs(caption = "(b) Elementary literacy rate"),
  ncol = 2)
ggsave(figureb13, file = "figureb13.pdf", width = 10, height = 2, units = "in")

#### ** Figure B14 ####
#### residualized RD plots, candidate characteristics

# Panel A: age
figureb14a <- plotRD(elections,
  "age", "Age (average)", "tse_code", spec = spec_baseline,
  file = "figureb14a.pdf", w = fw, h = fh)

# Panel B: female
figureb14b <- plotRD(elections,
  "female", "Female (fraction)", "tse_code", spec = spec_baseline,
  file = "figureb14b.pdf", w = fw, h = fh)

# Panel C: education
figureb14c <- plotRD(elections,
  "edu_univ", "University degree \n (fraction)", "tse_code", spec = spec_baseline,
  file = "figureb14c.pdf", w = fw, h = fh)

# Panel D: state of birth
figureb14d <- plotRD(elections,
  "birth_uf", "Born state state \n (fraction)", "tse_code", spec = spec_baseline,
  file = "figureb14d.pdf", w = fw, h = fh)

# Panel E: occupation, public sector
figureb14e <- plotRD(elections,
  "occ_pub", "Occupation: public sector \n (fraction)", "tse_code", spec = spec_baseline,
  file = "figureb14e.pdf", w = fw, h = fh)

# Panel F: occupation, technical
figureb14f <- plotRD(elections,
  "occ_sci", "Occupation: technical \n (fraction)", "tse_code", spec = spec_baseline,
  file = "figureb14f.pdf", w = fw, h = fh)

# Panel G: occupation, business
figureb14g <- plotRD(elections,
  "occ_bus", "Occupation: business \n (fraction)", "tse_code", spec = spec_baseline,
  file = "figureb14g.pdf", w = fw, h = fh)



figureb14 <- grid.arrange(
  figureb14a + labs(caption = "(a) Age"),
  figureb14b + labs(caption = "(b) Female"),
  figureb14c + labs(caption = "(c) University degree"),
  figureb14d + labs(caption = "(d) Born same state"),
  figureb14e + labs(caption = "(e) Occupation: public sector"),
  figureb14f + labs(caption = "(f) Occupation: technical"),
  figureb14g + labs(caption = "(g) Occupation: business"),
  ncol = 2)
ggsave(figureb14, file = "figureb14.pdf", width = 10, height = 8, units = "in")


#### ** Table 7 ####
#### RD estimates on candidate characteristics, all

# Panel A: demographic
output <- estRD(
  list(elections),
  lhs = c("age", "female", "edu_univ", "birth_uf"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table7a.tex")

# Panel B: previous occupation
output <- estRD(
  list(elections),
  lhs = c("occ_pub", "occ_sci", "occ_bus"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table7b.tex")

#### ** Figure B15 ####
#### residualized RD plots, winner characteristics

# Panel A: age
figureb15a <- plotRD(elections,
  "age_cwin", "Age", "tse_code", spec = spec_baseline,
  file = "figureb15a.pdf", w = fw, h = fh)

# Panel B: female
figureb15b <- plotRD(elections,
  "female_cwin", "Female", "tse_code", spec = spec_baseline,
  file = "figureb15b.pdf", w = fw, h = fh)

# Panel C: education
figureb15c <- plotRD(elections,
  "edu_univ_cwin", "University degree", "tse_code", spec = spec_baseline,
  file = "figureb15c.pdf", w = fw, h = fh)

# Panel D: state of birth
figureb15d <- plotRD(elections,
  "birth_uf_cwin", "Born state state", "tse_code", spec = spec_baseline,
  file = "figureb15d.pdf", w = fw, h = fh)

# Panel E: occupation, public sector
figureb15e <- plotRD(elections,
  "occ_pub_cwin", "Occupation: public sector", "tse_code", spec = spec_baseline,
  file = "figureb15e.pdf", w = fw, h = fh)

# Panel F: occupation, technical
figureb15f <- plotRD(elections,
  "occ_sci_cwin", "Occupation: technical", "tse_code", spec = spec_baseline,
  file = "figureb15f.pdf", w = fw, h = fh)

# Panel G: occupation, business
figureb15g <- plotRD(elections,
  "occ_bus_cwin", "Occupation: business", "tse_code", spec = spec_baseline,
  file = "figureb15g.pdf", w = fw, h = fh)



figureb15 <- grid.arrange(
  figureb15a + labs(caption = "(a) Age"),
  figureb15b + labs(caption = "(b) Female"),
  figureb15c + labs(caption = "(c) University degree"),
  figureb15d + labs(caption = "(d) Born same state"),
  figureb15e + labs(caption = "(e) Occupation: public sector"),
  figureb15f + labs(caption = "(f) Occupation: technical"),
  figureb15g + labs(caption = "(g) Occupation: business"),
  ncol = 2)
ggsave(figureb15, file = "figureb15.pdf", width = 10, height = 8, units = "in")


#### ** Table 8 ####
#### RD estimates on candidate characteristics, winner

# Panel A: demographic
output <- estRD(
  list(elections),
  lhs = c("age_cwin", "female_cwin", "edu_univ_cwin", "birth_uf_cwin"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table8a.tex")

# Panel B: previous occupation
output <- estRD(
  list(elections),
  lhs = c("occ_pub_cwin", "occ_sci_cwin", "occ_bus_cwin"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table8b.tex")

#### ** Figure B16 ####
#### residualized RD plots, political affiliation

# Panel A: previous candidacy
figureb16a <- plotRD(elections,
  "cand_ever", "Previous candidacy \n (number)", "tse_code", spec = spec_baseline,
  file = "figureb16a.pdf", w = fw, h = fh-.5)

# Panel B: incumbency
figureb16b <- plotRD(elections,
  "winner_ever", "Incumbency \n (number)", "tse_code", spec = spec_baseline,
  file = "figureb16b.pdf", w = fw, h = fh-.5)

# Panel C: small party
figureb16c <- plotRD(elections,
  "num_party_small", "Small party \n (number)", "tse_code", spec = spec_baseline,
  file = "figureb16c.pdf", w = fw, h = fh-.5)

# Panel D: PT party
figureb16d <- plotRD(elections,
  "party_13", "PT party \n (number)", "tse_code", spec = spec_baseline,
  file = "figureb16d.pdf", w = fw, h = fh-.5)

# Panel E: governor's party
figureb16e <- plotRD(elections,
  "gov_party", "Governor's party \n (number)", "tse_code", spec = spec_baseline,
  file = "figureb16e.pdf", w = fw, h = fh-.5)

# Panel F: previous candidacy
figureb16f <- plotRD(elections,
  "cand_ever_cwin", "Previous candidacy", "tse_code", spec = spec_baseline,
  file = "figureb16f.pdf", w = fw, h = fh-.5)

# Panel G: incumbency
figureb16g <- plotRD(elections,
  "winner_ever_cwin", "Incumbency", "tse_code", spec = spec_baseline,
  file = "figureb16g.pdf", w = fw, h = fh-.5)

# Panel H: small party
figureb16h <- plotRD(elections,
  "party_small_cwin", "Small party", "tse_code", spec = spec_baseline,
  file = "figureb16h.pdf", w = fw, h = fh-.5)

# Panel I: PT party
figureb16i <- plotRD(elections,
  "party_13_cwin", "PT party", "tse_code", spec = spec_baseline,
  file = "figureb16i.pdf", w = fw, h = fh-.5)

# Panel J: governor's party
figureb16j <- plotRD(elections,
  "gov_party_cwin", "Governor's party", "tse_code", spec = spec_baseline,
  file = "figureb16j.pdf", w = fw, h = fh-.5)


figureb16 <- grid.arrange(
  figureb16a + labs(caption = "(a) Previous candidacy, number of candidates"),
  figureb16b + labs(caption = "(b) Incumbency, number of candidates"),
  figureb16c + labs(caption = "(c) Small party, number of candidates"),
  figureb16d + labs(caption = "(d) PT party, number of candidates"),
  figureb16e + labs(caption = "(e) Governor's party, number of candidates"),
  figureb16f + labs(caption = "(f) Previous candidacy, winner"),
  figureb16g + labs(caption = "(g) Incumbency, winner"),
  figureb16h + labs(caption = "(h) Small party, winner"),
  figureb16i + labs(caption = "(i) PT party, winner"),
  figureb16j + labs(caption = "(j) Governor's party, winner"),
  ncol = 2)
ggsave(figureb16, file = "figureb16.pdf", width = 10, height = 10, units = "in")

#### ** Table 9 ####
#### RD estimates on political affiliation of candidates

# Panel A: all candidates
output <- estRD(
  list(elections), 
  lhs = c("cand_ever", "winner_ever", "num_party_small", "party_13", "gov_party"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table9a.tex")

# Panel B: winners
output <- estRD(
  list(elections), 
  lhs = c("cand_ever_cwin", "winner_ever_cwin", "party_small_cwin", "party_13_cwin", "gov_party_cwin"),
  spec = spec_baseline,
  bandwidths = c(bw, seq(5000, 150000, by = 5000)), ik = FALSE, mserd = TRUE
)
cat(output$output_table_bw_1, sep = "\n", file = "table9b.tex")


#### ** Figure B18 ####
#### bandwidth robustness, incumbency

plotBWs(
  output, "winner_ever_cwin", color = TRUE,
  file = "figureb18.pdf", w = fw, h = fh)


#### ** Figure B17 ####
#### RD estimates on party of winners
output <- estRD(
  list(elections),
  lhs = c(
    "party_10_cwin", "party_11_cwin", "party_12_cwin", "party_13_cwin", "party_14_cwin",
    "party_15_cwin", "party_19_cwin", "party_20_cwin", "party_22_cwin", "party_23_cwin", 
    "party_25_cwin", "party_35_cwin", "party_40_cwin", "party_43_cwin", "party_45_cwin", 
    "party_55_cwin", "party_41_cwin", "party_extreme_cwin"),
  spec = spec_baseline,
  bandwidths = bw
)

plotCoef(
  output[str_subset(names(output), "_bw_1")][-length(str_subset(names(output), "_bw_1"))], 
  "tworound",
  reg_lab_numeric = FALSE, reg_lab = c(
    "PRB", "PP", "PDT", "PT", "PTB", "PMDB", "PODE", "PSC", "PL", "PPS", "DEM", "PMB", "PSB", "PV", 
    "PSDB", "PSD (55)", "PSD (41)", "Extreme Party"
  ), 
  xlab = "Party of winner",
  annot_N = TRUE, flip = TRUE, 
  file = "figureb17.pdf", w = 10, h = 8
)


#### ** Figure B19 ####
#### residualized RD plots, campaign donations

# Panel A: total donations, average per candidate
figureb19a <- plotRD(spend,
  "lrece_cand_avg", "Total donations", "tse_code", spec = spec_baseline,
  file = "figureb19a.pdf", w = fw, h = fh)

# Panel B: individual donations, average per candidate
figureb19b <- plotRD(spend,
  "lrece_indv_cand_avg", "Individuals", "tse_code", spec = spec_baseline,
  file = "figureb19b.pdf", w = fw, h = fh)

# Panel C: corporate donations, average per candidate
figureb19c <- plotRD(spend,
  "lrece_corp_cand_avg", "Corporations", "tse_code", spec = spec_baseline,
  file = "figureb19c.pdf", w = fw, h = fh)

# Panel D: total donations, top two candidates
figureb19d <- plotRD(spend,
  "lrece_top2", "Total donations", "tse_code", spec = spec_baseline,
  file = "figureb19d.pdf", w = fw, h = fh)

# Panel E: individual donations, top two candidates
figureb19e <- plotRD(spend,
  "lrece_indv_top2", "Individuals", "tse_code", spec = spec_baseline,
  file = "figureb19e.pdf", w = fw, h = fh)

# Panel F: corporate donations, top two candidate
figureb19f <- plotRD(spend,
  "lrece_corp_top2", "Corporations", "tse_code", spec = spec_baseline,
  file = "figureb19f.pdf", w = fw, h = fh)


figureb19 <- grid.arrange(
  figureb19a + labs(caption = "(a) Total donations, average per candidate"),
  figureb19b + labs(caption = "(b) Donations from individuals, average per candidate"),
  figureb19c + labs(caption = "(c) Donations from corporations, average per candidate"),
  figureb19d + labs(caption = "(d) Total donations, top 2 candidates"),
  figureb19e + labs(caption = "(e) Donations from individuals, top 2 candidates"),
  figureb19f + labs(caption = "(f) Donations from corporations, top 2 candidates"),
  ncol = 2)
ggsave(figureb19, file = "figureb19.pdf", width = 10, height = 6, units = "in")


#### ** Table 11 ####
#### RD estimates on campaign donations

# Panel A: donations per candidate
output <- estRD(
  list(spend),
  lhs = c("lrece_cand_avg", "lrece_indv_cand_avg", "lrece_corp_cand_avg"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table11a.tex")

# Panel B: donations among top 2 candidates
output <- estRD(
  list(spend),
  lhs = c("lrece_top2", "lrece_indv_top2", "lrece_corp_top2"),
  spec = spec_baseline,
  bandwidths = bw
)
cat(output$output_table_bw_1, sep = "\n", file = "table11b.tex")

