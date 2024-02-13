#### Preamble ####
# Purpose: Set up regression specifications
# Author: Yunshu Zhang
# Date: 13 February 2022
# Contact: yunshu.zhang@mail.utoronto.ca


#### REGRESSION SPECIFICATIONS ####
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