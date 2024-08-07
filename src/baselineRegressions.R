# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Path --------------------------------------------------------------------
here::i_am("src/baselineRegressions.R")

# Packages ----------------------------------------------------------------
library(here)
library(readstata13)
library(fixest)
library(data.table)
library(modelsummary)
options(modelsummary_factory_default = 'kableExtra')
options(modelsummary_factory_latex = 'kableExtra')
options(modelsummary_factory_html = 'kableExtra')
library(marginaleffects)
library(kableExtra)
library(ggplot2)
library(collapse)

# Load Data ---------------------------------------------------------------
kline <- read.dta13(file = here::here("src", "data", "Kline2022.dta"))
setDT(kline)
source(here("src", "coef_names.R"))

# Regressions -------------------------------------------------------------
# Run Regressions ---------------------------------------------------------
# LPM
lm1 <- feols(cb ~ black + female + over40 + political_club + 
               academic_club + lgbtq_club + same_gender_pronouns + 
               gender_neutral_pronouns + associates + i(region4) + 
               i(wave), data = kline, vcov = ~job_id)
lm2 <- feols(cb ~ black + female + over40 + political_club + 
               academic_club + lgbtq_club + same_gender_pronouns + 
               gender_neutral_pronouns + associates + i(region4) + 
               i(wave), data = kline, vcov = ~job_id, subset = ~balanced == 1)
# Logit
lm3 <- feglm(cb ~ black + female + over40 + political_club + 
               academic_club + lgbtq_club + same_gender_pronouns + 
               gender_neutral_pronouns + associates + i(region4) + 
               i(wave), data = kline, family = binomial(link = "logit"),
             vcov = ~job_id)
lm4 <- feglm(cb ~ black + female + over40 + political_club + 
               academic_club + lgbtq_club + same_gender_pronouns + 
               gender_neutral_pronouns + associates + i(region4) + 
               i(wave), data = kline, family = binomial(link = "logit"),
             vcov = ~job_id, subset = ~balanced == 1)

# Regression Table
modelsummary(list(lm1 ,lm2, lm3, lm4), fmt = 4,
             estimate = "{estimate}{stars}",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_map = gm,
             gof_omit = "Std.Errors|IC|RMSE|R2 Within|R2 Within Adj.",
             coef_rename = cr,
             vcov = ~job_id,
             output = "latex",
             booktabs = TRUE, title = "Regressions of Résumé Characteristics on Contact Rates \\label{tab:Regressions}", 
             escape = FALSE) |>
  add_header_above(c(" ", "LPM" = 2, "Logit" = 2)) |> 
  add_header_above(c(" ", "Any contact in 30 days" = 4)) |> 
  kable_styling(latex_options = "scale_down", font_size = 10) |>
  footnote(general = "Notes: The dependent variable is a binary indicator for any contact in 30 days. Column (1) and (3) used the full sample, Column (2) and (4) the balanced sample. Standard errors in parantheses are clustered at the job level. * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", threeparttable = TRUE, general_title = "", escape = FALSE) |>
  save_kable(here::here("bld", "tables", "regressions.tex"))