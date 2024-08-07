# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Path --------------------------------------------------------------------
here::i_am("src/indWage.R")

# Packages ----------------------------------------------------------------
library(data.table)
library(readstata13)
library(fixest)
library(marginaleffects)
library(modelsummary)
options(modelsummary_factory_default = 'kableExtra')
options(modelsummary_factory_latex = 'kableExtra')
options(modelsummary_factory_html = 'kableExtra')
library(kableExtra)
library(collapse)
library(here)

# Load Data ---------------------------------------------------------------
IND1990_to_SIC3 <- read.dta13(file = here::here("src", "data", "IND1990_to_SIC3.dta"))
IND1990_to_SIC3$SIC2 <- substr(IND1990_to_SIC3$sic87, 1, 2)
setDT(IND1990_to_SIC3)

IPUMS_CPS <- read.dta13(file = here::here("src", "data", "IPUMS_CPS.dta"))
setDT(IPUMS_CPS)

source(here("src", "coef_names.R"))

# Clean Data  ---------------------------------------------------------------
# Add ind1990 codes
IPUMS_CPS$ind1990code <- get.origin.codes(IPUMS_CPS$ind1990,
                                          label.table = get.label(IPUMS_CPS,
                                                                  "IND1990"))
# Age to numeric
IPUMS_CPS$age <- as.numeric(IPUMS_CPS$age)
# Drop NIU and varying hours for UHRSWORK
IPUMS_CPS <- IPUMS_CPS[uhrsworkt %notin% c(997, 999)]
# Select only 4 and 8 for month in sample
IPUMS_CPS <- IPUMS_CPS[mish %in% c("four", "eight")]
# Drop NIU from IND1990
IPUMS_CPS <- IPUMS_CPS[ind1990 != 0]
# Drop NIU from EARNWEEK
IPUMS_CPS <- IPUMS_CPS[earnweek != 9999.99]
# Age 20 to 60
IPUMS_CPS <- IPUMS_CPS[age >= 20 & age <= 60]
# Limit to more than 35 hours worked per week
IPUMS_CPS <- IPUMS_CPS[uhrsworkt >= 35]
# Drop missing educ
IPUMS_CPS <- IPUMS_CPS[educ != 999]
# Drop missing hourly wage
IPUMS_CPS <- IPUMS_CPS[hourwage != 999.99]
# Include private, for profit only
IPUMS_CPS <- IPUMS_CPS[classwkr == "private, for profit"]
# Include employed people only
IPUMS_CPS <- IPUMS_CPS[empstat %in% c("at work", "has job, not at work last week")]
# Generate dummies for male and female persons
IPUMS_CPS[, female := 0]
IPUMS_CPS[sex == "female", female := 1]
IPUMS_CPS[, male := 0]
IPUMS_CPS[sex == "male", male := 1]
# Remove topcoded obs
# IPUMS_CPS <- IPUMS_CPS[earnweek != 2884.61]
IPUMS_CPS <- IPUMS_CPS[qearnwee == "not allocated" & quhrsworkt == "no change or children or armed forces"]
# Drop NA
sapply(IPUMS_CPS, function(x) sum(is.na(x)))

# Calculate hourly earnings -----------------------------------------------
IPUMS_CPS[, earnhour := earnweek / uhrsworkt]

IPUMS_CPS <- join(IPUMS_CPS, IND1990_to_SIC3, on = c("ind1990code" = "ind1990ddx"), 
                  how = "full", multiple = TRUE)
IPUMS_CPS$SIC2 <- as.numeric(IPUMS_CPS$SIC2)
IPUMS_CPS[, SIC_Kline := fifelse(SIC2 %in% 24:35, 24, 
                                 fifelse(SIC2 %in% 42:47, 42, 
                                         fifelse(SIC2 %in% 60:61, 60, 
                                                 fifelse(SIC2 %in% 63:65, 63, 
                                                         fifelse(SIC2 %in% 75:76, 75, SIC2)))))]
IPUMS_CPS <- IPUMS_CPS[SIC_Kline %in% c(20, 23, 24:35, 42:47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
                                        58, 59, 60:61, 62, 63:65, 70, 73, 75:76, 80, 87)]

IPUMS_CPS <- IPUMS_CPS[!is.na(earnweek) & !is.na(uhrsworkt) &
                         !is.na(male) & !is.na(educ) & !is.na(year) & !is.na(earnwt)]

lm1 <- feols(log(earnhour) ~ factor(male) * factor(SIC_Kline) ,
             data = IPUMS_CPS, weights = ~earnwt, vcov = ~cpsidp)
lm2 <- feols(log(earnhour) ~ factor(male) * factor(SIC_Kline)  + age, 
             data = IPUMS_CPS, weights = ~earnwt, vcov = ~cpsidp)
lm3 <- feols(log(earnhour) ~ factor(male) * factor(SIC_Kline)  + age + I(age^2), 
             data = IPUMS_CPS, weights = ~earnwt, vcov = ~cpsidp)
lm4 <- feols(log(earnhour) ~ factor(male) * factor(SIC_Kline)  + age + I(age^2) | educ,
             data = IPUMS_CPS, weights = ~earnwt, vcov = ~cpsidp)
lm5 <- feols(log(earnhour) ~ factor(male) * factor(SIC_Kline)  + age + I(age^2) | educ + year, 
             data = IPUMS_CPS, weights = ~earnwt, vcov = ~cpsidp)
lm6 <- feols(log(earnhour) ~ factor(male) * factor(SIC_Kline)  + age + I(age^2) | educ + year + occ2010, 
             data = IPUMS_CPS, weights = ~earnwt, vcov = ~cpsidp)
lm7 <- feols(earnhour ~ factor(male) * factor(SIC_Kline)  + age + I(age^2) | educ + year + occ2010, 
             data = IPUMS_CPS, weights = ~earnwt, vcov = ~cpsidp)



# Export Table to TeX -----------------------------------------------------
rows <- tribble(~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
                "Education FE", "No", "No", "No", "Yes", "Yes", "Yes",
                "Year FE", "No", "No", "No", "No", "Yes", "Yes",
                "Occupation FE", "No", "No", "No", "No", "No", "Yes")
attr(rows, "position") <- 101:103
modelsummary(list(lm1, lm2, lm3, lm4, lm5, lm6), fmt = 4,
             estimate = "{estimate}{stars}",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             coef_rename = cr,
             gof_map = gm,
             gof_omit = "FE|Std.Errors|IC|RMSE|Within",
             coef_omit = "^(?!.*factor\\(SIC_Kline\\)|.*factor\\(male\\)\\d+:factor\\(SIC_Kline\\)\\d+|.*age|.*Intercept|.*male)",
             output = "latex",
             vcov = ~cpsidp,
             booktabs = TRUE,
             title = "Estimation of Wage Gaps \\label{tab:WageGaps}",
             escape = FALSE, longtable = TRUE, add_rows = rows) |> 
  kable_styling(font_size = 8) |> 
  add_header_above(c(" ", "Log(Wage)" = 6)) |> 
  footnote(general = "Notes: All estimations use CPS earnings weights. Standard errors, in parantheses, are clustered at the individual level. * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", 
           threeparttable = TRUE, general_title = "", escape = FALSE) |>
  save_kable(here::here("bld", "tables", "wage_gaps.tex"))


# Estimate Differences ----------------------------------------------------
pct_wage_gaps <- comparisons(lm6, variables = "male", by = "SIC_Kline")
wage_gaps <- comparisons(lm7, variables = "male", by = "SIC_Kline")
wages <- predictions(lm7, by = "SIC_Kline")


# Tidy things up ----------------------------------------------------------
setDT(wage_gaps)
setDT(pct_wage_gaps)
setDT(wages)
wage_gaps_est <- wage_gaps[, .(SIC_Kline, estimate)]
pct_wage_gaps_est <- pct_wage_gaps[, .(SIC_Kline, estimate)]
wages_est <- wages[, .(SIC_Kline, estimate)]
setnames(pct_wage_gaps_est, "estimate", "pct_wage_gap")
setnames(wage_gaps_est, "estimate", "wage_gap")
setnames(wages_est, "estimate", "wages")

wage <- join(pct_wage_gaps_est, wage_gaps_est, on = "SIC_Kline",
             how = "inner", validate = "1:1")
wage <- join(wage, wages_est, on = "SIC_Kline",
             how = "inner", validate = "1:1")

# Save data ---------------------------------------------------------------
write.csv(wage, here::here("bld", "data", "wage_est.csv"),
          row.names = FALSE)

# Function to add asterisks based on p-value
add_significance_asterisks <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Apply function to add asterisks to estimates
pct_wage_gaps$estimate <- round(pct_wage_gaps$estimate, digits = 4)
pct_wage_gaps$estimate <- paste0(pct_wage_gaps$estimate, sapply(pct_wage_gaps$p.value, add_significance_asterisks))

# Export table
kbl(pct_wage_gaps[, .(SIC_Kline, estimate, std.error)],
    booktabs = TRUE, format = "latex", digits = 4,
    caption = "Estimates of Wage Differentials Across Industries",
    label = "WageDifferentials", linesep = "",
    align = c("c", "d", "c"),
    col.names = c("SIC", "{Percentage Difference}", "Std. Error")) |> 
  kable_styling(latex_options = "scale_down") |> 
  footnote(general = "Notes: This table reports estimates for the percentage differences of hourly earnings between men and women in each industry, based on the specification in Column (6) in Table \\\\ref{tab:WageGaps}, valuating all other variables at their means. All estimations use CPS earnings weights. Standard errors are clustered at the individual level. * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.",
           threeparttable = TRUE,
           general_title = "",
           escape = FALSE) |>
  gsub(pattern = "\\\\\\{Percentage Difference\\\\\\}", replacement = "{Percentage Difference}") |>
  save_kable(here::here("bld", "tables", "sic_wage_table.tex"))
