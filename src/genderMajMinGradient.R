# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Path --------------------------------------------------------------------
here::i_am("src/genderMajMinGradient.R")

# Packages ----------------------------------------------------------------
library(readstata13)
library(collapse)
library(fixest)
library(data.table)
library(marginaleffects)
library(modelsummary)
options(modelsummary_factory_default = 'kableExtra')
options(modelsummary_factory_latex = 'kableExtra')
options(modelsummary_factory_html = 'kableExtra')
library(ggplot2)
library(kableExtra)
library(tikzDevice)
library(here)

# Load Data ---------------------------------------------------------------
kline <- read.dta13(file = here::here("src", "data", "Kline2022.dta"))
setDT(kline)
ind_pct <- read.csv(here::here("bld", "data", "ind_pct.csv"))
ind_wage <- read.csv(here::here("bld", "data", "wage_est.csv"))
source(here("src", "coef_names.R"))

# Calculate difference in means by job ------------------------------------
diff_means <- kline[, .(mean_cb = mean(cb)), by = .(sic_combined, firm_id ,job_id, soc3, gender, black, state, wave)]
diff_means <- dcast(diff_means, sic_combined + firm_id + job_id + soc3 + state + wave + black ~ gender, value.var = "mean_cb")
diff_means[, delta_jf := Female - Male]
diff_means <- diff_means[!is.na(delta_jf)]

# Join Industry Info ------------------------------------------------------
diff_means <- join(diff_means, ind_pct, on = c("sic_combined" = "SIC_Kline"),
                   how = "left", validate = "m:1")
diff_means <- join(diff_means, ind_wage, on = c("sic_combined" = "SIC_Kline"),
                   how = "left", validate = "m:1")
kline <- join(kline, ind_pct, on = c("sic_combined" = "SIC_Kline"),
              how = "left", validate = "m:1")
kline <- join(kline, ind_wage, on = c("sic_combined" = "SIC_Kline"),
              how = "left", validate = "m:1")

# Regressions  ---------------------------------------------
resumeCtrls <- c("over40", "political_club", "academic_club",
                 "lgbtq_club", "same_gender_pronouns",
                 "gender_neutral_pronouns", "associates")

lm1 <- feols(delta_jf ~ pct_female*factor(black), data = diff_means, vcov = "HC1")
lm2 <- feols(delta_jf ~ pct_female*factor(black) | soc3, data = diff_means, vcov = "HC1")
lm3 <- feols(delta_jf ~ pct_female*factor(black) | soc3 + state, data = diff_means, vcov = "HC1")
lm4 <- feols(delta_jf ~ pct_female*factor(black) | soc3 + state + wave, data = diff_means, vcov = "HC1")
lm5 <- feols(cb ~ pct_female*factor(black)*factor(female) + .[resumeCtrls], data = kline, vcov = ~job_id)
lm6 <- feols(cb ~ pct_female*factor(black)*factor(female) + .[resumeCtrls] | soc3, data = kline, vcov = ~job_id)
lm7 <- feols(cb ~ pct_female*factor(black)*factor(female) + .[resumeCtrls] | soc3 + state, data = kline, vcov = ~job_id)
lm8 <- feols(cb ~ pct_female*factor(black)*factor(female) + .[resumeCtrls] | soc3 + state + wave, data = kline, vcov = ~job_id)

# Estimate Effects --------------------------------------------------------
predictions(lm1, newdata = datagrid(pct_female = 1, black = c(0, 1)))
comparisons(lm5, variables = "black", newdata = datagrid(pct_female = c(0, 1), female = 0))

# Plot Effects to Check ---------------------------------------------------
plot_predictions(lm1, condition = list("pct_female" = seq(0, 1, by = 0.01), "black"), vcov = "HC1")

# Export Tables to TeX ----------------------------------------------------
rows <- tribble(~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)", ~"(8)",
                "Résumé controls", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes",
                "SOC3 FE", "No", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes",
                "State FE", "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes",
                "Wave FE", "No", "No", "No", "Yes", "No", "No", "No", "Yes")
attr(rows, "position") <- 15:18
modelsummary(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8), fmt = 4,
             estimate = "{estimate}{stars}",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = "FE|IC|RMSE|Within",
             coef_map = c("(Intercept)" = "Constant",
                          "pct_female" = "Female share",
                          "factor(black)1" = "Black",
                          "factor(female)1" = "Female",
                          "pct_female:factor(female)1" = "Female share × Female",
                          "pct_female:factor(black)1" = "Female share × Black",
                          "pct_female:factor(black)1:factor(female)1" = "Female share × Black × Female"),
             output = "latex",
             gof_map = gm,
             booktabs = TRUE, title = "Estimates of Gender Gradient Conditional on Race \\label{tab:GenderMinMajGrad}", 
             escape = FALSE, add_rows = rows) |>
  add_header_above(c(" ", "Contact Gap" = 4, "Callback" = 4)) |> 
  kable_styling(latex_options = "scale_down") |>
  footnote(general = "Notes: Résumé controls include all the variables listed in Table \\\\ref{tab:SummaryStatistics} under résumé characteristics. Standard errors in parantheses. Columns (1) to (4) use robust standard errors. For Columns (5) to (8) standard errors are clustered at the job level. * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", threeparttable = TRUE, general_title = "", escape = FALSE) |>
  save_kable(here::here("bld", "tables", "est_gender_min_maj_gradient.tex"))

# Estimate Predictions ----------------------------------------------------
pred1 <- predictions(lm1, newdata = datagrid("pct_female" = seq(0, 1, by = 0.01), "black" = 1), vcov = "HC1")
setDT(pred1)
pred1[0 %between% list(conf.low, conf.high), sign := 1]

pred2 <- predictions(lm1, newdata = datagrid("pct_female" = seq(0, 1, by = 0.01), "black" = 0), vcov = "HC1")
setDT(pred2)
pred2[0 %between% list(conf.low, conf.high), sign := 1]

# Plot Gradients ----------------------------------------------------------
tikz(here("bld", "figures", "gender_minority_gradient.tex"), width = 5, height = 4, sanitize = TRUE)
par(mar = c(4.1, 4.1, 1, 1))
plot(pred1$pct_female, pred1$estimate, type = "n", ylim = c(-0.15, 0.15),
     xlab = "Female Share", ylab = "Contact Gap", xaxt = "n")
axis(1, at = seq(0, 1, by = 0.25))
grid(nx = NA, ny = NULL)
abline(h = 0, col = "grey")
polygon(c(rev(pred1$pct_female), pred1$pct_female),
        c(rev(pred1$conf.low), pred1$conf.high), col = adjustcolor("#ACA4E290", alpha.f=0.5) , border = NA)
polygon(c(rev(pred2$pct_female), pred2$pct_female),
        c(rev(pred2$conf.low), pred2$conf.high), col = adjustcolor("#5CBD9290", alpha.f=0.5) , border = NA)
lines(pred1$pct_female, pred1$conf.high, lty = 2, col = "#ACA4E2")
lines(pred1$pct_female, pred1$conf.low, lty = 2, col = "#ACA4E2")
lines(pred2$pct_female, pred2$conf.high, lty = 2, col = "#5CBD92")
lines(pred2$pct_female, pred2$conf.low, lty = 2, col = "#5CBD92")
lines(pred1$pct_female, pred1$estimate, col = "#ACA4E2", lwd = 2)
lines(pred2$pct_female, pred2$estimate, col = "#5CBD92", lwd = 2)

par(cex = 0.75)
legend("bottom", legend=c("White", "Black"),
       col=c("#5CBD92", "#ACA4E2"), lty = c(1, 1), lwd = 2,
       text.width = strwidth("White")[1]*1.5)
dev.off()