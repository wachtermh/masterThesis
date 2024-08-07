# Clear Workspace ---------------------------------------------------------
# rm(list = ls())

# Path --------------------------------------------------------------------
here::i_am("src/wageGradient.R")

# Packages ----------------------------------------------------------------
library(readstata13)
library(collapse)
library(fixest)
library(data.table)
library(marginaleffects)
library(tibble)
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
wage <- read.csv(here::here("bld", "data", "wage_est.csv"))
source(here("src", "coef_names.R"))

# Calculate difference in means by job ------------------------------------
diff_means <- kline[, .(mean_cb = mean(cb)), by = .(sic_combined, firm_id ,job_id, soc3, gender, state, wave)]
diff_means <- dcast(diff_means, sic_combined + firm_id + job_id + soc3 + state + wave ~ gender, value.var = "mean_cb")
diff_means[, delta_jf := Female - Male]
diff_means <- diff_means[!is.na(delta_jf)]

# Join Industry Info ------------------------------------------------------
diff_means <- join(diff_means, ind_pct, on = c("sic_combined" = "SIC_Kline"),
                   how = "left", validate = "m:1")
diff_means <- join(diff_means, wage, on = c("sic_combined" = "SIC_Kline"),
                   how = "left", validate = "m:1")
kline <- join(kline, ind_pct, on = c("sic_combined" = "SIC_Kline"),
              how = "left", validate = "m:1")
kline <- join(kline, wage, on = c("sic_combined" = "SIC_Kline"),
              how = "left", validate = "m:1")

# Regressions ---------------------------------------------
# Define resume controls
resumeCtrls <- c("black", "over40", "political_club", "academic_club",
                 "lgbtq_club", "same_gender_pronouns",
                 "gender_neutral_pronouns", "associates")

lm1 <- feols(delta_jf ~ pct_wage_gap, data = diff_means, vcov = "HC1")
lm2 <- feols(delta_jf ~ pct_wage_gap | soc3, data = diff_means, vcov = "HC1")
lm3 <- feols(delta_jf ~ pct_wage_gap | soc3 + state, data = diff_means, vcov = "HC1")
lm4 <- feols(delta_jf ~ pct_wage_gap | soc3 + state + wave, data = diff_means, vcov = "HC1")
lm5 <- feols(cb ~ pct_wage_gap*factor(female) + .[resumeCtrls], data = kline, vcov = ~job_id)
lm6 <- feols(cb ~ pct_wage_gap*factor(female) + .[resumeCtrls] | soc3, data = kline, vcov = ~job_id)
lm7 <- feols(cb ~ pct_wage_gap*factor(female) + .[resumeCtrls] | soc3 + state, data = kline, vcov = ~job_id)
lm8 <- feols(cb ~ pct_wage_gap*factor(female) + .[resumeCtrls] | soc3 + state + wave, data = kline, vcov = ~job_id)
lm9 <- feols(cb ~ pct_wage_gap*factor(female) + .[resumeCtrls] | soc3 + state + wave + sic_combined, data = kline, vcov = ~job_id)
lm10 <- feols(cb ~ pct_wage_gap*factor(female) + .[resumeCtrls] | soc3 + state + wave + firm_id, data = kline, vcov = ~job_id)

# Estimate Effects --------------------------------------------------------
predictions(lm1, newdata = datagrid(pct_wage_gap = seq(-0.2, 0.4, 0.1)))
predictions(lm5, newdata = datagrid(pct_wage_gap = seq(-1, 1, 0.1), female = 1))
comparisons(lm5, variables = "female", newdata = datagrid(pct_wage_gap = seq(-0.2, 0.4, 0.1)))

# Plot Effects to Check ---------------------------------------------------
plot_predictions(lm1, condition = list("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)), vcov = "HC1")
plot_comparisons(lm5, variables = "female", condition = list("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)), vcov = "HC1")

# Export Tables to TeX ----------------------------------------------------
rows <- tribble(~term, ~"(1)", ~"(2)",
                "Résumé controls", "No", "Yes")
attr(rows, "position") <- 9:10
modelsummary(list(lm1, lm5), fmt = 4,
             estimate = "{estimate}{stars}",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             coef_map = c("(Intercept)" = "Constant", 
                          "pct_wage_gap" = "Wage gap",
                          "factor(female)1" = "Female",
                          "pct_wage_gap:factor(female)1" = "Wage gap × Female"),
             gof_omit = "FE|Std.Errors|IC|RMSE|Within",
             gof_map = gm,
             output = "latex",
             vcov = "HC1",
             booktabs = TRUE, title = "Estimates of Wage Gradient \\label{tab:WageGradient}",
             add_rows = rows ,escape = FALSE) |> 
  add_header_above(c(" ", "Contact Gap" = 1, "Callback" = 1)) |> 
  kable_styling(latex_options = c("scale_down", "hold_position")) |>
  footnote(general = "Notes: Résumé controls include all the variables listed in Table \\\\ref{tab:SummaryStatistics} under résumé characteristics. Standard errors in parantheses. Column (1) uses robust standard errors. For Column (2), standard errors are clustered at the job level. * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", threeparttable = TRUE, general_title = "", escape = FALSE) |>
  save_kable(here::here("bld", "tables", "est_wage_gradient.tex"))

rows <- tribble(~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)", ~"(8)",
                "Résumé controls", "No", "No", "No", "No", "Yes", "Yes", "Yes", "Yes",
                "SOC3 FE", "No", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes",
                "State FE", "No", "No", "Yes", "Yes", "No", "No", "Yes", "Yes",
                "Wave FE", "No", "No", "No", "Yes", "No", "No", "No", "Yes")
attr(rows, "position") <- 9:12
modelsummary(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8), fmt = 4,
             estimate = "{estimate}{stars}",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             coef_map = c("(Intercept)" = "Constant", 
                          "pct_wage_gap" = "Wage gap",
                          "factor(female)1" = "Female",
                          "pct_wage_gap:factor(female)1" = "Wage gap × Female"),
             gof_omit = "FE|Std.Errors|IC|RMSE|Within",
             gof_map = gm,
             output = "latex",
             vcov = "HC1",
             booktabs = TRUE, title = "Additional Effects of Wage Gaps on Hiring Discrimination \\label{tab:WageGradientRob}",
             add_rows = rows, escape = FALSE) |> 
  add_header_above(c(" ", "Contact Gap" = 4, "Callback" = 4)) |> 
  kable_styling(latex_options = c("scale_down", "hold_position")) |>
  footnote(general = "Notes: Résumé controls include all the variables listed in Table \\\\ref{tab:SummaryStatistics} under résumé characteristics. Standard errors in parantheses. Columns (1) to (4) use robust standard errors. For Columns (5) to (8) standard errors are clustered at the job level. * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", threeparttable = TRUE, general_title = "", escape = FALSE) |>
  save_kable(here::here("bld", "tables", "est_wage_gradient_rob.tex"))

# Estimate Predictions ----------------------------------------------------
pred1 <- predictions(lm1, newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.01)))
setDT(pred1)
pred1[, unsign := 0]
pred1[0 %between% list(conf.low, conf.high), unsign := 1]

# Plot Gradients ----------------------------------------------------------
tikz(here("bld", "figures", "wage_gradient.tex"), width = 5, height = 4, sanitize = TRUE)
par(mar = c(4.1, 4.1, 1, 1))
plot(pred1$pct_wage_gap, pred1$estimate, type = "n", ylim = c(-0.12, 0.12), xlim = c(-0.2, 0.4),
     xlab = "Wage Gap", ylab = "Contact Gap", xaxt = "n", yaxt = "n")
axis(1, at = seq(-0.2, 0.4, by = 0.1))
axis(2, at = c(-0.1, -0.05, 0, 0.05, 0.1))
grid(nx = NA, ny = NULL)
abline(h = 0, col = "grey")
abline(v = mean(ind_wage$pct_wage_gap), col = "red", lty = 2)
polygon(c(rev(pred1$pct_wage_gap), pred1$pct_wage_gap),
        c(rev(pred1$conf.low), pred1$conf.high), col = adjustcolor("grey90", alpha.f=0.5) , border = NA)
lines(pred1$pct_wage_gap, pred1$estimate, lwd = 2)
lines(pred1$pct_wage_gap, pred1$conf.high, lty = 2)
lines(pred1$pct_wage_gap, pred1$conf.low, lty = 2)
dev.off()

# Plot Estimates for All Models -------------------------------------------
pred1 <- predictions(lm1, newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)))
pred2 <- predictions(lm2, newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)))
pred3 <- predictions(lm3, newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)))
pred4 <- predictions(lm4, newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)))


tikz(here("bld", "figures", "wage_gradient_rob_cg.tex"), width = 5, height = 4, sanitize = TRUE)
par(mar = c(4.1, 4.1, 1, 1))
plot(pred1$pct_wage_gap, pred1$estimate, type = "n", ylim = c(-0.12, 0.12), xlim = c(-0.2, 0.4),
     xlab = "Wage Gap", ylab = "Contact Gap", xaxt = "n", yaxt = "n")
axis(1, at = seq(-0.2, 0.4, by = 0.1))
axis(2, at = c(-0.1, -0.05, 0, 0.05, 0.1))
grid(nx = NA, ny = NULL)
abline(h = 0, col = "grey")
points(pred1$pct_wage_gap, pred1$estimate, pch = 0)
points(pred2$pct_wage_gap, pred2$estimate, pch = 1)
points(pred3$pct_wage_gap, pred3$estimate, pch = 2)
points(pred4$pct_wage_gap, pred4$estimate, pch = 5)
segments(x0 = pred1$pct_wage_gap, y0 = pred1$conf.low, x1 = pred1$pct_wage_gap, y1 = pred1$conf.high, lwd = 2)
segments(x0 = pred2$pct_wage_gap, y0 = pred2$conf.low, x1 = pred2$pct_wage_gap, y1 = pred2$conf.high, lwd = 2)
segments(x0 = pred3$pct_wage_gap, y0 = pred3$conf.low, x1 = pred3$pct_wage_gap, y1 = pred3$conf.high, lwd = 2)
segments(x0 = pred4$pct_wage_gap, y0 = pred4$conf.low, x1 = pred4$pct_wage_gap, y1 = pred4$conf.high, lwd = 2)
dev.off()

pred5 <- comparisons(lm5, variables = "female", newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)))
pred6 <- comparisons(lm6, variables = "female", newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)))
pred7 <- comparisons(lm7, variables = "female", newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)))
pred8 <- comparisons(lm8, variables = "female", newdata = datagrid("pct_wage_gap" = seq(-0.2, 0.4, by = 0.05)))

tikz(here("bld", "figures", "wage_gradient_rob_cb.tex"), width = 5, height = 4, sanitize = TRUE)
par(mar = c(4.1, 4.1, 1, 1))
plot(pred5$pct_wage_gap, pred5$estimate, type = "n", ylim = c(-0.12, 0.12), xlim = c(-0.2, 0.4),
     xlab = "Wage Gap", ylab = "Female Marginal Effect", xaxt = "n", yaxt = "n")
axis(1, at = seq(-0.2, 0.4, by = 0.1))
axis(2, at = c(-0.1, -0.05, 0, 0.05, 0.1))
grid(nx = NA, ny = NULL)
abline(h = 0, col = "grey")
points(pred5$pct_wage_gap, pred1$estimate, pch = 0)
points(pred6$pct_wage_gap, pred2$estimate, pch = 1)
points(pred7$pct_wage_gap, pred3$estimate, pch = 2)
points(pred8$pct_wage_gap, pred4$estimate, pch = 5)
segments(x0 = pred5$pct_wage_gap, y0 = pred5$conf.low, x1 = pred5$pct_wage_gap, y1 = pred5$conf.high, lwd = 2)
segments(x0 = pred6$pct_wage_gap, y0 = pred6$conf.low, x1 = pred6$pct_wage_gap, y1 = pred6$conf.high, lwd = 2)
segments(x0 = pred7$pct_wage_gap, y0 = pred7$conf.low, x1 = pred7$pct_wage_gap, y1 = pred7$conf.high, lwd = 2)
segments(x0 = pred8$pct_wage_gap, y0 = pred8$conf.low, x1 = pred8$pct_wage_gap, y1 = pred8$conf.high, lwd = 2)
dev.off()

