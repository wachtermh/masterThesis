# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Path --------------------------------------------------------------------
here::i_am("src/variance.R")

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
library(fplot)
library(kableExtra)
library(tikzDevice)
library(here)

options(scipen = 999)

# Load Data ---------------------------------------------------------------
kline <- read.dta13(file = here::here("src", "data", "Kline2022.dta"))
setDT(kline)


# Get number of firms and jobs -----------------------------------------
njobs <- kline[, .(njobs = .N), by = .(firm_id, sic_combined)]
nfirms <- njobs[, .(nfirms = .N), by = sic_combined]


# Firm Variance Race ------------------------------------------------------
firm_var_race <- kline[, .(mean_cb = mean(cb)), by = .(sic_combined, job_id, race, firm_id)]
firm_var_race <- dcast(firm_var_race, sic_combined + firm_id + job_id ~ race, value.var = "mean_cb")
firm_var_race[, thetahat_jf := Black - White]
firm_var_race <- firm_var_race[!is.na(thetahat_jf)]

firm_var_race[, njobs := .N, by = firm_id]
firm_var_race[, fweight := 1 / njobs]

firm_var_race <- feols(thetahat_jf ~ i(firm_id) - 1, firm_var_race,
                       weights = ~fweight, split = ~firm_id, vcov = "HC1")
firm_var_race <- coeftable(firm_var_race)

# Join number of firms
firm_var_race <- join(firm_var_race, nfirms, on = c("sample" = "sic_combined"), how = "left",
                      validate = "1:1")

mu_firm_race <- mean(firm_var_race$Estimate)
N_firms <- sum(nfirms$nfirms)

v <- (firm_var_race$Estimate - mu_firm_race)^2 - (((N_firms - 1) / N_firms) * (firm_var_race$`Std. Error`^2))
sigma_firm_race <- sqrt(mean(v))

# Firm Variance Gender ------------------------------------------------------
firm_var_gender <- kline[, .(mean_cb = mean(cb)), by = .(sic_combined, job_id, gender, firm_id)]
firm_var_gender <- dcast(firm_var_gender, sic_combined + firm_id + job_id ~ gender, value.var = "mean_cb")
firm_var_gender[, thetahat_jf := Female - Male]
firm_var_gender <- firm_var_gender[!is.na(thetahat_jf)]

firm_var_gender[, njobs := .N, by = firm_id]
firm_var_gender[, fweight := 1 / njobs]

firm_var_gender <- feols(thetahat_jf ~ i(firm_id) - 1, firm_var_gender,
                         weights = ~fweight, split = ~firm_id, vcov = "HC1")
firm_var_gender <- coeftable(firm_var_gender)

# Join number of firms
firm_var_gender <- join(firm_var_gender, nfirms, on = c("sample" = "sic_combined"), how = "left",
                        validate = "1:1")

mu_firm_gender <- mean(firm_var_gender$Estimate)
N_firms <- sum(nfirms$nfirms)

v <- (firm_var_gender$Estimate - mu_firm_gender)^2 - (((N_firms - 1) / N_firms) * (firm_var_gender$`Std. Error`^2))
sigma_firm_gender <- sqrt(mean(v))


# Industry Variance Race --------------------------------------------------
ind_var_race <- kline[, .(mean_cb = mean(cb)), by = .(sic_combined, job_id, race, firm_id)]
ind_var_race <- dcast(ind_var_race, sic_combined + firm_id + job_id ~ race, value.var = "mean_cb")
ind_var_race[, thetahat_jf := Black - White]
ind_var_race <- ind_var_race[!is.na(thetahat_jf)]

ind_var_race[, njobs := .N, by = firm_id]
ind_var_race[, fweight := 1 / njobs]

ind_var_race <- feols(thetahat_jf ~ i(sic_combined) - 1, ind_var_race,
                      weights = ~fweight, split = ~sic_combined, vcov = "HC1")
ind_var_race <- coeftable(ind_var_race)

ind_var_race <- join(ind_var_race, nfirms, on = c("sample" = "sic_combined"), how = "left",
                     validate = "1:1")

mu_ind_race = weighted.mean(ind_var_race$Estimate, ind_var_race$nfirms)
N_firms = sum(ind_var_race$nfirms)

plug <- weighted.mean(ind_var_race$Estimate^2, w = ind_var_race$nfirms) - mu_ind_race^2

correction <- sum(ind_var_race$`Std. Error`^2 * (ind_var_race$nfirms * (N_firms - ind_var_race$nfirms))) / N_firms^2

est <- plug - correction
sd_industry_race <- sqrt(est)

# Industry Variance Gender --------------------------------------------------
ind_var_gender <- kline[, .(mean_cb = mean(cb)), by = .(sic_combined, job_id, gender, firm_id)]
ind_var_gender <- dcast(ind_var_gender, sic_combined + firm_id + job_id ~ gender, value.var = "mean_cb")
ind_var_gender[, thetahat_jf := Female - Male]
ind_var_gender <- ind_var_gender[!is.na(thetahat_jf)]

ind_var_gender[, njobs := .N, by = firm_id]
ind_var_gender[, fweight := 1 / njobs]

ind_var_gender <- feols(thetahat_jf ~ i(sic_combined) - 1, ind_var_gender,
                        weights = ~fweight, split = ~sic_combined, vcov = "HC1")
ind_var_gender <- coeftable(ind_var_gender)

ind_var_gender <- join(ind_var_gender, nfirms, on = c("sample" = "sic_combined"), how = "left",
                       validate = "1:1")

mu_ind_gender = weighted.mean(ind_var_gender$Estimate, ind_var_gender$nfirms)
N_firms = sum(ind_var_gender$nfirms)

plug <- weighted.mean(ind_var_gender$Estimate^2, w = ind_var_gender$nfirms) - mu_ind_gender^2

correction <- sum(ind_var_gender$`Std. Error`^2 * (ind_var_gender$nfirms * (N_firms - ind_var_gender$nfirms))) / N_firms^2

est <- plug - correction
sd_industry_gender <- sqrt(est)


# Plot Distributions ------------------------------------------------------
plot_distr(firm_var_gender$Estimate)
plot_distr(firm_var_race$Estimate)
plot_distr(ind_var_gender$Estimate)
plot_distr(ind_var_race$Estimate)

tikz(here("bld", "figures", "firm_cg_gender.tex"), width = 5, height = 4, sanitize = TRUE)
par(mar = c(4.1, 4.1, 1, 1))
hist(firm_var_gender$Estimate, breaks = 30, xlim = c(-0.15, 0.2),
     xlab = "Contact Gap", main = NULL)
text(-0.15, 20, paste0("SD: ", round(sigma_firm_gender, digits = 4)), adj = 0)
box()
dev.off()

tikz(here("bld", "figures", "firm_cg_race.tex"), width = 5, height = 4, sanitize = TRUE)
par(mar = c(4.1, 4.1, 1, 1))
hist(firm_var_race$Estimate, breaks = 20, xlim = c(-0.15, 0.15),
     xlab = "Contact Gap", main = NULL)
text(-0.15, 20, paste0("SD: ", round(sigma_firm_race, digits = 4)), adj = 0)
box()
dev.off()

tikz(here("bld", "figures", "ind_cg_gender.tex"), width = 5, height = 4, sanitize = TRUE)
par(mar = c(4.1, 4.1, 1, 1))
hist(ind_var_gender$Estimate, breaks = 20, xlim = c(-0.1, 0.1),
     xlab = "Contact Gap", main = NULL)
text(-0.1, 3, paste0("SD: ", round(sd_industry_gender, digits = 4)), adj = 0)
box()
dev.off()

tikz(here("bld", "figures", "ind_cg_race.tex"), width = 5, height = 4, sanitize = TRUE)
par(mar = c(4.1, 4.1, 1, 1))
hist(ind_var_race$Estimate, breaks = 20, xlim = c(-0.06, 0.04),
     xlab = "Contact Gap", main = NULL)
text(-0.06, 4, paste0("SD: ", round(sd_industry_race, digits = 4)), adj = 0)
box()
dev.off()


# Save Data ---------------------------------------------------------------
write.csv(ind_var_gender, here("bld", "data", "ind_var_gender.csv"),
          row.names = FALSE)
