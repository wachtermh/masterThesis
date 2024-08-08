# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Path --------------------------------------------------------------------
here::i_am("src/descriptiveTables.R")

# Packages ----------------------------------------------------------------
library(here)
library(gtsummary)
library(broom)
library(cardx)
library(collapse)
library(labelled)
library(readstata13)
library(fastDummies)
library(kableExtra)
library(fixest)
library(data.table)

# Load Data ---------------------------------------------------------------
kline <- read.dta13(file = here::here("src", "data", "Kline2022.dta"))
setDT(kline)

ind_pct <- read.csv(here::here("bld", "data", "ind_pct.csv"))
setDT(ind_pct)
setorder(ind_pct, SIC_Kline)

sic_name <- read.csv(here::here("src", "data", "sic_names.csv"))
setDT(sic_name)
setorder(sic_name, sic_combined)

ind_wage <- read.csv(here::here("bld", "data", "wage_est.csv"))
setDT(ind_wage)
setorder(ind_wage, SIC_Kline)

ind_var_gender <- read.csv(here("bld", "data", "ind_var_gender.csv"))
setDT(ind_var_gender)
setorder(ind_var_gender, sample)

sic_comp <- c("20", "23", "24--35", "42--47", "48", "49", "50", "51", "52",
              "53", "54", "55", "56", "57", "58", "59", "60--61", "62",
              "63--65", "70", "73", "75--76", "80", "87")

# Join SIC names to codes -------------------------------------------------
ind_pct <- join(ind_pct, sic_name, on = c("SIC_Kline" = "sic_combined"), 
                how = "inner", validate = "1:1")

ind_sum <- join(ind_pct, ind_wage, on = c("SIC_Kline" = "SIC_Kline"), 
                how = "inner", validate = "1:1")

ind_sum <- join(ind_sum, ind_var_gender, on = c("SIC_Kline" = "sample"), 
                how = "inner", validate = "1:1")

kline <- join(kline, ind_sum, on = c("sic_combined" = "SIC_Kline"),
              how = "left", validate = "m:1")

# Clean Data --------------------------------------------------------------
kline$race <- factor(kline$race, levels = c("White", "Black"))
kline$gender <- factor(kline$gender, levels = c("Male", "Female"))
kline$region4 <- factor(kline$region4,
                        levels = c("1", "2", "3", "4"),
                        labels = c("Northeast", "Midwest", "South", "West")
)
kline <- dummy_cols(kline, "region4", omit_colname_prefix = TRUE)
kline <- dummy_cols(kline, "wave")
kline$sic_combined <- as.character(kline$sic_combined)
kline$soc3 <- as.character(kline$soc3)

var_label(kline) <- list(state = "State", wave = "Wave",
                         black = "Black", white = "White", female = "Female",
                         over40 = "Over 40", lgbtq_club = "LGBTQ club member",
                         political_club = "Political club",
                         academic_club = "Academic club",
                         gender_neutral_pronouns = "Gender-neutral pronouns",
                         same_gender_pronouns = "Same-gender pronouns",
                         associates = "Associate degree",
                         cb = "Any contact in 30 days",
                         email_cb = "Email", call_cb = "Voicemail",
                         text_cb = "Text", any_cb_0_14 = "Any contact in 14 days",
                         any_cb_15_30 = "Any contact in 15-30 days",
                         wave_1 = "Wave 1", wave_2 = "Wave 2", wave_3 = "Wave 3",
                         wave_4 = "Wave 4", wave_5 = "Wave 5")

# Balance Table -----------------------------------------------------------
tab1_race <- kline |>
  select(
    race, female, over40, lgbtq_club, academic_club, political_club,
    gender_neutral_pronouns, same_gender_pronouns, associates, Northeast,
    Midwest, South, West, wave_1, wave_2, wave_3, wave_4, wave_5, cb,
    call_cb, email_cb, text_cb, any_cb_0_14, any_cb_15_30
  ) |>
  tbl_summary(
    by = race,
    statistic = list(all_continuous() ~ "{mean}"),
    type = list(all_categorical() ~ "continuous"),
    digits = list(all_continuous() ~ 3)
  ) |>
  add_difference(estimate_fun = list(all_continuous() ~ function(x) style_number(x, digits = 3))) |>
  add_significance_stars(
    pattern = "{estimate}{stars}",
    hide_p = TRUE,
    thresholds = c(0.01, 0.05, 0.1)
  ) |>
  modify_footnote(update = everything() ~ NA) |>
  modify_header(all_stat_cols() ~ "{level}",
                label ~ "",
                estimate = "{{Difference}}"
  )

tab1_race_balanced <- kline |>
  dplyr::filter(balanced == "1") |>
  select(
    race, female, over40, lgbtq_club, academic_club, political_club,
    gender_neutral_pronouns, same_gender_pronouns, associates, Northeast,
    Midwest, South, West, wave_1, wave_2, wave_3, wave_4, wave_5, cb,
    call_cb, email_cb, text_cb, any_cb_0_14, any_cb_15_30
  ) |>
  tbl_summary(
    by = race,
    statistic = list(all_continuous() ~ "{mean}"),
    type = list(all_categorical() ~ "continuous"),
    digits = list(all_continuous() ~ 3)
  ) |>
  add_difference(estimate_fun = list(all_continuous() ~ function(x) style_number(x, digits = 3))) |>
  add_significance_stars(
    pattern = "{estimate}{stars}",
    hide_p = TRUE,
    thresholds = c(0.01, 0.05, 0.1)
  ) |>
  modify_footnote(update = everything() ~ NA) |>
  modify_header(all_stat_cols() ~ "{level}",
                label ~ "",
                estimate = "{{Difference}}"
  )

tbl_merge(list(tab1_race, tab1_race_balanced),
          tab_spanner = c("Full Sample", "Balanced Sample")
) |>
  as_kable_extra(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "l", "l", "d", "l", "l", "d")
  ) |>
  kable_styling(latex_options = "scale_down") |>
  pack_rows("Résumé characteristics", 1, 8, bold = FALSE) |>
  pack_rows("Geographic distribution", 9, 12, bold = FALSE) |>
  pack_rows("Wave distribution", 13, 17, bold = FALSE) |>
  pack_rows("Contact rates", 18, 23, bold = FALSE) |>
  footnote(general = "Notes: Asterisks indicate significant differences from zero at the following levels: * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", threeparttable = TRUE, general_title = "", escape = FALSE) |>
  gsub(pattern = "\\\\\\{Difference\\\\\\}", replacement = "{Difference}") |>
  save_kable(here::here("bld", "tables", "balance_race.tex"))

tab1_gender <- kline |>
  select(
    gender, over40, lgbtq_club, academic_club, political_club,
    gender_neutral_pronouns, same_gender_pronouns, associates, Northeast,
    Midwest, South, West, wave_1, wave_2, wave_3, wave_4, wave_5, cb,
    call_cb, email_cb, text_cb, any_cb_0_14, any_cb_15_30
  ) |>
  tbl_summary(
    by = gender,
    statistic = list(all_continuous() ~ "{mean}"),
    type = list(all_categorical() ~ "continuous"),
    digits = list(all_continuous() ~ 3)
  ) |>
  add_difference(estimate_fun = list(all_continuous() ~ function(x) style_number(x, digits = 3))) |>
  add_significance_stars(
    pattern = "{estimate}{stars}",
    hide_p = TRUE,
    thresholds = c(0.01, 0.05, 0.1)
  ) |>
  modify_footnote(update = everything() ~ NA) |>
  modify_header(all_stat_cols() ~ "{level}",
                label = "",
                estimate = "{{Difference}}"
  )

tab1_gender_balanced <- kline |>
  dplyr::filter(balanced == "1") |>
  select(
    gender, over40, lgbtq_club, academic_club, political_club,
    gender_neutral_pronouns, same_gender_pronouns, associates, Northeast,
    Midwest, South, West, wave_1, wave_2, wave_3, wave_4, wave_5, cb,
    call_cb, email_cb, text_cb, any_cb_0_14, any_cb_15_30
  ) |>
  tbl_summary(
    by = gender,
    statistic = list(all_continuous() ~ "{mean}"),
    type = list(all_categorical() ~ "continuous"),
    digits = list(all_continuous() ~ 3)
  ) |>
  add_difference(estimate_fun = list(all_continuous() ~ function(x) style_number(x, digits = 3))) |>
  add_significance_stars(
    pattern = "{estimate}{stars}",
    hide_p = TRUE,
    thresholds = c(0.01, 0.05, 0.1)
  ) |>
  modify_footnote(update = everything() ~ NA) |>
  modify_header(all_stat_cols() ~ "{level}",
                label = "",
                estimate = "{{Difference}}"
  )

tbl_merge(list(tab1_gender, tab1_gender_balanced),
          tab_spanner = c("Full Sample", "Balanced Sample")
) |>
  as_kable_extra(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "d", "c", "c", "d"),
    caption = "Summary Statistics",
    label = "SummaryStatistics",
    linesep = ""
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |>
  pack_rows("Résumé characteristics", 1, 7, bold = FALSE) |>
  pack_rows("Geographic distribution", 8, 11, bold = FALSE) |>
  pack_rows("Wave distribution", 12, 16, bold = FALSE) |>
  pack_rows("Contact rates", 17, 22, bold = FALSE) |>
  footnote(general = "Notes: This table, adapted from \\\\textcite{Kline2022b}, reports summary statistics of the correspondace study. The full sample consists of all firms, while the balanced sample only includes those present in each wave. Asterisks indicate significant differences from zero at the following levels: * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", threeparttable = TRUE, general_title = "", escape = FALSE) |>
  gsub(pattern = "\\\\\\{Difference\\\\\\}", replacement = "{Difference}") |>
  save_kable(here::here("bld", "tables", "balance_gender.tex"))


# Summary Statistics Gender and Race --------------------------------------

tbl_merge(list(tab1_gender, tab1_race),
          tab_spanner = c("Gender", "Race")) |>
  as_kable_extra(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "l", "l", "d", "l", "l", "d")
  ) |>
  kable_styling(latex_options = "scale_down", font_size = 10) |>
  pack_rows("Résumé characteristics", 1, 8, bold = FALSE) |>
  pack_rows("Geographic distribution", 9, 12, bold = FALSE) |>
  pack_rows("Wave distribution", 13, 17, bold = FALSE) |>
  pack_rows("Contact rates", 18, 23, bold = FALSE) |>
  footnote(general = "Notes: Asterisks indicate significant differences from zero at the following levels: * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", threeparttable = TRUE, general_title = "", escape = FALSE) |>
  gsub(pattern = "\\\\\\{Difference\\\\\\}", replacement = "{Difference}") |>
  save_kable(here::here("bld", "tables", "balance_gender_race.tex"))

# SIC Heterogeneity ------------------------------------------------------
ind_pct_female <- as_tibble(ind_pct$pct_female)  
ind_pct_black <- as_tibble(ind_pct$pct_black)
ind_wage_gaps <- as_tibble(ind_wage$wage_gap) 
sic_name <- as_tibble(ind_pct$sic_name)
sic_comp <- as_tibble(sic_comp)

kline |>
  select(gender, sic_combined, cb) |>
  tbl_strata2(
    strata = sic_combined,
    ~ .x |>
      tbl_summary(
        by = gender,
        statistic = list(all_continuous() ~ "{mean}"),
        type = list(all_categorical() ~ "continuous"),
        digits = list(all_continuous() ~ 3),
        label = list(cb = .y)
      ) |>
      add_difference(estimate_fun = list(all_continuous() ~ function(x) style_number(x, digits = 3))) |>
      add_significance_stars(
        pattern = "{estimate}{stars}",
        hide_p = TRUE,
        thresholds = c(0.01, 0.05, 0.1)
      ) |>
      modify_footnote(update = everything() ~ NA),
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL),
    .quiet = TRUE
  ) |>
  modify_table_body(~ .x %>% dplyr::mutate(ind_pct_female, share_female = round(value, 3))) |>
  # modify_table_body(~ .x %>% dplyr::mutate(ind_wage_gaps, wage_gaps = round(value, 3))) |>
  modify_table_body(~ .x %>% dplyr::mutate(sic_name, sic_name = value)) |> 
  modify_table_body(~ .x %>% dplyr::mutate(sic_comp, sic_comp = value)) |> 
  modify_header(all_stat_cols() ~ "{level}",
                label = "SIC",
                estimate = "{{Difference}}",
                share_female = "Female Share",
                sic_name = "Industry",
                sic_comp = "Composition"
                # wage_gaps = "Wage gaps"
  ) |>
  as_kable_extra(
    format = "latex",
    booktabs = TRUE,
    align = c("c", "c", "c", "d", "c", "l", "c"),
    linesep = "",
    caption = "Difference in Callback by Industry",
    label = "DifferenceCallback"
  ) |>
  kable_styling(latex_options = c("scale_down", "hold_position")) |>
  footnote(general = "Notes: Female share denotes the share of female employees in a given industry. Asterisks indicate significant differences from zero at the following levels: * $p < 0.1$, ** $p < 0.05,$ *** $p < 0.01$.", threeparttable = TRUE, general_title = "", escape = FALSE) |>
  gsub(pattern = "\\\\\\{Difference\\\\\\}", replacement = "{Difference}") |>
  save_kable(here::here("bld", "tables", "sic_cb_table.tex"))

sic_het <- kline |>
  select(gender, sic_combined, cb) |>
  tbl_strata2(
    strata = sic_combined,
    ~ .x |>
      tbl_summary(
        by = gender,
        statistic = list(all_continuous() ~ "{mean}"),
        type = list(all_categorical() ~ "continuous"),
        digits = list(all_continuous() ~ 3),
        label = list(cb = .y)
      ) |>
      add_difference(estimate_fun = list(all_continuous() ~ function(x) style_number(x, digits = 3))) |>
      add_significance_stars(
        pattern = "{estimate}{stars}",
        hide_p = TRUE,
        thresholds = c(0.01, 0.05, 0.1)
      ) |>
      modify_footnote(update = everything() ~ NA),
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL),
    .quiet = TRUE
  ) |>
  modify_table_body(~ .x %>% dplyr::mutate(ind_pct_female, share_female = round(value, 3))) |> 
  modify_table_body(~ .x %>% dplyr::mutate(sic_name, sic_name = value)) |> 
  modify_header(all_stat_cols() ~ "{level}",
                label = "SIC",
                estimate = "{{Difference}}",
                share_female = "Female Share",
                sic_name = "Industry")
sic_het <- setDT(sic_het$table_body)
sic_het <- sic_het[, .(label, stat_1, stat_2, estimate)]
sic_het$Male <- as.numeric(sic_het$Male)
setnames(sic_het, c("label", "stat_1", "stat_2", "estimate"),
         c("sic_combined", "Male", "Female", "Difference"))
sic_het$Male <- as.numeric(sic_het$Male)
sic_het$Female <- as.numeric(sic_het$Female)
sic_het <- join(sic_het, ind_pct[, .(SIC_Kline, pct_female)], on = c("sic_combined" = "SIC_Kline"),
                how = "left", validate = "1:1")
setorder(sic_het, pct_female)
sic_het[, cbDiff := (Male - Female) / Female * 100]
sic_het[, pct_male := 1 - pct_female]
sic_het[, femShareDiff := (pct_male - pct_female) / pct_female * 100]

# Variation of Variables --------------------------------------------------
kline$soc3 <- as.numeric(kline$soc3)
kbl(setorder(kline[, .("Industries" = uniqueN(sic_combined), "Firms" = uniqueN(firm_id)), by = soc3], soc3),
    booktabs = TRUE, format = "latex",
    caption = "Variation of Industries and Firms Across Occupations",
    label = "VarIndFirmOcc", linesep = "",
    align = c("c", "c", "c"),
    col.names = c("Occupation", "{Industry}", "Firm")) |> 
  kable_styling(latex_options = "scale_down", font_size = 10) |> 
  footnote(general = "Notes: This table reports the number of unique industries and firms across occupations.",
           threeparttable = TRUE,
           general_title = "",
           escape = FALSE) |>
  add_header_above(c(" ", "Unique N" = 2)) |>
  gsub(pattern = "\\\\\\{Industry\\\\\\}", replacement = "{Industry}") |>
  save_kable(here::here("bld", "tables", "ind_firm_var_occ.tex"))

kbl(setorder(kline[, .("Industries" = uniqueN(sic_combined), "Firms" = uniqueN(firm_id)), by = state], state),
    booktabs = TRUE, format = "latex",
    caption = "Variation of Industries and Firms Across States",
    label = "VarIndFirmState", linesep = "",
    align = c("c", "c", "c"),
    col.names = c("State", "{Industry}", "Firm")) |> 
  kable_styling(latex_options = "scale_down", font_size = 10) |> 
  footnote(general = "Notes: This table reports the number of unique industries and firms across states.",
           threeparttable = TRUE,
           general_title = "",
           escape = FALSE) |>
  add_header_above(c(" ", "Unique N" = 2)) |> 
  gsub(pattern = "\\\\\\{Industry\\\\\\}", replacement = "{Industry}") |>
  save_kable(here::here("bld", "tables", "ind_firm_var_state.tex"))

kbl(setorder(kline[, .("Industries" = uniqueN(sic_combined), "Firms" = uniqueN(firm_id)), by = wave], wave),
    booktabs = TRUE, format = "latex",
    caption = "Variation of Industries and Firms Across Waves",
    label = "VarIndFirmWave", linesep = "",
    align = c("c", "c", "c"),
    col.names = c("Wave", "{Industry}", "Firm")) |> 
  kable_styling(latex_options = "scale_down", font_size = 10) |> 
  footnote(general = "Notes: This table reports the number of unique industries and firms across waves.",
           threeparttable = TRUE,
           general_title = "",
           escape = FALSE) |>
  add_header_above(c(" ", "Unique N" = 2)) |> 
  gsub(pattern = "\\\\\\{Industry\\\\\\}", replacement = "{Industry}") |>
  save_kable(here::here("bld", "tables", "ind_firm_var_wave.tex"))