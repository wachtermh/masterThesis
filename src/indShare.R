# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Path --------------------------------------------------------------------
here::i_am("src/indShare.R")

# Packages ----------------------------------------------------------------
library(readxl)
library(plyr)
library(dplyr)
library(data.table)
library(collapse)
library(here)
library(readstata13)

# Load Data --------------------------------------------------------
# Import EEO1 Table
eeo1 <- read_excel(here::here("src", "data", "EEO1 2021 PUF.xlsx"), 
                   col_types = "text")
setDT(eeo1)

# Import Kline et al. 2022
kline <- read.dta13(file = here::here("src", "data", "Kline2022.dta"))
setDT(kline)

# Import NAICS to SIC Crosswalk 
NAICS6_to_SIC4 <- read.dta13(here::here("src", "data", "NAICS6_to_SIC4.dta"))

# Import SIC to SIC_Kline
SIC_SIC_Kline <- read.csv(here::here("src", "data", "SIC_SIC_Kline.csv"))
setDT(SIC_SIC_Kline)

# Function for Crosswalk --------------------------------------------------
# Create Crosswalk from NAICS to SIC
Build_a_Bridge <- function(a,b) {
  NewCW <- NAICS6_to_SIC4[ ,c(1,2,5:7)]
  NewCW$NAICS <- substr(NewCW$NAICS_Code, 1,a)
  NewCW$SIC <- substr(NewCW$SIC4, 1,b)
  NewCW <- ddply(NewCW, .(NAICS,SIC), numcolwise(sum)) %>%
    ddply(.(NAICS), mutate, Est_Sum = sum(Establishments), Emp_Sum = sum(Employees), Pay_Sum = sum(Annual_Payroll))
  NewCW$Est_weight <- round(NewCW$Establishments/NewCW$Est_Sum, 2)
  NewCW$Emp_weight <- round(NewCW$Employees/NewCW$Emp_Sum, 2)
  NewCW$Pay_weight <- round(NewCW$Annual_Payroll/NewCW$Pay_Sum, 2)
  CustomCW <- NewCW[ ,c(1:5,9:11)]
}


# Clean Data --------------------------------------------------------------
eeo1 <- eeo1[!is.na(NAICS3) & is.na(CBSA) & !is.na(NAICS2)]
charCols <- c("Nation", "Region", "Division", "State", "CBSA", "County", "NAICS2",
              "NAICS2_Name", "NAICS3", "NAICS3_Name")
cols <- setdiff(colnames(eeo1), charCols)
eeo1 <- eeo1[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

# Create NAICS3 to SIC2 Crosswalk -----------------------------------------
# NAICS3 to SIC2
naics3_sic2 <- Build_a_Bridge(3, 2)
setDT(naics3_sic2)


# Aggregate ---------------------------------------------------------------
eeo1 <- join(eeo1, naics3_sic2, on = c("NAICS3" = "NAICS"), how = "left", multiple = TRUE)
eeo1 <- join(eeo1, SIC_SIC_Kline, on = "SIC", how = "left", multiple = TRUE)
eeo1 <- eeo1[!is.na(SIC_Kline)]
eeo1 <- eeo1[, lapply(.SD, sum, na.rm = TRUE), by = SIC_Kline, .SDcols = cols]

# Calculate female share
eeo1[, pct_female := FT10 / TOTAL10]
# Calculate black share
eeo1[, pct_black:= BLKT10 / TOTAL10]
# Calculate female share of managers
eeo1[, pct_fem_manag := FT1 / TOTAL1]
# Calculate male share of managers
eeo1[, pct_male_manag := MT1 / TOTAL1]

# Save data ---------------------------------------------------------------
write.csv(eeo1, here::here("bld", "data", "ind_pct.csv"),
          row.names = FALSE)