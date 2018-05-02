##########################################################
# Read in needed packages
##########################################################

library("tidyverse")
#library("spse") # Needs to be installed separately
library("readxl")
library("texreg")

##########################################################
# Define constants
##########################################################

nr <- 431   # Number of regions

##########################################################
# Read in data 
##########################################################

data <- read_excel("./data/derived/data19982010V2.xlsx", 1)
logsum95 <- read_csv("./data/derived/matrix1995logsum.csv")
logsum04 <- read_csv("./data/derived/matrix2004Logsum.csv")
logsum10 <- read_csv("./data/derived/matrix2010logsum.csv")

##########################################################
# Create matrices from logsum dataframes
##########################################################

m95 <- as.matrix(logsum95[, 2: ncol(logsum95)])
m04 <- as.matrix(logsum04[, 2: ncol(logsum04)])
m10 <- as.matrix(logsum10[, 2: ncol(logsum10)])
m0  <- matrix(0, nr, nr)

##########################################################
# Create big matrix
##########################################################

m_1  <- cbind(m95, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_2  <- cbind(m0, m95, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_3  <- cbind(m0, m0, m95, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_4  <- cbind(m0, m0, m0, m95, m0, m0, m0, m0, m0, m0, m0, m0)
m_5  <- cbind(m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0, m0)
m_6  <- cbind(m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0)
m_7  <- cbind(m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0)
m_8  <- cbind(m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0)
m_9  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0)
m_10 <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m04, m0, m0)
m_11 <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m10, m0)
m_12 <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m10)

wmat <- rbind(m_1, m_2, m_3, m_4, m_5, m_6, m_7, m_8 ,m_9, 
              m_10, m_11, m_12)

##########################################################
# Create endogenous variables
##########################################################

endo <- dplyr::select(data, jaar, gemnr, Bev1565, starts_with("banen_"), -banen_blm1)
endo <- mutate_at(endo, vars(Bev1565:banen_blm7), funs(log(. / sum(.))))
endo_lhs <- endo %>% # just to be sure data is properly sorted
    arrange(jaar, gemnr) 

# take the first difference and calculate by year

endo_lhs_t <- filter(endo_lhs, jaar != 1998)
endo_lhs_t_1 <- filter(endo_lhs, jaar != 2010)
endo_lhs <- endo_lhs_t - endo_lhs_t_1

# Trime datasets
data <- filter(data, jaar != 2010)  
endo <- filter(endo, jaar != 2010)  

# select and rename variables
endo_lhs <- endo_lhs %>%
   dplyr::select(Bev1565, starts_with("banen_")) %>%
   rename(
     y_bev = Bev1565, 
     y_e2  = banen_blm2,
     y_e3  = banen_blm3,
     y_e4  = banen_blm4,
     y_e5  = banen_blm5,
     y_e6  = banen_blm6,
     y_e7  = banen_blm7
   )

endo <- endo %>%
  dplyr::select(Bev1565, starts_with("banen_")) %>%
  rename(
    x_bev = Bev1565, 
    x_e2  = banen_blm2,
    x_e3  = banen_blm3,
    x_e4  = banen_blm4,
    x_e5  = banen_blm5,
    x_e6  = banen_blm6,
    x_e7  = banen_blm7
  )

# bind datasets
data <- bind_cols(endo_lhs, endo, data)

# Finally, create weighted lags
data <- data %>%
  mutate(
    `IWN_bev_t-1` = Noord*log(wmat%*%exp(x_bev)),
    `IWN_blm2_t-1` = Noord*log(wmat%*%exp(x_e2)),
    `IWN_blm3_t-1` = Noord*log(wmat%*%exp(x_e3)),
    `IWN_blm4_t-1` = Noord*log(wmat%*%exp(x_e4)),
    `IWN_blm5_t-1` = Noord*log(wmat%*%exp(x_e5)),
    `IWN_blm6_t-1` = Noord*log(wmat%*%exp(x_e6)),
    `IWN_blm7_t-1` = Noord*log(wmat%*%exp(x_e7)),
    
    `IWZ_bev_t-1` = Zuid*log(wmat%*%exp(x_bev)),
    `IWZ_blm2_t-1` = Zuid*log(wmat%*%exp(x_e2)),
    `IWZ_blm3_t-1` = Zuid*log(wmat%*%exp(x_e3)),
    `IWZ_blm4_t-1` = Zuid*log(wmat%*%exp(x_e4)),
    `IWZ_blm5_t-1` = Zuid*log(wmat%*%exp(x_e5)),
    `IWZ_blm6_t-1` = Zuid*log(wmat%*%exp(x_e6)),
    `IWZ_blm7_t-1` = Zuid*log(wmat%*%exp(x_e7)),
    
    `IWO_bev_t-1` = Overig*log(wmat%*%exp(x_bev)),
    `IWO_blm2_t-1` = Overig*log(wmat%*%exp(x_e2)),
    `IWO_blm3_t-1` = Overig*log(wmat%*%exp(x_e3)),
    `IWO_blm4_t-1` = Overig*log(wmat%*%exp(x_e4)),
    `IWO_blm5_t-1` = Overig*log(wmat%*%exp(x_e5)),
    `IWO_blm6_t-1` = Overig*log(wmat%*%exp(x_e6)),
    `IWO_blm7_t-1` = Overig*log(wmat%*%exp(x_e7))
  )

##########################################################
# Create model specifications
##########################################################

eq1 <- y_bev ~ 0+x_bev+dwoningen + oppervlakte + jd24jaar + od65jaar +
              water + Schiphol + Delfzijl + Almere + Nverkeer + Zverkeer + Overkeer
eq2 <- y_e2 ~ 0+`IWN_bev_t-1` + `IWZ_bev_t-1` + `IWO_bev_t-1` + x_e2 + 
              landbouw + groen + Nverkeer + Zverkeer + Delfzijl + Haarlem + Almere + bedrterrein 
eq3 <- y_e3 ~ 0 + `IWN_bev_t-1`+ `IWN_blm2_t-1` + `IWZ_blm2_t-1` + `IWN_blm5_t-1` + `IWN_blm6_t-1` + `IWZ_blm6_t-1` + x_e3 +
              Zverkeer + Schiphol + Delfzijl + Groningen + Delft + Almere + Rotterdam + LQGroothandel
eq4 <- y_e4 ~ 0 + `IWN_bev_t-1` + `IWZ_bev_t-1` + `IWO_bev_t-1` + `IWO_blm2_t-1` + x_e4 +
              Nverkeer + Zverkeer + Overkeer + Schiphol + Delfzijl + Delft + Almere
eq5 <- y_e5 ~ 0+`IWN_bev_t-1` + `IWZ_bev_t-1` + `IWO_bev_t-1` +`IWN_blm4_t-1` + `IWZ_blm4_t-1` + `IWO_blm4_t-1` + x_e5 + 
              landbouw + groen + water + Schiphol + Delfzijl + Almere + Amsterdam + Rotterdam + LQHoreca
eq6 <- y_e6 ~ 0 + `IWN_bev_t-1` + `IWZ_bev_t-1` + `IWO_blm4_t-1` + `IWN_blm5_t-1`+ `IWO_blm5_t-1` + x_e6 + 
              water + Nbedrterrein + Delft + Amsterdam
eq7 <- y_e7 ~ 0 + `IWN_bev_t-1` + `IWZ_bev_t-1` + `IWO_bev_t-1` + x_e7 + 
              Schiphol + Delfzijl + Delft

##########################################################
# Do estimation
##########################################################

source("code/fgs3sls.R")
source("code/print_fgs3sls.R")
source("code/write_output.R")

formula <- list(tp1 = eq1, tp2 = eq2, tp3 = eq3, tp4 = eq4, tp5 = eq5, tp6 = eq6, tp7 = eq7)
sp_model <- fgs3sls(formula, data=data, w=wmat,
             lags=list(
                c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
                      ), 
             errors=list(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
             )
print_fgs3sls(sp_model)
mlist <- write_output(sp_model)
texreg(mlist, file = "output/reproduction.tex", single.row = FALSE, longtable = TRUE, use.packages = FALSE)
screenreg(mlist)
