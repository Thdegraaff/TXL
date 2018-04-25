##########################################################
# Read in needed packages
##########################################################

library("tidyverse")
#library("spse") # Needs to be installed separately
library("readxl")

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

# Finally bind datasets
data <- bind_cols(endo_lhs, endo, data)

##########################################################
# Create model specifications
##########################################################

eq1 <- y_bev ~ 0+x_bev+dwoningen + oppervlakte + jd24jaar + od65jaar +water + Schiphol + landbouw
eq2 <- y_e2 ~ 0+x_e2+oppervlakte + bedrterrein + water + landbouw
eq3 <- y_e3 ~ 0+x_e3+oppervlakte + bedrterrein + water + landbouw
eq4 <- y_e4 ~ 0+x_e4+oppervlakte + bedrterrein + water + landbouw 
eq5 <- y_e5 ~ 0+x_e5+oppervlakte + bedrterrein + water + landbouw 
eq6 <- y_e6 ~ 0+x_e6+oppervlakte + bedrterrein + water + landbouw 
eq7 <- y_e7 ~ 0+x_e7+oppervlakte + bedrterrein + water + landbouw 

##########################################################
# Do estimation
##########################################################

source("code/fgs3sls.R")

formula <- list(tp1 = eq1, tp2 = eq2, tp3 = eq3, tp4 = eq4, tp5 = eq5, tp6 = eq6, tp7 = eq7)
output <- fgs3sls(formula, data=data, w=wmat,
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

