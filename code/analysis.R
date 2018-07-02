##########################################################
# Read in needed packages
##########################################################

library("tidyverse")
library("readxl")
library("texreg")
source("code/fgs3sls.R")
source("code/print_fgs3sls.R")
source("code/write_output.R")

##########################################################
# Define constants
##########################################################

nr <- 403  # Number of regions

##########################################################
# Read in data 
##########################################################

lisa <- read_csv(file = "./data/derived/lisa.csv")
landuse <- read_csv(file = "./data/derived/landuse.csv")
lisa <- lisa %>% 
  select(gem2014nr:sector_tigris, banen) %>% # select what is needed per sector
  spread(sector_tigris, banen) %>% # from long to wide
  select(-Onbekend) # remove onbekend
lisa[is.na(lisa)] <- 1 # replace all na's in dataframe
  
  
logsum00 <- read_tsv("./data/src/logsums_muni_2000.asc", col_names = FALSE)
logsum04 <- read_tsv("./data/src/logsums_muni_2004.asc", col_names = FALSE)
logsum10 <- read_tsv("./data/src/logsums_muni_2010.asc", col_names = FALSE)
logsum14 <- read_tsv("./data/src/logsums_muni_2014.asc", col_names = FALSE)

##########################################################
# Merg Lisa data with land use data
##########################################################

landuse <- landuse %>%
  rename(gem2014nr = Gemeente)

lisa <- left_join(lisa, landuse, by = c("gem2014nr" = "gem2014nr", "lisa_jaar" = "lisa_jaar"))

##########################################################
# Fill in missing network pairs with -999; This one still has an error
##########################################################

fill_in_matrix <- function(dat, dat_g){
  dat_t <- dat[,1:2]
  dat_g <- dat_g[,1:2]
  dat_temp <- setdiff(dat_g, dat_t)
  print(dat_temp)
  dat_temp <- mutate(dat_temp, X3 = ifelse(X1 >0, -999, 0))
  print(dat_temp)
  dat <- bind_rows(dat, dat_temp)
  dat <- arrange(dat, X1, X2)
}

logsum00 <- fill_in_matrix(logsum00, logsum14)
logsum04 <- fill_in_matrix(logsum04, logsum14)
logsum10 <- fill_in_matrix(logsum10, logsum14)

##########################################################
# Create matrices from logsum dataframes
##########################################################

create_matrix <- function(dat) {
  dat <- spread(dat, X2, X3)
  as.matrix(dat[,2:(nr+1)])
}

m00 <- exp(create_matrix(logsum00))
m04 <- exp(create_matrix(logsum04))
m10 <- exp(create_matrix(logsum10))
m14 <- exp(create_matrix(logsum04))
m0  <- matrix(0, nr, nr)

rm(logsum00, logsum04, logsum10, logsum14)

##########################################################
# Create big matrix
##########################################################

m_97  <- cbind(m00, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_98  <- cbind(m0, m00, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_99  <- cbind(m0, m0, m00, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_00  <- cbind(m0, m0, m0, m00, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_01  <- cbind(m0, m0, m0, m0, m00, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_02  <- cbind(m0, m0, m0, m0, m0, m00, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_03  <- cbind(m0, m0, m0, m0, m0, m0, m00, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_04  <- cbind(m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_05  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_06  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_07  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0, m0, m0, m0)
m_08  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0, m0, m0)
m_09  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0, m0)
m_10  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m04, m0, m0, m0, m0, m0, m0)
m_11  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m10, m0, m0, m0, m0, m0)
m_12  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m10, m0, m0, m0, m0)
m_13  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m10, m0, m0, m0)
m_14  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m14, m0, m0)
m_15  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m14, m0)
m_16  <- cbind(m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m0, m14)

wmat <- rbind(m_97, m_98, m_99, m_00, m_01, m_02, m_03, m_04, m_05, m_06, m_07, m_08, m_09,
              m_10, m_11, m_12, m_13, m_14, m_15, m_16)

rm(m_97, m_98, m_99, m_00, m_01, m_02, m_03, m_04, m_05, m_06, m_07, m_08, m_09,
      m_10, m_11, m_12, m_13, m_14, m_15, m_16)

##########################################################
# Create endogenous variables
##########################################################

endo <- mutate_at(lisa, vars(Consumentendiensten:Zorg), funs(log(. / sum(.))))
endo_lhs <- endo %>% # just to be sure data is properly sorted
  arrange(lisa_jaar, gem2014nr) 

# take the first difference and calculate by year

endo_lhs_t <- filter(endo_lhs, lisa_jaar != 1996)
endo_lhs_t_1 <- filter(endo_lhs, lisa_jaar != 2016)
endo_lhs <- endo_lhs_t - endo_lhs_t_1

# Trime datasets
#data <- filter(data, lisa_jaar != 2016)  # data still has to be constructed
endo <- filter(endo, lisa_jaar != 2016)  

# select and rename variables
endo_lhs <- endo_lhs %>%
  dplyr::select(Consumentendiensten:Zorg) %>%
  rename(
    y_con  = Consumentendiensten, 
    y_det  = Detailhandel,
    y_fin  = 'Financiële en zakelijke dienstverlening',
    y_agr  = Landbouw,
    y_log  = Logistiek,
    y_ind  = 'Nijverheid en industrie',
    y_ove  = 'Overheid en onderwijs',
    y_zor  =  Zorg
  )

endo <- endo %>%
  dplyr::select(Consumentendiensten:Water) %>%
  rename(
    x_con  = Consumentendiensten, 
    x_det  = Detailhandel,
    x_fin  = 'Financiële en zakelijke dienstverlening',
    x_agr  = Landbouw,
    x_log  = Logistiek,
    x_ind  = 'Nijverheid en industrie',
    x_ove  = 'Overheid en onderwijs',
    x_zor  =  Zorg, 
    x_infra = Infrastructuur, 
    x_wonen = Wonen, 
    x_voorzieningen = Voorzieningen, 
    x_groen = 'sted. Groen',
    x_bedr = bedrijfsterrein, 
    x_nat = natuur, 
    x_glas = glastuinbouw, 
    x_landbouw = landbouw, 
    x_water = Water
  )

# bind datasets
data <- bind_cols(endo_lhs, endo)

Finally, create weighted lags
data <- data %>%
  mutate(
    `IW_con_t-1` = log(wmat%*%exp(x_con)),
    `IW_det_t-1` = log(wmat%*%exp(x_det)),
    `IW_fin_t-1` = log(wmat%*%exp(x_fin)),
    `IW_agr_t-1` = log(wmat%*%exp(x_agr)),
    `IW_log_t-1` = log(wmat%*%exp(x_log)),
    `IW_ind_t-1` = log(wmat%*%exp(x_ind)),
    `IW_ove_t-1` = log(wmat%*%exp(x_ove)),
    `IW_zor_t-1` = log(wmat%*%exp(x_zor))
  )

##########################################################
# Create model specifications
##########################################################

eq1 <- y_con ~ 1 + x_con + x_nat + x_water + x_wonen + x_infra#+ `IW_fin_t-1` + `IW_ove_t-1`
eq2 <- y_det ~ 1 + x_det + x_nat + x_water + x_wonen + x_infra#+ `IW_ind_t-1`
eq3 <- y_fin ~ 1 + x_fin + x_nat + x_water + x_wonen + x_infra#+ `IW_ind_t-1`
#eq4 <- y_agr ~ 0 + x_agr + `IW_det_t-1`
eq4 <- y_log ~ 1 + x_log + x_nat + x_water + x_wonen + x_infra#+ `IW_det_t-1`
eq5 <- y_ind ~ 1 + x_ind + x_nat + x_water + x_wonen + x_infra#+ `IW_ove_t-1`
eq6 <- y_ove ~ 1 + x_ove + x_nat + x_water + x_wonen + x_infra#+ `IW_zor_t-1`
eq7 <- y_zor ~ 1 + x_zor + x_nat + x_water + x_wonen + x_infra#+ `IW_ove_t-1`

##########################################################
# Do estimation
##########################################################

formula <- list(tp1 = eq1, tp2 = eq2, tp3 = eq3, tp4 = eq4, tp5 = eq5, tp6 = eq6, tp7 = eq7)
sp_model <- fgs3sls(formula, data=data, w=wmat,
                    lags=list(
                      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                      c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
                    ), 
                    errors=list(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
)
print_fgs3sls(sp_model)
mlist <- write_output(sp_model)
texreg(mlist, file = "output/first_run.tex", single.row = FALSE, longtable = TRUE, use.packages = FALSE, digits = 5)
screenreg(mlist)