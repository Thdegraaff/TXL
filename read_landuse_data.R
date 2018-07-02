library("readxl")
library("dplyr")
library("readr")

data_1996 <- read_excel("data/src/landuse_gemeenten.xlsx", sheet = "1996")
data_2000 <- read_excel("data/src/landuse_gemeenten.xlsx", sheet = "2000")
data_2003 <- read_excel("data/src/landuse_gemeenten.xlsx", sheet = "2003")
data_2006 <- read_excel("data/src/landuse_gemeenten.xlsx", sheet = "2006")
data_2008 <- read_excel("data/src/landuse_gemeenten.xlsx", sheet = "2008")
data_2010 <- read_excel("data/src/landuse_gemeenten.xlsx", sheet = "2010")
data_2012 <- read_excel("data/src/landuse_gemeenten.xlsx", sheet = "2012")
data_2015 <- read_excel("data/src/landuse_gemeenten.xlsx", sheet = "2015")

lisa_jaar <- rep("1997", 404)
data_1997 <- cbind( data_1996[,1], lisa_jaar, 0.75*data_1996[,2:11] + 0.25*data_2000[,2:11] )
lisa_jaar <- rep("1998", 404)
data_1998 <- cbind( data_1996[,1], lisa_jaar, 0.50*data_1996[,2:11] + 0.50*data_2000[,2:11] )
lisa_jaar <- rep("1999", 404)
data_1999 <- cbind( data_1996[,1], lisa_jaar, 0.25*data_1996[,2:11] + 0.75*data_2000[,2:11] )

lisa_jaar <- rep("2001", 404)
data_2001 <- cbind( data_1996[,1], lisa_jaar, 0.66*data_2000[,2:11] + 0.34*data_2003[,2:11] )
lisa_jaar <- rep("2002", 404)
data_2002 <- cbind( data_1996[,1], lisa_jaar, 0.34*data_2000[,2:11] + 0.66*data_2003[,2:11] )

lisa_jaar <- rep("2004", 404)
data_2004 <- cbind( data_1996[,1], lisa_jaar, 0.66*data_2003[,2:11] + 0.34*data_2006[,2:11] )
lisa_jaar <- rep("2005", 404)
data_2005 <- cbind( data_1996[,1], lisa_jaar, 0.34*data_2003[,2:11] + 0.66*data_2006[,2:11] )

lisa_jaar <- rep("2007", 404)
data_2007 <- cbind( data_1996[,1], lisa_jaar, 0.5*data_2006[,2:11] + 0.5*data_2008[,2:11] )

lisa_jaar <- rep("2009", 404)
data_2009 <- cbind( data_1996[,1], lisa_jaar, 0.5*data_2008[,2:11] + 0.5*data_2010[,2:11] )

lisa_jaar <- rep("2011", 404)
data_2011 <- cbind( data_1996[,1], lisa_jaar, 0.5*data_2010[,2:11] + 0.5*data_2012[,2:11] )

lisa_jaar <- rep("2013", 404)
data_2013 <- cbind( data_1996[,1], lisa_jaar, 0.66*data_2012[,2:11] + 0.34*data_2015[,2:11] )
data_2014 <- cbind( data_1996[,1], 0.34*data_2012[,2:11] + 0.66*data_2015[,2:11] )

lisa_jaar <- rep("2016", 404)
data_2016 <- cbind( data_1996[,1], lisa_jaar, 0.66*data_2015[,2:11] + 0.34*data_2014[,2:11] )

lisa_jaar <- rep("1996", 404)
data_1996 <- cbind( data_1996[,1], lisa_jaar, data_1996[,2:11] )
lisa_jaar <- rep("2000", 404)
data_2000 <- cbind( data_2000[,1], lisa_jaar, data_2000[,2:11] )
lisa_jaar <- rep("2003", 404)
data_2003 <- cbind( data_2003[,1], lisa_jaar, data_2003[,2:11] )
lisa_jaar <- rep("2006", 404)
data_2006 <- cbind( data_2006[,1], lisa_jaar, data_2006[,2:11] )
lisa_jaar <- rep("2008", 404)
data_2008 <- cbind( data_2008[,1], lisa_jaar, data_2008[,2:11] )
lisa_jaar <- rep("2010", 404)
data_2010 <- cbind( data_2010[,1], lisa_jaar, data_2010[,2:11] )
lisa_jaar <- rep("2012", 404)
data_2012 <- cbind( data_2012[,1], lisa_jaar, data_2012[,2:11] )
lisa_jaar <- rep("2015", 404)
data_2015 <- cbind( data_2015[,1], lisa_jaar, data_2015[,2:11] )

lisa_jaar <- rep("2014", 404)
data_2014 <- cbind( data_2014[,1] , lisa_jaar, data_2014[,2:11] )
data_2014[,1] <- as.character(data_2014[,1]) 
colnames(data_2014)[which(names(data_2014) == "data_2014[, 1]")] <- "Gemeente"

df <- bind_rows( data_1996[1:403,] , data_1997[1:403,] , data_1998[1:403,] , data_1999[1:403,] , data_2000[1:403,] , 
                 data_2001[1:403,] , data_2002[1:403,] , data_2003[1:403,] , data_2004[1:403,] , data_2005[1:403,] , 
                 data_2006[1:403,] , data_2007[1:403,] , data_2008[1:403,] , data_2009[1:403,] , data_2010[1:403,] , 
                 data_2011[1:403,] , data_2012[1:403,] , data_2013[1:403,] , data_2014[1:403,] , data_2015[1:403,] , 
                 data_2016[1:403,] )

df <- df[,1:11]

write_csv(df, "./data/derived/landuse.csv", col_names = TRUE)
