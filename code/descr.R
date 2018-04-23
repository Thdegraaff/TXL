library("tidyverse")
library("ggthemes")

# Lees de lisa in

lisa <- read_csv(file = "./data/derived/lisa.csv")

# Maak eerst plots voor de totalen

lisa_tot <- lisa %>%
  group_by(lisa_jaar, sector_tigris) %>%
  summarise(
    banen = sum(banen)
  )

p_tot <- ggplot(lisa_tot, aes(lisa_jaar, banen)) +
  geom_line(size = 2) + 
  facet_wrap( ~ sector_tigris) +    
  xlab("Jaar") + 
  ylab("Totaal aantal banen") + 
  theme_economist() + scale_colour_economist()

pdf(file = "./figs/totaal_banen.pdf", width = 11, height = 10)
p_tot
dev.off()

# Then plot for growth figures

lisa <- lisa %>%
  group_by(sector_tigris, gem2014nr) %>%
  mutate(
    banen_lag1 = banen/(lag(banen,1)+0.001)-1
  ) %>%
  filter(lisa_jaar!=1996)

boxplot_banen <- ggplot(lisa, aes(lisa_jaar, banen_lag1)) +
  geom_boxplot(aes(group=lisa_jaar)) + 
  facet_wrap( ~ sector_tigris, scale ="free") +    
  xlab("Jaar") + 
  ylab("Gemiddelde jaarlijkse verandering (%)") + 
  theme_economist() + scale_colour_economist()

pdf(file = "./figs/boxplot_banen.pdf", width = 11, height = 10)
boxplot_banen
dev.off()

lisa_tot <- lisa %>%
  group_by(lisa_jaar, sector_tigris) %>%
  summarise(
    banen_lag1_mean = mean(banen_lag1, na.rm = TRUE),
    banen_lag1_sd = sd(banen_lag1, na.rm = TRUE)
  )

lisa_tot <- lisa_tot %>%
  filter(sector_tigris!='Onbekend') %>%
  filter(sector_tigris!='Landbouw')

change_mean <- ggplot(lisa_tot, aes(lisa_jaar, banen_lag1_mean)) +
  geom_line(size = 2) + 
  facet_wrap( ~ sector_tigris) +    
  xlab("Jaar") + 
  ylab("Gemiddelde jaarlijkse verandering (%)") + 
  theme_economist() + scale_colour_economist()

pdf(file = "./figs/gem_verandering_banen.pdf", width = 11, height = 10)
change_mean 
dev.off()

change_sd <- ggplot(lisa_tot, aes(lisa_jaar, banen_lag1_sd)) +
  geom_line(size = 2) + 
  facet_wrap( ~ sector_tigris) +    
  xlab("Jaar") + 
  ylab("Standaard deviatie jaarlijkse verandering (%)") + 
  theme_economist() + scale_colour_economist()

pdf(file = "./figs/sd_verandering_banen.pdf", width = 11, height = 10)
change_sd 
dev.off()

# Check for outliers

lisa_detailhandel <- lisa %>%
  filter(sector_tigris == "Detailhandel") %>%
  filter(lisa_jaar == 1997) %>%
  arrange(-banen_lag1)

lisa_detailhandel2 <- lisa %>%
  filter(sector_tigris == "Detailhandel") %>%
  filter(lisa_jaar == 2005) %>%
  arrange(-banen_lag1)

lisa_overheid <- lisa %>%
  filter(sector_tigris == "Overheid en onderwijs") %>%
  filter(lisa_jaar == 2000) %>%
  arrange(-banen_lag1)

lisa_overheid2 <- lisa %>%
  filter(sector_tigris == "Overheid en onderwijs") %>%
  filter(lisa_jaar == 2001) %>%
  arrange(-banen_lag1)




