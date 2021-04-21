print("International collaborations and quality of patents: Correlation plot RCA and importance of international collab")
# Last modified 21.04.2021 / CR
require(data.table)
require(ggplot2)
library(countrycode)
library(viridis)
library(reshape2)
library(fst)
library(dplyr)

rm(list = ls())

## Load data
coeff <- read.csv2("/scicore/home/weder/rutzer/innoscape/International_collaborations/plot_qual_rca/dat_coeff.csv", sep = ",")
rca   <- read.fst("/scicore/home/weder/rutzer/innoscape/International_collaborations/plot_qual_rca/agg_ctry_tech.fst")
rca   <- data.frame(rca)
rca_agg   <- rca %>% filter(p_year %in% seq(1990, 2015)) %>% group_by(Technology, Country) %>% summarize(rca_ctry_tech = mean(rca_ctry_tech, na.rm = T)) 
rca_agg   <- rca %>% filter(p_year == 2015)


## Merge both data together
plot_dat <- merge(coeff, rca_agg, by.x = c("ctry_name", "model"), by.y = c("Country", "Technology"))
plot_dat <- mutate(plot_dat, estimate = as.numeric(as.character(estimate)))

###########################
# Create correlation plot #
###########################
ggplot(data = filter(plot_dat, rca_ctry_tech > 0 & !(term %in% c("FI", "KR", "AT", "NO"))), aes(y = rca_ctry_tech, x = estimate, label = term)) +
geom_point() +
geom_smooth(method="lm",fullrange = F,formula =y~x, se = FALSE) +
geom_text(position = position_nudge(y = 0.1)) +
ylab("RCA") +
xlab("estimated coefficient")

summary(lm(rca_ctry_tech ~ model + estimate:model, data = plot_dat))
