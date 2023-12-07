#Install and load packages
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
library(tidyverse)
library(nflfastR)
library(ggimage)

#Load in logos
nfllogos <- read_csv("/Users/dylanwilkerson/Documents/CSV:XLSX/NFL Logos.csv")

#Load in data
data <- load_pbp(2023)

#Filter data for second half
secondhalf <- data %>%
  filter(qtr > 2)

#Drop NA's and get offense
secondhalfepaoff <- secondhalf %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(posteam) %>%
  summarise(total_epaoff = sum(epa))

#Drop NA's and get defense
secondhalfepadef <- secondhalf %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(defteam) %>%
  summarise(total_epadef = sum(epa))

#Rename columns
colnames(secondhalfepaoff)[1] = "team"
colnames(secondhalfepadef)[1] = "team"

#Join offensive and defensive tables
secondhalfepa <- left_join(secondhalfepaoff, secondhalfepadef, by = "team")

#Join combined stats with Logos
final <- left_join(secondhalfepa, nfllogos, by = "team")

#Get mean EPA for graph
meanepa<-mean(final$total_epaoff)

#Create plot
secondhalfplot <- ggplot(final, aes(total_epaoff, total_epadef)) +
  geom_image(aes(image = logo), size = .1) +
  geom_hline(yintercept = meanepa) +
  geom_vline(xintercept = meanepa) +
  labs(title = "EPA Earned/Allowed in the second Half",
       subtitle = "Created by Dylan Wilkerson/@wilkersonadylan",
       x = "Total EPA Gained on Offense",
       y = "Total EPA Allowed on Defense") +
  theme_light() +
  geom_text(aes(x=-75, y=40, label="Bad Offense, Bad Defense"),
            size = 5) +
  geom_text(aes(x=-75, y=-30, label="Bad Offense, Good Defense"),
            size = 5) +
  geom_text(aes(x= 40, y=-47, label="Good Offense, Good Defense"),
            size = 5) +
  geom_text(aes(x= 25, y=25, label="Good Offense, Bad Defense"),
            size = 5)

#Print plot
secondhalfplot
