
# If you don't have readr installed yet, uncomment and run the line below
# install.packages("readr")

library(readr)

temps <- read_csv("data/110-tavg-all-5-1895-2018.csv", skip=4)
head(temps)

# install.packages("lubridate")

# If you don't have lubridate installed yet uncomment the line below and run it
#install.packages("lubridate")

# NOTE: IF YOU GET AN ERROR ABOUT NOT HAVING A PACKAGE CALLED stringi
# UNCOMMENT AND RUN THE LINES BELOW IF YOU HAVE A WINDOWS MACHINE

#install.packages("glue", type="win.binary")
#install.packages("stringi", type="win.binary")
#install.packages("stringr", type="win.binary")
#install.packages("lubridate", type="win.binary")

# UNCOMMENT AND RUN THE LINES BELOW IF YOU HAVE A MAC MACHINE

#install.packages("glue", type="mac.binary")
#install.packages("stringi", type="mac.binary")
#install.packages("stringr", type="mac.binary")
#install.packages("lubridate", type="mac.binary")

library(lubridate)

# Converting the date into a date format R recognizes
# This requires using paste0() to add a day to the date, so 189501 turns into 18950101

temps$Date <- ymd(paste0(temps$Date, "01"))

# Extracting the year
temps$Year <- year(temps$Date)

# Extracting the month
temps$month <- month(temps$Date)
temps$month <- as.numeric(temps$month)
temps$month_label <- month(temps$Date, label=T)

# Creating a column with rounded numbers
temps$rounded_value <- round(temps$Value, digits=0)

# Turning the year into a factor so it'll chart easier
temps$Year <- as.factor(as.character(temps$Year))


head(temps)

# If you don't have readr installed yet, uncomment and run the line below
# install.packages("ggplot2")

library(ggplot2)

ggplot(temps, aes(x=month, y=Value, group=Year)) +
  geom_line(alpha=.5) +
  scale_x_continuous(breaks=seq(1,12,1), limits=c(1,12)) +
  theme_minimal() +
  labs(y="average temperature", title="Monthly temperature since 1895", caption="Source: NOAA")


# If you don't have readr installed yet, uncomment and run the line below
# install.packages("gghighlight")

library(gghighlight)

# adding some alpha to the line so there's some transparency
ggplot(temps, aes(x=month, y=Value, color=Year)) +
  geom_line(alpha=.8) +
  scale_x_continuous(breaks=seq(1,12,1), limits=c(1,12)) +
  theme_minimal() +
  labs(y="average temperature", title="Monthly temperature since 1895", caption="Source: NOAA") +
  # NEW CODE BELOW
  gghighlight(max(as.numeric(Year)), max_highlight = 4L)



## ggrepel


ages <- read_csv("data/ages.csv")

# We'll focus on the movies from the Toms

# If you don't have dplyr or stringr installed yet, uncomment and run the lines below
#install.packages("dplyr")
#install.packages("stringr")

library(dplyr)
library(stringr)

toms <- ages %>% 
  filter(str_detect(actor, "Tom"))

ggplot(data=toms,
       aes(x=actor_age,
           y=actress_age,
           color=actor,
           label=Movie)) +
  geom_point() +
  theme_minimal() +
  labs(y="Actress age", x="Actor age", title="Tom Hanks versus Tom Cruise ages",
       caption="Source; Vulture.com, IMDB") +
  geom_text() 

# if you don't have the ggrepel package installed yet, uncomment and
# run the line below
#install.packages("ggrepel")

library(ggrepel)

ggplot(data=toms,
       aes(x=actor_age,
           y=actress_age,
           color=actor,
           label=Movie)) +
  geom_point() +
  theme_minimal() +
  labs(y="Actress age", x="Actor age", title="Tom Hanks versus Tom Cruise ages",
       caption="Source; Vulture.com, IMDB") +
  geom_text_repel()

## ridgeplot

# If you don't have ggridges or viridis installed yet, uncomment and run the lines below
#install.packages("ggridges")
#install.packages("viridis")

library(ggridges)
library(viridis)

ggplot(temps, aes(x=rounded_value, y=month_label, fill = ..x..)) +
  geom_density_ridges_gradient(scale=3, rel_min_height = 0.01) +
  scale_fill_viridis(name="Temp. [F]", option="C") +
  labs(title="Average temperatures since 1895", y="", x="Temperature", 
       caption="Source: NOAA") +
  theme_minimal()

ggplot(ages, aes(x=actress_age, y=actor, fill = ..x..)) +
geom_density_ridges_gradient(scale=2, rel_min_height = 0.01) +
scale_fill_viridis(name="Actress age", option="C") +
labs(title="Distribution of actress ages for each actor", y="", x="Actress age",
caption="Source: Vulture.com, IMDB") +
theme_minimal()

## heatmap


anom <- read_csv("data/1880-2018.csv", skip=4)
head(anom)

anom$Date <- ymd(paste0(anom$Year, "01"))

# Extracting the year
anom$Year <- year(anom$Date)

# Extracting the month
anom$month <- month(anom$Date)
anom$month <- as.numeric(anom$month)
anom$month_label <- month(anom$Date, label=T)

# Turning the year into a factor so it'll chart easier
anom$Year <- as.factor(as.character(anom$Year))

library(forcats)

anom <- anom %>% 
  mutate(Year=fct_rev(factor(Year)))

ggplot(anom, aes(y=Year, x=month_label, fill=Value)) +
  geom_tile(color="white", width=.9, height=1.1) +
  theme_minimal() +
  scale_fill_gradient2(midpoint=0, low="blue", high="red", limits=c(-1.3, 1.3)) +
  labs(title="Global Land and Ocean Temperature Anomalies", x="", y="", 
       caption="Source: NOAA", fill="Anomaly") +
  scale_x_discrete(position = "top") +
  theme(legend.position="top")

## Mr. Rogers


# This code is pretty much from Owen Phillips

rogers <- read_csv("data/mrrogers.csv")
rogers$episodenumbers <- as.factor(rogers$episodenumbers)
rogers$colorcodes <- as.factor(rogers$colorcodes)

cn <- levels(rogers$colorcodes)


na.omit(rogers) %>% ggplot(aes(x=episodenumbers)) + 
  geom_bar(aes(fill = factor(colorcodes))) + 
  scale_fill_manual(values = cn) + theme_minimal() + 
  labs(fill = "", x= "", 
       title = "Mister Rogers' Cardigans of Many Colors", 
       subtitle = " ", 
       caption = "") +
  guides(fill=guide_legend(ncol=2)) + 
  scale_x_discrete(breaks = c(1466, 1761),
                   labels = c("1979", "2001")) + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) + 
  theme(legend.position = "none") + ylim(0,1)

## Time is a flat circle


# uncomment and run the lines below to install the right packages

#install.packages("ggbeeswarm")
#devtools::install_github("AliciaSchep/gglabeller") 
#install.packages("scales")
#install.packages("readr")

library(ggbeeswarm)
library(gglabeller)
library(scales)

library(readr)

l <- "https://github.com/washingtonpost/data-school-shootings/raw/master/school-shootings-data.csv"
data <- read_csv(l, 
                 col_types = cols(date = col_date(format = "%m/%d/%Y")))

theeeeme <- theme_minimal() + theme(
  axis.text.y=element_blank(), 
  axis.ticks=element_blank(), 
  axis.line.y=element_blank(),
  panel.grid=element_blank())

ggplot(data, aes(date, 0, size=enrollment)) +
  geom_beeswarm(method = "frowney", groupOnX=FALSE, dodge.width=0.5, alpha=0.5) +
  scale_size_area(max_size = 8) +
  scale_x_date(date_breaks="1 year", labels=date_format("%Y"),
               limits=as.Date(c('1998-04-01', '2023-04-01'))) +
  scale_y_continuous(limits=c(-20, 10)) +
  coord_polar(direction = -1) +
  theeeeme
