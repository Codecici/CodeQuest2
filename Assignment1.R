# Assignment 1

# On the web page:https://www.drroyspencer.com/latest-global-temperatures/
  
# Download the four data series, 
# i.e., temperature measurements at the four different atmospheric locations: 
# the Lower-Troposphere, Mid-Troposphere, Tropopause, and Lower-Stratosphere. 
# Each dataset is found using its unique URL. 

rm (list = ls())
library(tidyverse)
library(zoo)
library(ggplot2)
library(janitor)
library(lubridate)

# Download the data serie for the Lower-Troposphere
df_lowert <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.1/tlt/uahncdc_lt_6.1.txt")
head(df_lowert)
tail(df_lowert)
View(df_lowert)

# comments starts on $Year equal to "Year", write code that is dynamic
df_lowert$Year
which(df_lowert$Year %in% "Year")
# dynamic select
df_lowert <- df_lowert[1:which(df_lowert$Year %in% "Year")-1, ]
# This works!
tail(df_lowert)

# Use only the Global temperature from each dataset (i.e., the third data column). 
# Rename Globe variable
df_lowert <- df_lowert %>% 
  select(Year, Mo, Globe) %>% 
  rename(Globe_lowert = Globe)
df_lowert


# Download the data serie for the Mid-Troposphere
df_mid <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.1/tmt/uahncdc_mt_6.1.txt")
head(df_mid)
tail(df_mid)
View(df_mid)

# comments starts on $Year equal to "Year", write code that is dynamic
df_mid$Year
which(df_mid$Year %in% "Year")
# dynamic select
df_mid <- df_mid[1:which(df_mid$Year %in% "Year")-1, ]
# This works!
tail(df_mid)

# Use only the Global temperature from each dataset (i.e., the third data column). 
# Rename Globe variable
df_mid <- df_mid %>% 
  select(Year, Mo, Globe) %>% 
  rename(Globe_mid = Globe)
df_mid


# Download the data serie for the Troposhere
df_tropopause <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.1/ttp/uahncdc_tp_6.1.txt")
head(df_tropopause)
tail(df_tropopause)
View(df_tropopause)

# comments starts on $Year equal to "Year", write code that is dynamic
df_tropopause$Year
which(df_tropopause$Year %in% "Year")
# dynamic select
df_tropopause <- df_tropopause[1:which(df_tropopause$Year %in% "Year")-1, ]
# This works!
tail(df_tropopause)

# Use only the Global temperature from each dataset (i.e., the third data column). 
# Rename Globe variable
df_tropopause <- df_tropopause %>% 
  select(Year, Mo, Globe) %>% 
  rename(Globe_tropopause = Globe)
df_tropopause

# Download the data serie for the Lower-Stratophere
df_lowers <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.1/tls/uahncdc_ls_6.1.txt")
head(df_lowers)
tail(df_lowers)
View(df_lowers)

# comments starts on $Year equal to "Year", write code that is dynamic
df_lowers$Year
which(df_lowers$Year %in% "Year")
# dynamic select
df_lowers <- df_lowers[1:which(df_lowers$Year %in% "Year")-1, ]
# This works!
tail(df_lowers)

# Use only the Global temperature from each dataset (i.e., the third data column). 
# Rename Globe variable
df_lowers <- df_lowers %>% 
  select(Year, Mo, Globe) %>% 
  rename(Globe_lowers = Globe)
df_lowers


# Combined four the four data series
df_tidy <- df_lowert %>%
  left_join(df_mid, by = c("Year", "Mo")) %>%
  left_join(df_tropopause, by = c("Year", "Mo")) %>%
  left_join(df_lowers, by = c("Year", "Mo"))
df_tidy


# Creat a Data variable, and truning a charater into a numeric
df_tidy <- df_tidy %>% 
  mutate(Date = ymd(paste(Year, Mo, 1, sep = "-"))) %>% 
  mutate(Year = year(Date),
         Month = month(Date)) %>% 
  select(Year, Month, Date, Globe_lowert, Globe_mid, Globe_tropopause, Globe_lowers) %>% 
  mutate_if(is.character, as.numeric)
df_tidy


# Calculate the 12-month(right-aligned) moving average 
df_tidy <- df_tidy %>% 
  mutate(moving_lowert=zoo::rollmean(Globe_lowert, 12, fill=NA, align = "right")) %>% 
  mutate(moving_mid=zoo::rollmean(Globe_mid, 12, fill=NA, align = "right")) %>% 
  mutate(moving_troposphere=zoo::rollmean(Globe_tropopause, 12, fill=NA, align = "right")) %>% 
  mutate(moving_lowers=zoo::rollmean(Globe_lowers, 12, fill=NA, align = "right")) %>% 
  select(Year, Month, Date, moving_lowert, moving_mid, moving_troposphere, moving_lowers)
df_tidy


# Summarise the four 12-month moving average, using group_by 
df_tidy_summarise <- df_tidy %>% 
  group_by(Date) %>% 
  summarise(Average=mean(c(moving_lowert, moving_mid, moving_troposphere, moving_lowers))) 
df_tidy_summarise


# Use mutate fun instead of summarise, calculate a new variable called Average 
df_tidy <- df_tidy %>% 
  group_by(Date) %>% 
  mutate(Average=mean(c(moving_lowert, moving_mid, moving_troposphere, moving_lowers)))
df_tidy


# Filter data from January 1980 onwards
figur <- df_tidy %>% 
  filter(Year >= 1980) %>% 
  group_by(Date) %>% 
  rename( `Lower Troposphere` = moving_lowert,
          `Mid Troposphere` = moving_mid,
          Tropopause = moving_troposphere,
          `Lower Stratosphere` = moving_lowers) %>%   
  select(Date,`Lower Troposphere`, `Mid Troposphere`, Tropopause, `Lower Stratosphere`, Average) %>% 
  pivot_longer(-Date, names_to = "Location", values_to = "Temperature") %>% 
  ggplot(aes(x = Date, y = Temperature,  color = Location)) +
  geom_line(size = 0.5) +
  labs(title = "The four 12-month moving averages of Atmospheric, and their average",
       subtitle = "From 1980-01-01 to 2024-12-01",
       x = "Date",
       y = "Temperature",
       color = "Location") +
  theme_minimal() +
  theme(legend.position = "bottom")
figur


