# Assignment 2

# Task 1.

#You will find the data for these tasks at the following url:
  # https://raw.githubusercontent.com/uit-sok-1005-v23/uit-sok-1005-v23.github.io/main/storedata.csv.
# The data file contains 7185 observations and 9 variables.

rm(list=ls())

library(tidyverse)
library(lubridate)
library(janitor)

#last ned data
df <- read_csv("https://raw.githubusercontent.com/uit-sok-1005-v23/uit-sok-1005-v23.github.io/main/storedata.csv")

# 1: For the last 3 months of 2017, calculate the total Sales by month, for Region 1 and Region 9 in the Customer_Segment, Corporate, and Consumer. This output is Table 1.
Table1 <- df %>% 
  mutate(Year = year(Order_Date),
         Month = month(Order_Date)) %>%
  arrange(Year, Month) %>% 
  filter(Order_Date >= as.Date("2017-10-01"), 
         Region %in% c("Region 1", "Region 9"),
         Customer_Segment %in% c("Corporate", "Consumer")) %>% 
  group_by(Year, Month, Region, Customer_Segment) %>% 
  summarise(Total_Sales = sum(Sales))
Table1


# 2: Make a plot of the monthly total Sales in Region 1 and Region 13 in 2015, 2016, and 2017. This output is Figure 1.
Figure1 <- df %>% 
  mutate(Year = year(Order_Date),
         Month = month(Order_Date),
         Day = day(Order_Date)) %>%
  arrange(Year, Month, Day) %>% 
  filter(Year %in% c("2015", "2016", "2017"),
         Region %in% c("Region 1", "Region 13")) %>% 
  group_by(Order_Date, Region,) %>% 
  summarise(Total_Sales = sum(Sales)) %>% 
  ggplot(aes(x = Order_Date, y = Total_Sales, color = Region))+
  geom_line() +
  xlab("Year") + 
  ylab("Total Sales Amount") + 
  ggtitle("The monthly total sale in Region 1 and Region 13 in 2015, 2016, and 2017")+
  theme_minimal() +
  theme(legend.position = "bottom")
Figure1


# 3: In Figure 1, identify the months where the total Sales in Region 13 is greater than the total Sales in Region 1. This output is Table 2.
Table2 <-df %>% 
  mutate(Year = year(Order_Date),
         Month = month(Order_Date)) %>%
  arrange(Year, Month) %>% 
  filter(Year %in% c("2015", "2016", "2017"),
         Region %in% c("Region 1", "Region 13")) %>% 
  group_by(Year, Month, Region) %>% 
  summarise(Total_Sales = sum(Sales)) %>% 
  pivot_wider(names_from = Region, values_from = Total_Sales) %>%
  filter(`Region 13` > `Region 1`)
Table2
  

# 4: Find the average Profit per Customer_Segment and Product_Category in 2017, for all regions except Region 3, 5 and 8. What segment produced the highest average profit? This output is Table 3.
Table3 <- df %>% 
  mutate(Year = year(Order_Date)) %>%
  filter(Year == "2017",
         !Region %in% c("Region 3", "Region 5", "Region 8")) %>%
  group_by(Customer_Segment, Product_Category) %>%
  summarise(Average_Profit= mean(Profit)) %>% 
  pivot_wider(names_from = Product_Category, values_from = Average_Profit)
Table3


# Task 2.

library(tidyr)
library(dplyr)
library(rvest)
library(ggplot2)

url <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"

webpage <- read_html(url)

nodes <- html_nodes(webpage, "table")  
df_motor <- html_table(nodes[[1]])    

# Sjekk strukturen i dataene
str(df_motor)


# Sletter unnecessary clutter og rydder tabellen
colnames(df_motor) <- df_motor[1, ]
df_motor <- df_motor[-1, ]
head(df_motor)

df_motor <- df_motor %>% 
  separate(`WLTP-tall`, c("WLTP-tall", "vekk-wltp"), "k") %>% 
  separate(`STOPP`, c("STOPP", "vekk-stopp"), "k") %>% 
  separate(`Avvik`, c("Avvik", "vekk-avvik"), "%") %>% 
  select(-'vekk-wltp', -'vekk-stopp', -'vekk-avvik') 


df_motor[df_motor == "x"] <- NA
df_motor <- na.omit(df_motor)

df_motor <- df_motor %>% 
  rename("Model" = "Modell (temp. varierte fra 0° til -10°)",
         "WLTP" = "WLTP-tall") %>% 
  mutate(`WLTP`= as.numeric(WLTP),
         `STOPP`= as.numeric(STOPP))


# a)
figure1_motor <- df_motor %>% 
  ggplot(aes(x = WLTP, y = STOPP))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
  theme_minimal()+
  labs(title = "The relationship between the expected mileage(WLTP) and actual mileage (STOPP)",
       subtitle = "Alle punktene ligger under den røde linjen, noe som betyr at de faktiske rekkeviddene i testen er lavere enn de forventede rekkeviddene.",
       x = "Expected mileage/km that an electirc car is stated to have(WLTP) ",
       y = "Actual mileage/km in the Motor's Range test(STOPP)")+
  xlim(200,600)+
  ylim(200,600)
figure1_motor  



# b)
lm(STOPP ~ WLTP, data = df_motor)

figure2_motor <- df_motor %>%
  ggplot(aes(x = WLTP, y = STOPP))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
  geom_smooth(method = lm)+
  theme_minimal()+
  labs(title = "The relationship between the expected mileage(WLTP) and actual mileage (STOPP)",
       subtitle = "Alle punktene ligger under den røde linjen, noe som betyr at de faktiske rekkeviddene i testen er lavere enn de forventede rekkeviddene.",
       x = "Expected mileage/km that an electirc car is stated to have(WLTP) ",
       y = "Actual mileage/km in the Motor's Range test(STOPP)")+
  xlim(200,600)+
  ylim(200,600)
figure2_motor  

# Etter bruk av lm()-funksjonen får vi koeffisientene: -26.645 (intercept) og 0.8671 (WLTP), 
# som danner den blå linjen i figure2_motor. 
# Dette betyr at for hver kilometer forventet rekkevidde øker, øker den faktiske rekkevidden med 0.8671 km.
 
  
  
