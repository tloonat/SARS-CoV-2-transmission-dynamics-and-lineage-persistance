library(ggplot2)
library(ape)
library(repr)
library("readxl")
library('gridExtra')
#library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library("ggsci")
library(ggalt)
library("Hmisc")
library("scales")
library(ggpattern)
require(tidyverse)
library("readxl")
library("lubridate")
require("ggalt")
library("wesanderson")



#################################################
###### READING ALL DATA  ########################
#################################################
data2<-read_excel('~/Downloads/Data Visualisation/Fig1 Data/Updated_SA_all data_15Dec21_updated.xlsx')
data2$Nextstrain_variants1<-factor(data2$Nextstrain_variants1,levels = c("Other Lineages","20H (Beta, V2)","Delta", 'Delta (AY.45)'))


data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Other Lineages","20H (Beta, V2)","Delta", "Delta (AY.45"))
data2$division<-factor(data2$division,levels = c("Mpumalanga","North West","Northern Cape","Free State","Eastern Cape","Limpopo","Western Cape","Gauteng","KwaZulu-Natal"))

data2$days<-as.Date(cut(data2$date,breaks = "day",start.on.monday = FALSE))
data2$date<-as.Date(cut(data2$date,breaks = "week",start.on.monday = FALSE))
data2$date2<-as.Date(cut(data2$date,breaks = "2 week",start.on.monday = FALSE))
data2$date4<-as.Date(cut(data2$date,breaks = "1 month",start.on.monday = FALSE))
data2<- data2 %>% filter(division!="South Africa")




EC_df<-subset(data2,division=='Eastern Cape')
KZN_df<-subset(data2,division=='KwaZulu-Natal')
WC_df<-subset(data2,division=='Western Cape')
GP_df<-subset(data2, division=='Gauteng')
FS_df<-subset(data2, division=='Free State')
LP_df<-subset(data2, division=='Limpopo')
MP_df<-subset(data2, division=='Mpumalanga')
NC_df<-subset(data2, division=='Northern Cape')
NW_df<-subset(data2, division=='North West')




library (readr)

urlfile="https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv"

mydata<-read_csv(url(urlfile))
provincial_cases<-mydata
provincial_cases$days<-as.Date(cut(as.Date(provincial_cases$date,format='%d-%m-%Y'),
                                   breaks = "day",
                                   start.on.monday = FALSE))

provincial_cases$date<-as.Date(cut(as.Date(provincial_cases$date,format='%d-%m-%Y'),
                                   breaks = "week",
                                   start.on.monday = FALSE))

provincial_cases <- within(provincial_cases,GP_daily <- ave(GP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,KZN_daily <- ave(KZN,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,WC_daily <- ave(WC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,EC_daily <- ave(EC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,LP_daily <- ave(LP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,MP_daily <- ave(MP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,NC_daily <- ave(NC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,NW_daily <- ave(NW,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,FS_daily <- ave(FS,FUN = function(x) c(x[1],diff(x))))

provincial_cases <- within(provincial_cases,total_daily <- ave(total,FUN = function(x) c(x[1],diff(x))))


###Fixing artefact in case numbers for 23rd Nov 2021
provincial_cases[provincial_cases$days=="2021-11-23", "GP_daily"] <- 1823
provincial_cases[provincial_cases$days=="2021-11-23", "EC_daily"] <- 22
provincial_cases[provincial_cases$days=="2021-11-23", "FS_daily"] <- 24
provincial_cases[provincial_cases$days=="2021-11-23", "KZN_daily"] <- 62
provincial_cases[provincial_cases$days=="2021-11-23", "LP_daily"] <- 44
provincial_cases[provincial_cases$days=="2021-11-23", "MP_daily"] <- 57
provincial_cases[provincial_cases$days=="2021-11-23", "NW_daily"] <- 98
provincial_cases[provincial_cases$days=="2021-11-23", "NC_daily"] <- 26
provincial_cases[provincial_cases$days=="2021-11-23", "WC_daily"] <- 74
provincial_cases[provincial_cases$days=="2021-11-23", "total_daily"] <- 2230


provincial_cases[provincial_cases$days=="2021-12-12", "total_daily"] <- 15000


library(tidyverse)
library(zoo)

rollspan <- 7 # span of rolling average, in days.

provincial_cases <- provincial_cases %>% 
  dplyr::mutate(GP_daily_7day = zoo::rollmean(GP_daily, k = 7, fill = NA),
                KZN_daily_7day = zoo::rollmean(KZN_daily, k = 7, fill = NA),
                WC_daily_7day = zoo::rollmean(WC_daily, k = 7, fill = NA),
                EC_daily_7day = zoo::rollmean(EC_daily, k = 7, fill = NA),
                LP_daily_7day = zoo::rollmean(LP_daily, k = 7, fill = NA),
                MP_daily_7day = zoo::rollmean(MP_daily, k = 7, fill = NA),
                NC_daily_7day = zoo::rollmean(NC_daily, k = 7, fill = NA),
                total_daily_7day = zoo::rollmean(total_daily, k = 7, fill = NA)
                
  ) %>% 
  dplyr::ungroup()



prop.table(table(data2$days, data2$Nextstrain_variants))
data2$division<-factor(data2$division,levels = c("Mpumalanga","North West","Northern Cape","Free State","Eastern Cape","Limpopo","Western Cape","Gauteng","KwaZulu-Natal"))

data2$days<-as.Date(cut(data2$date,breaks = "day",start.on.monday = FALSE))
data2$date<-as.Date(cut(data2$date,breaks = "week",start.on.monday = FALSE))
data2$date2<-as.Date(cut(data2$date,breaks = "2 week",start.on.monday = FALSE))
data2$date4<-as.Date(cut(data2$date,breaks = "1 month",start.on.monday = FALSE))
data2<- data2 %>% filter(division!="South Africa")
data2<- subset(data2, !is.na(division))

P <- prop.table(table(data2$days, data2$Nextstrain_variants1), margin=1)

temp<-as.data.frame(P)
names(temp)[1] <- 'days'
temp <- temp %>% 
  dplyr::mutate(Freq_7day = zoo::rollmean(Freq, k = 10, fill = NA)
  ) %>% 
  dplyr::ungroup()
head(temp)

temp2<-provincial_cases[c("days","total_daily_7day")]
head(temp2)

library(plyr)
temp3<-join(temp, temp2,
            type = "left")

tail(temp3)

temp3$days<-as.Date(cut(as.Date(temp3$days,format='%Y-%m-%d'),
                        breaks = "day",
                        start.on.monday = FALSE))


# Create empty data frame
extra.data <- data.frame()

# Populate the data frame using a for loop
for (i in seq(as.Date("2021/12/07"), by = "day", length.out = 9)) {
  # Get the row data
  days <- as.Date(i)
  total_daily_7day <- subset(temp2,days==as.Date(i))$total_daily_7day
  
  
  # Populate the row
  extra.data <- data.frame()
  for (i in seq(as.Date("2021/12/07"), by = "day", length.out = 9)) {
    days <- as.Date(i)
    total_daily_7day <- subset(temp2, days == as.Date(i))$total_daily_7day
    
  new.row1 <- data.frame(days = days, Var2 = "Other Lineages", Freq=1,Freq_7day = 9.939394e-01,total_daily_7day=total_daily_7day)
  new.row2 <- data.frame(days = days, Var2 = "Delta",  Freq=0,Freq_7day = 3.030303e-03,total_daily_7day=total_daily_7day)
  new.row3 <- data.frame(days = days, Var2 = "20H (Beta, V2)",  Freq=0,Freq_7day = 3.030303e-03,total_daily_7day=total_daily_7day)
  new.row4 <- data.frame(days = days, Var2 = "Delta (AY.45)", Freq=0, Freq_7day = 0.024351080, total_daily_7day = total_daily_7day)
  
  # Add the row
  extra.data <- rbind(extra.data, new.row1)
  extra.data <- rbind(extra.data, new.row2)
  extra.data <- rbind(extra.data, new.row3)
  extra.data <- rbind(extra.data, new.row4)
}
# Print the data frame
extra.data

temp3 <- rbind(temp3, extra.data)

cutoff_date <- as.Date("2021-11-15")

# Filter the data to exclude everything after the cutoff date
temp3_filtered <- temp3[temp3$days <= cutoff_date, ]

p_Epi_SA <- ggplot() + 
  theme_minimal() + 
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") + 
  scale_fill_manual(values=c('grey80','#e9c46a','mediumseagreen','purple'), 
                    name='Variants', 
                    labels=c('Other Lineages', 'Beta (B.1.351)', 'Delta (B.1.617.2/AY.x)', 'Delta(AY.45)')) + 
  geom_density(data=temp3_filtered, aes(x = days, y = total_daily_7day * Freq_7day, fill = Var2), 
               stat="identity", linewidth=0.4, position='stack') + 
  ylab('Daily Cases\n(7-day Moving Average)') + 
  xlab('Date') + 
  scale_y_continuous(name = "Daily Cases\n(7-day Moving Average)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom", legend.text= element_text(size=8, family="Helvetica")) + 
  ggtitle('Epidemic and Variant Dynamics in South Africa')

# Print the plot
print(p_Epi_SA)




# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)

# Reading the Excel file
data2 <- read_excel('~/Downloads/Data Visualisation/Fig1 Data/Updated_SA_all data_15Dec21_updated.xlsx')

# Define lineages and provinces
data2$Nextstrain_variants1 <- factor(data2$Nextstrain_variants1, levels = c("Other Lineages", "20H (Beta, V2)", "Delta", "Delta (AY.45)"))
data2$division <- factor(data2$division, levels = c("Mpumalanga", "North West", "Northern Cape", "Free State", "Eastern Cape", "Limpopo", "Western Cape", "Gauteng", "KwaZulu-Natal"))

# Convert dates to 'days' format
data2$days <- as.Date(cut(data2$date, breaks = "day"))

# Filter out rows where 'division' is 'South Africa'
data2 <- data2 %>% filter(division != "South Africa")


Nextstrain_variants1_colours = c('grey80', '#e9c46a', 'mediumseagreen', 'purple')


cutoff_date <- as.Date("2021-11-15")

# Filter the data to exclude everything after the cutoff date
temp4_filtered <- temp3[temp3$days <= cutoff_date, ]


province_epi2 <-ggplot(data2, aes(x = days, y = division, fill = Nextstrain_variants1)) + 
  geom_point(position = position_jitter(width = 0.3, height = 0.3), shape = 21, 
             color = 'grey33', size = 2.5, alpha = 1) + 
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month") + 
  scale_fill_manual(values = Nextstrain_variants1_colours) +
  labs(title = "COVID-19 Lineage Sequences Across Provinces",
       x = "Time",
       y = "Provinces",
       fill = "Lineage") + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
print(province_epi2)


