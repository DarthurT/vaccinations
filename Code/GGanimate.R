#----------------------
#Load libraries
#----------------------

library(data.table)
library(funModeling)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(imputeTS)
library(tsoutliers)
library(lubridate)
library(zoo)
library(xts)
library(forecast)
library(timeSeries)
library(stringi)
library(tidyverse)
library(tibbletime)
library(anomalize)
library(tcltk)
library(readxl)
library(scales)
library(grid)
library(tidyverse)
library(gganimate)
library(gifski)
library(ggthemes)
library(ggrepel)
library(hrbrthemes)

#----------------------
#Data location
#----------------------

path <- "/data/"
setwd(path) #set directory to declared path

vac.data.name <- "ChildVaccStatBI_2018-19.csv"


#----------------------
#Import data
#----------------------
vac.data  <- read.csv(vac.data.name,
                              header = TRUE,
                              skip = 0,
                              na.strings ="NA",
                              stringsAsFactors = FALSE)


test <- vac.data %>% subset(vac.data$OrgType == "UK")



data.24m <- test %>% filter(grepl('24m', VacCode)) 
data.24m$Age <- "24 Months"

data.12m <- test %>% filter(grepl('12m', VacCode)) 
data.12m$Age <- "12 Months"

data.5y  <- test %>% filter(grepl('5y', VacCode)) 
data.5y$Age <- "5 years"

data.all <- rbind(data.24m, data.12m, data.5y)
data.all <-  data.all  %>% filter(!grepl('Cohort', VacCode)) 
#data.all <-  data.all  %>% filter(grepl('MMR|MenC', VacCode)) 

data.mean <- data.all %>%  group_by(Age,Year) %>% mutate(Mean = mean(Value, na.rm=TRUE))

data.mean.plot <-data.mean  %>%
  mutate(Year.clean = case_when(Year == "2009-10" ~ 2010,
                                Year == "2010-11" ~ 2011,
                                Year == "2011-12" ~ 2012,
                                Year == "2012-13" ~ 2013,
                                Year == "2013-14" ~ 2014,
                                Year == "2014-15" ~ 2015,
                                Year == "2015-16" ~ 2016,
                                Year == "2016-17" ~ 2017,
                                Year == "2017-18" ~ 2018,
                                Year == "2018-19" ~ 2019))


#install.packages('C:/Users/daniel.thompson/Documents/R/ggthemr-master.zip', repos = NULL, type = "win.binary")

data.mean.plot_slowdown <- data.mean.plot %>%
  group_by(Year) %>%
  mutate(show_time = case_when(Year.clean %in% c(2018,2019) ~ 4,
                               TRUE           ~ 1)) %>%
         uncount(show_time) %>%
           group_by(Year.clean) %>%
           mutate(reveal_time = row_number()) %>%
           ungroup()

#----------------------
#Plot data
#----------------------

p <- ggplot(data = data.mean.plot, aes(x = Year.clean, y = Mean, group = Age)) +
  #geom_segment(aes(xend = 2019, yend = Mean), linetype = 2, colour = 'grey') +
  geom_line() +
  geom_point(size =2) +
  geom_text_repel(aes(label = Age), hjust = 1, vjust = -1, colour = 'white') +
  geom_label(label = "95% WHO Target", x = 2018, y = 95.4) +
  transition_reveal(Year.clean) +
  coord_cartesian(clip = 'off') + 
  ylim(85,97) +
  scale_x_continuous(breaks = round(seq(2010,2020, by = 1),1)) +
  # theme(plot.margin = margin(10, 40, 40, 40)) +  
  labs(x = "Year",
       y = "Average Coverage Percentage (%)",
       title = "UK Average Childhood Vaccination Coverage by Age",
       subtitle = "Includes all 13 routine childhood vaccinations",
       caption = "Data from @NHSDigital") +
  geom_hline(yintercept = 95, alpha = 0.25, colour = 'blue') + 
  theme_ft_rc()

 

animate(p, height = 500, width =550,end_pause = 30)
  
anim_save("vaccination.gif",p)



p <- ggplot(data = data.mean.plot, aes(x = Year.clean, y = Mean, group = Age, colour = Age)) +
  
 # geom_line(aes(x = Year.clean, y = Value, group = VacCode, colour = Age), alpha = 0.25) +
  geom_line() +
  geom_segment(aes(xend = 2019, yend = Mean), linetype = 2, colour = 'grey') +
  geom_point(size =2) +
  geom_text(aes(x = 2019.5, label = Mean), hjust = 0) +
  transition_reveal(Year.clean) +
  theme_minimal() +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) +
  scale_x_continuous(breaks = round(seq(min(data.mean.plot$Year.clean), max(data.mean.plot$Year.clean), by = 1),1)) +
  ylim(85,97) +
  labs(x = "Year", y = "Mean vaccination Coverage (%)", colour = "Vaccination Age") 



  p <- ggplot(data = data.mean.plot, aes(x = Year.clean, y = Value, group = VacCode)) +
  geom_line(alpha = 0.8) +
  geom_line(aes(x = Year.clean, y = Mean, group = Age, colour = Age),size = 1) +
  geom_segment(aes(xend = 2019, yend = Value), linetype = 2, colour = 'grey',alpha = 0.8) +
  geom_point(aes(x = Year.clean, y = Mean, group = Age, colour = Age),size =2) +
  geom_text_repel(aes(x=2019.85,label = VacCode), hjust = 0.1, alpha = 0.75) +
  scale_x_continuous(breaks = round(seq(min(data.mean.plot$Year.clean),2020, by = 1),1)) +
  facet_grid(Age ~ .,scales = "fixed") +
  transition_reveal(Year.clean) +
 ylim(85,97) +
   # theme(plot.margin = margin(10, 40, 40, 40)) +  
  labs(x = "Year", y = "Vaccination Coverage (%)", colour = "Average vaccination coverage at age:") 
  
  

  animate(p, height = 800, width =1000)

 # geom_hline(yintercept=95, linetype="dashed", 
 #            color = "red") +




