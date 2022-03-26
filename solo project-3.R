
library("viridis")   #theme package
library(readr)
library(dplyr)
library(tidyverse)
#disablying scientific notiation in R
options(scipen = 999)

cu_19 <- read_csv("Desktop/21 Spring/STA404/intrvw19 2/intrvw19/fmli194.csv")

me_19 <- read_csv("Desktop/21 Spring/STA404/intrvw19 2/intrvw19/memi194.csv")

#create new df
cu_19<-cu_19%>%select(REF_RACE,FSALARYM,TOTEX4PQ,IRAX,LIQUIDX)
me_19<-me_19%>%select(MEMBRACE,EDUCA,INC_HRSQ)
#drop na function
me_19<- me_19 %>% drop_na(INC_HRSQ)
me_19<- me_19 %>% drop_na(EDUCA)
cu_19<- cu_19 %>% drop_na(IRAX)
cu_19<- cu_19 %>% drop_na(LIQUIDX)

# changing the numeric into factor with race
cu_19$REF_RACE <- as.factor(cu_19$REF_RACE)
str(cu_19$REF_RACE)
levels(cu_19$REF_RACE) <- c("White","Black","Native American", "Asian", "Pacific Islander", "Multi-race")
cu_19$REF_RACE

me_19$MEMBRACE <- as.factor(me_19$MEMBRACE)
str(me_19$MEMBRACE)
levels(me_19$MEMBRACE) <- c("White","Black","Native American", "Asian", "Pacific Islander", "Multi-race")
me_19$MEMBRACE

#Changing educational level into scale
me_19$EDUCA <- case_when(me_19$EDUCA <= 4 ~ "High School Graduate or Lower",
                         between(me_19$EDUCA, 4, 6) ~ "Attend College or Higher Education",
                         me_19$EDUCA >= 7 ~ "College Degree or Higher")



#__________________________________________________
#this plot shows the average mean income before taxes of different races
i<-ggplot() +  
  geom_boxplot(aes(x=FSALARYM, y=REF_RACE, fill=REF_RACE),data=cu_19) +
  labs(x="Income before tax", y="Race",
       caption = ~italic('Data Source: U.S. Bureau of Labor Statistics'),
       fill = "Races")+
  ggtitle("Distribution of Household Income among Different Races 2019", 
          subtitle ="")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Set2")+
  scale_x_continuous(labels=scales::comma)

ggsave("Income before tax.png",i)

#__________________________________________________
# Education level among different races
e<-ggplot() +  
  geom_bar(aes(x=MEMBRACE, fill =EDUCA),data=me_19,
           stat="count", position = "fill") +
  labs(x="Races", y="Percentage",
       caption = ~italic('Data Source: U.S. Bureau of Labor Statistics'),
       fill = "Education Level")+
  ggtitle("Education Level among Different Races", subtitle ="")+
  theme(plot.title = element_text(hjust = 0.5))
e
ggsave("Educational level.png",e)


#__________________________________________________
#Graph for savings and retirements level among different races
s<-ggplot(data=cu_19_savings, mapping=aes(x=REF_RACE, y=Savings, fill=REF_RACE))+
  coord_flip()+
  stat_summary(fun.data=mean_sdl, geom="bar") +
  labs(x="Race", y="Savings and Retirements ($)",
       caption = ~italic('Data Source: U.S. Bureau of Labor Statistics'),
       fill = "Races")+
  ggtitle("Average Savings among Different Races 2019", 
          subtitle ="")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(labels=scales::comma)
ggsave("savings.png",s)


#__________________________________________________
 #hours worked per week
h<-ggplot(data=me_19, mapping=aes(x=MEMBRACE, y=INC_HRSQ, fill=MEMBRACE))+
  stat_summary(fun.data=mean_sdl, geom="bar") +
  labs(x="Race", y="Hours per Week",
       caption = ~italic('Data Source: U.S. Bureau of Labor Statistics'),
       fill = "Races")+
  ggtitle("Working Hours per Week among Different Races 2019", 
          subtitle ="")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Set2")
h
ggsave("workingHours.png",h)
