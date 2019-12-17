library(readxl)
library(tidyverse)
datos260919 <- read_excel("data/datos260919.xlsx", sheet=2)
#save(datos, file="spiders_data.csv")
#datos<-read.csv(file="spiders_data.csv")
datos <- datos260919
dim(datos)
head(datos)
summary(datos)

## data set: 1 juveniles de campo, 2: adultos de campo, 3: adultos cautiverio
## exp_n: número de corrida anidado en cada data set
## w_color: wasp color: 1 BOB, 2 Black
## s_stage: 1: adulto, 2: juvenil
## h_resp: 1: Prey alert and swivel, 2: Follow or stalk, 
## 3: Crouch, jump and contact prey, 4: Pierce and ingestion
## 5:Undetected   or ignored, 6:Detect and retrieve   
## time: 0-5 min : 1 5-10 min: 2 10-20 min: 3  20-30 min: 4 30-40 min: 5
## repeat.: Number of times of response 3 during the entire observation
## dist: 1: 0-50 mm, 2: 50 -100 mm, 3: 100-more mm, 
## Distance between wasp and spider,  for responses 1 and 3
## drag: Spider Drag line laydown: 1 yes, 2 no
## w_size: wasp size mm
## s_sac: spider size AC mm
## s_slc: spider size LC mm
## s_aaa: spider size AA mm
## s_sla: spider size LA mm
## type: type of wasp

# erase NAs and weird genera
# 
datos <- datos %>% 
  mutate(w_color=recode(w_color,`1`="BOB",`2`="Black"))%>% 
  mutate(s_type=recode(data_set,`1`="Field Juvenile",
                         `2`="Field Adult",
                         `3`="Captivity Adult")) %>%  
  mutate(s_size = (s_slc+s_sla)) %>% 
  mutate(h_resp2=recode(h_resp,`1`="1.Prey alert and swivel",
                        `2`="2.Follow or stalk",
                        `3`="3.Crouch, jump and contact prey",
                        `4`="4.Pierce and ingestion",
                        `5`="5.Undetected or ignored",
                        `6`="6.Detect visually and withdraw",
                        `0`="7.None")) %>%  
  mutate(time2 = recode_factor(time,
                        `1`="0-5 min",
                        `2`="5-10 min",
                        `3`="10-20 min",
                        `4`="20-30 min",
                        `5`="30-40 min")) %>% 
  mutate(typegr = recode(type,
                         `Chromoteleia`="Chrom. & Scelio",
                         `Scelio`="Chrom. & Scelio",
                         `Macroteleia`="Macroteleia",
                         `Baryconus`="Baryconus",
                         `Pseudoheptascelio`="Pseudoheptascelio"))

summary(datos)
datos <- datos %>% filter(type!="Pseudoheptascelio")

datos$ID <- paste0(datos$exp_n,datos$data_set)

datos$type <- as.factor(as.character(datos$type))
datos <- datos %>% select(data_set,exp_n,h_resp2,time2, "repeat",dist,drag,typegr, type, w_color, w_size,s_type, s_size, s_sla, ID)

head(datos)

library(readxl)
datos230919 <- read_excel("data/datos230919.xlsx", sheet = "Hoja1", skip = 2)
names(datos230919) <- c("n_exp", "backgr", "BOB1", "BOB2", "BOB3", "BOB4",
                 "B1", "B2", "B3", "B4", "resp","first", "DA","DE","DI","silk","s_sac","s_slc","s_saa","s_sla")
datos230919 <- datos230919[,-c(3:10,13,14,15)]
head(datos230919)
summary(datos230919)

datos_cont <- datos230919 %>% 
  mutate(first=recode(first,`0`="Black",
                         `1`="BOB")) %>%  
  mutate(backgr=recode(backgr,`1`="White",
                      `2`="Black")) %>% 
  mutate(s_size = (s_slc+s_sla)) %>% 
  mutate(silk=recode(silk,`0`="No silk",
                      `1`="Silk")) %>% 
  mutate(resp=recode(resp,`1`="Same actions",
                    `2`="Different actions",
                    `3`="Different actions")) 


# L. jemineus took the same actions with both black and BOB wasps = 1
# L. jemineus took different actions with black and BOB, and prefered BOB = 2
# L. jemineus took different actions with black and BOB, and prefered Black = 3


datos_cont <- datos_cont %>% select(n_exp,backgr,resp,s_size, silk, first)


