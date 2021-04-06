# Descriptivas
library(haven)
library(tidyverse)
library(psych)

attach$Edad <- gsub("años", '', attach$Edad)
attach$Edad <- gsub(" .", '', attach$Edad)

attach_d <- attach %>% 
  filter(between(Edad, 16, 69))

attach_d <- attach_d %>% 
  drop_na(Ans1)

# calculo edad
attach_d$Edad <- as.numeric(attach_d$Edad)

attach_d$Edad %>% cut(., breaks = c(16, 22, 40, 64, 74), include.lowest = FALSE)

attach_d <- attach_d %>% 
  mutate(Edad1 = cut(Edad, breaks = c(16, 22, 40, 64, 74), include.lowest = FALSE))

attach_d %>% 
  group_by(Edad1) %>% 
  count()

attach_d %>%
  group_by(Edad1) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)


## sexo
attach_d %>% select(G_nero, Orien_Sex, Orien_Sex_Otro)

attach_d %>%
  group_by(G_nero) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)

## raza
 attach_d$raza
 
 attach_d %>%
  group_by(raza) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)

 
# language
names(attach_d)

attach %>% 
  select(LenguaMat, LengMat2, Raza_otro)

 attach_d %>%
  group_by(LenguaMat) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)

 
# estados civil
attach %>% 
  select(EdoCvil)

attach_d %>%
  group_by(EdoCvil) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)


# education
attach %>% 
  select(Edu, Edu_otro)

attach_d %>%
  group_by(Edu) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)


# employment
names(attach_d)
View(attach_d %>% select(Empleo1, Empleo2, Empleo3))

attach_d %>%
  group_by(Empleo1) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)

# Keyworker
attach_d %>%
  group_by(Empleo2) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)

### 
attach_d$PerHogar
attach_d %>%
  group_by(PerHogar) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)


### ninos
names(attach_d)
attach_d$NNA1

attach_d %>%
  group_by(NNA1) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)

### mental health
attach_d %>%
  group_by(SaludMen1) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)


### incoming
attach_d %>%
  group_by(Q70) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)

# politica
mean(attach_d$Q77_15) * 10
sd(attach_d$Q77_15) * 10


### Salud 
attach_d %>%
  group_by(Sal1) %>% 
  summarise(Percentage=n()) %>%
  mutate(Percentage = Percentage/sum(Percentage)*100)

