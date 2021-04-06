library(haven)
library(tidyverse)
library(psych)

attach <- read_sav("ATTACH México_AnonimoOctubre.sav")


# Ansiedad Trastorno emocional - Ansiedad – Cuestionario abreviado 7a
# entre más puntaje más ansiedad en todos los reactivos
# de 1 a 5
ansiedad <- attach %>% 
  select("Ans1", "Ans2", "Ans3", "Ans4", "Ans5", "Ans6", "Ans7")
ansiedad %>% summarise_all(funs(sum(is.na(.))))
# 80 completos
alpha(ansiedad)

ansiedad %>% 
  na.omit() %>% 
  alpha()


# Ecala de soledad solo 10 de 20
# más puntaje más soledad: 1, 4, 5, 6, 8
# visceversa: 2, 3, 7, 9, 10
# de 1 a 4
soledad <- attach %>% 
  select("Sol1", "Sol2", "Sol3", "Sol4", "Sol5", "Sol6", "Sol7", "Sol8", "Sol9", "Sol10")
soledad %>% summarise_all(funs(sum(is.na(.))))
# 80 completos

soledad <- soledad %>% 
  mutate(Sol2 = abs(Sol2 - 4) + 1,
         Sol3 = abs(Sol3 - 4) + 1,
         Sol7 = abs(Sol7 - 4) + 1,
         Sol9 = abs(Sol9 - 4) + 1,
         Sol10 = abs(Sol10 - 4) + 1)

soledad %>% 
  na.omit() %>% 
  alpha()


# CUESTIONARIO SOBRE LA SALUD DEL PACIENTE  9 (PHQ - 9)
# mas puntaje más depresión
# puntaje de 0 a 3
phq9 <- attach %>% 
  select("PHQ1", "PHQ2", "PHQ3", "PHQ4", "PHQ5", "PHQ6", "PHQ7", "PHQ8", "PHQ9")
phq9 %>% summarise_all(funs(sum(is.na(.))))
# 80 completos

phq9 %>% 
  na.omit() %>% 
  alpha()



# Resiliencia
# entre más puntaje más resiliencia: 1, 3, 5
# visceversa: 2, 4, 6
# puntuaciones: del 1 al 5
resiliencia <- attach %>% 
  select("Res1", "Resi2", "Resi3", "Resi4", "Resi5", "Resi6")
resiliencia %>% summarise_all(funs(sum(is.na(.))))
# 79 completos

resiliencia <- resiliencia %>% 
  mutate(Resi2 = abs(Resi2 - 5) + 1,
         Resi4 = abs(Resi4 - 5) + 1,
         Resi6 = abs(Resi6 - 5) + 1
         )

resiliencia %>% 
  na.omit() %>% 
  alpha()

# Corononavirus
# 1 Haga clic en SÍ (a mi) si te impactó a ti. 
# 2 Haga clic en SÍ (persona en mi hogar) si impactó a otra(s) persona(s) en tu hogar.
# 3 Haga clic en NO si no te impactó a ti u otra(s) persona(s) en su hogar.
# 4 Haga clic en NA si el enunciado no aplica a ti o a otra(s) persona(s) en tu hogar.
#IMPORTANTE: Si te impacto tanto a ti como a alguna persona en tu hogar por favor hacer clic en las dos opciones (SÍ (a mi) y en SÍ (persona en mi hogar)).
cs <- attach %>% 
  select("CS1_1", "CS1_2", "CS1_3", "CS1_4", 
         "CS2_1", "CS2_2", "CS2_3", "CS2_4",
         "CS3_1",  "CS3_2",  "CS3_3", 
         "CS4_1", "CS4_2", "CS4_3", "CS4_4", 
         "CS5_1", "CS5_2", "CS5_3", "CS5_4", 
         "CS6_1", "CS6_2", "CS6_3", "CS6_4",
         "CS7_1", "CS7_2", "CS7_3", "CS7_4",
         "Q93_1", "Q93_2", "Q93_3", "Q93_4")
cs %>% summarise_all(funs(sum(is.na(.))))
# variable por que es por renglon=4


# salud general
# Sal1 = puntaje: 1 es excelente: 5 es mala
# Sal2 = puntaje: 1 act fisica completamenteBIEN: 5 mala-nada
# Sal3 = puntaje: 1:10 entre más puntaje más dolor
# Sal4 = puntaje: 1 nada de cansancio: 5 muy severo
# Sal5 = puntaje: 1 excelente con actividades dirias: 5 mal
salud <- attach %>% 
  select("Sal1", "Sal2", "Sal3", "Sal4", "Sal5")
salud %>% summarise_all(funs(sum(is.na(.))))
# 78 completos

salud %>% 
  na.omit() %>% 
  alpha()


# percepción de amenaza
# entre más puntaje más p amenaza: 1, 2, 4, 5, 6 y 7
# visceversa: 3, 
#puntaje: de 1:7
p_amenza <- attach %>% 
  select("PA1", "PA2", "PA3", "PA4", "PA5", "PA6", "PA7")
p_amenza %>% summarise_all(funs(sum(is.na(.))))
# 67 completos

p_amenza <- p_amenza %>% 
  mutate(PA3 = abs(PA3 - 7) + 1)

p_amenza %>% 
  na.omit() %>% 
  alpha()



# Gobierno y noticias
#puntaje: 1 = nocierto: 7= cierto
# ResGob1: apoyo gob-edo retrinja circulación
# ResGob2: apoyo gob-edo fuerte que baje la propagación
# ResGob3: quiero que el gob-edo castigue que no queden en casa
# ResGob4: quiero que el gob-edo castigue que no distancia social
# ResGob5: molesta que el gob-edo force quedarse en casa
# ResGob6: enoja que el gob-edo me diga qué hacer
# ResGob7: debemos invertir en vacunas
# ResGob8: quier más investigación con recursos del edo
# ResGob9: buena idea que el gob-edo regrese dinero a ciudadano-negocios
# ResGob10: estímulo monetario del gob-edo es buena idea
# ResGob11: No confio en la info del gob-edo
# ResGob12: el gob-edo esconde información
# Noticias1: veo muchas noticias COVID
# Noticias2: trato de no ver noticias COVID
# Noticias3: busco muchas noticias COVID
gob_not <- attach %>% 
  select("ResGob1", "ResGob2", "ResGob3", "ResGob4", "ResGob5", "ResGob6", 
           "ResGob7", "ResGob8", "ResGob9", "ResGob10", "ResGob11", "ResGob12",
           "Noticias1", "Noticias2", "Noticias3")
gob_not %>% summarise_all(funs(sum(is.na(.))))
# 67 completos

gob_not %>% 
  na.omit() %>% 
  alpha()

# Proposito de vida
# entre más puntaje más próposito 
# puntaje de 1:5
propovida <- attach %>% select("PV1", "PV2", "PV3", "PV4")
propovida %>% summarise_all(funs(sum(is.na(.))))
# 67 completos

propovida %>% 
  na.omit() %>% 
  alpha()


##### alfa https://rpubs.com/jboscomendoza/alfa_cronbach_r







