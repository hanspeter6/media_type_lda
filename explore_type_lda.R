library(plm)
library(nlme)
library(tidyr)
library(dplyr)
# reading in the different datasets:
set02 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02.rds")
set05 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05.rds")
set10 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10.rds")
set12 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12.rds")
set14 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14.rds")

# 
# # Gasoline contains two variables which are individual and time indexes
# data("Gasoline", package = "plm")
# Gas <- pdata.frame(Gasoline, index = c("country", "year"), drop.index = FALSE)

# for 2002 
set02$age <- factor(set02$age, labels = c("15-24","25-44", "45-54","55+"))
set02$race <- factor(set02$race,labels = c("black", "coloured", "indian", "white"))
set02$edu <- factor(set02$edu, labels = c("<matric", "matric",">matric" ))
set02$lsm <- factor(set02$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"))
set02$sex <- factor(set02$sex, labels = c("male", "female"))
set02$hh_inc <- factor(set02$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000")) # NB 2012 levels

sex_02 <- set02 %>%
        group_by(category = sex) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

age_02 <- set02 %>%
        group_by(category = age) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

edu_02 <- set02 %>%
        group_by(category = edu) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

race_02 <- set02 %>%
        group_by(category = race) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

hh_inc_02 <- set02 %>%
        group_by(category = hh_inc) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

lsm_02 <- set02 %>%
        group_by(category = lsm) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

frame_02 <- rbind.data.frame(sex_02,
                             age_02,
                             edu_02,
                             race_02,
                             hh_inc_02,
                             lsm_02) %>%
        mutate(year = 2002) %>%
        select(category, year, everything())

# for 2005 
set05$age <- factor(set05$age, labels = c("15-24","25-44", "45-54","55+"))
set05$race <- factor(set05$race,labels = c("black", "coloured", "indian", "white"))
set05$edu <- factor(set05$edu, labels = c("<matric", "matric",">matric" ))
set05$lsm <- factor(set05$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"))
set05$sex <- factor(set05$sex, labels = c("male", "female"))
set05$hh_inc <- factor(set05$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000")) # NB 2012 levels

sex_05 <- set05 %>%
        group_by(category = sex) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

age_05 <- set05 %>%
        group_by(category = age) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

edu_05 <- set05 %>%
        group_by(category = edu) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

race_05 <- set05 %>%
        group_by(category = race) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

hh_inc_05 <- set05 %>%
        group_by(category = hh_inc) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

lsm_05 <- set05 %>%
        group_by(category = lsm) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

frame_05 <- rbind.data.frame(sex_05,
                             age_05,
                             edu_05,
                             race_05,
                             hh_inc_05,
                             lsm_05) %>%
        mutate(year = 2005) %>%
        select(category, year, everything())


# for 2010 
set10$age <- factor(set10$age, labels = c("15-24","25-44", "45-54","55+"))
set10$race <- factor(set10$race,labels = c("black", "coloured", "indian", "white"))
set10$edu <- factor(set10$edu, labels = c("<matric", "matric",">matric" ))
set10$lsm <- factor(set10$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"))
set10$sex <- factor(set10$sex, labels = c("male", "female"))
set10$hh_inc <- factor(set10$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000")) # NB 2012 levels

sex_10 <- set10 %>%
        group_by(category = sex) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

age_10 <- set10 %>%
        group_by(category = age) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

edu_10 <- set10 %>%
        group_by(category = edu) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

race_10 <- set10 %>%
        group_by(category = race) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

hh_inc_10 <- set10 %>%
        group_by(category = hh_inc) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

lsm_10 <- set10 %>%
        group_by(category = lsm) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

frame_10 <- rbind.data.frame(sex_10,
                             age_10,
                             edu_10,
                             race_10,
                             hh_inc_10,
                             lsm_10) %>%
        mutate(year = 2010) %>%
        select(category, year, everything())

# for 2012 
set12$age <- factor(set12$age, labels = c("15-24","25-44", "45-54","55+"))
set12$race <- factor(set12$race,labels = c("black", "coloured", "indian", "white"))
set12$edu <- factor(set12$edu, labels = c("<matric", "matric",">matric" ))
set12$lsm <- factor(set12$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"))
set12$sex <- factor(set12$sex, labels = c("male", "female"))
set12$hh_inc <- factor(set12$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"))

sex_12 <- set12 %>%
        group_by(category = sex) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

age_12 <- set12 %>%
        group_by(category = age) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

edu_12 <- set12 %>%
        group_by(category = edu) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

race_12 <- set12 %>%
        group_by(category = race) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

hh_inc_12 <- set12 %>%
        group_by(category = hh_inc) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

lsm_12 <- set12 %>%
        group_by(category = lsm) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

frame_12 <- rbind.data.frame(sex_12,
                         age_12,
                         edu_12,
                         race_12,
                         hh_inc_12,
                         lsm_12) %>%
        mutate(year = 2012) %>%
        select(category, year, everything())


# for 2014 
set14$age <- factor(set14$age, labels = c("15-24","25-44", "45-54","55+"))
set14$race <- factor(set14$race,labels = c("black", "coloured", "indian", "white"))
set14$edu <- factor(set14$edu, labels = c("<matric", "matric",">matric" ))
set14$lsm <- factor(set14$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"))
set14$sex <- factor(set14$sex, labels = c("male", "female"))
set14$hh_inc <- factor(set14$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"))

sex_14 <- set14 %>%
        group_by(category = sex) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

age_14 <- set14 %>%
        group_by(category = age) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

edu_14 <- set14 %>%
        group_by(category = edu) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

race_14 <- set14 %>%
        group_by(category = race) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

hh_inc_14 <- set14 %>%
        group_by(category = hh_inc) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

lsm_14 <- set14 %>%
        group_by(category = lsm) %>%
        summarise(newspapers = mean(newspapers),
                  magazines = mean(magazines),
                  tv = mean(tv),
                  radio = mean(radio),
                  internet = mean(internet))

frame_14 <- rbind.data.frame(sex_14,
                             age_14,
                             edu_14,
                             race_14,
                             hh_inc_14,
                             lsm_14) %>%
        mutate(year = 2014) %>%
        select(category, year, everything())


# putting it together
type_frame <- rbind.data.frame(frame_02,
                               frame_05,
                               frame_10,
                               frame_12,
                               frame_14)


type_frame <- pdata.frame(type_frame)#, index = c("country", "year"), drop.index = FALSE)

# basic nlme

# per grouping

type_frame_age <- type_frame %>%
        filter(category %in% c("15-24","25-44", "45-54","55+" ))

type_frame_race <- type_frame %>%
        filter(category %in% c("black", "coloured", "indian", "white"))

type_frame_inc <- type_frame %>%
        filter(category %in% c("<R2500","R2500-R6999","R7000-R11999",">=R12000"))

type_frame_sex <- type_frame %>%
        filter(category %in% c("male", "female"))

type_frame_edu <- type_frame %>%
        filter(category %in% c("<matric", "matric",">matric"))

type_frame_lsm <- type_frame %>%
        filter(category %in% c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"))

# newspapers
newsp_age <- groupedData(newspapers ~ as.numeric(as.character(year))| category, data = type_frame_age)
newsp_race <- groupedData(newspapers ~ as.numeric(as.character(year))| category, data = type_frame_race)
newsp_inc <- groupedData(newspapers ~ as.numeric(as.character(year))| category, data = type_frame_inc)
newsp_sex <- groupedData(newspapers ~ as.numeric(as.character(year))| category, data = type_frame_sex)
newsp_edu <- groupedData(newspapers ~ as.numeric(as.character(year))| category, data = type_frame_edu)
newsp_lsm <- groupedData(newspapers ~ as.numeric(as.character(year))| category, data = type_frame_lsm)

jpeg("lds_newsp_age.jpeg")
plot(newsp_age, aspect = 2, xlab = "year", main = "Newspapers and Age")
dev.off()

jpeg("lds_newsp_race.jpeg")
plot(newsp_race, aspect = 2, xlab = "year", main = "Newspapers and Race")
dev.off()

jpeg("lds_newsp_inc.jpeg")
plot(newsp_inc, aspect = 2, xlab = "year", main = "Newspapers and Household Income")
dev.off()

jpeg("lds_newsp_sex.jpeg")
plot(newsp_sex, aspect = 2, xlab = "year", main = "Newspapers and Gender")
dev.off()

jpeg("lds_newsp_edu.jpeg")
plot(newsp_edu, aspect = 2, xlab = "year", main = "Newspapers and Education")
dev.off()

jpeg("lds_newsp_lsm.jpeg")
plot(newsp_lsm, aspect = 2, xlab = "year", main = "Newspapers and LSM")
dev.off()

# internet
int_age <- groupedData( internet ~ as.numeric(as.character(year))| category, data = type_frame_age)
int_race <- groupedData(internet ~ as.numeric(as.character(year))| category, data = type_frame_race)
int_inc <- groupedData(internet ~ as.numeric(as.character(year))| category, data = type_frame_inc)
int_sex <- groupedData(internet ~ as.numeric(as.character(year))| category, data = type_frame_sex)
int_edu <- groupedData(internet ~ as.numeric(as.character(year))| category, data = type_frame_edu)
int_lsm <- groupedData(internet ~ as.numeric(as.character(year))| category, data = type_frame_lsm)

jpeg("lds_int_age.jpeg")
plot(int_age, aspect = 2, xlab = "year", main = "Internet and Age")
dev.off()

jpeg("lds_int_race.jpeg")
plot(int_race, aspect = 2, xlab = "year", main = "Internet and Race")
dev.off()

jpeg("lds_int_inc.jpeg")
plot(int_inc, aspect = 2, xlab = "year", main = "Internet and Household Income")
dev.off()

jpeg("lds_int_sex.jpeg")
plot(int_sex, aspect = 2, xlab = "year", main = "Internet and Gender")
dev.off()

jpeg("lds_int_edu.jpeg")
plot(int_edu, aspect = 2, xlab = "year", main = "Internet and Education")
dev.off()

jpeg("lds_int_lsm.jpeg")
plot(int_lsm, aspect = 2, xlab = "year", main = "Internet and LSM")
dev.off()