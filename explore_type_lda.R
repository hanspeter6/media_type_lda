library(plm)
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)

# reading in the different datasets:
set95c <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_1995/set95c.rds")
set02c <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02c.rds")
set05c <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05c.rds")
set08c <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/set08c.rds")
set10c <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10c.rds")
set12c <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12c.rds")
set14c <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14c.rds")

# function to create frames (for all except '95 since doesnt have internet)
frames <- function(set, category) {
        require(dplyr)
        
        set$age <- factor(set$age, labels = c("15-24","25-44", "45-54","55+"), ordered = TRUE)
        set$race <- factor(set$race,labels = c("black", "coloured", "indian", "white"), ordered = TRUE)
        set$edu <- factor(set$edu, labels = c("<matric", "matric",">matric" ) ,ordered = TRUE)
        set$lsm <- factor(set$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = TRUE)
        set$sex <- factor(set$sex, labels = c("male", "female"), ordered = TRUE)
        set$hh_inc <- factor(set$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"), ordered = TRUE) # NB 2012 levels
        
        
        set %>%
                group_by_(category = category) %>%
                summarise(news = mean(newspapers),
                          mags = mean(magazines),
                          tvs = mean(tv),
                          radios = mean(radio),
                          internets = mean(internet),
                          alls = mean(all),
                          up_all = mean(all) + (2 * sd(all)/sqrt(length(all))),
                          low_all = mean(all) - (2 * sd(all)/sqrt(length(all))),
                          up_newspapers = mean(newspapers) + (2 * sd(newspapers)/sqrt(length(newspapers))),
                          low_newspapers = mean(newspapers) - (2 * sd(newspapers)/sqrt(length(newspapers))),
                          up_magazines = mean(magazines) + (2 * sd(magazines)/sqrt(length(magazines))),
                          low_magazines = mean(magazines) - (2 * sd(magazines)/sqrt(length(magazines))),
                          up_tv = mean(tv) + (2 * sd(tv)/sqrt(length(tv))),
                          low_tv = mean(tv) - (2 * sd(tv)/sqrt(length(tv))),
                          up_radio = mean(radio) + (2 * sd(radio)/sqrt(length(radio))),
                          low_radio = mean(radio) - (2 * sd(radio)/sqrt(length(radio))),
                          up_internet = mean(internet) + (2 * sd(internet)/sqrt(length(internet))),
                          low_internet = mean(internet) - (2 * sd(internet)/sqrt(length(internet)))
                )
        
}


# create a vector to use for internet '95:
# considered range(set10c$internet)
set95c <- set95c %>%
        mutate(internet = -1)

# also, adjusted function to exclude lsm
frame_95 <- rbind.data.frame(frames(set95c,"sex"),
                             frames(set95c,"age"),
                             frames(set95c,"edu"),
                             frames(set95c,"race"),
                             frames(set95c, "hh_inc")) %>% # dont have lsm...
        mutate(year = 1995) %>%
        select(category, year, everything())


frame_02 <- rbind.data.frame(frames(set02c,"sex"),
                             frames(set02c,"age"),
                             frames(set02c,"edu"),
                             frames(set02c,"race"),
                             frames(set02c, "hh_inc"),
                             frames(set02c,"lsm")) %>%
                                     mutate(year = 2002) %>%
                                     select(category, year, everything())

frame_05 <- rbind.data.frame(frames(set05c,"sex"),
                             frames(set05c,"age"),
                             frames(set05c,"edu"),
                             frames(set05c,"race"),
                             frames(set05c, "hh_inc"),
                             frames(set05c,"lsm")) %>%
        mutate(year = 2005) %>%
        select(category, year, everything())

frame_08 <- rbind.data.frame(frames(set08c,"sex"),
                             frames(set08c,"age"),
                             frames(set08c,"edu"),
                             frames(set08c,"race"),
                             frames(set08c, "hh_inc"),
                             frames(set08c,"lsm")) %>%
        mutate(year = 2008) %>%
        select(category, year, everything())

frame_10 <- rbind.data.frame(frames(set10c,"sex"),
                             frames(set10c,"age"),
                             frames(set10c,"edu"),
                             frames(set10c,"race"),
                             frames(set10c, "hh_inc"),
                             frames(set10c,"lsm")) %>%
        mutate(year = 2010) %>%
        select(category, year, everything())

frame_12 <- rbind.data.frame(frames(set12c,"sex"),
                             frames(set12c,"age"),
                             frames(set12c,"edu"),
                             frames(set12c,"race"),
                             frames(set12c, "hh_inc"),
                             frames(set12c,"lsm")) %>%
        mutate(year = 2012) %>%
        select(category, year, everything())

frame_14 <- rbind.data.frame(frames(set14c,"sex"),
                             frames(set14c,"age"),
                             frames(set14c,"edu"),
                             frames(set14c,"race"),
                             frames(set14c, "hh_inc"),
                             frames(set14c,"lsm")) %>%
        mutate(year = 2014) %>%
        select(category, year, everything())

# putting it together
type_frame <- rbind.data.frame(#frame_95,
                               frame_02,
                               frame_05,
                               frame_08,
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

# considering plots of all media on demographic categories
jpeg("plot_type_combined_age.jpeg")
g <- ggplot(data = type_frame_age)
g <- g + geom_line(aes(year, news, group = category, colour = "newspaper"))
g <- g + geom_line(aes(year, mags, group = category, colour = "magazine"))
g <- g + geom_line(aes(year, radios, group = category, colour = "radio"))
g <- g + geom_line(aes(year, tvs, group = category, colour = "tv"))
g <- g + geom_line(aes(year, internets, group = category, colour = "internet"))
g <- g + geom_line(aes(year, alls, group = category, colour = "all"))
g <- g + scale_colour_discrete(name="Media")
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g + labs(y = "engagement", title = "Age")
g
dev.off()

jpeg("plot_type_combined_race.jpeg")
g <- ggplot(data = type_frame_race)
g <- g + geom_line(aes(year, news, group = category, colour = "newspaper"))
g <- g + geom_line(aes(year, mags, group = category, colour = "magazine"))
g <- g + geom_line(aes(year, radios, group = category, colour = "radio"))
g <- g + geom_line(aes(year, tvs, group = category, colour = "tv"))
g <- g + geom_line(aes(year, internets, group = category, colour = "internet"))
g <- g + geom_line(aes(year, alls, group = category, colour = "all"))
g <- g + scale_colour_discrete(name="Media")
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g + labs(y = "engagement", title = "Race")
g
dev.off()


jpeg("plot_type_combined_inc.jpeg")
g <- ggplot(data = type_frame_inc)
g <- g + geom_line(aes(year, news, group = category, colour = "newspaper"))
g <- g + geom_line(aes(year, mags, group = category, colour = "magazine"))
g <- g + geom_line(aes(year, radios, group = category, colour = "radio"))
g <- g + geom_line(aes(year, tvs, group = category, colour = "tv"))
g <- g + geom_line(aes(year, internets, group = category, colour = "internet"))
g <- g + geom_line(aes(year, alls, group = category, colour = "all"))
g <- g + scale_colour_discrete(name="Media")
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g + labs(y = "engagement", title = "Income")
g
dev.off()

jpeg("plot_type_combined_sex.jpeg")
g <- ggplot(data = type_frame_sex)
g <- g + geom_line(aes(year, news, group = category, colour = "newspaper"))
g <- g + geom_line(aes(year, mags, group = category, colour = "magazine"))
g <- g + geom_line(aes(year, radios, group = category, colour = "radio"))
g <- g + geom_line(aes(year, tvs, group = category, colour = "tv"))
g <- g + geom_line(aes(year, internets, group = category, colour = "internet"))
g <- g + geom_line(aes(year, alls, group = category, colour = "all"))
g <- g + scale_colour_discrete(name="Media")
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g + labs(y = "engagement", title = "Gender")
g
dev.off()

jpeg("plot_type_combined_edu.jpeg")
g <- ggplot(data = type_frame_edu)
g <- g + geom_line(aes(year, news, group = category, colour = "newspaper"))
g <- g + geom_line(aes(year, mags, group = category, colour = "magazine"))
g <- g + geom_line(aes(year, radios, group = category, colour = "radio"))
g <- g + geom_line(aes(year, tvs, group = category, colour = "tv"))
g <- g + geom_line(aes(year, internets, group = category, colour = "internet"))
g <- g + geom_line(aes(year, alls, group = category, colour = "all"))
g <- g + scale_colour_discrete(name="Media")
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g + labs(y = "engagement", title = "Education Level")
g
dev.off()

jpeg("plot_type_combined_lsm.jpeg")
g <- ggplot(data = type_frame_lsm)
g <- g + geom_line(aes(year, news, group = category, colour = "newspaper"))
g <- g + geom_line(aes(year, mags, group = category, colour = "magazine"))
g <- g + geom_line(aes(year, radios, group = category, colour = "radio"))
g <- g + geom_line(aes(year, tvs, group = category, colour = "tv"))
g <- g + geom_line(aes(year, internets, group = category, colour = "internet"))
g <- g + geom_line(aes(year, alls, group = category, colour = "all"))
g <- g + scale_colour_discrete(name="Media")
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g + labs(y = "engagement", title = "Living Standards Measures (LSM)")
g
dev.off()

# newspapers
newsp_age <- groupedData(news ~ as.numeric(as.character(year))| category, data = type_frame_age)
newsp_race <- groupedData(news ~ as.numeric(as.character(year))| category, data = type_frame_race)
newsp_inc <- groupedData(news ~ as.numeric(as.character(year))| category, data = type_frame_inc)
newsp_sex <- groupedData(news ~ as.numeric(as.character(year))| category, data = type_frame_sex)
newsp_edu <- groupedData(news ~ as.numeric(as.character(year))| category, data = type_frame_edu)
newsp_lsm <- groupedData(news ~ as.numeric(as.character(year))| category, data = type_frame_lsm)

jpeg("lds_newsp_age.jpeg")
g <- ggplot(data = newsp_age, aes(year, news, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = newsp_age$up_newspapers, ymin = newsp_age$low_newspapers, alpha = 0.5)
g <- g + labs(y = "newspapers", title = "Newspapers and Age")
g
dev.off()

jpeg("lds_newsp_race.jpeg")
g <- ggplot(data = newsp_race, aes(year, news, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = newsp_race$up_newspapers, ymin = newsp_race$low_newspapers, alpha = 0.5)
g <- g + labs(y = "newspapers", title = "Newspapers and Race")
g
dev.off()

jpeg("lds_newsp_inc.jpeg")
g <- ggplot(data = newsp_inc, aes(year, news, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = newsp_inc$up_newspapers, ymin = newsp_inc$low_newspapers, alpha = 0.5)
g <- g + labs(y = "newspapers", title = "Newspapers and Household Income")
g
dev.off()

jpeg("lds_newsp_sex.jpeg")
g <- ggplot(data = newsp_sex, aes(year, news, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = newsp_sex$up_newspapers, ymin = newsp_sex$low_newspapers, alpha = 0.5)
g <- g + labs(y = "newspapers", title = "Newspapers and Gender")
g
dev.off()

jpeg("lds_newsp_edu.jpeg")
g <- ggplot(data = newsp_edu, aes(year, news, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = newsp_edu$up_newspapers, ymin = newsp_edu$low_newspapers, alpha = 0.5)
g <- g + labs(y = "newspapers", title = "Newspapers and Education")
g
dev.off()

jpeg("lds_newsp_lsm.jpeg")
g <- ggplot(data = newsp_lsm, aes(year, news, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = newsp_lsm$up_newspapers, ymin = newsp_lsm$low_newspapers, alpha = 0.5)
g <- g + labs(y = "newspapers", title = "Newspapers and LSM")
g
dev.off()

# magazines
mags_age <- groupedData(mags ~ as.numeric(as.character(year))| category, data = type_frame_age)
mags_race <- groupedData(mags ~ as.numeric(as.character(year))| category, data = type_frame_race)
mags_inc <- groupedData(mags ~ as.numeric(as.character(year))| category, data = type_frame_inc)
mags_sex <- groupedData(mags ~ as.numeric(as.character(year))| category, data = type_frame_sex)
mags_edu <- groupedData(mags ~ as.numeric(as.character(year))| category, data = type_frame_edu)
mags_lsm <- groupedData(mags ~ as.numeric(as.character(year))| category, data = type_frame_lsm)

jpeg("lds_mags_age.jpeg")
g <- ggplot(data = mags_age, aes(year, mags, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = mags_age$up_magazines, ymin = mags_age$low_magazines, alpha = 0.5)
g <- g + labs(y = "magazines", title = "magazines and Age")
g
dev.off()

jpeg("lds_mags_race.jpeg")
g <- ggplot(data = mags_race, aes(year, mags, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = mags_race$up_magazines, ymin = mags_race$low_magazines, alpha = 0.5)
g <- g + labs(y = "magazines", title = "magazines and Race")
g
dev.off()

jpeg("lds_mags_inc.jpeg")
g <- ggplot(data = mags_inc, aes(year, mags, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = mags_inc$up_magazines, ymin = mags_inc$low_magazines, alpha = 0.5)
g <- g + labs(y = "magazines", title = "magazines and Household Income")
g
dev.off()

jpeg("lds_mags_sex.jpeg")
g <- ggplot(data = mags_sex, aes(year, mags, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = mags_sex$up_magazines, ymin = mags_sex$low_magazines, alpha = 0.5)
g <- g + labs(y = "magazines", title = "magazines and Gender")
g
dev.off()

jpeg("lds_mags_edu.jpeg")
g <- ggplot(data = mags_edu, aes(year, mags, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = mags_edu$up_magazines, ymin = mags_edu$low_magazines, alpha = 0.5)
g <- g + labs(y = "magazines", title = "magazines and Education")
g
dev.off()

jpeg("lds_mags_lsm.jpeg")
g <- ggplot(data = mags_lsm, aes(year, mags, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = mags_lsm$up_magazines, ymin = mags_lsm$low_magazines, alpha = 0.5)
g <- g + labs(y = "magazines", title = "magazines and LSM")
g
dev.off()


# radio
radios_age <- groupedData(radios ~ as.numeric(as.character(year))| category, data = type_frame_age)
radios_race <- groupedData(radios ~ as.numeric(as.character(year))| category, data = type_frame_race)
radios_inc <- groupedData(radios ~ as.numeric(as.character(year))| category, data = type_frame_inc)
radios_sex <- groupedData(radios ~ as.numeric(as.character(year))| category, data = type_frame_sex)
radios_edu <- groupedData(radios ~ as.numeric(as.character(year))| category, data = type_frame_edu)
radios_lsm <- groupedData(radios ~ as.numeric(as.character(year))| category, data = type_frame_lsm)

jpeg("lds_radios_age.jpeg")
g <- ggplot(data = radios_age, aes(year, radios, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = radios_age$up_radio, ymin = radios_age$low_radio, alpha = 0.5)
g <- g + labs(y = "radio", title = "radio and Age")
g
dev.off()

jpeg("lds_radios_race.jpeg")
g <- ggplot(data = radios_race, aes(year, radios, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = radios_race$up_radio, ymin = radios_race$low_radio, alpha = 0.5)
g <- g + labs(y = "radio", title = "radio and Race")
g
dev.off()

jpeg("lds_radios_inc.jpeg")
g <- ggplot(data = radios_inc, aes(year, radios, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = radios_inc$up_radio, ymin = radios_inc$low_radio, alpha = 0.5)
g <- g + labs(y = "radio", title = "radio and Household Income")
g
dev.off()

jpeg("lds_radios_sex.jpeg")
g <- ggplot(data = radios_sex, aes(year, radios, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = radios_sex$up_radio, ymin = radios_sex$low_radio, alpha = 0.5)
g <- g + labs(y = "radio", title = "radio and Gender")
g
dev.off()

jpeg("lds_radios_edu.jpeg")
g <- ggplot(data = radios_edu, aes(year, radios, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = radios_edu$up_radio, ymin = radios_edu$low_radio, alpha = 0.5)
g <- g + labs(y = "radio", title = "radio and Education")
g
dev.off()

jpeg("lds_radios_lsm.jpeg")
g <- ggplot(data = radios_lsm, aes(year, radios, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = radios_lsm$up_radio, ymin = radios_lsm$low_radio, alpha = 0.5)
g <- g + labs(y = "radio", title = "radio and LSM")
g
dev.off()

# tvs
tv_age <- groupedData(tvs ~ as.numeric(as.character(year))| category, data = type_frame_age)
tv_race <- groupedData(tvs ~ as.numeric(as.character(year))| category, data = type_frame_race)
tv_inc <- groupedData(tvs ~ as.numeric(as.character(year))| category, data = type_frame_inc)
tv_sex <- groupedData(tvs ~ as.numeric(as.character(year))| category, data = type_frame_sex)
tv_edu <- groupedData(tvs ~ as.numeric(as.character(year))| category, data = type_frame_edu)
tv_lsm <- groupedData(tvs ~ as.numeric(as.character(year))| category, data = type_frame_lsm)

jpeg("lds_tv_age.jpeg")
g <- ggplot(data = tv_age, aes(year, tvs, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = tv_age$up_tv, ymin = tv_age$low_tv, alpha = 0.5)
g <- g + labs(y = "tv", title = "tv and Age")
g
dev.off()

jpeg("lds_tv_race.jpeg")
g <- ggplot(data = tv_race, aes(year, tvs, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = tv_race$up_tv, ymin = tv_race$low_tv, alpha = 0.5)
g <- g + labs(y = "tv", title = "tv and Race")
g
dev.off()

jpeg("lds_tv_inc.jpeg")
g <- ggplot(data = tv_inc, aes(year, tvs, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = tv_inc$up_tv, ymin = tv_inc$low_tv, alpha = 0.5)
g <- g + labs(y = "tv", title = "tv and Household Income")
g
dev.off()

jpeg("lds_tv_sex.jpeg")
g <- ggplot(data = tv_sex, aes(year, tvs, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = tv_sex$up_tv, ymin = tv_sex$low_tv, alpha = 0.5)
g <- g + labs(y = "tv", title = "tv and Gender")
g
dev.off()

jpeg("lds_tv_edu.jpeg")
g <- ggplot(data = tv_edu, aes(year, tvs, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = tv_edu$up_tv, ymin = tv_edu$low_tv, alpha = 0.5)
g <- g + labs(y = "tv", title = "tv and Education")
g
dev.off()

jpeg("lds_tv_lsm.jpeg")
g <- ggplot(data = tv_lsm, aes(year, tvs, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = tv_lsm$up_tv, ymin = tv_lsm$low_tv, alpha = 0.5)
g <- g + labs(y = "tv", title = "tv and LSM")
g
dev.off()

# internet
int_age <- groupedData( internets ~ as.numeric(as.character(year))| category, data = type_frame_age)
int_race <- groupedData(internets ~ as.numeric(as.character(year))| category, data = type_frame_race)
int_inc <- groupedData(internets ~ as.numeric(as.character(year))| category, data = type_frame_inc)
int_sex <- groupedData(internets ~ as.numeric(as.character(year))| category, data = type_frame_sex)
int_edu <- groupedData(internets ~ as.numeric(as.character(year))| category, data = type_frame_edu)
int_lsm <- groupedData(internets ~ as.numeric(as.character(year))| category, data = type_frame_lsm)

jpeg("lds_int_age.jpeg")
g <- ggplot(data = int_age, aes(year, internets, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = int_age$up_internet, ymin = int_age$low_internet, alpha = 0.5)
g <- g + labs(y = "internet", title = "Internet and Age")
g
dev.off()

jpeg("lds_int_race.jpeg")
g <- ggplot(data = int_race, aes(year, internets, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = int_race$up_internet, ymin = int_race$low_internet, alpha = 0.5)
g <- g + labs(y = "internet", title = "Internet and Race")
g
dev.off()

jpeg("lds_int_inc.jpeg")
g <- ggplot(data = int_inc, aes(year, internets, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = int_inc$up_internet, ymin = int_inc$low_internet, alpha = 0.5)
g <- g + labs(y = "internet", title = "Internet and Income")
g
dev.off()

jpeg("lds_int_sex.jpeg")
g <- ggplot(data = int_sex, aes(year, internets, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = int_sex$up_internet, ymin = int_sex$low_internet, alpha = 0.5)
g <- g + labs(y = "internet", title = "Internet and Gender")
g
dev.off()

jpeg("lds_int_edu.jpeg")
g <- ggplot(data = int_edu, aes(year, internets, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = int_edu$up_internet, ymin = int_edu$low_internet, alpha = 0.5)
g <- g + labs(y = "internet", title = "Internet and Education")
g
dev.off()

jpeg("lds_int_lsm.jpeg")
g <- ggplot(data = int_lsm, aes(year, internets, group = category))
g <- g + geom_point( color = "blue", size = 2, fill = "white", alpha = 0.5)
g <- g +  geom_line(size = 0.2)
g <- g + facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6))
g <- g +  geom_errorbar(size = 0.3, width = 0.4, ymax = int_lsm$up_internet, ymin = int_lsm$low_internet, alpha = 0.5)
g <- g + labs(y = "internet", title = "Internet and LSM")
g
dev.off()

# 1 experiment a bit with lme (radio and age):
type_frame_age$year <- as.numeric(as.character(type_frame_age$year)) - 2000
grouped_age <- groupedData(radios ~ year | category, data = type_frame_age)
plot(grouped_age)

list_age <- lmList(grouped_age)
plot(list_age) # residuals look good
plot(intervals(list_age)) # shows random effects for both intercept and year

age_radio_lme <- lme(list_age)
summary(age_radio_lme) # shows strong correlation between intercept and year AND small fixed effect for year.. non significant fixed effects..

preds <- predict(age_radio_lme, level = 0:1)

plot(augPred(age_radio_lme), aspect = "xy", grid = T)

# 2 experiment a bit with lme (internet and age):
type_frame_age$year <- as.numeric(as.character(type_frame_age$year)) - 2000
grouped_age <- groupedData(internets ~ year | category, data = type_frame_age)
plot(grouped_age)
list_age <- lmList(grouped_age)
plot(list_age) # residuals look good
plot(intervals(list_age)) # shows random effects for both intercept and year

age_radio_lme <- lme(internets ~ year, data = grouped_age)#, random = ~ 1 | category)
summary(age_radio_lme) # shows strong correlation between intercept and year AND small fixed effect for year.. non significant fixed effects..

preds <- predict(age_radio_lme, level = 0:1)

plot(augPred(age_radio_lme), aspect = "xy", grid = T)
