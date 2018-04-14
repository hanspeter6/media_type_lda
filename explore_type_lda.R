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

# # a nested option
# frames2 <- function(set, category1, category2) {
#         require(dplyr)
#         
#         set$age <- factor(set$age, labels = c("15-24","25-44", "45-54","55+"), ordered = TRUE)
#         set$race <- factor(set$race,labels = c("black", "coloured", "indian", "white"), ordered = TRUE)
#         set$edu <- factor(set$edu, labels = c("<matric", "matric",">matric" ) ,ordered = TRUE)
#         set$lsm <- factor(set$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = TRUE)
#         set$sex <- factor(set$sex, labels = c("male", "female"), ordered = TRUE)
#         set$hh_inc <- factor(set$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"), ordered = TRUE) # NB 2012 levels
#         set$cluster <- factor(set$cluster, labels = c("1", "2", "3", "4"))
#         
#         
#         set %>%
#                 group_by_(category1 = category1, category2 = category2) %>%
#                 summarise(news = mean(newspapers),
#                           mags = mean(magazines),
#                           tvs = mean(tv),
#                           radios = mean(radio),
#                           internets = mean(internet),
#                           alls = mean(all),
#                           up_all = mean(all) + (2 * sd(all)/sqrt(length(all))),
#                           low_all = mean(all) - (2 * sd(all)/sqrt(length(all))),
#                           up_newspapers = mean(newspapers) + (2 * sd(newspapers)/sqrt(length(newspapers))),
#                           low_newspapers = mean(newspapers) - (2 * sd(newspapers)/sqrt(length(newspapers))),
#                           up_magazines = mean(magazines) + (2 * sd(magazines)/sqrt(length(magazines))),
#                           low_magazines = mean(magazines) - (2 * sd(magazines)/sqrt(length(magazines))),
#                           up_tv = mean(tv) + (2 * sd(tv)/sqrt(length(tv))),
#                           low_tv = mean(tv) - (2 * sd(tv)/sqrt(length(tv))),
#                           up_radio = mean(radio) + (2 * sd(radio)/sqrt(length(radio))),
#                           low_radio = mean(radio) - (2 * sd(radio)/sqrt(length(radio))),
#                           up_internet = mean(internet) + (2 * sd(internet)/sqrt(length(internet))),
#                           low_internet = mean(internet) - (2 * sd(internet)/sqrt(length(internet)))
#                 )
#         
# }

# single level
frames <- function(set, category) {
        require(dplyr)
        
        set$age <- factor(set$age, labels = c("15-24","25-44", "45-54","55+"), ordered = TRUE)
        set$race <- factor(set$race,labels = c("black", "coloured", "indian", "white"), ordered = TRUE)
        set$edu <- factor(set$edu, labels = c("<matric", "matric",">matric" ) ,ordered = TRUE)
        set$lsm <- factor(set$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = TRUE)
        set$sex <- factor(set$sex, labels = c("male", "female"), ordered = TRUE)
        set$hh_inc <- factor(set$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"), ordered = TRUE) # NB 2012 levels
        set$cluster <- factor(set$cluster, labels = c("cluster1", "cluster2", "cluster3", "cluster4"))
        
        
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

# # create a vector to use for internet '95:
# # considered range(set10c$internet)
# set95c <- set95c %>%
#         mutate(internet = -1)
# 
# # also, adjusted function to exclude lsm
# frame_95 <- rbind.data.frame(frames(set95c,"sex"),
#                              frames(set95c,"age"),
#                              frames(set95c,"edu"),
#                              frames(set95c,"race"),
#                              frames(set95c, "hh_inc")) %>% # dont have lsm...
#         mutate(year = 1995) %>%
#         select(category, year, everything())

# function to bind the frames by year
frame_bind <- function(set, year) {
        rbind.data.frame(frames(set,"sex"),
                         frames(set,"age"),
                         frames(set,"edu"),
                         frames(set,"race"),
                         frames(set, "hh_inc"),
                         frames(set,"lsm")) %>% # frames(set,"cluster")
                mutate(year = year) %>%
                select(category, year, everything())
        
}

frame_02 <- frame_bind(set02c, 2002)
frame_05 <- frame_bind(set05c, 2005)
frame_08 <- frame_bind(set08c, 2008)
frame_10 <- frame_bind(set10c, 2010)
frame_12 <- frame_bind(set12c, 2012)
frame_14 <- frame_bind(set14c, 2014)

# putting it together
type_frame <- rbind.data.frame(#frame_95,
                               frame_02,
                               frame_05,
                               frame_08,
                               frame_10,
                               frame_12,
                               frame_14)

# # create a grouped object for nlme:
# news_grouped <- groupedData(news ~ year | category, data = type_frame, order.groups = TRUE)
# 
# plot(news_grouped, grid = TRUE)


# # putting it into plm data frame type
# type_frame <- pdata.frame(type_frame)#, index = c("country", "year"), drop.index = FALSE)

# basic nlme

# frames per grouping
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
type_frame_cluster <- type_frame %>%
        filter(category %in% c("cluster1", "cluster2", "cluster3", "cluster4"))

# considering plots of all media on demographic categories
# defining a function
all_plots <- function(data, title) {
        ggplot(data = data, title = "title") +
                geom_line(aes(year, news, group = category, colour = "newspaper")) +
                geom_line(aes(year, mags, group = category, colour = "magazine")) +
                geom_line(aes(year, radios, group = category, colour = "radio")) +
                geom_line(aes(year, tvs, group = category, colour = "tv")) +
                geom_line(aes(year, internets, group = category, colour = "internet")) +
                geom_line(aes(year, alls, group = category, colour = "all")) +
                scale_colour_discrete(name="Media") +
                facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
                labs(y = "engagement", title = title)
        
}

# try a function to draw all categories on a media instead:
all_plots_news <- function(data) {
        ggplot(data = data) +
                geom_line(aes(year, news, group = category)) +
                facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
                labs(y = "engagement", title = "newspapers")
        
}

all_plots_news(type_frame)

jpeg("plot_type_combined_age.jpeg")
all_plots(type_frame_age, "Age")
dev.off()

jpeg("plot_type_combined_race.jpeg")
all_plots(type_frame_race, "Population Group")
dev.off()

jpeg("plot_type_combined_inc.jpeg")
all_plots(type_frame_inc, "Household Income")
dev.off()

jpeg("plot_type_combined_sex.jpeg")
all_plots(type_frame_sex, "Gender")
dev.off()

jpeg("plot_type_combined_edu.jpeg")
all_plots(type_frame_edu, "Education Level")
dev.off()

jpeg("plot_type_combined_lsm.jpeg")
all_plots(type_frame_lsm, "Living Standards Measure LSM")
dev.off()

jpeg("plot_type_combined_cluster.jpeg")
all_plots(type_frame_cluster, "Clusters")
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


# Radio and Age with Fitted Values
radios_age <- radios_age %>%
        mutate(preds = as.vector(fitted(lme1_radio_age)))
jpeg("lds_radios_age_fitted.jpeg")
ggplot(data = radios_age, aes(year, radios, group = category)) +
        geom_point(color = "blue", size = 2, fill = "white", alpha = 0.5) +
        geom_line(size = 0.2) +
        geom_line(aes(year, preds, group = category), colour = "red", size = 0.3, linetype = 2 ) +
        facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
        geom_errorbar(size = 0.3, width = 0.4, ymax = radios_age$up_radio, ymin = radios_age$low_radio, alpha = 0.5) +
        labs(y = "radio", title = "Radio and Age with Fitted Values")
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
int_age <- groupedData(internets ~ as.numeric(as.character(year))| category, data = type_frame_age)
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


# 1 experiment a bit with lme (newspapers and age):

# check out a few with package plots:
plot(tv_lsm)
plot(radios_age) # done
plot(int_inc)
plot(newsp_edu)
plot(mags_race)
plot(newsp_age) # curved...
plot(int_age) # looks like growth curve (non linear)

# fit a basic single level lme model to one of these (age and radio)
# considier individual fixed lm fits
# # first need to turn year from factor (in type frames) to numeric
radios_age$year <- as.vector(as.numeric(as.character(radios_age$year)))

list_radio_age <- lmList(radios ~ I(year - 2008), data = radios_age)
plot(list_radio_age) # residuals look ok for the amount of data
plot(intervals(list_radio_age))

# confirms earlier view. And suggests random effects on intercept but possibly not on the slopes:
# could consider both options??

# first try an lme model with random effects on both intercept and slope:
lme1_radio_age <- lme(list_radio_age)
summary(lme1_radio_age) # shows high correlation between random effects

lme2_radio_age <- update(lme1_radio_age, random = pdDiag( ~ year) )
summary(lme2_radio_age)

anova(lme1_radio_age, lme2_radio_age)

# comparing the first model with a simple linear regression fit on the data:
lm_radio_age <- lm(radios ~ I(year - 2008), data = radios_age)
anova(lme1_radio_age, lm_radio_age)

# considering confidence intervals for coefficients:
intervals(lme1_radio_age)
 
# # consider predictions:
preds_lme <- predict(lme1_radio_age, primary = ~ I(year - 2008)) # see fitted plots

# also consider diagnostics:
# homoscedasticity : actually done in plot of residuals
# already did consider correlation structure pDiag







# try to fit a single model to all for radio: maybe then leave out the last two measurements, predict and compare the errors:
# contstruct grouped object
radio_frame_grouped <- groupedData(radios ~ year| category, data = type_frame)
news_frame_grouped <- groupedData(news ~ year| category, data = type_frame)
# default plot of the grouped object
plot(radio_frame_grouped, main = "radio")
plot(news_frame_grouped, main = "newspapers")

# my own, including regression lines from lme model
radio_frame_grouped <- radio_frame_grouped %>%
        mutate(preds = as.vector(fitted(radio_all_lme1)))

jpeg("radio_all_lme.jpeg")
ggplot(data = radio_frame_grouped, aes(year, radios, group = category)) +
        geom_point(color = "blue", size = 1, alpha = 0.5) +
        geom_line(size = 0.2) +
        facet_wrap( ~ category, nrow = 2) +
        theme(axis.text.x = element_text(size = 6)) + 
        geom_errorbar(size = 0.3, width = 0.4, aes(ymax = up_radio, ymin = low_radio), alpha = 0.5) +
        geom_line(aes(year, preds, group = category), colour = "red", size = 0.3, linetype = 2 ) +
        labs(y = "relative engagement", title = "Radio")
dev.off()      
# considering individual linear models  
list_radio_all <- lmList(radios ~ I(year - 2008), data = radio_frame_grouped)

jpeg("list_radio_all.jpeg")
plot(intervals(list_radio_all))
dev.off()

# fitting first basic model
radio_all_lme1 <- lme(list_radio_all)
# looking at the summary
summary(radio_all_lme1)
# comments:
# could consider diagonal correlation structure
# could 

# diagnostic plot of residuals:
plot(radio_all_lme1) # could consider some alternative model to deal with centering of maybe (although sparse...)

# produce table of random effects:
intervals(radio_all_lme1)$reStruct$category

# comparison with single lm model
radio_all_lm <- lm(radios ~ I(year - 2008), data = radio_frame_grouped)
anova(radio_all_lme1, radio_all_lm)

# try and predict 2012 and 2014 values based on a mixed effects model for the previous four years..
radio_train <- radio_frame_grouped %>%
        filter(year %in% c(2002,2005,2008,2010))
radio_test <- radio_frame_grouped %>%
        filter(year %in% c(2012, 2014))
        
# recreating grouped objects
radio_train_grouped <- groupedData(radios ~ I(year - 2006) | category, data = radio_train)
radio_test_grouped <- groupedData(radios ~ I(year - 2013) | category, data = radio_test)

# considering individual fits
list_radio_all_train <- lmList(radio_train_grouped)
plot(intervals(list_radio_all_train))

# fitting basic model on both slope and intercept
radio_all_lme_train <- lme(list_radio_all_train)

# looking at the summary
summary(radio_all_lme_train)

# consider predicting radio on test set
radio_test_predictions <- predict(radio_all_lme_train, newdata = radio_test_grouped)

# consider error
mstd_test_error_radio <- sqrt(mean((radio_test_predictions - radio_test_grouped$radios)^2)) # small enough??

# discussion: considering random effects tables...
# ie want to draw table to random effects slope and for





# newspapers
news_frame_grouped <- groupedData(news ~ year| category, data = type_frame)
plot(news_frame_grouped, main = "newspapers")
list_news_all <- lmList(news ~ I(year - 2008), data = news_frame_grouped)
plot(intervals(list_news_all))
news_all_lme <- lme(list_news_all)
summary(news_all_lme)
news_all_lm <- lm(news ~ I(year - 2008), data = news_frame_grouped)
anova(news_all_lme, news_all_lm)
plot(news_all_lme) # may not satisfy homoscedasticity assumptions

# tv
tv_frame_grouped <- groupedData(tvs ~ year| category, data = type_frame)
plot(tv_frame_grouped, main = "tv")
list_tv_all <- lmList(tvs ~ I(year - 2008), data = tv_frame_grouped)
plot(intervals(list_tv_all))
tv_all_lme <- lme(list_tv_all)
summary(tv_all_lme)
tv_all_lm <- lm(tvs ~ I(year - 2008), data = tv_frame_grouped)
anova(tv_all_lme, tv_all_lm)
plot(tv_all_lme) # may not satisfy homoscedasticity assumptions: actually definitely does not satisfy..



# trying the whole lot into single, nested set: 

# category unordered
type_frame$category <- factor(type_frame$category, ordered = FALSE)
test <- gather(type_frame, key = type, value = engagement, news, mags, tvs, radios, internets, alls)
test$type <- factor(test$type)

test_grouped <- groupedData(engagement ~ year | category/type, data = test)[,c(1,2,15,16)]

formula(test_grouped)
plot(test_grouped)
plot(test_grouped, display = 1, collapse = 2)
test_list <- lmList(test_grouped)
plot(intervals(test_list))

test_lme <- lme(engagement ~ year | category/type, data = test)
summary(test_lme)
plot(test_lme)