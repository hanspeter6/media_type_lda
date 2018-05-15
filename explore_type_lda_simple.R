# library(plm)
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# reading in the different datasets (would need to be simple only...)
# set95c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_1995/set95c_simple.rds")
set02c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02c_simple.rds")
set05c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05c_simple.rds")
set08c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/set08c_simple.rds")
set10c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10c_simple.rds")
set12c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12c_simple.rds")
set14c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14c_simple.rds")

# EXPLORATION OF DESCRIPTIVE STUFF BY YEAR
# try to create single frame:
# add year to first two frames:
ex_set <- function(set, year) {
        set[,c(1,3:8,14)] %>%
                mutate(year = year)
}

big_set_simple <- rbind.data.frame(ex_set(set02c_simple, 2002),
                            ex_set(set05c_simple, 2005),
                            ex_set(set08c_simple, 2008),
                            ex_set(set10c_simple, 2010),
                            ex_set(set12c_simple, 2012),
                            ex_set(set14c_simple, 2014))

# some exploratory descriptive stats plots:
expl_demogs <- function(set, category) {
        if(category == "race") {
                level = c("black", "coloured", "indian", "white")
                title = "Population Group"
        }
        if(category == "edu") {
                level = c(c("<matric", "matric",">matric"))
                title = "Education Level"
        }
        if(category == "age") {
                level = c(c("15-24","25-44", "45-54","55+"))
                title = "Age Group"
        }
        if(category == "lsm") {
                level = c("1-2", "3-4", "5-6", "7-8", "9-10")
                title = "LSM"
        }
        if(category == "sex") {
                level = c("male", "female")
                title = "Gender"
        }
        if(category == "hh_inc") {
                level = c("<5000","5000-10999","11000-19999",">=20000")
                title = "Household Income"
        }
        
        ggplot(data = set) +
                aes(x = factor(year)) +
                aes_string(fill = category) +
                geom_bar(stat = "count", position = position_dodge()) +
                scale_fill_discrete(labels = level) +
                labs(title = title, x = "year") +
                guides(fill=guide_legend(title=NULL)) +
                theme(plot.title = element_text(size = 8),
                      axis.text.y = element_text(size = 6),
                      axis.text.x = element_text(size = 6)) 
}

jpeg('exDemogPlots_simple.jpeg', quality = 100, type = "cairo")
grid.arrange(expl_demogs(big_set_simple, "sex"),
             expl_demogs(big_set_simple, "age"),
             expl_demogs(big_set_simple, "race"),
             expl_demogs(big_set_simple, "edu"),
             expl_demogs(big_set_simple, "hh_inc"),
             expl_demogs(big_set_simple, "lsm"),
             ncol=2, nrow = 3)
dev.off()

## preparing dataset for LDA
# function to create frames (for all except '95 since doesnt have internet)
# single level
frames <- function(set, category) {
        require(dplyr)
        
        # set factor labels (NB double check levels)
        set$age <- factor(set$age, labels = c("15-24","25-44", "45-54","55+"), ordered = TRUE)
        set$race <- factor(set$race,labels = c("black", "coloured", "indian", "white"), ordered = TRUE)
        set$edu <- factor(set$edu, labels = c("<matric", "matric",">matric" ) ,ordered = TRUE)
        set$lsm <- factor(set$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = TRUE)
        set$sex <- factor(set$sex, labels = c("male", "female"), ordered = TRUE)
        set$hh_inc <- factor(set$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"), ordered = TRUE)
        set$cluster <- factor(set$cluster, labels = c("c1", "c2", "c3", "c4"), ordered = TRUE)
        
        
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
# # for now, simply zero
# set95c_simple <- set95c_simple %>%
#         mutate(internet = 0)
# 
# # also, adjusted function to exclude lsm
# frame_95 <- rbind.data.frame(frames(set95c_simple,"sex"),
#                              frames(set95c_simple,"age"),
#                              frames(set95c_simple,"edu"),
#                              frames(set95c_simple,"race"),
#                              frames(set95c_simple, "hh_inc"),
#                              frames(set95c_simple, "cluster")) %>% # dont have lsm...
#         mutate(year = 1995) %>%
#         select(category, year, everything())

# function to bind the frames by year
frame_bind <- function(set, year) {
        rbind.data.frame(frames(set,"sex"),
                         frames(set,"age"),
                         frames(set,"edu"),
                         frames(set,"race"),
                         frames(set, "hh_inc"),
                         frames(set,"lsm"),
                         frames(set, "cluster")) %>%
                mutate(year = year) %>%
                select(category, year, everything())
        
}

frame_02_simple <- frame_bind(set02c_simple, 2002)
frame_05_simple <- frame_bind(set05c_simple, 2005)
frame_08_simple <- frame_bind(set08c_simple, 2008)
frame_10_simple <- frame_bind(set10c_simple, 2010)
frame_12_simple <- frame_bind(set12c_simple, 2012)
frame_14_simple <- frame_bind(set14c_simple, 2014)

# putting it together
type_frame_simple <- rbind.data.frame(#frame_95,
        frame_02_simple,
        frame_05_simple,
        frame_08_simple,
        frame_10_simple,
        frame_12_simple,
        frame_14_simple)

saveRDS(type_frame_simple, "type_frame_simple.rds")

# type_frame_typeGathered <- gather(type_frame, key = "type", value = "engagement", news, mags, tvs, radios, internets, alls)

# # change category ordered to unorders
# type_frame$category <- factor(type_frame$category, ordered = FALSE)

# EXPLORING

# considering plots of all media on demographic categories
# defining a function
all_plots <- function(data, title = "All Media Types") {
        ggplot(data = data, title = title) +
                geom_line(aes(year, news, group = category, colour = "newspaper")) +
                geom_line(aes(year, mags, group = category, colour = "magazine")) +
                geom_line(aes(year, radios, group = category, colour = "radio")) +
                geom_line(aes(year, tvs, group = category, colour = "tv")) +
                geom_line(aes(year, internets, group = category, colour = "internet")) +
                geom_line(aes(year, alls, group = category, colour = "all")) +
                scale_colour_discrete(name="Media") +
                facet_grid(. ~ category) +
                theme(axis.text.x = element_text(size = 6)) +
                labs(y = "engagement", title = title)
        
}

vector_row1_simple <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2_simple <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
p_up_simple <- all_plots(type_frame[which(type_frame$category %in% vector_row1),])
p_down_simple <- all_plots(type_frame[which(type_frame$category %in% vector_row2),])

jpeg("all_plots_simple.jpeg", quality = 100)
grid.arrange(p_up_simple, p_down_simple, nrow = 2)
dev.off()

# function to plot details eith error bars: medium per category:
plot_medium_by_category <- function(data, medium, category) {# category: one of age, race, income, sex, education, lsm, cluster
        # medium: one of: newspapers, magazines, radio, tv, internet
        age_levels <- c("15-24","25-44", "45-54","55+" )
        race_levels <- c("black", "coloured", "indian", "white")
        inc_levels <- c("<R2500","R2500-R6999","R7000-R11999",">=R12000")
        sex_levels <- c("male", "female")
        edu_levels <- c("<matric", "matric",">matric")
        lsm_levels <- c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
        cluster_levels <- c("c1", "c2", "c3", "c4")
        
        if(category == "age") {
                temp_levels <- age_levels
        }
        if(category == "race") {
                temp_levels <- race_levels
        }
        if(category == "income") {
                temp_levels <- inc_levels
        }
        if(category == "sex") {
                temp_levels <- sex_levels
        }
        if(category == "education") {
                temp_levels <- edu_levels
        }
        if(category == "lsm") {
                temp_levels <- lsm_levels
        }
        if(category == "cluster") {
                temp_levels <- cluster_levels
        }
        
        temp_frame <- data %>%
                filter(category %in% temp_levels)
        
        if(medium == "newspapers") {
                a <- "news"
                b <- "low_newspapers"
                c <- "up_newspapers"
                d <- "newspapers"
                e <- paste("Newspapers and ", category)
        }
        if(medium == "magazines") {
                a <- "mags"
                b <- "low_magazines"
                c <- "up_magazines"
                d <- "magazines"
                e <- paste("Magazines and ", category)
        }
        if(medium == "tv") {
                a <- "tvs"
                b <- "low_tv"
                c <- "up_tv"
                d <- "tv"
                e <- paste("Television and ", category)
        }
        if(medium == "radio") {
                a <- "radios"
                b <- "low_radio"
                c <- "up_radio"
                d <- "radio"
                e <- paste("Radio and ", category)
        }
        if(medium == "internet") {
                a <- "internets"
                b <- "low_internet"
                c <- "up_internet"
                d <- "internet"
                e <- paste("Internet and ", category)
        }
        
        
        ggplot(temp_frame, aes_string("year", a, group = "category")) +
                geom_point( color = "blue", size = 1, fill = "white", alpha = 0.5) +
                geom_line(size = 0.2) +
                facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
                geom_errorbar(aes_string(ymin = b, ymax = c),size = 0.3, width = 0.4, alpha = 0.5) +
                labs(y = d, title = e)
}

p_news_age_simple <- plot_medium_by_category(type_frame, "newspapers", "age") # etc..any combination...
p_radio_income_simple <- plot_medium_by_category(type_frame, "radio", "income") # etc..any combination...
p_tv_race_simple <- plot_medium_by_category(type_frame, "tv", "race")
p_internet_lsm_simple <- plot_medium_by_category(type_frame, "internet", "lsm")
p_internet_age_simple <- plot_medium_by_category(type_frame, "internet", "age")
p_mags_edu_simple <- plot_medium_by_category(type_frame, "magazines", "education")

jpeg("medium_category_simple.jpeg", quality = 100)
grid.arrange(p_news_age_simple,
             p_radio_income_simple,
             p_tv_race_simple,
             p_internet_lsm_simple,
             p_internet_age_simple,
             p_mags_edu, nrow = 3)
dev.off()
# etc...

# MODELING

## RADIO
radio_grouped_simple <- groupedData(radios ~ year | category, data = type_frame_simple)
# plot(radio_grouped) # check
radio_list_simple <- lmList(radios ~ I(year - mean(year)) | category, data = radio_grouped_simple)
# plot(intervals(radio_list))
radio_lme_simple <- lme(radio_list_simple)
# summary(radio_lme)

## NEWSPAPERS
news_grouped_simple <- groupedData(news ~ year | category, data = type_frame_simple)
# plot(news_grouped) # check
news_list_simple <- lmList(news ~ I(year - mean(year)) | category, data = news_grouped_simple)
# plot(intervals(news_list))
news_lme_simple <- lme(news_list_simple)
# summary(news_lme)

## MAGAZINES
mags_grouped_simple <- groupedData(mags ~ year | category, data = type_frame_simple)
# plot(mags_grouped) # check
mags_list_simple <- lmList(mags ~ I(year - mean(year)) | category, data = mags_grouped_simple)
# plot(intervals(mags_list))
mags_lme_simple <- lme(mags_list_simple)
# summary(mags_lme)

## TV
tvs_grouped_simple <- groupedData(tvs ~ year | category, data = type_frame_simple)
# plot(tvs_grouped) # check
tvs_list_simple <- lmList(tvs ~ I(year - mean(year)) | category, data = tvs_grouped_simple)
# plot(intervals(tvs_list))
tvs_lme_simple <- lme(tvs_list_simple)
# summary(tvs_lme)

## INTERNET
internet_grouped_simple <- groupedData(internets ~ year | category, data = type_frame_simple)
# plot(internet_grouped) # check
internet_list_simple <- lmList(internets ~ I(year - mean(year)) | category, data = internet_grouped_simple)
# plot(intervals(internet_list))
internet_lme_simple <- lme(internet_list_simple)
# summary(internet_lme)

# Own plots of Medium and Categories with Fitted Values
# add model predicted values to data frame
type_frame_preds_simple <- type_frame_simple %>%
        mutate(preds_radio = as.vector(fitted(radio_lme_simple))) %>%
        mutate(preds_news = as.vector(fitted(news_lme_simple))) %>%
        mutate(preds_mags = as.vector(fitted(mags_lme_simple))) %>%
        mutate(preds_tv = as.vector(fitted(tvs_lme_simple))) %>%
        mutate(preds_internet = as.vector(fitted(internet_lme_simple)))

# function for plotting fitted models
plot_fitted <- function(data, medium) { # medium: one of: newspapers, magazines, radio, tv, internet
        
        if(medium == "newspapers") {
                a <- "news"
                b <- "preds_news"
                c <- "up_newspapers"
                d <- "low_newspapers"
                e <- "newspapers"
                f <- "Newspapers with Fitted Values"
        }
        if(medium == "magazines") {
                a <- "mags"
                b <- "preds_mags"
                c <- "up_magazines"
                d <- "low_magazines"
                e <- "magazines"
                f <- "Magazines with Fitted Values"
        }
        if(medium == "tv") {
                a <- "tvs"
                b <- "preds_tv"
                c <- "up_tv"
                d <- "low_tv"
                e <- "tv"
                f <- "TV with Fitted Values"
        }
        if(medium == "radio") {
                a <- "radios"
                b <- "preds_radio"
                c <- "up_radio"
                d <- "low_radio"
                e <- "radio"
                f <- "Radio with Fitted Values"
        }
        if(medium == "internet") {
                a <- "internets"
                b <- "preds_internet"
                c <- "up_internet"
                d <- "low_internet"
                e <- "internet"
                f <- "Internet with Fitted Values"
        }
        
        #plot
        ggplot(data = data, aes_string("year", a, group = "category")) +
                geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
                geom_line(size = 0.2) +
                geom_line(aes_string("year", b, group = "category"), colour = "red", size = 0.3, linetype = 2 ) +
                facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
                geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
                labs(y = e, title = f)
        
}

## RADIO
pf_radio_up_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row1_simple),],
                           medium = "radio")
pf_radio_down_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row2_simple),],
                             medium = "radio")
jpeg("radio_fitted_simple.jpeg", quality = 100)
grid.arrange(pf_radio_up_simple, pf_radio_down_simple, nrow = 2)
dev.off()

## TV
pf_tv_up_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row1_simple),],
                                  medium = "tv")
pf_tv_down_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row2_simple),],
                                    medium = "tv")
jpeg("tv_fitted_simple.jpeg", quality = 100)
grid.arrange(pf_tv_up_simple, pf_tv_down_simple, nrow = 2)
dev.off()

## NEWSPAPERS
pf_newspapers_up_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row1_simple),],
                               medium = "newspapers")
pf_newspapers_down_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row2_simple),],
                                 medium = "newspapers")
jpeg("newspapers_fitted_simple.jpeg", quality = 100)
grid.arrange(pf_newspapers_up_simple, pf_newspapers_down_simple, nrow = 2)
dev.off()

## MAGAZINES
pf_magazines_up_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row1_simple),],
                               medium = "magazines")
pf_magazines_down_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row2_simple),],
                                 medium = "magazines")
jpeg("magazines_fitted_simple.jpeg", quality = 100)
grid.arrange(pf_magazines_up_simple, pf_magazines_down_simple, nrow = 2)
dev.off()

## INTERNET
pf_internet_up_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row1_simple),],
                               medium = "internet")
pf_internet_down_simple <- plot_fitted(data = type_frame_preds_simple[which(type_frame_simple$category %in% vector_row2_simple),],
                                 medium = "internet")
jpeg("internet_fitted_simple.jpeg", quality = 100)
grid.arrange(pf_internet_up_simple, pf_internet_down_simple, nrow = 2)
dev.off()

