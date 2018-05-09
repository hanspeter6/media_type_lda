# library(plm)
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# reading in the different datasets (would need to be simple only...)
set95c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_1995/set95c_simple.rds")
set02c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02c_simple.rds")
set05c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05c_simple.rds")
set08c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/set08c_simple.rds")
set10c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10c_simple.rds")
set12c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12c_simple.rds")
set14c_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14c_simple.rds")

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

frame_02 <- frame_bind(set02c_simple, 2002)
frame_05 <- frame_bind(set05c_simple, 2005)
frame_08 <- frame_bind(set08c_simple, 2008)
frame_10 <- frame_bind(set10c_simple, 2010)
frame_12 <- frame_bind(set12c_simple, 2012)
frame_14 <- frame_bind(set14c_simple, 2014)

# putting it together
type_frame <- rbind.data.frame(#frame_95,
                               frame_02,
                               frame_05,
                               frame_08,
                               frame_10,
                               frame_12,
                               frame_14)

saveRDS(type_frame, "type_frame.rds")

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

vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
p_up <- all_plots(type_frame[which(type_frame$category %in% vector_row1),])
p_down <- all_plots(type_frame[which(type_frame$category %in% vector_row2),])

jpeg("all_plots.jpeg", quality = 100)
grid.arrange(p_up, p_down, nrow = 2)
dev.off()

# # try a function to draw all categories on a media instead:
# all_plots_news <- function(data) {
#         ggplot(data = data) +
#                 geom_line(aes(year, news, group = category)) +
#                 facet_grid(. ~ category) + theme(axis.text.x = element_text(size = 6)) +
#                 labs(y = "engagement", title = "newspapers")
#         
# }
# 
# all_plots_news(type_frame)
# 
# jpeg("plot_type_combined_age.jpeg")
# all_plots(type_frame_age, "Age")
# dev.off()
# 
# jpeg("plot_type_combined_race.jpeg")
# all_plots(type_frame_race, "Population Group")
# dev.off()
# 
# jpeg("plot_type_combined_inc.jpeg")
# all_plots(type_frame_inc, "Household Income")
# dev.off()
# 
# jpeg("plot_type_combined_sex.jpeg")
# all_plots(type_frame_sex, "Gender")
# dev.off()
# 
# jpeg("plot_type_combined_edu.jpeg")
# all_plots(type_frame_edu, "Education Level")
# dev.off()
# 
# jpeg("plot_type_combined_lsm.jpeg")
# all_plots(type_frame_lsm, "Living Standards Measure LSM")
# dev.off()
# 
# jpeg("plot_type_combined_cluster.jpeg")
# all_plots(type_frame_cluster, "Clusters")
# dev.off()

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

p_news_age <- plot_medium_by_category(type_frame, "newspapers", "age") # etc..any combination...
p_radio_income <- plot_medium_by_category(type_frame, "radio", "income") # etc..any combination...
p_tv_race <- plot_medium_by_category(type_frame, "tv", "race")
p_internet_lsm <- plot_medium_by_category(type_frame, "internet", "lsm")
p_internet_age <- plot_medium_by_category(type_frame, "internet", "age")
p_mags_edu <- plot_medium_by_category(type_frame, "magazines", "education")

jpeg("medium_category.jpeg", quality = 100)
grid.arrange(p_news_age,
             p_radio_income,
             p_tv_race,
             p_internet_lsm,
             p_internet_age,
             p_mags_edu, nrow = 3)
dev.off()
# etc...

# MODELING

## RADIO
radio_grouped <- groupedData(radios ~ year | category, data = type_frame)
# plot(radio_grouped) # check
radio_list <- lmList(radios ~ I(year - mean(year)) | category, data = radio_grouped)
# plot(intervals(radio_list))
radio_lme <- lme(radio_list)
# summary(radio_lme)

## NEWSPAPERS
news_grouped <- groupedData(news ~ year | category, data = type_frame)
# plot(news_grouped) # check
news_list <- lmList(news ~ I(year - mean(year)) | category, data = news_grouped)
# plot(intervals(news_list))
news_lme <- lme(news_list)
# summary(news_lme)

## MAGAZINES
mags_grouped <- groupedData(mags ~ year | category, data = type_frame)
# plot(mags_grouped) # check
mags_list <- lmList(mags ~ I(year - mean(year)) | category, data = mags_grouped)
# plot(intervals(mags_list))
mags_lme <- lme(mags_list)
# summary(mags_lme)

## TV
tvs_grouped <- groupedData(tvs ~ year | category, data = type_frame)
# plot(tvs_grouped) # check
tvs_list <- lmList(tvs ~ I(year - mean(year)) | category, data = tvs_grouped)
# plot(intervals(tvs_list))
tvs_lme <- lme(tvs_list)
# summary(tvs_lme)

## INTERNET
internet_grouped <- groupedData(internets ~ year | category, data = type_frame)
# plot(internet_grouped) # check
internet_list <- lmList(internets ~ I(year - mean(year)) | category, data = internet_grouped)
# plot(intervals(internet_list))
internet_lme <- lme(internet_list)
# summary(internet_lme)

# Own plots of Medium and Categories with Fitted Values
# add model predicted values to data frame
type_frame_preds <- type_frame %>%
        mutate(preds_radio = as.vector(fitted(radio_lme))) %>%
        mutate(preds_news = as.vector(fitted(news_lme))) %>%
        mutate(preds_mags = as.vector(fitted(mags_lme))) %>%
        mutate(preds_tv = as.vector(fitted(tvs_lme))) %>%
        mutate(preds_internet = as.vector(fitted(internet_lme)))

# function for plotting fitted models
plot_fitted <- function(data = type_frame_preds, medium) { # medium: one of: newspapers, magazines, radio, tv, internet
        
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
        ggplot(data, aes_string("year", a, group = "category")) +
                geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
                geom_line(size = 0.2) +
                geom_line(aes_string("year", b, group = "category"), colour = "red", size = 0.3, linetype = 2 ) +
                facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
                geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
                labs(y = e, title = f)
        
}

## RADIO
pf_radio_up <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row1),],
                           medium = "radio")
pf_radio_down <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row2),],
                           medium = "radio")
jpeg("radio_fitted.jpeg", quality = 100)
grid.arrange(pf_radio_up, pf_radio_down, nrow = 2)
dev.off()

## TV
pf_tv_up <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row1),],
                           medium = "tv")
pf_tv_down <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row2),],
                             medium = "tv")
jpeg("tv_fitted.jpeg", quality = 100)
grid.arrange(pf_tv_up, pf_tv_down, nrow = 2)
dev.off()

## NEWSPAPERS
pf_news_up <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row1),],
                        medium = "newspapers")
pf_news_down <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row2),],
                          medium = "newspapers")
jpeg("news_fitted.jpeg", quality = 100)
grid.arrange(pf_news_up, pf_news_down, nrow = 2)
dev.off()

## MAGAZINES
pf_mags_up <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row1),],
                        medium = "magazines")
pf_mags_down <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row2),],
                          medium = "magazines")
jpeg("mags_fitted.jpeg", quality = 100)
grid.arrange(pf_mags_up, pf_mags_down, nrow = 2)
dev.off()

## INTERNET
pf_internet_up <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row1),],
                        medium = "internet")
pf_internet_down <- plot_fitted(data = type_frame_preds[which(type_frame$category %in% vector_row2),],
                          medium = "internet")
jpeg("internet_fitted.jpeg", quality = 100)
grid.arrange(pf_internet_up, pf_internet_down, nrow = 2)
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

# default plot of the grouped object
plot(radio_frame_grouped, main = "radio")

# my own, including regression lines from lme model
radio_frame_grouped <- radio_frame_grouped %>%
        mutate(preds = as.vector(fitted(radio_all_lme1)))

radio_frame_grouped$year <- as.vector(as.numeric(as.character(radio_frame_grouped$year)))

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


fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
ranef(fm1)
random.effects(fm1)             # same as above
random.effects(fm1, augFrame = TRUE)
print(random.effects(fm1))