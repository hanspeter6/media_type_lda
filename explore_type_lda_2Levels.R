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

# a nested option
frames2 <- function(set, category1, category2) {
        require(dplyr)
        
        set$age <- factor(set$age, labels = c("15-24","25-44", "45-54","55+"), ordered = TRUE)
        set$race <- factor(set$race,labels = c("black", "coloured", "indian", "white"), ordered = TRUE)
        set$edu <- factor(set$edu, labels = c("<matric", "matric",">matric" ) ,ordered = TRUE)
        # set$lsm <- factor(set$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = TRUE)
        set$sex <- factor(set$sex, labels = c("male", "female"), ordered = TRUE)
        set$hh_inc <- factor(set$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"), ordered = TRUE) # NB 2012 levels
        # set$cluster <- factor(set$cluster, labels = c("c1", "c2", "c3", "c4"))
        
        
        set %>%
                group_by_(category1 = category1, category2 = category2) %>%
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

# function to bind the frames by year first level race
frame_bind_race <- function(set, year) {
        rbind.data.frame(frames2(set,"race","sex"),
                         frames2(set,"race","age"),
                         frames2(set,"race","edu"),
                         # frames2(set,"race","race"),
                         frames2(set,"race", "hh_inc")  )    %>%
                         # frames2(set,"race","lsm"),
                         # frames2(set,"race", "cluster")),
   
                mutate(year = year) %>%
                select(category1,category2, year, everything())
        
}

frame_02_race <- frame_bind_race(set02c_simple, 2002)
frame_05_race <- frame_bind_race(set05c_simple, 2005)
frame_08_race <- frame_bind_race(set08c_simple, 2008)
frame_10_race <- frame_bind_race(set10c_simple, 2010)
frame_12_race <- frame_bind_race(set12c_simple, 2012)
frame_14_race <- frame_bind_race(set14c_simple, 2014)

# putting it together
type_frame_race <- rbind.data.frame(#frame_95,
        frame_02_race,
        frame_05_race,
        frame_08_race,
        frame_10_race,
        frame_12_race,
        frame_14_race)

type_frame_race <- as.data.frame(type_frame_race)
# type_frame_race$category1 <- factor(type_frame_race$category1, ordered = FALSE)
# type_frame_race$category2 <- factor(type_frame_race$category2, ordered = FALSE)

# isBalanced(radio_grouped_race) # True
# 
# # RADIO 2
radio_grouped_race <- groupedData(radios ~ year | category1/category2, data = type_frame_race[,c(1:8)])
# plot(radio_grouped_race) # check
radio_list_race <- lmList(radios ~ I(year-mean(year)) | category1/category2, data = type_frame_race[,c(1:8)])
# plot(intervals(radio_list_race))
control <- lmeControl(opt = "optim") # using optim .... get warnings but does create fit...
radio_lme_race <- lme(radio_grouped_race, control = control)
# summary(radio_lme_race)

# # NEWSPAPERS 2
news_grouped_race <- groupedData(news ~ year | category1/category2, data = type_frame_race[,c(1:8)])
# plot(news_grouped_race) # check
news_list_race <- lmList(news ~ I(year-mean(year)) | category1/category2, data = type_frame_race[,c(1:8)])
# plot(intervals(news_list_race))
control <- lmeControl(opt = "optim") # using optim .... get warnings but does create fit...
news_lme_race <- lme(news_grouped_race, control = control)
# summary(news_lme_race)

# # MAGAZINES 2
mags_grouped_race <- groupedData(mags ~ year | category1/category2, data = type_frame_race[,c(1:8)])
# plot(mags_grouped_race) # check
mags_list_race <- lmList(mags ~ I(year-mean(year)) | category1/category2, data = type_frame_race[,c(1:8)])
# plot(intervals(mags_list_race))
control <- lmeControl(opt = "optim") # using optim .... get warnings but does create fit...
mags_lme_race <- lme(mags_grouped_race, control = control)
# summary(mags_lme_race)

# # TV 2
tvs_grouped_race <- groupedData(tvs ~ year | category1/category2, data = type_frame_race[,c(1:8)])
# plot(tvs_grouped_race) # check
tvs_list_race <- lmList(tvs ~ I(year-mean(year)) | category1/category2, data = type_frame_race[,c(1:8)])
# plot(intervals(tvs_list_race))
control <- lmeControl(opt = "optim") # using optim .... get warnings but does create fit...
tvs_lme_race <- lme(tvs_grouped_race, control = control)
# summary(tvs_lme_race)


## TRY PLOT TV WITH TV FITTED VALUES
tvs_grouped_race <- tvs_grouped_race %>%
        mutate(preds_tv = as.vector(fitted(tvs_lme_race)))

ggplot(tvs_grouped_race, aes_string("year", "tvs", group = "category1")) +
        geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
        geom_line(size = 0.2) +
        geom_line(aes_string("year", "preds_tv", group = "category1"), colour = "red", size = 0.3, linetype = 2 ) +
        facet_grid(category1 ~ category2) + theme(axis.text.x = element_text(size = 6))


        
        ggplot(data= type_frame_preds2, aes_string("year", "tvs", group = "category1")) +
        geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
        geom_line(size = 0.2) +
        geom_line(aes_string("year", "preds_tv", group = "category1"), colour = "red", size = 0.3, linetype = 2 ) +
        facet_grid(category1 ~ category2) + theme(axis.text.x = element_text(size = 6))

        


# # INTERNET 2
internets_grouped_race <- groupedData(internets ~ year | category1/category2, data = type_frame_race[,c(1:8)])
# plot(internets_grouped_race) # check
internets_list_race <- lmList(internets ~ I(year-mean(year)) | category1/category2, data = type_frame_race[,c(1:8)])
# plot(intervals(internets_list_race))
control <- lmeControl(opt = "optim") # using optim .... get warnings but does create fit...
internets_lme_race <- lme(internets_grouped_race, control = control)
# summary(internets_lme_race)

# try to plot with fitted values

# Own plots of Medium and Categories with Fitted Values
# add model predicted values to data frame
type_frame_preds_race <- type_frame_race %>%
        mutate(preds_radio = as.vector(fitted(radio_lme_race))) %>%
        mutate(preds_news = as.vector(fitted(news_lme_race))) %>%
        mutate(preds_mags = as.vector(fitted(mags_lme_race))) %>%
        mutate(preds_tv = as.vector(fitted(tvs_lme_race))) %>%
        mutate(preds_internet = as.vector(fitted(internets_lme_race)))

# function for plotting fitted models
plot_fitted_race <- function(data = type_frame_preds_race, medium) { # medium: one of: newspapers, magazines, radio, tv, internet
        
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
        
        # dataset:
        
        
        #plot
        ggplot(data) +
                geom_point(aes_string("year", a), color = "blue", size = 1, fill = "white", alpha = 0.5) +
                geom_line(size = 0.2) +
                geom_line(aes_string("year", b), colour = "red", size = 0.3, linetype = 2 ) +
                facet_grid(category1 ~ category2) + theme(axis.text.x = element_text(size = 6)) +
                geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
                labs(y = e, title = f)
        
}

ggplot(data= type_frame_preds2, aes_string("year", "tvs", group = "category1")) +
        geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
        geom_line(size = 0.2) +
        geom_line(aes_string("year", "preds_tv", group = "category1"), colour = "red", size = 0.3, linetype = 2 ) +
        facet_grid(category1 ~ category2) + theme(axis.text.x = element_text(size = 6))

vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")

## RADIO
pf_radio_up2 <- plot_fitted2(data = type_frame_preds2[which(type_frame_race$category2 %in% c(vector_row1, vector_row2)),],
                           medium = "radio")
# pf_radio_down2 <- plot_fitted2(data = type_frame_preds2[which(type_frame_race$category2 %in% vector_row2),],
#                              medium = "radio")
# jpeg("radio_fitted2.jpeg", quality = 100)
# grid.arrange(pf_radio_up2, pf_radio_down2, nrow = 2)
# dev.off()

## TV
pf_tv_up2 <- plot_fitted2(data = type_frame_preds2,
                             medium = "tv")
# pf_tv_down2 <- plot_fitted2(data = type_frame_preds2[which(type_frame_race$category2 %in% vector_row2),],
#                                medium = "tv")
# jpeg("tv_fitted2.jpeg", quality = 100)
# grid.arrange(pf_tv_up2, pf_tv_down2, nrow = 2)
# dev.off()

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

