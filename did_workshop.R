#-------------------------------------------------------------------#
#-------------------------DiD workshop UNSW-------------------------#
#-------------------------------------------------------------------#

# Materials compiled by:
  # Nils Wloemert (nils.wloemert@wu.ac.at)
  # Daniel Winkler (daniel.winkler@wu.ac.at)
# Please do not distribute without permission

#-------------------------------------------------------------------#
#---------------------------Load packages---------------------------#
#-------------------------------------------------------------------#

library(ggplot2)
library(psych)
library(data.table)
library(tidyverse)
options(scipen = 999)

#-------------------------------------------------------------------#
#---------------------------Preliminaries---------------------------#
#-------------------------------------------------------------------#

#panel data models
#-------------------------------------------------------------------
#load data
music_data <- fread("https://raw.githubusercontent.com/WU-RDS/RMA2022/main/data/music_data.csv")
head(music_data)
#convert to factor
music_data$song_id <- as.factor(music_data$song_id)
music_data$genre <- as.factor(music_data$genre)
#number of unique songs in data set
length(unique(music_data$song_id))

#example plot to visualize the data structure
ggplot(music_data, aes(x = week, y = streams/1000000,group = song_id, fill = song_id, color = song_id)) +
  geom_area(position = "stack", alpha = 0.65) +
  labs(x = "Week",y = "Total streams (in million)", title = "Weekly number of streams by song") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5,color = "#666666"),legend.position = "none")

#another example plot for 9 random songs from the sample
sample_songs <- sample(music_data$song_id,9,replace = F)
ggplot(music_data %>% dplyr::filter(song_id %in% sample_songs), aes(x = week, y = streams/1000000)) +
  geom_area(fill = "steelblue", color = "steelblue",alpha = 0.5) + facet_wrap(~song_id, scales = "free_y") +
  labs(x = "Week", y = "Total streams (in million)", title = "Weekly number of streams by country") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5,color = "#666666"))

#another example
sample_song <- sample(music_data$song_id,1,replace = F)
plot_data <- music_data %>% dplyr::filter(song_id %in% sample_song) %>% as.data.frame()
plot_data_long <- gather(plot_data %>% dplyr::select(-release_date,-weeks_since_release), variable, value, streams:adspend, factor_key=TRUE)
plot_data_long
ggplot(plot_data_long, aes(x = week, y = value)) +
  geom_area(fill = "steelblue", color = "steelblue",alpha = 0.5) + facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(x = "Week", y = "Value", title = "Development of key variables over time") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5,color = "#666666"))

#estimate the baseline model
fe_m0 <- lm(log(streams) ~ log(radio+1) + log(adspend+1) ,
         data = music_data)
summary(fe_m0)

#... + control for song age
fe_m1 <- lm(log(streams) ~ log(radio+1) + log(adspend+1) + log(weeks_since_release+1),
         data = music_data)
summary(fe_m1)

#... + playlist follower variable
fe_m2 <- lm(log(streams) ~ log(radio+1) + log(adspend+1) + log(weeks_since_release+1) + log(playlist_follower),
         data = music_data)
summary(fe_m2)

#... + song fixed effects
fe_m3 <- lm(log(streams) ~ log(radio+1) + log(adspend+1) + log(playlist_follower) + log(weeks_since_release+1) +
           as.factor(song_id),
         data = music_data)
summary(fe_m3)
library(stargazer)
stargazer(fe_m0,fe_m1,fe_m2,fe_m3,type="text")

#... same as m3 using the fixest package
library(fixest) #https://lrberge.github.io/fixest/
fe_m4 <- feols(log(streams) ~ log(radio+1) + log(adspend+1) + log(playlist_follower) + log(weeks_since_release+1)
            | song_id,
            data = music_data)
etable(fe_m4,se = "cluster")

#... + week fixed effects
fe_m5 <- feols(log(streams) ~ log(radio+1) + log(adspend+1) + log(playlist_follower) + log(weeks_since_release+1)
            | song_id + week,
            data = music_data)
etable(fe_m4,fe_m5,se = "cluster")
#extract fixed effects coefficients
fixed_effects <- fixef(fe_m5)
summary(fixed_effects)

#mixed effects model
library(lme4) #https://github.com/lme4/lme4
music_data$log_playlist_follower <- log(music_data$playlist_follower)
music_data$log_streams <- log(music_data$streams)
re_m1 <- lmer(log_streams ~ log(radio+1) + log(adspend+1) + log_playlist_follower + log(weeks_since_release+1) + (1 + log_playlist_follower | song_id), data=music_data)
summary(re_m1)
library(sjPlot)
plot_model(re_m1, show.values = TRUE, value.offset = .3)
# plot random-slope-intercept
plot_model(re_m1, type="pred", terms=c("log_playlist_follower","song_id"),
                   pred.type="re", ci.lvl=NA) +
  scale_colour_manual(values=hcl(0,100,seq(40,100,length=97))) +
  theme(legend.position = "bottom", legend.key.size=unit(0.3,'cm')) + guides(colour = guide_legend(nrow = 5))

#-------------------------------------------------------------------#
#----------------Difference-in-Differences estimator----------------#
#-------------------------------------------------------------------#

#load data
did_data <- fread("https://raw.github.com/WU-RDS/RMA2022/main/data/did_data_exp.csv")
#pre-processing
did_data$song_id <- as.character(did_data$song_id)
did_data$treated_fct <- factor(did_data$treated,levels = c(0,1),labels = c("non-treated","treated"))
did_data$post_fct <- factor(did_data$post,levels = c(0,1),labels = c("pre","post"))
did_data$week <- as.Date(did_data$week)
did_data <- did_data %>% dplyr::filter(!song_id %in% c("101","143","154","63","161","274")) %>% as.data.frame()

#inspect data
head(did_data)
did_data %>% dplyr::group_by(treated) %>%
  dplyr::summarise(unique_songs = n_distinct(song_id))
library(panelView) #https://yiqingxu.org/packages/panelview/
panelview(streams ~ treated_post, data = did_data,  index = c("song_id","week"), pre.post = TRUE, by.timing = TRUE)
panelview(streams ~ treated_post, data = did_data ,  index = c("song_id","week"), type = "outcome")
did_data <- did_data %>% group_by(song_id) %>% dplyr::mutate(mean_streams = mean(streams)) %>% as.data.frame()
panelview(streams ~ treated_post, data = did_data %>% dplyr::filter(mean_streams<70000),  index = c("song_id","week"), type = "outcome")

#alternatively, split plot by group
#compute the mean streams per group and week
did_data <- did_data %>%
  dplyr::group_by(treated_fct,week) %>%
  dplyr::mutate(mean_streams_grp=mean(log(streams))) %>% as.data.frame()
#set color scheme for songs
cols <- c(rep("gray",length(unique(did_data$song_id))))
#set labels for axis
abbrev_x <- c("-10", "", "-8", "",
              "-6", "", "-4", "",
              "-2", "", "0",
              "", "+2", "", "+4",
              "", "+6", "", "+8",
              "", "+10")
#axis titles and names
title_size = 26
font_size = 24
line_size =1/2
#create plot
ggplot(did_data) +
  geom_step(aes(x=week,y=log(streams), color = song_id), alpha = 0.75) +
  geom_step(aes(x =week, y = mean_streams_grp),color = "black", size = 2, alpha=0.5) +
  #geom_vline(xintercept = as.Date("2018-03-19"),color="black",linetype="dashed") +
  labs(x="week before/after playlist listing",y="ln(streams)",
       title="Number of weekly streams per song") +
  scale_color_manual(values = cols) + theme_bw() +
  scale_x_continuous(breaks = unique(did_data$week), labels = abbrev_x) +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text.x = element_text(size = font_size),
        panel.grid.major.y = element_line(color = "gray75",
                                          size = 0.25,
                                          linetype = 1),
        panel.grid.minor.y =  element_line(color = "gray75",
                                           size = 0.25,
                                           linetype = 1),
        plot.title = element_text(color = "#666666",size=title_size),
        axis.title = element_text(size = font_size),
        axis.text  = element_text(size=font_size),
        plot.subtitle = element_text(color = "#666666",size=font_size),
        axis.text.x=element_text(size=font_size)
  ) + facet_wrap(~treated_fct)

#run baseline did model
did_m1 <- lm(log(streams+1) ~ treated * post,
                         data = did_data %>% dplyr::filter(week!=as.Date("2018-03-19")))
summary(did_m1)

#same model using fixed effects specification
did_m2 <- feols(log(streams+1) ~ treated * post |
                          song_id + week,
                          cluster = "song_id",
                          data = did_data  %>% dplyr::filter(week!=as.Date("2018-03-19")))
etable(did_m2, se = "cluster")

#did PACKAGE
library(did)
#treatment happens between period 10 & 11
#G is indicator of when a unit is treated
did_data$period <- as.numeric(factor(as.character(did_data$week),
                  levels = as.character(sort(unique(did_data$week)))))
did_data$log_streams <- log(did_data$streams + 1)
did_data$G <- 11 * did_data$treated
did_data$id <- as.numeric(did_data$song_id)

simple_gt_att <- att_gt(yname = "log_streams",
       tname = "period",
       idname = "id",
       gname = "G",
       data = did_data)
#"simple" gives a weighted average of group-time effects weighted by group (treatment time) size
# since we only have one group this is fine here
aggte(simple_gt_att, type = "simple")
#this is the same (for a single group)
aggte(simple_gt_att, type = "group")


#parallel pre-treatment trend assessment
#1. inspect period-specific effects
did_data <- did_data %>% dplyr::group_by(song_id) %>%
  dplyr::mutate(period = seq(n())) %>%
  as.data.frame()
did_m3 <- fixest::feols(log(streams) ~ i(period, treated, ref = 10) | song_id + period,
                                        cluster = "song_id",
                                        data = did_data)
etable(did_m3, se="cluster")
fixest::iplot(did_m3,
              xlab = 'Time to treatment (treatment = week 11)',
              main = 'TWFE DiD')

#2. placebo-test for pre-treatment parallel trend test
did_data_placebo <- did_data %>% dplyr::filter(period<11) %>% as.data.frame()
did_data_placebo$post <- ifelse(did_data_placebo$period>=5,1,0)
placebo_model <- feols(log(streams+1) ~ treated * post |
                                      song_id + week,
                                      cluster = "song_id",
                                      data = did_data_placebo)
etable(placebo_model, se = "cluster")

#"honest" pre-treatment trend assessment; https://github.com/asheshrambachan/HonestDiD
library(Rglpk)
library(HonestDiD)
iplot(did_m3)
betahat <- summary(did_m3)$coefficients #save the coefficients
sigma <- summary(did_m3)$cov.scaled #save the covariance matrix
#significant result is robust to allowing for violations of parallel trends
#up to >twice as big as the max violation in the pre-treatment period.
HonestDiD::createSensitivityResults_relativeMagnitudes(
  betahat = betahat, #coefficients
  sigma = sigma, #covariance matrix
  numPrePeriods = 10, #num. of pre-treatment coefs
  numPostPeriods = 10, #num. of post-treatment coefs
  Mbarvec = seq(0.5,2.5,by=0.5) #values of Mbar
)

#did package
#for time < group these are pseudo ATTs -> pre-test for parallel trends
summary(simple_gt_att)

#this is the same (for a single group) as aggregation by exposure time
aggte(simple_gt_att, type = "dynamic")

ggdid(simple_gt_att)

#conditional pre-test
set.seed(123)
did_data$genre <- as.factor(did_data$genre)
conditional_gt_att <- att_gt(yname = "log_streams",
                        tname = "period",
                        idname = "id",
                        gname = "G",
                        xformla = ~genre,
                        data = did_data)
ggdid(conditional_gt_att)
summary(conditional_gt_att)
aggte(conditional_gt_att, type = "simple")

#-------------------------------------------------------------------#
#------------------Heterogeneous treatment effects------------------#
#-------------------------------------------------------------------#

#heterogeneity across treated units
#example genre
did_m4 <- feols(log(streams+1) ~ treated_post * as.factor(genre) |
                  song_id + week,
                cluster = "song_id",
                data = did_data  %>% dplyr::filter(week!=as.Date("2018-03-19")))
etable(did_m4, se = "cluster")

#heterogeneity across time
did_data$treated_post_1 <- ifelse(did_data$week > as.Date("2018-03-19") & did_data$week <= (as.Date("2018-03-19")+21) & did_data$treated==1,1,0)
did_data$treated_post_2 <- ifelse(did_data$week > as.Date("2018-04-09") & did_data$week <= (as.Date("2018-04-09")+21) & did_data$treated==1,1,0)
did_data$treated_post_3 <- ifelse(did_data$week > as.Date("2018-04-30") & did_data$treated==1,1,0)
did_m5 <- feols(log(streams+1) ~ treated_post_1 + treated_post_2 + treated_post_3 |
                  song_id + week,
                cluster = "song_id",
                data = did_data  %>% dplyr::filter(week!=as.Date("2018-03-19")))
etable(did_m5, se = "cluster")

t_post_1 <- aggte(simple_gt_att, "dynamic", min_e = 1, max_e = 3)
t_post_2 <- aggte(simple_gt_att, "dynamic", min_e = 4, max_e = 6)
t_post_3 <- aggte(simple_gt_att, "dynamic", min_e = 7)

data.frame( period = c("post 1", "post 2", "post 3"),
            att    = c(t_post_1$overall.att,  t_post_2$overall.att,  t_post_3$overall.att),
            se     = c(t_post_1$overall.se,  t_post_2$overall.se, t_post_3$overall.se))

t_post_1

#-------------------------------------------------------------------#
#----------------Matching treated and control units-----------------#
#-------------------------------------------------------------------#

library(WeightIt) #https://github.com/ngreifer/WeightIt
#prepare matching data
psm_matching <- did_data %>%
  group_by(song_id) %>%
  dplyr::summarise(treated = max(treated),
                   genre = dplyr::first(genre),
                   danceability = dplyr::first(danceability),
                   valence = dplyr::first(valence),
                   duration_ms = dplyr::first(duration_ms),
                   major_label = dplyr::first(major_label),
                   playlist_follower_pre = mean(log(playlist_followers[week<=as.Date("2018-03-12")]+1),na.rm = T),
                   n_playlists_pre = mean(log(n_playlists[week<=as.Date("2018-03-12")]+1),na.rm = T),
                   artist_fame_pre = mean(log(artist_fame[week<=as.Date("2018-03-12")]+1),na.rm = T),
                   ig_followers_pre = mean(log(ig_followers[week<=as.Date("2018-03-12")]+1),na.rm = T),
                   streams_pre1 = mean(log(streams[week<=(as.Date("2018-03-19")-7*1)]+1)),
                   streams_pre2 = mean(log(streams[week<=(as.Date("2018-03-19")-7*2)]+1)),
                   streams_pre3 = mean(log(streams[week<=(as.Date("2018-03-19")-7*3)]+1)),
                   streams_pre4 = mean(log(streams[week<=(as.Date("2018-03-19")-7*4)]+1)),
                   streams_pre5 = mean(log(streams[week<=(as.Date("2018-03-19")-7*5)]+1)),
                   streams_pre6 = mean(log(streams[week<=(as.Date("2018-03-19")-7*6)]+1)),
                   streams_pre7 = mean(log(streams[week<=(as.Date("2018-03-19")-7*7)]+1)),
                   streams_pre8 = mean(log(streams[week<=(as.Date("2018-03-19")-7*8)]+1)),
                   streams_pre9 = mean(log(streams[week<=(as.Date("2018-03-19")-7*9)]+1)),
                   streams_pre10 = mean(log(streams[week<=(as.Date("2018-03-19")-7*10)]+1))
  ) %>% as.data.frame()
head(psm_matching)

#estimate treatment propensity model
w_out <- weightit(treated ~
                    streams_pre1 + streams_pre5 + streams_pre10 + danceability + valence +
                    duration_ms  + playlist_follower_pre + n_playlists_pre,
                  data = psm_matching, estimand = "ATT", method = "ps", include.obj = T, link = "logit")
#results of logit model
summary(w_out$obj)
#matching summary
summary(w_out)
#distribution of weights
psych::describe(w_out$weights,quant = c(0.01,0.05,0.25,0.5,0.75,0.95,0.99))
hist(w_out$weights)

#assess covariate balance
library(cobalt) #https://ngreifer.github.io/cobalt/
bal.tab(w_out, stats = c("m", "v"), thresholds = c(m = .05))
bal.plot(w_out, var.name = "prop.score", which = "both",
                      type = "histogram", mirror = T, colors = c("grey", "white"))
love.plot(w_out,var.order = "unadjusted",
          line = TRUE,
          threshold = .1,
          colors = c("darkgrey","black"))
#extract weights
weight_df <- data.frame(song_id = psm_matching$song_id, weight = w_out$weights)
#merge weights to data
did_data <- plyr::join(did_data,weight_df[,c("song_id","weight")], type='left', match='first')
#estimate weighted DiD model
did_m6 <- feols(log(streams+1) ~ treated * post |
                  song_id + week,
                cluster = "song_id",
                weights = did_data[did_data$week!=as.Date("2018-03-19"), "weight"],
                data = did_data  %>% dplyr::filter(week!=as.Date("2018-03-19")))
#compare results with non-matched sample
etable(did_m2,did_m6, se = "cluster")


#-------------------------------------------------------------------#
#------------------------Synthetic control--------------------------#
#-------------------------------------------------------------------#

library(gsynth) #https://yiqingxu.org/packages/gsynth/
#inspect data
panelview(streams ~ treated_post, data = did_data,  index = c("song_id","week"), pre.post = TRUE, by.timing = TRUE)
did_data$log_streams <- log(did_data$streams)
did_data$log_ig_followers <- log(did_data$ig_followers + 1)
did_data$log_playlist_followers <- log(did_data$playlist_followers + 1)
did_data$log_n_postings <- log(did_data$n_postings + 1)
did_data$log_n_playlists <- log(did_data$n_playlists + 1)

#estimation
sc_m1 <- gsynth(log_streams ~ treated_post, #+
                #log_ig_followers + log_playlist_followers +
                #log_n_postings + log_n_playlists,
                data = did_data,
                #r = 0, CV = FALSE,
                r = c(0, 5), CV = TRUE,
                index = c("song_id","week"), force = "two-way", se = TRUE,
                inference = "parametric", nboots = 1000,
                parallel = TRUE, cores = 4)
#ATT
cumu_eff <- cumuEff(sc_m1, cumu = TRUE, id = NULL, period = c(0,10)) #cumulative  ATT
plot(sc_m1) #effects plot
plot(sc_m1, type = "raw") #incl raw data
plot(sc_m1, type = "gap", id = 3, main = "Example song")
sc_m1$est.att #ATT and CI per period
sc_m1$est.avg #ATT
sc_m1$est.beta #beta coefficients form factor model

#counterfactual prediction
plot(sc_m1, type = "counterfactual", raw = "none", xlab = "Time")
plot(sc_m1, type = "counterfactual", raw = "all") #incl raw data
plot(sc_m1, type = "counterfactual", id = 3) #for individual units
#factor model
plot(sc_m1, type = "factors", xlab = "Time")
plot(sc_m1, type = "loadings")

#-------------------------------------------------------------------#
#----------------Synthetic Difference-in-Differences----------------#
#-------------------------------------------------------------------#

library(synthdid) #https://synth-inference.github.io/synthdid/articles/synthdid.html
#data pre-processing
did_data_synthdid <- did_data %>%
  dplyr::select(song_id,week,streams,treated_post) %>%
  dplyr::mutate(streams = log(streams)) %>%
  as.data.frame()
#data setup
setup = panel.matrices(did_data_synthdid)
#estimation
synthdid_m1 = synthdid_estimate(setup$Y, setup$N0, setup$T0)
#treatment effect
plot(synthdid_m1)
se = sqrt(vcov(synthdid_m1, method='placebo'))
sprintf('point estimate: %1.2f', synthdid_m1)
sprintf('95%% CI (%1.2f, %1.2f)', synthdid_m1 - 1.96 * se, synthdid_m1 + 1.96 * se)
#control unit contribution
synthdid_units_plot(synthdid_m1, se.method='placebo')
#assessing parallel pre-treatment trends
plot(synthdid_m1, overlay=1,  se.method='placebo')
plot(synthdid_m1, overlay=.8,  se.method='placebo')

#compare to standard DiD estimator and synthetic control
synthdid_m2  = did_estimate(setup$Y, setup$N0, setup$T0)
synthdid_m3  = sc_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(synthdid_m2, synthdid_m1, synthdid_m3)
names(estimates) = c('Diff-in-Diff','Synthetic Diff-in-Diff','Synthetic Control')
print(unlist(estimates))

#-------------------------------------------------------------------#
#-------------------Time-varying treatment effects------------------#
#-------------------------------------------------------------------#

#staggered adoption
#https://yiqingxu.org/packages/gsynth/articles/tutorial.html
# upcoming package: https://github.com/zachporreca/staggered_adoption_synthdid
#load data
did_data_staggered <- fread("https://raw.githubusercontent.com/WU-RDS/RMA2022/main/data/did_data_staggered.csv")
did_data_staggered$song_id <- as.character(did_data_staggered$song_id )
did_data_staggered$week <- as.Date(did_data_staggered$week)
#data preparation
did_data_staggered$log_streams <- log(did_data_staggered$streams+1)
did_data_staggered$log_song_age <- log(did_data_staggered$song_age+1)
did_data_staggered$log_n_playlists <- log(did_data_staggered$n_playlists+1)
did_data_staggered$log_playlist_followers <- log(did_data_staggered$playlist_followers+1)
did_data_staggered$log_n_postings <- log(did_data_staggered$n_postings+1)

#inspect data
panelview(log_streams ~ treated_post, data = did_data_staggered,  index = c("song_id","week"), pre.post = TRUE, by.timing = TRUE)
panelview(log_streams ~ treated_post, data = did_data_staggered,  index = c("song_id","week"), type = "outcome")
panelview(log_streams ~ treated_post, data = did_data_staggered,  index = c("song_id","week"), type = "outcome", main = "Number of weekly streams", by.group = TRUE)

#run model
stag_m1 <- gsynth(log_streams ~ treated_post + log_song_age + log_n_playlists + log_playlist_followers + log_n_postings,
              data = did_data_staggered,  index = c("song_id","week"),
              se = TRUE, inference = "parametric",
              r = c(0, 5), CV = TRUE, force = "two-way",
              nboots = 1000, seed = 02139)
#ATT
cumu_eff <- cumuEff(stag_m1, cumu = TRUE, id = NULL, period = c(0,10)) #cumulative  ATT
cumu_eff
plot(stag_m1) #effects plot
plot(stag_m1, type = "gap")
plot(stag_m1, type = "gap", id = 79, main = "Example song")
stag_m1$est.att #ATT and CI per period
stag_m1$est.avg #ATT
stag_m1$est.beta #beta coefficients form factor model

#counterfactual prediction
plot(stag_m1, type = "raw")
plot(stag_m1, type = "counterfactual", id = 79) #for individual units
#factor model
plot(stag_m1, type = "factors", xlab = "Time")
plot(stag_m1, type = "loadings")

# staggered did
did_data_staggered$period <- as.numeric(factor(as.character(did_data_staggered$week),
                                               levels = as.character(sort(unique(did_data_staggered$week)))))
# add first period treated
did_data_staggered_G <- did_data_staggered %>%
  filter(treated == 1, week == treat_week) %>%
  select(song_id, G = period)
did_data_staggered <- left_join(did_data_staggered,
                                did_data_staggered_G,
                                by = "song_id")
did_data_staggered$G <- coalesce(did_data_staggered$G, 0)
did_data_staggered$id <- as.numeric(did_data_staggered$song_id)

set.seed(123)
#increase bootstrap for reliability!
did_stag <- att_gt(yname = "log_streams",
       tname = "period",
       idname = "id",
       gname = "G",
       biters = 2000,
       data = did_data_staggered
)

summary(did_stag)
## Full aggregation
aggte(did_stag, type = "simple", biters=20000)
## Aggregate time relative to treatment
aggte(did_stag, type = "dynamic")
ggdid(aggte(did_stag, type = "dynamic", biters = 20000))
## Aggregate total effect by treatment time
aggte(did_stag, type = "group", biters = 20000)
## Aggregate total effect by calendar time
aggte(did_stag, type = "calendar", biters = 20000)

#compare with standard DiD
stag_m2 <- feols(log(streams+1) ~ treated_post |
                  song_id + week,
                cluster = "song_id",
                data = did_data_staggered)
etable(stag_m2, se = "cluster")


#----------------------------------------------------------------#
#-------------Difference-in-Difference-in-Difference-------------#
#----------------------------------------------------------------#
set.seed(123)
is_unsw <- c(0, 1)
is_dash <- c(0, 1)
groups <- expand.grid(is_unsw = is_unsw, is_dash = is_dash)
for (i in 1:15) {
  groups <- rbind(groups, groups)
}
groups_pre <- groups |> mutate(id = seq_len(n()), is_post = 0)
groups_post <- groups_pre |> mutate(is_post = 1)

groups_df <- rbind(groups_pre, groups_post)
nrow(groups_df)

sample_y <- function(df) {
  is_unsw <- df$is_unsw
  is_dash <- df$is_dash
  is_post <- df$is_post
  df$y <- 10 +
    2 * is_unsw +
    3 * is_dash +
    4 * is_unsw * is_dash +
    -1.5 * is_post +
    2 * is_unsw * is_post +
    -3 * is_dash * is_post +
    4 * is_unsw * is_dash * is_post + # <- treated with delta = 4
    rnorm(nrow(df), 0, 1)
  return(df)
}
panel <- sample_y(groups_df)

head(panel)

model_ols <- feols(y ~
                     is_unsw +
                     is_dash +
                     is_unsw * is_dash +
                     is_post +
                     is_unsw * is_post +
                     is_dash * is_post +
                     is_unsw * is_dash * is_post,
                   panel, cluster = "id")
model_fe <- feols(y ~
                    is_unsw:is_dash:is_post |
                    id +
                    is_post +
                    is_unsw * is_post +
                    is_dash * is_post, panel)
etable(model_ols, model_fe)


means <- panel %>%
  group_by(is_unsw, is_dash, is_post) %>%
  summarise(y = mean(y)) %>%
  arrange(is_post)
diffs <- means %>%
  group_by(is_unsw, is_dash) %>%
  summarize(y = diff(y))
with(diffs,
     ## Diff-in-Diff for UNSW
     y[is_unsw == 1 & is_dash == 1] -
       y[is_unsw == 1 & is_dash == 0] -
       ## Counterfactual Diff-in-Diff for WU
       (y[is_unsw == 0 & is_dash == 1] -
          y[is_unsw == 0 & is_dash == 0]))

#-------------------------------------------------------------------#
#-------------Bayesian structural time-series approach--------------#
#-------------------------------------------------------------------#

library(CausalImpact) #https://google.github.io/CausalImpact/CausalImpact.html

#load data
ci_data <- fread("https://raw.githubusercontent.com/WU-RDS/RMA2022/main/data/ci_data.csv")

#data preparation
#control songs
ctrl_data <- ci_data %>% dplyr::filter(treated==0) %>% as.data.frame()
ctrl_data_wide <- spread(ctrl_data[,-4], song_id, streams)
head(ctrl_data_wide)
x <- ctrl_data_wide[,-1]
#treated song
treated_data <- ci_data %>% dplyr::filter(treated==1) %>% as.data.frame()
y <- treated_data[,3]

#run CI model
#create dataframe
data_ci <- cbind(y, x)
#visualize time series
matplot(data_ci, type = "l")
#specify number of pre and post periods
pre_period <- c(1,38)
post_period <- c(38+1, 77)
#run model
ci_m1 <- CausalImpact(data_ci, pre_period, post_period)
#result summary
plot(ci_m1)
summary(ci_m1)
summary(ci_m1, "report")
plot(ci_m1$model$bsts.model, "coefficients")


