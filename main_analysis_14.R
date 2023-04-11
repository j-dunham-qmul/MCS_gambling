library(foreign)
library(dplyr)
library(default)
library(MASS)
library(glue)
library(poLCA)
library(estimatr) #for  robust SEs
library(modelsummary)

#options----
options(max.print=50)

#data_sets----

df1 <- read.dta(file = r"(C:\Users\James\Desktop\Gambling\Data\MCS_14\UKDA-8156-stata\stata\stata13\mcs6_cm_derived.dta)", convert.factors = F)
df2 <- read.dta(file = r"(C:\Users\James\Desktop\Gambling\Data\MCS_14\UKDA-8156-stata\stata\stata13\mcs6_cm_interview.dta)", convert.factors = F)
weights <- read.dta(file = r"(C:\Users\James\Desktop\Gambling\Data\longitudinal_family_file\stata\stata13\mcs_longitudinal_family_file.dta)",
                    convert.factors = F)



df <- df2 %>%
  inner_join(df1, by=c("MCSID", "FCNUM00")) %>% 
    inner_join(weights, by=c("MCSID"))


df[df<0] <- NA #non response values coded as negative in this sweep. 
  
#feature engineering----

df$gender <- df$FCCSEX00.x

SDQ_elements <- list(emotional_problems="FEMOTION", conduct_problems="FCONDUCT", hyperactivity="FHYPER", peer_problems="FPEER", 
                     prosociality="FPROSOC", total_difficulties="FEBDTOT")

cognitive_metrics <- list(word_score = "FCWRDSC")

df$FCGANG00[df$FCGANG00==3] <- 1 #option 3 is previously but no longer belong to gang, collapse for convenience

anti_social_behaviour <- list(stolen_from_shop = "FCSTOL00", damaged = "FCDAMG00", knife_or_other_weapon = "FCKNIF00", 
                              physical_with_someone = "FCHITT00", stolen_from_person = "FCSTLN00", warned_by_police = "FCCAUT00", 
                              gang_member = "FCGANG00", used_weapon = "FCWEPN00")

asb_df <- df[, unlist(anti_social_behaviour)]
asb_df[asb_df == 2] <- 0
df$n_asb <- rowSums(asb_df, na.rm=TRUE) *NA^!rowSums(!is.na(asb_df))
df$asb <- case_when(df$n_asb>=1~1,
                    df$n_asb==0~2,
                    NA~NA, 
                    T~NA)


gambled <- list(fruities="FCGAMA00", private_bets = "FCGMBL00", betting_shop = "FCGAEM00", any_other_gambling = "FCGAMJ00")
gamble_df <- df[, unlist(gambled)]
gamble_df[gamble_df == 2] <- 0   #"no" to 0
df$n_gamble <- rowSums(gamble_df, na.rm=TRUE) *NA^!rowSums(!is.na(gamble_df))  #number of gambling activities, 2nd part to remove rows where all 0. Hacky but works
#alternatively could do dplyr::anti_join() shenanigans to get NA less row df
df$gambled <- case_when(df$n_gamble>=1~1, 
                        df$n_gamble==0~2, 
                        NA~NA, 
                        T~NA)

df$drank <- case_when(df$FCALCD00==1~1, 
                      df$FCALCD00==2~2,
                      df$FCALCD00<0~NA,
                      T~NA)

df$smoked <- case_when(df$FCSMOK00 %in% c(2, 4, 5, 6)~1,            #initially made a mistake using e cigs instead of smoking var, now changed
                       df$FCSMOK00 %in% 1 ~ 2, 
                       df$FCSMOK00 < 0 ~ NA, 
                       TRUE~NA) 

drink_smoke <- list(smoked="smoked", drank="FCALCD00")

drugs = list(weed="FCCANB00", any_other_drugs="FCOTDR00")
df$tried_drugs <- (case_when(df$FCCANB00 == 1 | df$FCOTDR00 == 1 ~ 1, 
                             df$FCCANB00 == 2 & df$FCOTDR00 == 2 ~ 2,
                             TRUE~NA))

sex = list(had_sex="FCSEXX00", used_protection="FCCONP0A") #only whether the "last time" they had sex involved contraceptives. Also,
#the "had sex" variable is weird, mostly "Not applicable answers given, no idea why this would be the case...

risky_behaviours <- list(smoked="smoked", gambled="gambled", drank="drank", tried_drugs="tried_drugs", been_anti_social="asb")

for (row in 1:nrow(df)){
  df$n_risky[row] <- sum(df[row, unlist(risky_behaviours)] == 1, na.rm = T)*NA^!sum(!is.na(df[row, unlist(risky_behaviours)]))
}

n_behaviour <- list()
for (behaviour in risky_behaviours){
  n_behaviour[behaviour] <- assign(x=paste("n", behaviour, sep="_"), value=sum(df[behaviour]==1, na.rm=T))
}

not_NA <- list()
for (behaviour in risky_behaviours){
  not_NA[behaviour] <- assign(x=paste("not_NA", behaviour, sep="_"), value=sum(!is.na(df[behaviour]), na.rm=T))
}

proportions = list()
for (behaviour in risky_behaviours){
  proportions[behaviour] <- assign(x=paste("prop", behaviour, sep="_"), value=(sum(df[behaviour]==1, na.rm=T))/sum(!is.na(df[behaviour]), na.rm=T))
}



#regressions etc----

models <- list(mod1 = lm(n_risky ~ FHYPER + FCGTRISKT, 
                        data = df, 
                        weights = FOVWT2*WEIGHT2) , 
               mod2 = lm(n_risky ~ FHYPER + FCGTRISKT + FCCSEX00.x + FCWRDSC, 
                         data = df, 
                         weights = FOVWT2*WEIGHT2))

modelsummary(models, vcov = c("iid", "robust"), stars = T)

#ordered logit. Using weights generates a warning that can be ignored
summary(MASS::polr(formula = factor(n_risky, ordered = T) ~ FCGTRISKT + FHYPER + FCCSEX00.x + FCWRDSC,
                   data = df, Hess = T, weights = FOVWT2*WEIGHT2))
