library(TeachingDemos)

# txtStart("exp2-analysis-output.txt")
# Your code
#txtStop()

library(aod)
library(Rcpp)
library(rms) 
library(lme4) #for model
library(dplyr)
library(plyr)
library(reshape) #for melt
library(ggplot2) #for plots
library(ggrepel)
library(effects) #for getting predicted means and associated CIs from the model
library(lsmeans) #for getting contrasts and associated CIs from the model 
library(sciplot)
library(gridExtra) #grid.table applied to df2 yields a table
library(xtable) #for LaTeX code
library(binom) #for calculating binomial confidence intervals
#(which can however be obtained more easily from a model with the effects package)

#Define 2 colors for ModType plots, 4 colors for Mod plots:
colors.2 <- c("#B3CDE3", "#FBB4AE")
colors.4 <- c("#B3CDE3", "#4f8bbe", "#FBB4AE", "#f42a18")

#Set working directory:
setwd("/home/dora/Dropbox/Academic/Research/2020-09-16-18 ELM 1 online (a) Superlative-modified numerals and negation: A negotiable cost/paper/")

#Load data in wide format. Melt into long format:

df2.wide <- read.csv('exp2-results-wide.csv', header = T)
head(df2.wide, n = 10)
df2 <- melt(df2.wide)
df2 <- tbl_df2(df2)
df2

names(df2)[names(df2) == "variable"] <- "Participant"
names(df2)[names(df2) == "value"] <- "Response"
df2$Response <- factor(df2$Response)

df2

str(df2)

df2 <- select(df2, Participant, ModMon, ModType, Mod, Pol1, Pol2, Env, Response)

df2$Env<- factor(df2$Env, levels=c('AntCond','RestUniv'))
df2$Pol1<- factor(df2$Pol1, levels=c('Pos','Neg'))
df2$Pol2 <- factor(df2$Pol2, levels=c('Pos','Neg'))
df2$ModType <- factor(df2$ModType, levels=c('Comp','Sup'))
df2$ModMon<-factor(df2$ModMon, levels=c('UE','DE'))
df2$Mod<-factor(df2$Mod, levels=c('MoreThan','LessThan', 'AtLeast','AtMost'))

levels(df2$Env)
levels(df2$Pol1)
levels(df2$Pol2)
levels(df2$ModType)
levels(df2$ModMon)
levels(df2$Mod)

str(df2)

#Calculate binomial confidence intervals directly from the raw means, using the binom package:

#- Select factors of interest .
#- Get Mean, Successes (counts of 1), Count (total number of responses).
#- These will be summarized over Participant and Response for combinations of the factors of interest.

# By Mod.

df2.mod.msc <- ddply(df2, c('Env', 'Pol1', 'Pol2', 'Mod'), summarize,
                    Mean = mean(as.numeric(as.character(Response))),
                    Successes = sum(as.numeric(as.character(Response))),
                    Count = length(Response)
)

df2.mod.msc

CIs <- binom.confint(x=df2.mod.msc$Successes, n=df2.mod.msc$Count, methods="wilson")

CIs

df2.mod.mscci <- cbind(df2.mod.msc, CIs[,5:6])

df2.mod.mscci
#This can be used to generate a table with the raw means and their associated 95% binomial CIs.
#First, extract Env, Pol, Mod, Mean, lower, and upper. 
df2.mod.mscci.latex <-select(df2.mod.mscci, Env, Pol1, Pol2, Mod, Mean, lower, upper)
df2.mod.mscci.latex 
#Second, print to LaTeX table, with lower and upper rounded. 
xtable(df2.mod.mscci.latex) #Digits for lower and upper automatically rounded to two decimal places.
#Done!

df2.mod.mscci$Pol1Pol2 <- interaction(df2.mod.mscci$Pol1, df2.mod.mscci$Pol2, drop = TRUE) #drop unused levels
levels(df2.mod.mscci$Pol1Pol2)
df2.mod.mscci$Pol1Pol2 <- factor(df2.mod.mscci$Pol1Pol2, levels=c('Pos.Pos','Pos.Neg','Neg.Pos','Neg.Neg'))
levels(df2.mod.mscci$Pol1Pol2)
str(df2.mod.mscci)

#Now, let's plot the raw means and their associated CIs.
ggplot(df2.mod.mscci, aes(x=Pol1Pol2, y=Mean, fill=Mod)) +
  facet_wrap(~ Env) +
  geom_bar(position=position_dodge(width = .9), stat="identity",colour="black",size=.25) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.4,position=position_dodge(.9),size=.33) +
  # ylim(c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.2), minor_breaks = seq(0, 1, 0.02)) + #draws major (minor) y ticks at 0.2 (0.02) intervals
  xlab("") +
  ylab("Mean rating") +
  theme_bw() +
  # geom_text(aes(label = round(Mean, digits = 2)), size = 4, position = position_dodge(1), vjust=-.45) + #prints mean above each bar
  theme(text = element_text(size=17)) + #makes label size 15
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_fill_manual(values=colors.4) +
  ggsave("exp2-mod-raw.png", width=10, height=4, dpi=300)

#A plot with y in intervals of .25 and which prints the means above the bars:
ggplot(df2.mod.mscci, aes(x=Pol1Pol2, y=Mean, fill=Mod)) +
  facet_wrap(~ Env) +
  geom_bar(position=position_dodge(width = .9), stat="identity",colour="black",size=.25) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.4,position=position_dodge(.9),size=.33) +
  ylim(c(0,1)) +
  # scale_y_continuous(breaks=seq(0,1,0.2), minor_breaks = seq(0, 1, 0.02)) + #draws major (minor) y ticks at 0.2 (0.02) intervals
  xlab("") +
  ylab("Mean rating") +
  theme_bw() +
  geom_text(aes(label = round(Mean, digits = 2)), size = 4, position = position_dodge(1), vjust=-.45) + #prints mean above each bar
  theme(text = element_text(size=17)) + #makes label size 15
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_fill_manual(values=colors.4) +
  ggsave("exp2-mod-raw-withprintedmeans.png", width=10, height=4, dpi=300)
  

# Fit logistic mixed-effects models. 
# Fixed effects -- all the main factors of interest. 
# Random effects -- intercept for Participant; random slopes -- the maximal we can fit in a principled way (no interactions, all 2-way, all3-way).
# Random slope: 
# Without: converges in 1 minute. 
# With (1+ (ModMon + ModType + Pol1 + Pol2 + Env)|Participant):  Didn't use to converge but now converges in <7 mins, though with isSingular warning. Can use.
# With (1+ (ModMon + ModType + Pol1 + Pol2 + Env)^2|Participant): Was still running after 40 mins. Not sure if it will converge.

df2.model <- glmer(Response ~ ModMon * ModType * Pol1 * Pol2 * Env + (1+ (ModMon + ModType + Pol1 + Pol2 + Env)|Participant), 
                                  family = 'binomial',
                                  data=df2, 
                                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(df2.model, corr=FALSE)

# Get model results in LaTeX form.
xtable(coef(summary(df2.model)), digits=c(0,2,2,3,4))

# # If desired, get and plot predicted marginal mean probabilities for each group, with associated confidence intervals. 
# # effects package
# 
# df2.modmon.modtype.eff <- allEffects(df2.model)
# #eff
# #plot(eff) #quick and dirty plot
# df2.modmon.modtype.eff_df2 <- as.data.frame(df2.modmon.modtype.eff[["ModMon:ModType:Pol1:Pol2:Env"]])
# 
# df2.modmon.modtype.eff_df2 
# # "lower/upper" are the 95% CIs for the predicted marginal mean probabilities for each group 
# 
# #Get summary in LaTeX form, with values rounded off in all numerical columns.
# df2.modmon.modtype.eff_df2 %>% 
#   mutate_if(is.numeric, funs(round(., 2))) %>% 
#   select(., Env, Pol1, Pol2, ModMon, ModType, fit, se, lower, upper) %>%
#   xtable() %>%
#   print(., include.rownames=FALSE)
# 
# str(df2.modmon.modtype.eff_df2)
# 
# Plot as usual to get predicted means and their associated CIs.

# Otherwise, proceed to extract predicted contrasts with lsmeans.
# In contr, use adjust = "holm".
# If too many comparisons, use adjust = "none", then adjust later in contr after filtering for the contrasts of interest.


#Contrasts between ModTypes, by ModMon.
# Get contrasts between Comp and Sup by Env at specific levels of Pol1, Pol2, and ModMon.
# Warning: Results may be misleading due to involvement in interactions.

# # First method: Just replace " " in first line here:
# df2.model %>%
#   lsmeans(., specs = ~ ModType | Env, at = list(Pol1 = "Pos", Pol2 ="Pos", ModMon="UE"), type = "response") %>%
#   lsmeans::contrast(., method = "pairwise") %>%
#   rbind(., adjust="holm") %>%
#   summary(., infer=TRUE) %>%
#   select(., Env, contrast, odds.ratio, asymp.LCL, asymp.UCL, z.ratio, p.value) %>%
#   xtable(., digits=c(0,0,0,2,2,2,3,4)) %>%
#   print(., include.rownames=FALSE)
  
# Second method: Just run the lines for the relevant combo, then collate, then print to LaTeX.
df2.means.between.1 <- lsmeans(df2.model, specs = ~ ModType | Env, at = list(Pol1 = "Pos", Pol2 ="Pos", ModMon="UE"), type = "response")
df2.contr.between.1 <- summary(rbind(lsmeans::contrast(df2.means.between.1, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.between.2 <- lsmeans(df2.model, specs = ~ ModType | Env, at = list(Pol1 = "Pos", Pol2 ="Pos", ModMon="DE"), type = "response")
df2.contr.between.2 <- summary(rbind(lsmeans::contrast(df2.means.between.2, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.between.3 <- lsmeans(df2.model, specs = ~ ModType | Env, at = list(Pol1 = "Pos", Pol2 ="Neg", ModMon="UE"), type = "response")
df2.contr.between.3 <- summary(rbind(lsmeans::contrast(df2.means.between.3, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.between.4 <- lsmeans(df2.model, specs = ~ ModType | Env, at = list(Pol1 = "Pos", Pol2 ="Neg", ModMon="DE"), type = "response")
df2.contr.between.4 <- summary(rbind(lsmeans::contrast(df2.means.between.4, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.between.5 <- lsmeans(df2.model, specs = ~ ModType | Env, at = list(Pol1 = "Neg", Pol2 ="Pos", ModMon="UE"), type = "response")
df2.contr.between.5 <- summary(rbind(lsmeans::contrast(df2.means.between.5, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.between.6 <- lsmeans(df2.model, specs = ~ ModType | Env, at = list(Pol1 = "Neg", Pol2 ="Pos", ModMon="DE"), type = "response")
df2.contr.between.6 <- summary(rbind(lsmeans::contrast(df2.means.between.6, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.between.7 <- lsmeans(df2.model, specs = ~ ModType | Env, at = list(Pol1 = "Neg", Pol2 ="Neg", ModMon="UE"), type = "response")
df2.contr.between.7 <- summary(rbind(lsmeans::contrast(df2.means.between.7, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.between.8 <- lsmeans(df2.model, specs = ~ ModType | Env, at = list(Pol1 = "Neg", Pol2 ="Neg", ModMon="DE"), type = "response")
df2.contr.between.8 <- summary(rbind(lsmeans::contrast(df2.means.between.8, method = "pairwise"), adjust = "holm"), infer = TRUE)
df2.contr.between.8

df2.contr.between <-rbind(df2.contr.between.1, df2.contr.between.2, df2.contr.between.3, df2.contr.between.4, df2.contr.between.5, df2.contr.between.6, df2.contr.between.7, df2.contr.between.8) 

df2.contr.between %>% 
  select(., Env, contrast, odds.ratio, asymp.LCL, asymp.UCL, z.ratio, p.value) %>%
  xtable(., digits=c(0,0,0,2,2,2,3,4)) 
# %>%
#   print(., include.rownames=FALSE)

#Contrasts within ModTypes, by ModMon.
# Get contrasts between levels of Pol2 by Env for Pol1 = Neg for each Mod.
# Warning: Results may be misleading due to involvement in interactions.
# The model results did not reveal any significant interaction.
# Still, such interactions are likely to exist, so we'll take the results provisionally rather than definitively.

# First method: Just replace " " in first line here:
# df2.model %>%
#   lsmeans(., specs = ~ Pol2 | Env, at = list(Pol1 = "Pos", ModType="Comp",ModMon="UE"), type = "response") %>%
#   lsmeans::contrast(., method = "pairwise") %>%
#   rbind(., adjust="holm") %>%
#   summary(., infer=TRUE) %>%
#   select(., Env, contrast, odds.ratio, asymp.LCL, asymp.UCL, z.ratio, p.value) %>%
#   xtable(., digits=c(0,0,0,2,2,2,3,4)) %>%
#   print(., include.rownames=FALSE)

# Second method: Just run the lines for the relevant combo, then collate, then print to LaTeX.

df2.means.within.1 <- lsmeans(df2.model, specs = ~ Pol2 | Env, at = list(Pol1 = "Pos", ModType="Comp",ModMon="UE"), type = "response")
df2.contr.within.1 <- summary(rbind(lsmeans::contrast(df2.means.within.1, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.within.2 <- lsmeans(df2.model, specs = ~ Pol2 | Env, at = list(Pol1 = "Pos", ModType="Comp",ModMon="DE"), type = "response")
df2.contr.within.2 <- summary(rbind(lsmeans::contrast(df2.means.within.2, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.within.3 <- lsmeans(df2.model, specs = ~ Pol2 | Env, at = list(Pol1 = "Pos", ModType="Sup",ModMon="UE"), type = "response")
df2.contr.within.3 <- summary(rbind(lsmeans::contrast(df2.means.within.3, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.within.4 <- lsmeans(df2.model, specs = ~ Pol2 | Env, at = list(Pol1 = "Pos", ModType="Sup",ModMon="DE"), type = "response")
df2.contr.within.4 <- summary(rbind(lsmeans::contrast(df2.means.within.4, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.within.5 <- lsmeans(df2.model, specs = ~ Pol2 | Env, at = list(Pol1 = "Neg", ModType="Comp",ModMon="UE"), type = "response")
df2.contr.within.5 <- summary(rbind(lsmeans::contrast(df2.means.within.5, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.within.6 <- lsmeans(df2.model, specs = ~ Pol2 | Env, at = list(Pol1 = "Neg", ModType="Comp",ModMon="DE"), type = "response")
df2.contr.within.6 <- summary(rbind(lsmeans::contrast(df2.means.within.6, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.within.7 <- lsmeans(df2.model, specs = ~ Pol2 | Env, at = list(Pol1 = "Neg", ModType="Sup",ModMon="UE"), type = "response")
df2.contr.within.7 <- summary(rbind(lsmeans::contrast(df2.means.within.7, method = "pairwise"), adjust = "holm"), infer = TRUE)

df2.means.within.8 <- lsmeans(df2.model, specs = ~ Pol2 | Env, at = list(Pol1 = "Neg", ModType="Sup",ModMon="DE"), type = "response")
df2.contr.within.8 <- summary(rbind(lsmeans::contrast(df2.means.within.8, method = "pairwise"), adjust = "holm"), infer = TRUE)
df2.contr.within.8

df2.contr.within <-rbind(df2.contr.within.1, df2.contr.within.2, df2.contr.within.3, df2.contr.within.4, df2.contr.within.5, df2.contr.within.6, df2.contr.within.7, df2.contr.within.8) 
 
df2.contr.within %>% 
  select(., Env, contrast, odds.ratio, asymp.LCL, asymp.UCL, z.ratio, p.value) %>%
  xtable(., digits=c(0,0,0,2,2,2,3,4)) 
# %>%
#   print(., include.rownames=FALSE)
