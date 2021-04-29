library(TeachingDemos)

# txtStart("exp3-analysis-output.txt")
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
library(gridExtra) #grid.table applied to df3 yields a table
library(xtable) #for LaTeX code
library(binom) #for calculating binomial confidence intervals
#(which can however be obtained more easily from a model with the effects package)

#Define 2 colors for ModType plots, 4 colors for Mod plots:
colors.2 <- c("#B3CDE3", "#FBB4AE")
colors.4 <- c("#B3CDE3", "#4f8bbe", "#FBB4AE", "#f42a18")

#Set working directory:
setwd("/home/dora/Dropbox/Academic/Research/2020-09-16-18 ELM 1 online (a) Superlative-modified numerals and negation: A negotiable cost/paper/")

#Load data in wide format. Melt into long format:

df3.wide <- read.csv('exp3-results-wide.csv', header = T)
head(df3.wide, n = 10)
df3 <- melt(df3.wide)
df3 <- tbl_df3(df3)
df3

names(df3)[names(df3) == "variable"] <- "Participant"
names(df3)[names(df3) == "value"] <- "Response"
df3$Response <- factor(df3$Response)

df3

str(df3)

#Set level order (first becomes reference level):
df3$Env <- factor(df3$Env, levels=c('AntCond', 'MatrixNeg'))
df3$Pol <- factor(df3$Pol, levels=c('Pos','Neg'))
df3$ModType <- factor(df3$ModType, levels=c('Comp','Sup'))
df3$ModMon<-factor(df3$ModMon, levels=c('UE','DE'))
df3$Mod <- factor(df3$Mod, levels=c('MoreThan', 'LessThan', 'AtLeast', 'AtMost'))
levels(df3$Env)
levels(df3$Pol)
levels(df3$ModType)
levels(df3$ModMon)
levels(df3$Mod)
df3

str(df3)

#Calculate binomial confidence intervals directly from the raw means, using the binom package:

#- Select factors of interest .
#- Get Mean, Successes (counts of 1), Count (total number of responses).
#- These will be summarized over Participant and Response for combinations of the factors of interest.

# By Mod.

df3.mod.msc <- ddply(df3, c('Env', 'Pol', 'Mod'), summarize,
                    Mean = mean(as.numeric(as.character(Response))),
                    Successes = sum(as.numeric(as.character(Response))),
                    Count = length(Response)
)

df3.mod.msc

CIs <- binom.confint(x=df3.mod.msc$Successes, n=df3.mod.msc$Count, methods="wilson")

CIs

df3.mod.mscci <- cbind(df3.mod.msc, CIs[,5:6])

df3.mod.mscci
#This can be used to generate a table with the raw means and their associated 95% binomial CIs.
#First, extract Env, Pol, Mod, Mean, lower, and upper. 
df3.mod.mscci.latex <-select(df3.mod.mscci, Env, Pol, Mod, Mean, lower, upper)
df3.mod.mscci.latex 
#Second, print to LaTeX table, with lower and upper rounded. 
xtable(df3.mod.mscci.latex) #Digits for lower and upper automatically rounded to two decimal places.
#Done!


#Now, let's plot the raw means and their associated CIs.
ggplot(df3.mod.mscci, aes(x=Pol, y=Mean, fill=Mod)) +
  facet_wrap(~ Env) +
  geom_bar(position=position_dodge(width = .9), stat="identity",colour="black",size=.25) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.4,position=position_dodge(.9),size=.33) +
  # ylim(c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.2), minor_breaks = seq(0, 1, 0.02)) + #draws major (minor) y ticks at 0.2 (0.02) intervals
  xlab("") +
  ylab("Mean rating") +
  theme_bw() +
  # geom_text(aes(label = round(Mean, digits = 2)), size = 5, position = position_dodge(0.9), vjust=-.45) + #prints mean above each bar
  theme(text = element_text(size=17)) + #makes label size 15
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_fill_manual(values=colors.4) +
  ggsave("exp3-mod-raw.png", width=10, height=4, dpi=300)

# Fit logistic mixed-effects models. 
# Fixed effects -- all the main factors of interest. 
# Random effects -- intercept for Participant; random slopes -- the maximal we can fit in a principled way (no interactions, all 2-way, all3-way).
# Random slope:
# No slopes: converges fast.
# (1 + (ModMon + ModType + Pol + Env)|Participant): Converges <1min. isSingular warning. OK to use.
# (1 + (ModMon + ModType + Pol + Env)^2|Participant): 7 mins and didn't converge.

df3.model <- glmer(Response ~ ModMon * ModType * Pol * Env + (1 +(ModMon + ModType + Pol + Env)|Participant), family = 'binomial',
                                  data=df3, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(df3.model,corr=F)

# Get model results in LaTeX form.
xtable(coef(summary(df3.model)), digits=c(0,2,2,3,4))

# # If desired, get and plot predicted marginal mean probabilities for each group, with associated confidence intervals. 
# # effects package
# 
# df3.eff <- allEffects(df3.model)
# #eff
# #plot(eff) #quick and dirty plot
# df3.eff_df3 <- as.data.frame(df3.eff[["ModMon:ModType:Pol:Env"]])
# 
# df3.eff_df3 
# # "lower/upper" are the 95% CIs for the predicted marginal mean probabilities for each group 
# 
# #Get summary in LaTeX form, with values rounded off in all numerical columns.
# df3.eff_df3 %>% 
#   mutate_if(is.numeric, funs(round(., 2))) %>% 
#   select(., Env, Pol, ModMon, ModType, fit, se, lower, upper) %>%
#   xtable() %>%
#   print(., include.rownames=FALSE)
# 
# str(df3.eff_df3)
# 
# # Plot as usual to get predicted means and their associated CIs.

# Otherwise, proceed to extract predicted contrasts with lsmeans.
# In contr, use adjust = "holm".
# If too many comparisons, use adjust = "none", then adjust later in contr after filtering for the contrasts of interest.

#Contrasts between ModTypes, by ModMon.

df3.means.between.1 <- lsmeans(df3.model, specs = ~ ModType | Env, at = list(Pol = "Pos",  ModMon="UE"), type = "response")
df3.contr.between.1 <- summary(rbind(lsmeans::contrast(df3.means.between.1, method = "pairwise"), adjust = "holm"), infer = TRUE)

df3.means.between.2 <- lsmeans(df3.model, specs = ~ ModType | Env, at = list(Pol = "Pos",  ModMon="DE"), type = "response")
df3.contr.between.2 <- summary(rbind(lsmeans::contrast(df3.means.between.2, method = "pairwise"), adjust = "holm"), infer = TRUE)

df3.means.between.3 <- lsmeans(df3.model, specs = ~ ModType | Env, at = list(Pol = "Neg",  ModMon="UE"), type = "response")
df3.contr.between.3 <- summary(rbind(lsmeans::contrast(df3.means.between.3, method = "pairwise"), adjust = "holm"), infer = TRUE)

df3.means.between.4 <- lsmeans(df3.model, specs = ~ ModType | Env, at = list(Pol = "Neg",  ModMon="DE"), type = "response")
df3.contr.between.4 <- summary(rbind(lsmeans::contrast(df3.means.between.4, method = "pairwise"), adjust = "holm"), infer = TRUE)

df3.contr.between <-rbind(df3.contr.between.1, df3.contr.between.2, df3.contr.between.3, df3.contr.between.4)

df3.contr.between
df3.contr.between %>% 
  select(., Env, contrast, odds.ratio, asymp.LCL, asymp.UCL, z.ratio, p.value) %>%
  xtable(., digits=c(0,0,0,2,2,2,3,4))

#Contrasts within ModTypes, by ModMon.

df3.means.within.1 <- lsmeans(df3.model, specs = ~ Env | Pol, at = list(ModMon="UE", ModType="Comp"), type = "response")
df3.contr.within.1<- summary(rbind(lsmeans::contrast(df3.means.within.1, method = "pairwise"), adjust = "holm"), infer = TRUE)

df3.means.within.2 <- lsmeans(df3.model, specs = ~ Env | Pol, at = list(ModMon="DE", ModType="Comp"), type = "response")
df3.contr.within.2<- summary(rbind(lsmeans::contrast(df3.means.within.2, method = "pairwise"), adjust = "holm"), infer = TRUE)

df3.means.within.3 <- lsmeans(df3.model, specs = ~ Env | Pol, at = list(ModMon="UE", ModType="Sup"), type = "response")
df3.contr.within.3<- summary(rbind(lsmeans::contrast(df3.means.within.3, method = "pairwise"), adjust = "holm"), infer = TRUE)

df3.means.within.4 <- lsmeans(df3.model, specs = ~ Env | Pol, at = list(ModMon="DE", ModType="Sup"), type = "response")
df3.contr.within.4<- summary(rbind(lsmeans::contrast(df3.means.within.4, method = "pairwise"), adjust = "holm"), infer = TRUE)

df3.contr.within <-rbind(df3.contr.within.1, df3.contr.within.2, df3.contr.within.3, df3.contr.within.4)

df3.contr.within %>% 
  select(., Pol, contrast, odds.ratio, asymp.LCL, asymp.UCL, z.ratio, p.value) %>%
  xtable(., digits=c(0,0,0,2,2,2,3,4))
