library(TeachingDemos)

# txtStart("exp1-analysis-output.txt")
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
library(effects) #for getting predicted means and associated CIs from the model
library(lsmeans) #for getting contrasts and associated CIs from the model 
library(sciplot)
library(gridExtra) #grid.table applied to df1 yields a table
library(xtable) #for LaTeX code
library(binom) #for calculating binomial confidence intervals
#(which can however be obtained more easily from a model with the effects package)

#Define 2 colors for ModType plots, 4 colors for Mod plots:
colors.2 <- c("#B3CDE3", "#FBB4AE")
colors.4 <- c("#B3CDE3", "#4f8bbe", "#FBB4AE", "#f42a18")

#Set working directory:
setwd("/home/dora/Dropbox/Academic/Research/2020-09-16-18 ELM 1 online (a) Superlative-modified numerals and negation: A negotiable cost/paper/")

#Load data in wide format. Melt into long format:

df1.wide <- read.csv('exp1-results-wide.csv', header = T)
head(df1.wide, n = 10)
df1 <- melt(df1.wide)
df1 <- tbl_df1(df1)
df1

#Rename the variables automatically created by melt for subjects and their response:
#(variable for subject collects all the elements Part1, Part2, etc., into one vector)
#(variable for response collect all the individual responses given, regardless of subject or condition, into one vector)

names(df1)[names(df1) == "variable"] <- "Participant"
names(df1)[names(df1) == "value"] <- "Response"
df1$Response <- factor(df1$Response)

str(df1)

df1 <- select(df1, Participant, Env, Pol, ModType, ModMon, Mon, Response)

#Set level order (first becomes reference level):
df1$Env <- factor(df1$Env, levels=c('Decl', 'AntCond','RestUniv'))
df1$Pol <- factor(df1$Pol, levels=c('Pos','Neg'))
df1$ModType <- factor(df1$ModType, levels=c('Comp','Sup'))
df1$ModMon<-factor(df1$ModMon, levels=c('UE','DE'))
df1$Mod <- factor(df1$Mod, levels=c('MoreThan', 'LessThan', 'AtLeast', 'AtMost'))
levels(df1$Env)
levels(df1$Pol)
levels(df1$ModType)
levels(df1$ModMon)
levels(df1$Mod)
df1

str(df1)

#Calculate binomial confidence intervals directly from the raw means, using the binom package:

#- Select factors of interest .
#- Get Mean, Successes (counts of 1), Count (total number of responses).
#- These will be summarized over Participant and Response for combinations of the factors of interest.

# By Mod.

df1.mod.msc <- ddply(df1, c('Env', 'Pol', 'Mod'), summarize,
                 Mean = mean(as.numeric(as.character(Response))),
                 Successes = sum(as.numeric(as.character(Response))),
                 Count = length(Response)
)

df1.mod.msc

CIs <- binom.confint(x=df1.mod.msc$Successes, n=df1.mod.msc$Count, methods="wilson")

CIs

df1.mod.mscci <- cbind(df1.mod.msc, CIs[,5:6])

df1.mod.mscci
#This can be used to generate a table with the raw means and their associated 95% binomial CIs.
#First, extract Env, Pol, Mod, Mean, lower, and upper. 
df1.mod.mscci.latex <-select(df1.mod.mscci, Env, Pol, Mod, Mean, lower, upper)
df1.mod.mscci.latex 
#Second, print to LaTeX table, with lower and upper rounded. 
xtable(df1.mod.mscci.latex) #Digits for lower and upper automatically rounded to two decimal places.
#Done!


#Now, let's plot the raw means and their associated CIs.
ggplot(df1.mod.mscci, aes(x=Pol, y=Mean, fill=Mod)) +
  facet_wrap(~ Env) +
  geom_bar(position=position_dodge(width = .9), stat="identity",colour="black",size=.25) +
  geom_errorbar(aes(ymin=lower, ymax=upper),
                width=.4,position=position_dodge(.9),size=.33) +
  # ylim(c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,0.2), minor_breaks = seq(0, 1, 0.02)) + #draws major (minor) y ticks at 0.2 (0.02) intervals
  xlab("") +
  # ylab("Mean rating") +
  ylab("") +
  theme_bw() +
  # geom_text(aes(label = round(Mean, digits = 2)), size = 4, position = position_dodge(0.9), vjust=-.3) + #prints mean above each bar
  theme(text = element_text(size=17)) + #makes label size 15
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_fill_manual(values=colors.4) +
  ggsave("exp1-mod-raw.png", width=10, height=4, dpi=300)

# Fit logistic mixed-effects models. 
# Fixed effects -- all the main factors of interest. 
# Random effects -- intercept for Participant; random slopes -- the maximal we can fit in a principled way (no interactions, all 2-way, all3-way).
# Random slope:
# No slopes: converges fast.
# (1 + (ModMon + ModType + Pol + Env)|Participant): Didn't use to converge but now converges in <7 mins, though with isSingular warning. Can use.

df1.model <- glmer(Response ~  ModMon * ModType * Pol * Env + (1 + (ModMon + ModType + Pol + Env)|Participant), 
                       family = 'binomial',
                       data=df1, 
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(df1.model, corr=FALSE)

# Get model results in LaTeX form.
xtable(coef(summary(df1.model)), digits=c(0,2,2,3,4))

# # If desired, get and plot predicted marginal mean probabilities for each group, with associated confidence intervals. 
# # effects package
# 
# df1.modmon.modtype.eff <- allEffects(df1.model)
# # df1.modmon.modtype.eff
# # plot(df1.modmon.modtype.eff) #quick and dirty plot
# df1.modmon.modtype.eff_df1 <- as.data.frame(df1.modmon.modtype.eff[["ModMon:ModType:Pol:Env"]])
# 
# df1.modmon.modtype.eff_df1 
# # "lower/upper" are the 95% CIs for the predicted marginal mean probabilities for each group 
# 
# #Get summary in LaTeX form, with values rounded off in all numerical columns.
# df1.modmon.modtype.eff_df1 %>% 
#   mutate_if(is.numeric, funs(round(., 2))) %>% 
#   select(., Env, Pol, ModMon, ModType, fit, se, lower, upper) %>%
#   xtable() %>%
#   
#   print(., include.rownames=FALSE)
# 
# str(df1.modmon.modtype.eff_df1)
# 
# Plot as usual to get predicted means and their associated CIs.

# Otherwise, proceed to extract predicted contrasts with lsmeans.
# In contr, use adjust = "holm".
# If too many comparisons, use adjust = "none", then adjust later in contr after filtering for the contrasts of interest.
# 
# In the first line put in the contrast of interest. 
# # E.g., below, testing for levels of Modtype by Env at Pol = Neg and ModMon = UE.
# # Essentially checking MoreThan vs. AtLeast in Decl-Neg, AntCond-Neg, and RestUniv-Neg.
# # Warning: Results may be misleading due to involvement in interactions.
# # This is a warning that we are evaluating main effects in a model that contains interactions.
# # The model results did not reveal any significant interaction.
# # Still, such interactions are likely to exist, so we'll take the results provisionally rather than definitively.
# 
# Then run the next 5 lines to get the xtable summary.
# Change values in first line and repeat to get other contrasts.

#Contrasts between ModTypes, by ModMon, e.g., MoreThan vs. Atleast in Decl-Pos:

df1.means.between.1 <- lsmeans(df1.model, specs = ~ ModType | Env, at = list(Pol = "Pos", ModMon = "UE"), type = "response")
df1.contr.between.1 <- summary(rbind(lsmeans::contrast(df1.means.between.1, method = "pairwise"), adjust = "holm"), infer = TRUE)

df1.means.between.2 <- lsmeans(df1.model, specs = ~ ModType | Env, at = list(Pol = "Pos", ModMon = "DE"), type = "response")
df1.contr.between.2 <- summary(rbind(lsmeans::contrast(df1.means.between.2, method = "pairwise"), adjust = "holm"), infer = TRUE)

df1.means.between.3 <- lsmeans(df1.model, specs = ~ ModType | Env, at = list(Pol = "Neg", ModMon = "UE"), type = "response")
df1.contr.between.3 <- summary(rbind(lsmeans::contrast(df1.means.between.3, method = "pairwise"), adjust = "holm"), infer = TRUE)

df1.means.between.4 <- lsmeans(df1.model, specs = ~ ModType | Env, at = list(Pol = "Neg", ModMon = "DE"), type = "response")
df1.contr.between.4 <- summary(rbind(lsmeans::contrast(df1.means.between.4, method = "pairwise"), adjust = "holm"), infer = TRUE)

df1.contr.between <-rbind(df1.contr.between.1, df1.contr.between.2, df1.contr.between.3, df1.contr.between.4)

df1.contr.between %>% 
  select(., Env, contrast, odds.ratio, asymp.LCL, asymp.UCL, z.ratio, p.value) %>%
  xtable(., digits=c(0,0,0,2,2,2,3,4))  
# %>%
#   print(., include.rownames=FALSE)


#Careful: <.0001 displayed as 0.000. Correct after copy-paste.

# Contrasts within ModTypes, by ModMon, e.g., AtMost in Decl-Neg vs. AntCond-Neg:
# We only care about Pol=Neg.
 
df1.means.within.1 <- lsmeans(df1.model, specs = ~ Env, at = list(Pol = "Neg", ModType="Comp",ModMon="UE"), type = "response")
df1.contr.within.1 <- summary(rbind(lsmeans::contrast(df1.means.within.1, method = "pairwise"), adjust = "holm"), infer = TRUE)

df1.means.within.2 <- lsmeans(df1.model, specs = ~ Env, at = list(Pol = "Neg", ModType="Comp",ModMon="DE"), type = "response")
df1.contr.within.2 <- summary(rbind(lsmeans::contrast(df1.means.within.2, method = "pairwise"), adjust = "holm"), infer = TRUE)

df1.means.within.3 <- lsmeans(df1.model, specs = ~ Env, at = list(Pol = "Neg", ModType="Sup",ModMon="UE"), type = "response")
df1.contr.within.3 <- summary(rbind(lsmeans::contrast(df1.means.within.3, method = "pairwise"), adjust = "holm"), infer = TRUE)

df1.means.within.4 <- lsmeans(df1.model, specs = ~ Env, at = list(Pol = "Neg", ModType="Sup",ModMon="DE"), type = "response")
df1.contr.within.4 <- summary(rbind(lsmeans::contrast(df1.means.within.4, method = "pairwise"), adjust = "holm"), infer = TRUE)

df1.contr.within <-rbind(df1.contr.within.1, df1.contr.within.2, df1.contr.within.3, df1.contr.within.4)

df1.contr.within %>%
  select(., contrast, odds.ratio, asymp.LCL, asymp.UCL, z.ratio, p.value) %>%
  xtable(., digits=c(0,0,2,2,2,3,4)) 
# %>%
#   print(., include.rownames=FALSE)



