library(tidyverse)
library(ggpubr)
library(dplyr)
library(rstatix)
library(car)
library(broom)
library(ggstatsplot)
library(ggplot2)
library(statsExpressions)
# (1) DATA PREPARATION
#_____________________
# IMPORT DATRA
library(readr)
enzymes <- read_csv("C:/Users/banel/Desktop/DESKTOP/ASSIGNMENT RANDOM/enzymes.txt")
view(enzymes)
enzymes2 <- enzymes %>%
  select(enzyme1,enzyme2,enzyme3,enzyme4,enzyme5,enzyme) %>%
  add_column(id = 1:nrow(enzymes), .before = 
               1)
head(enzymes2)

enzymes2=enzymes2%>%
  gather(key="enzyme",value="enzymescore",enzyme1,enzyme2,enzyme3,enzyme4,enzyme5,enzyme)%>%
  convert_as_factor(id, enzyme)

head(enzymes2,3)
kbl(enzymes2)
# (2) SUMMARY STATISTICS
summary(enzymes)
enzymes2%>%
  group_by(enzyme)%>%
  get_summary_stats(enzymescore,type="full",
                    show=c("n","mean","sd","max","min","se","q1","median","q3"))
# OR
table=enzymes2%>%
  group_by(enzyme)%>%
  get_summary_stats(enzymescore,type="full",
                    show=c("n","mean","sd","max","min","se","q1","median","q3"))
kbl(table)

# (3) VISUALIZATION
ggboxplot(enzymes2,x="enzyme",y="enzymescore",add="point")

# (4) CHECKING ASSUMPTIONS
# Check sample size assumption
enzymes2 %>%
  group_by(enzyme) %>%
  summarise(N = n())

#(a) outliers
table=enzymes2%>%
  group_by(enzyme)%>%
  identify_outliers(enzymescore)
table
#   OR 
kbl(table)

#(b) normality
enzyme_normality_check = enzymes2%>%
  group_by(enzyme)%>%
  shapiro_test(enzymescore)
enzyme_normality_check

#(c) qqplot
ggqqplot(enzymes2,"enzymescore", facet.by ="enzyme")

# (5) COMPUTATIONS

res.aov=anova_test(data=enzymes2, dv=enzymescore,
                   wid= id, within = enzyme)
get_anova_table(res.aov)

# (6)Post-Hoc test
ave(enzymes2$enzymescore, enzymes2$enzyme, FUN = mean)
pwc=enzymes2%>%
  pairwise.t.test(
    enzymescore~enzyme,paired=TRUE,p.adjust.method = "bonferroni")%>%
  select(-df,-statistic) #remove details



