library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(kableExtra)
library(ggstatsplot)
library(ggplot2)
library(dplyr)
library(statsExpressions)
#One-way repeated measures ANOVA
#_______________________________
#Data preparation
# (i) DATA PREPEARATION
#______________________

# Imprt data from a text reader
#______________________________
#(i) HPOTHESIS
# H0 : there is no significance diffence in the means soil quality score between
#      the six enzymes
# H1 : there is a significance difference in the means doil quality score betweent
#      the six enzymes,
# OR : atleast one enzyme mean soil quality score score is different from the rest

library(readr)
enzymes <- read_csv("C:/Users/banel/Desktop/DESKTOP/ASSIGNMENT RANDOM/enzymes.txt")
view(enzymes)

enzymes2 <- enzymes %>%
  select(enzyme1,enzyme2,enzyme3,enzyme4,enzyme5,enzyme) %>%
  add_column(id = 1:nrow(enzymes), .before = 
               1)
head(enzymes2)

#gather column enzyme1,enzyme2,enzyme3,enzyme4,enzyme5,enzyme into long format
#convert id and teatment in factor variable

enzymes2=enzymes2%>%
  gather(key="enzyme",value="enzymescore",enzyme1,enzyme2,enzyme3,enzyme4,enzyme5,enzyme)%>%
  convert_as_factor(id, enzyme)
head(enzymes2, 3)
kbl(enzymes2)

#============================================================================================

# (ii) SUMMARY STATISTICS
#________________________
# Grouping data by enzyme and compute : min,n,sd,se,Q1,Median, and Q3

summary(enzymes)
table=enzymes2%>%
  group_by(enzyme)%>%
  get_summary_stats(enzymescore,type="full",
                    show=c("n","mean","sd","max","min","se","q1","median","q3"))
print(table)


#============================================================================================

# (iii) VISUALIZATION
#____________________
enzymes_bxp = ggboxplot(enzymes2,x="enzyme",y="enzymescore",add="point")
enzymes_bxp 

#============================================================================================

# (iv) CHECK ASSUMPTIONS
#_______________________

# CHECK SAMPLE SIZE ASSUMPTIONS
#______________________________
# n is less than 50 suggesting that a QQ plot may not be good.

enzymes2 %>%
  group_by(enzyme) %>%
  summarise(N = n())

# (1) OUTLIERS
#_______________________
#4 outliers present from the enzyme data with their id,
#namely: enzyme id=3, enzyme1 id=8, enzyme3 id=3 and 8.
# and one xtreme outlier in enzyme3 id=3

enzymes2 %>%
  group_by(enzyme) %>%
  identify_outliers(enzymescore)


# (2) NORMALITY ASSUMPTIONS
#__________________________
#all the p-values of enzymes corresponding to the enzyme soil quality score
#are greater than 0.05, and the conclusion is the enzyme soil quality score
#is normally distributed except that of enzyme3 which is less than 0.05,
#and the QQ plot shows that not all the points of the enzyme3 lie on the 
# reference line, therefore is not normally distributed.

enzymes2 %>%
  group_by(enzyme) %>%
  shapiro_test(enzymescore)

ggqqplot(enzymes2, "enzymescore", facet.by = "enzyme")

#===========================================================================================

# (v) COMPUTATION
#________________
#Enzyme soil quality score is statistically significantly different on all enzymes
#with an f=6.45 and pvalue =0.004<0.05 and ges=0.337

res.aov <- anova_test(data = enzymes2, dv = enzymescore, wid = id, within = enzyme)
get_anova_table(res.aov)

#===========================================================================================
# (vi) POST-HOC TESTS
#____________________
# only 1 pairwise test which is statistically significant i.e enzyme1&enzyme5,
#the rest of the pairwise are not significant

# pairwise comparisons
pwc <- enzymes2 %>%
  pairwise_t_test(
    enzymescore ~ enzyme, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

#==========================================================================================

# (vii) REPORT
#_____________
#enzyme soil quality score is statistically significantly different at all enzymes
# f=6.45, p value= 0.004 ang ges= 0.337
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "enzyme")
pwc
ggplot(data = enzymes2) +
  geom_boxplot(mapping = aes(x = enzyme, y = enzymescore)) +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "enzyme")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 6.45, p= "0.004>0.0001", parameter = "2.46,22.13",
  type = "expression", detailed = TRUE
)
ggboxplot(
  enzymes2, x = "enzyme", y = c("enzymescore"), 
  merge = TRUE, palette = "jco"
) + 
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, tip.length = 0, 
    step.increase = 0.1, 
  ) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression")
  )


#==========================================================================================
