# (i) LOADING PREREQUISITES LIBRARIES
library(tidyverse)
library(ggpubr)
library(dplyr)
library(rstatix)
library(car)
library(broom)
library(datarium)
library(GGally)
library(purrr)

# (ii) DATA PREEPARATION
#_______________________
# IMPORT DATA
library(readr)
algae <- read_csv("C:/Users/banel/Desktop/DESKTOP/ASSIGNMENT RANDOM/algae.txt")
View(algae)

algae <- algae %>%
  select(chlorophyll, biovolume,
         class) %>%
  add_column(id = 1:nrow(algae), .before =
               1)
head(algae)

# (iii) VISUALIZATION
#____________________
# the box plots mostly in all classes it shows the presene of outliers
# with the cholorophyll median always greater than that of the biovolume
#in all classes
ggboxplot(
  algae, x = "class", y = 
    c("chlorophyll","biovolume"), 
  merge = TRUE, palette = "jco"
)

# (iv) SUMMARY STATISTICS
#________________________
# Grouping data by enzyme and compute : min,n,sd,se,Q1,Median, and Q3
# the chlorophyll always has got higher median and mean values in all clases

table=algae%>%
  group_by(class)%>%
  get_summary_stats(chlorophyll,biovolume,type="full",
                    show=c("n","mean","sd","max","min","se","q1","median","q3"))
print(table)

# (v) ASSUMPTIONS AND PRELIMINARY TESTS
#______________________________________
#(1) CHECK SAMPLE SIZE ASSUMPTION
# the sample size is greater than 50 the the QQ PLOT IS PREFERABLE
algae %>%
  group_by(class) %>%
  summarise(N = n())

#(2) IDENTIFY UNIVARIATE OUTLIERS
#________________________________
#clear indication that there are outliers on the chlorophyll and some are
#extreme outliers
algae %>%
  group_by(class) %>%
  identify_outliers(chlorophyll)

#also clear that there are outliers on the biovolume and some are
#extreme outliers
algae %>%
  group_by(class) %>%
  identify_outliers(biovolume)

#(3) DETECT MULTIVARIATE OUTLIERS
#________________________________
# Outliers are present
algae %>%
  group_by(class) %>%
  mahalanobis_distance(-id) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

#(4) CHECK UNIVARIATE NORMALITY ASUMPTIONS
#_________________________________________
# the p-value indicate that the data is not normally distributed
#since all the p-values are less than 0.05.
algae %>%
  group_by(class) %>%
  shapiro_test(chlorophyll, biovolume) %>%
  arrange(class)

# QQ plot of chlorophyll
#the points are not lying on the reference line this means
#the data is not normally distributed throughout the classes
ggqqplot(algae, "chlorophyll", facet.by = "class",
         ylab = "chlorophyll", ggtheme = theme_bw())

# QQ plot of biovolume
#the points are not lying on the reference line this means
#the data is not normally distributed throughout the classes
ggqqplot(algae, "biovolume", facet.by = "class",
         ylab = "biovolume", ggtheme = theme_bw())

# (5) MULTIVARIATE NORMALITY
#___________________________
# we can not assume multivariate normal distribution since
#the p value is less than 0.05.
algae %>%
  select(chlorophyll, biovolume) %>%
  mshapiro_test()

# (6) IDENTIFY MULTICOLLINEARITY
#_______________________________
#there is no multicolinearity since the p-value= 2.02e-153
#is less than 0.0001, with r=0.84
algae %>% cor_test(chlorophyll,biovolume)

# (7) CHECK LINEAR ASSUMPTIONS
#_____________________________
linearity <- algae %>%
  select(chlorophyll, biovolume, class) %>%
  group_by(class) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
linearity
linearity$plots # plots

# (8) CHECK THE HOMOGENEITY OF COVARIANCE ASSUMPTION
#___________________________________________________
#The test is statistically significant since p=7.34e-117 < 0.001, 
#so the data have violated the assumption of homogeneity of 
#variance-covariance matrices
box_m(algae[, c("chlorophyll", "biovolume")], algae$class)

# (9) CHECK THE HOMOGENEITY OF VARIANCE ASSUMPTION
#_________________________________________________
#The Levene’s test is significant (p =8.41e-12 & 5.52e- 7< 0.05), 
#so there was no homogeneity of variances.
algae %>% 
  gather(key = "variable", value = "value", chlorophyll, biovolume) %>%
  group_by(variable) %>%
  levene_test(value ~ class)

# (i) COMPUTATION
#________________
#There is a statistically significant difference between the classes 
# on the combined dependent variables 
#chlorophyll and biovolume, F(4, 1162) = 15.985, p=9.541e-13< 0.0001.
model_algae <- lm(cbind(chlorophyll, biovolume) ~ class, algae)
Manova(model_algae, test.statistic = "Pillai")

# (ii) POST-HOC TEST ###############################################
#(1) compute univariate one way anova
pwc=algae %>%
  gather(key="variables", value="value",chlorophyll,biovolume)%>%
  group_by(variables) %>%
  games_howell_test(value~class)%>%
  select(-estimate,-conf.low,-conf.high)
pwc

group_data(algae) %>% 
  welch_anova_test(chlorophyll~class)
# or do Kriskal wilks test
grouped.data %>% kruskal_test(value~class)
# or use aov()
grouped.data %>%anova_test(value~class)
# (2) COMPUTE MULTIVARIATE PAIRWISE COMPARISON
#####################################################################

# (i) REPORT
#A one-way multivariate analysis of variance was performed to determine the effect of algae class 
#on Chlorophyll and biovolume. 
#With three different class: kisel, bluegreen and fure.
#There was a statistically significant difference between the classes 
#on the combined dependent variables 
#(chlorophyl and biovolume), F(4, 1162) = 15.98, p=9.5e-13 < 0.0001.

# (ii) SUMMARY 
#_____________
# Visualization: box plots with p-values
# the bluegreen & fure,bluegreen & fure,fure & kisel
# shows no significance
pwc=algae %>%
  gather(key="variables", value="value",chlorophyll,biovolume)%>%
  group_by(variables) %>%
  games_howell_test(value~class)%>%
  select(-estimate,-conf.low,-conf.high)
pwc
pwc <- pwc %>% add_xy_position(x = "class")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 15.985, p= "9.5e-13<0.0001", parameter = "4,1162",
  type = "expression", detailed = TRUE
)
pwc

ggboxplot(
  algae, x = "class", y = c("chlorophyll", "biovolume"), 
  merge = TRUE, palette = "jco"
) + 
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, tip.length = 0, 
    step.increase = 0.1, step.group.by = "variables",
    color = "variables"
  ) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression"))