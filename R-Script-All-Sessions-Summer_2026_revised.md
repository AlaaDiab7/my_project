---
title: "R Practices All Sessions"
author: "Sebastian Fischer/ Marius Matthiä / Laura Bisinger / Tobit Zaun"
date: "2024-03-15"
output:
  word_document: default
  html_document: default
  pdf_document: default
---

```{r global options, include=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE, eval = FALSE, echo = FALSE)
```

A note before we start: R is an open programming environment. The various packages and commands we use in this practice may therefore change over time. Also, new and improved packages may be invented and various packages may offer the same or similar commands. Therefore, it is good to use Google (or ChatGPT) to find new and improved ways to do the statistics we need for this practice.

A first and highly important step is the setup of a working directory. Please use this directory to save the Practice.rmd file, the data, and the folder with the images

You can set your working directory to any folder you like. For this practice it is the best, if you set it to your already created user folder, so that you keep all files between sessions. For this, first open the "R" folder in your user documents directory
A:\Module und Vorlesungen\Modul Quantitative Methods SS 2024\Practice SoSe 2024

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Schritt 1.png){width="214"}


2.  Right click on the file path and copy the address as text.

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Schritt 2.png){width="379"}

3.  Paste the copied path in the R markdown file. You have to change the "\\" to "/" symbols manually. As you need this path multiple times, you can do so by searching and replacing "YOUR FILE PATH" with the correct path.

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Schritt 3.png){width="379"}
```{r Session 1, eval = FALSE, echo = FALSE}
### Session 1: Introduction to R 

#### preperations #### 


# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())
   

##### installing and loading packages #####
# this checks whether the package is already installed and only does it when necessary
# then loads the packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("psych")){install.packages("psych")}; library(psych)
if(!require("mosaic")){install.packages("mosaic")}; library(mosaic)
if(!require("dplyr")){install.packages("dplyr")}; library(dplyr)
if(!require("tibble")){install.packages("tibble")}; library(tibble)
if(!require("openxlsx")){install.packages("openxlsx")}; library(openxlsx)
if(!require("ggplot2")){install.packages("ggplot2")}; library(ggplot2)
if(!require("Hmisc")){install.packages("Hmisc")}; library(Hmisc)
if(!require("apaTables")){install.packages("apaTables")}; library(apaTables)
if(!require("officer")){install.packages("officer")}; library(officer)
if(!require("flextable")){install.packages("flextable")}; library(flextable)
if(!require("haven")){install.packages("haven")}; library(haven)
if(!require("skimr")){install.packages("skimr")}; library(skimr)
if(!require("tidyr")){install.packages("tidyr")}; library(tidyr)
if(!require("ggfortify")){install.packages("ggfortify")}; library(ggfortify)
if(!require("broom")){install.packages("broom")}; library(broom)
if(!require("sjPlot")){install.packages("sjPlot")}; library(sjPlot)
if(!require("car")){install.packages("car")}; library(car)
if(!require("MBESS")){install.packages("MBESS")}; library(MBESS)
if(!require("olsrr")){install.packages("olsrr")}; library(olsrr)
if(!require("interactions")){install.packages("interactions")}; library(interactions)
if(!require("sandwich")){install.packages("sandwich")}; library(sandwich)
if(!require("lavaan")){install.packages("lavaan")}; library(lavaan)
if(!require("multilevel")){install.packages("multilevel")}; library(multilevel)
if(!require("lattice")){install.packages("lattice")}; library(lattice)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)



# uploading a dataset from Excel to R 
Intro<-read_excel("Introduction dataset_Spotify music.xlsx")



#### getting familiar with the data ####

# getting more information on a certain package or function 
?psych
help("psych")
?describe
help("describe")

# getting a description of the uploaded dataset. Make sure to reference the correct package. 
psych::describe(Intro)

# taking a look at a specific variable in the dataset 
describe(Intro$loudness)

# another function for getting a first overview of the dataset
str(Intro)


## calculating means with and without missing values

# if danceability has any missing values the mean score can't be calculated
mean(Intro$danceability)
# so we get an NA (not available)

# this removes the NA-values while computing the mean
mean(Intro$danceability, na.rm=TRUE)

# this checks which row(s) of the variable danceability are NAs
which(is.na(Intro$danceability))


# an alternative way to write the same code
# the pipe ( %>% ) "transports" the code on its left side into the () of the function after the pipe
# it takes a while to get used to this way of writing but it improves readability
Intro$danceability %>% 
  is.na() %>% 
  which()


# looking at descriptive statistics
mean(Intro$duration_ms)
sd(Intro$duration_ms)



#### data visualization ####

# barplot
barplot(table(Intro$key), ylab = "Frequency")

# again an alternative way of writing the code
Intro$key %>%
  table() %>%
  barplot(ylab = "Frequency")

#histogram
hist(Intro$tempo, main = "Histogram tempo", xlab = "Tempo")

# again an alternative way of writing the code
Intro$tempo %>%
  hist(main = "Histogram tempo", xlab = "Tempo")

# line graph
plot(density(Intro$tempo), main = "Line graph", xlab = "Tempo", ylab = "Density")

# again an alternative way of writing the code
Intro$tempo %>% 
  density() %>% 
  plot(main = "Line graph", xlab = "Tempo", ylab = "Density")

# yet another option to get an overview over a certain variable
mosaic::favstats(~danceability, data=Intro)
boxplot(Intro$tempo~Intro$key)



#### subsetting data(sets) ####

mean(Intro$duration_ms)

# creating a subset of songs with a higher duration than the mean
Intro2 <- subset(Intro, Intro$duration_ms > 246306.2)

# we can also safe values and reuse them
dur_mean <- mean(Intro$duration_ms)
Intro3 <- subset(Intro, Intro$duration_ms > dur_mean)

# checking whether subsetting worked
isTRUE(all(Intro2$duration_ms >= 246306.2))


## other examples of subsetting
# c() combines values into a list
# [,x] selects variables (columns)
# [x,] selects rows
Intro4 <- Intro[,c("danceability","key","acousticness")]
Intro5 <- Intro[Intro$key>4,]
Intro6 <- Intro[c(1:5),]


#### saving our data ####

# saving your work in different document formats 

# excel doc
write.xlsx(Intro2, "Intro2_240318.xlsx")

# also excel doc but differnt
write.csv(Intro2, "Intro2_240318.csv")

# R data file
saveRDS(Intro2, "Intro2_240318.rds")

#This would be it for the first practice session.
#Task 8: Homework: In preparation for next session: Investigate the correlation between two variables in the data
#Also take a look at this Article: Berg, J. M. (2022). One-hit wonders versus hit makers: Sustaining success in creative industries. Administrative Science Quarterly, 67(3), 630-673. 
#How did the author use the information from Spotify? 


```

### Session 2: Correlation

Task 1: investigate the correlations among the variables

1.	Use R to produce a scatterplot and to calculate intercorrelations between variables
2.	Interpret the correlation matrix: Which values catch your attention? What do these values indicate?
3.	Produce a correlation matrix in APA Style.

Task 2: Work with the Excel tool:

1.	Get acquainted with the tool. Raw scores are in cells highlighted yellow. Other cells include formulas.

2.	Introduce an outlier with high/low values on one of the variables/both of the variables: How does this change the correlation among the variables?

3.	Change the data to achieve a perfect positive correlation of r = 1. 

4.	Change the data to achieve a negative correlation (r =-.05) and a correlation of r = 0

5.	Investigate t-values: How strong a correlation (for DF = 18) would we need in order for it to be significant at Alpha = .05? How about Alpha = .01?

6.	What could we do to reach significance (Alpha = .05) if we know that the (population) correlation is r = .26? 


```{r Session 2, eval = FALSE, echo = FALSE}

### Session 2: Correlations


# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("ggplot2")){install.packages("ggplot2")}; library(ggplot2)
if(!require("Hmisc")){install.packages("Hmisc")}; library(Hmisc)
if(!require("apaTables")){install.packages("apaTables")}; library(apaTables)
if(!require("officer")){install.packages("officer")}; library(officer)
if(!require("flextable")){install.packages("flextable")}; library(flextable)
if(!require("dplyr")){install.packages("dplyr")}; library(dplyr)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)


#?readxl
#read data into R, call the data 'Intro'


Intro <- read_excel("Introduction dataset_Spotify music.xlsx")



#### Task 1 - Scatterplot ####
# Task 1: Use R to produce a scatterplot and to calculate intercorrelations between variables #

#First, we need a list of the variables in the dataset. This can be done, for example, with this command:
list(Intro)


# We can investigate the relation between two variables using a scatterplot

#add a linear fit line to the plot
plot(Intro$acousticness ~ Intro$energy, xlab = "energy", ylab = "acousticness")
plot(Intro$energy, Intro$acousticness, xlab = "energy", ylab = "acousticness")


# alternative 
#Intro %>% 
 # select(energy, acousticness) %>% 
  #plot(xlab = "energy", ylab = "acousticness")


# add a linear fit line
plot(Intro$acousticness~Intro$energy, xlab = "energy", ylab = "acousticness")
abline(lm(Intro$acousticness~Intro$energy),col = "green")


# we can also do this with one command 
Intro %>% 
  { 
    plot(.$energy, .$acousticness, xlab = "energy", ylab = "acousticness") 
    abline(lm(acousticness ~ energy, data = .), col = "green")
  }


# alternative with ggplot2
Intro %>%
  ggplot(aes(x = energy, y = acousticness)) +
  geom_point(color = "darkgreen", size = 1.5, shape = 21) +
  geom_smooth(method = "lm", color = "darkorange", se = FALSE) +
  labs(x = "energy", y = "acousticness") + 
  theme_minimal()

# this makes it more colorful but less usefull!
Intro %>%
  mutate(random_color = sample(colors(), n(), replace = TRUE)) %>%  # creating random colors
  ggplot(aes(x = energy, y = acousticness, color = random_color)) +
  geom_point(
    size = 1.5,
    #shape = 21
             ) +
  geom_smooth(method = "lm", color = "darkorange", se = FALSE) +
  labs(x = "energy", y = "acousticness") +
  theme_minimal() +
  theme(legend.position = "none")  # no legend for random colors



#### Task 1 - Intercorrelations ####
#Now calculate the correlation among variables that are interval or ratio scaled (we can look at correlations among multiple variables at once)
correl<-Intro[,c("acousticness", "danceability", "duration_ms", "energy", "instrumentalness","liveness", "loudness", "speechiness", "tempo", "valence")]

# alternative
#correl <- Intro %>% 
 # select(acousticness, danceability, duration_ms, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence)


#Produce a correlation matrix from the output:
corr<-rcorr(as.matrix(correl))
corr
# This is relatively difficult to interpret. Hence, there is an alternative way to produce an APA Style correlation matrix.


#The final table
apa.cor.table(correl)

#Most packages have many options. You can always use the "?" command to see, what options there are (see "Help" window)
?apaTables

# saving the correlation table to our working directory
apa.cor.table(correl, filename = "Correlation_table.rtf")
```

### Session 3: Advanced data preparation for psychological analyses

During the next practice session, we will work on data from a study with a number of small businesses in Germany. A main topic of the study was 'error management', that is the way by which people handle errors at work. In addition to scales on error management (which we may use towards the end of this course), a number of psychological scales were assessed among individuals, for example their affective commitment towards work, personal initiative at work, but also workplace-describing variables from the Job Diagnostic Survey. In this practice session, we need to prepare the data so that we can work with it during the next weeks.

Tasks:

Task 1: Load data from SPSS(.sav). Name of the data: "Practice dataset_error management.sav"

Task 2: Find variables that are part of the affective commitment scale and calculate Cronbach's Alpha. Then combine them into a single mean value.

Cronbach's alpha is a measure for the interrelatedness among items, that is the degree to which they measure something similar. Common thresholds suggest that Cronbach's alpha should be above  α {\displaystyle \alpha } = .75 to indicate sufficient interrelatedness for summarizing multiple items into a single value

Affective commitment items are:

(1) I would be happy to work at my organization until I retire.
(2) I am proud to tell others I work for this organization.
(3) I do not feel particularly attached to my organization. (REVERSED)
(4) I feel a strong sense of belonging to my organization.
(5) I believe that my personal values fit with the values of my organization.

(Allen, N. J., & Meyer, J. P. (1990). The measurement and antecedents of affective, continuance and normative commitment to the organization. Journal of Occupational Psychology, 63(1), 1–18. https://doi.org/10.1111/j.2044-8325.1990.tb00506.x)

Task 3: Do the same for the personal initiative (EI) scale 

Personal initiative items are:
(1) I actively attack problems.
(2) Whenever something goes wrong, I search for a solution immediately.
(3) Whenever there is a chance to get actively involved, I take it.
(4) I take initiative immediately even when others don't.
(5) I use opportunities quickly in order to attain my goals.
(6) Usually I do more than I am asked to do.
(7) I am particularly good at realizing ideas.

(Fay, D., & Frese, M. (2001). The concept of personal initiative: An overview of validity studies. Human Performance, 14(1), 97-124.)


Task 4: Calculate more variables. Some of the new variables are reversed and need to be recoded before analysis

Items from the Job Diagnostic Survey are:
Sv: Subjective value of the work a person does. Example: The tasks I do at work are simple and repetitive.
Fb: Feedback through work. Example: When working, I can see the quality of my work.
Ts: Task significance. Example: The way I do my work influences many others.
Ti: task independence. Example: At work, I can do tasks from start to finish.
Aut: Autonomy. Example: My work gives me the opportunity to decide by myself how I do my work.
  
Task 5: Save the data so we can use it during the next sessions



```{r Session 3, eval = FALSE, echo = FALSE}
# Session 3: Advanced Data preperation

# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("haven")){install.packages("haven")}; library(haven)
if(!require("psych")){install.packages("psych")}; library(psych)
if(!require("dplyr")){install.packages("dplyr")}; library(dplyr)
if(!require("apaTables")){install.packages("apatables")}; library(apaTables)
if(!require("magrittr")){install.packages("magrittr")}; library(magrittr)


#### Task 1 ####
# Task 1: Load data from SPSS
data <- read_sav("Practice dataset_error management.sav")



#### Task 2 ####
#Task 2: We often need to combine several items into a single scale score before we can continue with our analyses. 
#To compute the reliability (Cronbach's alpha) of a scale before we can combine different indicators into one scale value.


#investigate the alpha function in the package
??alpha
?psych
?alpha


#calculate alpha for affective commitment 
# com_1,com_2,com_3r,com_4,com_5

psych::alpha(data[c("com_1","com_2","com_3r","com_4","com_5")], na.rm=T)
# the functions warns us when items are reversed and hence negatively correlated with the first principal component

# check.keys = T reverses the items with negative loading on the fpc
psych::alpha(data[c("com_1","com_2","com_3r","com_4","com_5")], check.keys = TRUE, na.rm=T)


#Compare this to the intercorrelations among the variables

correl_com<- data[,c("com_1","com_2","com_3r","com_4","com_5")]
apa.cor.table(correl_com)


#dplyr:case_match has broader usability
data$com_3rec <- data$com_3r %>% 
  case_match(0 ~ 4, 1 ~ 3, 2 ~ 2, 3 ~ 1, 4 ~ 0)
# for example you can recode multiple numbers grouped together
# ... case_match(1:20 ~ 1, 21:40 ~ 2 ...)
# with case_match numeric values don't need ""
# other values do need ""
# .default = x , for any unmatched value


#as an alternative, we can calculate the reversed scale as: 
data$com_3rec_alt<-5 - data$com_3r


#using the recoded item, results are the same
psych::alpha(data[c("com_1","com_2","com_3rec","com_4","com_5")], check.keys = TRUE, na.rm=T)


#Again, compare this to the intercorrelations among the variables

correl_com2<- data[,c("com_1","com_2","com_3r", "com_3rec", "com_4","com_5")]
apa.cor.table(correl_com2)


# Calculate the scale mean in the data, name it 'com'
data$com <- (data$com_1+data$com_2+data$com_3rec+data$com_4+data$com_5)/5

# we can also calculate the scale mean with the apply function
data$com_apply <- apply(data[, c("com_1", "com_2", "com_3r", "com_4", "com_5")], 1, mean, na.rm = TRUE)

# or using two similar alternatives from the dplyr package
data <- data %>%
  mutate(com_mean1 = rowMeans(across(c(com_1, com_2, com_3rec, com_4, com_5)), na.rm = TRUE))

#### Task 3 ####
#Task 3: Do the same for the personal initiative (EI) scale (Fay, D., & Frese, M. (2001). The concept of personal initiative: An overview of validity studies. Human performance, 14(1), 97-124.)

#PI
#the personal initiative scale consists of seven items:
#ei1, ei2, ei3, ei4, ei5, ei6", ei7

psych::alpha(data[c("ei1","ei2","ei3","ei4","ei5", "ei6", "ei7")], na.rm=T)
#Calculate the scale mean in the data, name it 'pi'
data$pi <- (data$ei1+data$ei2+data$ei3+data$ei4+data$ei5+data$ei6+data$ei7)/7




#### detour factor analysis ####
#A quick detour into factor analysis

#While we know from Cronbach's alpha, that both personal initiative and affective commitment are reliable, we may also want to see whether the two variables are distinct, in that they form separate constructs. This we can do using factor analyses.You will hear more on factor analysis in 7th semester (diagnostics).

data_for_factor_analysis<- data[,c("com_1","com_2","com_3rec", "com_4","com_5", "ei1","ei2","ei3","ei4","ei5", "ei6", "ei7")]

#This is how this can be done:
factanal(na.omit(data_for_factor_analysis),factors = 1, rotation='promax')
factanal(na.omit(data_for_factor_analysis),factors = 2, rotation='promax')
factanal(na.omit(data_for_factor_analysis),factors = 3, rotation='promax')
factanal(na.omit(data_for_factor_analysis),factors = 4, rotation='promax')
# We can conclude, that a two-factor solution fits the data relatively well with both factors explaining an equal amount of variance. 
# However, three-factor solutions and four-factor solutions indicate some (limited) overlap between both constructs 
# (items with items loading on more than one factor).




#### Task 4 ####
#Task 4. Calculate more variables

#Again, some of the new variables are reversed and need to be recoded before analysis
#"dplyr::recode" specifies that the "recode" function of package dplyr has to be used. This is useful if 2 packages have a command with the same name.
data$jd_14_ts1_rec<-dplyr::recode(data$jd_14_ts1_r,"0"=4,"1"=3,"2"=2,"3"=1,"4"=0, .missing = NULL,)
data$jd_15_ti1_rec<-dplyr::recode(data$jd_15_ti1_r,"0"=4,"1"=3,"2"=2,"3"=1,"4"=0, .missing = NULL,)
data$jd_17_aut2_rec<-dplyr::recode(data$jd_17_aut2_r,"0"=4,"1"=3,"2"=2,"3"=1,"4"=0, .missing = NULL,)
data$jd_18_fb2_rec<-dplyr::recode(data$jd_18_fb2_r,"0"=4,"1"=3,"2"=2,"3"=1,"4"=0, .missing = NULL,)
data$jd_12_sv1_rec<-dplyr::recode(data$jd_12_sv1_r,"0"=4,"1"=3,"2"=2,"3"=1,"4"=0, .missing = NULL,)

# alternative that does everything in one command
data <- data %>%
  mutate(across(c(starts_with("jd_"), ends_with("r")), 
                ~ case_match(.,
                             0 ~ 4,
                             1 ~ 3,
                             2 ~ 2,
                             3 ~ 1,
                             4 ~ 0), 
                .names = "{.col}ec"))



#First investigate inter-correlations among items
correl<- data[,c("jd_12_sv1_rec","jd_23_sv2","jd_13_fb1","jd_18_fb2_rec","jd_14_ts1_rec","jd_25_ts2","jd_15_ti1_rec","jd_24_ti2","jd_16_aut1","jd_17_aut2_rec")]


#print a correlation table in APA Style
apa.cor.table(correl)


#then calculate alphas for the scales
psych::alpha(data[c("jd_12_sv1_rec","jd_23_sv2")], na.rm=T)
psych::alpha(data[c("jd_13_fb1","jd_18_fb2_rec")], na.rm=T)
psych::alpha(data[c("jd_14_ts1_rec","jd_25_ts2")], na.rm=T)
psych::alpha(data[c("jd_15_ti1_rec","jd_24_ti2")], na.rm=T)
psych::alpha(data[c("jd_16_aut1","jd_17_aut2_rec")], na.rm=T)
# Alphas are not great, which is often the case for two-item scales. Therefore, these should be avoided. 


# Nonetheless, produce scale means in the data
data$sv <- data$jd_12_sv1_rec + data$jd_23_sv2 / 2
data$fb <- data$jd_13_fb1 + data$jd_18_fb2_rec / 2
data$ts <- data$jd_14_ts1_rec + data$jd_25_ts2 / 2
data$ti <- data$jd_15_ti1_rec + data$jd_24_ti2 / 2
data$aut <- data$jd_16_aut1 + data$jd_17_aut2_rec / 2

# alternative

data <- data %>%
  mutate(
    sv = (jd_12_sv1_rec + jd_23_sv2) / 2,
    fb = (jd_13_fb1 + jd_18_fb2_rec) / 2,
    ts = (jd_14_ts1_rec + jd_25_ts2) / 2,
    ti = (jd_15_ti1_rec + jd_24_ti2) / 2,
    aut = (jd_16_aut1 + jd_17_aut2_rec) / 2
  )



#### Task 5 ####
#Task 5: Save the data so we can use it during the next sessions

install.packages("openxlsx")
library(openxlsx)
openxlsx::write.xlsx(data, "data_session3.xlsx")

```



### Session 4: Simple linear regression

Tasks:

1. Read the data we stored in session 3. Remember we saved it in excel format.

2. Calculate linear regression commitment on personal initiative

3. Diagnose the model and interpret results

4. Compute standardized model and compare results with unstandardized model. Compare results from standardized and unstandardized regression models and the correlation


```{r Session 4, eval = FALSE, echo = FALSE}

# Session 4: Simple linear regression

# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("skimr")){install.packages("skimr")}; library(skimr)
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("apaTables")){install.packages("apaTables")}; library(apaTables)



#### Task 1 ####
# load the data
data <- read_excel("data_session3.xlsx")



# get an overview over the data
skim(data)



#### Task 2 ####
#Task 2: Calculate linear regression commitment on personal initiative
?lm
reg<-lm(com~pi,data,na.action = na.exclude)
summary(reg)



#### Task 3 ####
#Task 3: Diagnose the model: 

# when which isnt specified it usually runs the first 4 diagnostics (which = 1, which = 2, which = 3, which = 5)
plot(reg)

plot(reg, which = 1)
# which = 1 residuals vs fitted plot
# checks for non-linearity and unequal variance (heteroscedasticity).
# Ideal: Random scattering without pattern.

plot(reg, which = 2)
# which = 2 qq-plot
# checks whether the residuals are normally distributed.
# Ideal: The points should be close to the line.

plot(reg, which = 3)
# which = 3 scale location plot (/spread location plot)
# checks for homoscedasticity (constant variance of residuals)
# ideal: horizontal line and random scatter without a clear pattern.

plot(reg, which = 4)
# which = Cook’s distance plot
# Identifies influential observations that might significantly affect the regression model.

plot(reg, which = 5)
# which = 5 residuals vs leverage
# identifies influential points that may disproportionately affect the model
# points with high leverage AND high residuals are potentially problematic.



#### Task 4 ####  
#Task 4: Compute standardized model and compare results with unstandardized model
reg.std<-lm(scale(com)~scale(pi),data,na.action = na.exclude)
summary(reg.std)


#Compare results from standardized and unstandardized regression models with the correlation
correl<-data[,c("com","pi")]
library(apaTables)
apa.cor.table(correl)

```

### Session 5: Standardized vs. Unstandardized data, partial correlation

From practice 4:
Calculate linear regression commitment on personal initiative. Diagnose the model and interpret results

New in practice 5: 

Task 1: Exclude cases based on model diagnostics

Task 2: Compute standardized model and compare results with unstandardized model

Task 3: Compare results from standardized and unstandardized regression models with with the correlation

Task 4: Use apatables to produce a regression table

Task 5: Add gender ("Gesch") and age ("Alter") as additional variables, one by one, and produce a multiple regression table

Task 6: Partial Correlation (In Excel, not in R):

From the lecture: What is the relationship of self-discipline and the overall GPA if we partial out the achievement test score?
	First, based on what you know about partial correlation, make and assumption: Will partialing out the achievement test score influence the relationship of self-discipline and GPA strongly? And in which direction?
	Then, calculate the partial correlation using the formula and the partial correlation tool
	
Last, use the partial correlation tool: Try to set up the tool in such a way that
	r_(XY.Z) is 0 while r_XY is not 0
	r_(XY.Z) is stronger than r_XY
	r_(XY.Z) is weaker than r_XY


```{r Session 5, eval = FALSE, echo = FALSE}

# Session Session 5: Standardized vs. Unstandardized data, partial correlation



# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("dplyr")){install.packages("dplyr")}; library(dplyr)
if(!require("tidyr")){install.packages("tidyr")}; library(tidyr)
if(!require("ggfortify")){install.packages("ggfortify")}; library(ggfortify)
if(!require("broom")){install.packages("broom")}; library(broom)
if(!require("sjPlot")){install.packages("sjPlot")}; library(sjPlot)
if(!require("apaTables")){install.packages("apaTables")}; library(apaTables)



#load the data
data <- read_excel("data_session3.xlsx")



# From last practice: Diagnose the model and interpret results
# Aside from the plots as shown above, we can get additional information (e.g. on Cook’s distance) with the ‘broom’ package. 
# unfortunately, the broom package works with complete cases only. 
# Therefore, we need to produce data that includes only complete cases for the variables of interest. 
# Using complete cases on all variables used in the regression makes sense, especially in multiple regression models, 
# as differences between models could otherwise be due to the exclusion or inclusion of cases.


#this is how we can create a data sheet with complete cases only:
data2<-data[complete.cases(data$pi, data$com, data$Gesch, data$Alter),]
View(data2)

# alternative way using tidyr
data2 <- data %>%
  drop_na(pi, com, Gesch, Alter)

#Regression from last practice
reg<-lm(com~pi,data,na.action = na.exclude)

#rerun the regression with the new data sheet data2

reg2<-lm(com~pi,data2)
summary(reg2)

tab_model(reg, reg2)

#how did the regression result change? Try to write the code for the regression using the initial data sheet ('data')



#Diagnostics with the broom package. You can find a good explanation here: https://broom.tidymodels.org/articles/broom.html
?broom

#functions of the broom package
tidy(reg2)
glance(reg2)
augment(reg2)

#We need the augment function to produce additional statistics:
model.diag.metrics <- augment(reg2)
head(model.diag.metrics)



#Now, we want to plot this information with ggplot

?ggplot
ggplot2::ggplot(model.diag.metrics, aes(pi, .cooksd)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(x = pi, y = 0, xend = pi, yend = .cooksd), color = "red", linewidth = 0.3)


ggplot2::ggplot(model.diag.metrics, aes(pi, com)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = pi, yend = .fitted), color = "red", size = 0.3)



par(mfrow = c(2, 2)) 
# changes the plot window to display 4 plots at once (two by two)
plot(reg2) 

#plots all 4 plots 
# see session 4 for an explanation



#another alternative (not needed):
autoplot(reg2)
model.diag.metrics %>%
  top_n(3, wt = .cooksd)

plot(reg2, which=4)



#### Task 1 ####
#Task 1: Exclude cases based on model diagnostics

# Create an explicit 'index' column for row numbers
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = row_number())

# Sort by Cook's distance (largest first)
model.diag.metrics_sorted <- model.diag.metrics %>%
  arrange(desc(.cooksd))  

# Select the top 3 rows because those are our outliers
top_3_influential <- model.diag.metrics_sorted %>%
  slice(1:3)

# Extract the 'index' column to get row indices
top_indices <- top_3_influential %>%
  pull(index) 

# all in one 
top_indices_alt <- model.diag.metrics %>%
  mutate(index = row_number()) %>%
  arrange(desc(.cooksd)) %>%
  slice(1:3) %>% 
  pull(index)

# Filter original data (data2) based on the row indices
Outliers <- data2 %>% filter(row_number() %in% top_indices)

# alternative: the numbers in to plots are eqivalent to the number of the rows
test <- data2 %>% 
  slice(262, 293, 416)
# looks good 


# lets exclude the outliers
data3<-data2[data2$IDN != 2919 & data2$IDN != 3524 & data2$IDN != 3628, ] 

# alternative
data3 <- data2 %>% 
  slice(-c(262, 293, 416))

View(data3)
reg3<-lm(com~pi,data3)
summary(reg3)
tab_model(reg2, reg3)

#Alternative to compute model on a subset of the data (e.g. those between the age of 18 and 40)

reg4<-lm(com~pi, data=subset(data2, data2$Alter >= 18 & data2$Alter < 40),na.action = na.exclude)
summary(reg4)
reg4.1<-lm(com~pi, data=subset(data2, data2$Alter >= 18 & data2$Alter < 30),na.action = na.exclude)
summary(reg4.1)

#Now the model on those who are between 40 and 65 years old

reg5<-lm(com~pi, data=subset(data2, data2$Alter >= 50 & data2$Alter < 65),na.action = na.exclude)
summary(reg5)

#Take a look at both models side by side - is there a difference?
#sjPlot is a package we can use to look at regression results (but not as nice as in apatables)
tab_model(reg4, reg4.1,reg5)



#### Task 2 ####
#Task 2: Compute standardized model and compare results with unstandardized model
reg.std<-lm(scale(com)~scale(pi),data2)
summary(reg.std)

tab_model(reg2, reg.std)



#### Task 3 ####
#Task 3: Compare results from standardized and unstandardized regression models with with the correlation
correl<-data2[,c("com","pi")]
apa.cor.table(correl)



#### Task 4 ####
#Task 4: Use apatables to produce a correlation and a regression table
library(apaTables)
#correlation table:
apa.cor.table(correl)



# Task 5 ####
#Task 5: Add gender ("Gesch") and age ("Alter") as additional variables, one by one - this is multiple regression


reg6<-lm(com~pi, data2)
summary(reg6)
reg7<-lm(com~pi + Gesch, data2)
summary(reg7)
reg8<-lm(com~pi + Gesch + Alter, data2)
summary(reg8)

tab_model(reg6, reg7,reg8)

#Print the multiple regression table

library(apaTables)
apa.reg.table(reg6, reg7, reg8, filename = "reg6_7_8.doc")
```

### Session 6: Multiple linear regression 

Tasks:

1. Read the data from the previous practices; assemble the multiple regression table from last practice

2. Investigate a regression model including commitment as the dependent variable as well as personal initiative (pi) and age (Alter) as independent variables. How do (standardized) predictors in a multiple regression model differ from the correlations?

3. Interpret the results of the multiple regression model.

4. Diagnose the multiple regression model – are there outliers and are variance inflation factors in a normal range?


5. We can use regression to find out, which variables are particularly relevant for commitment, for example as part of a consulting project? Add variables to the data (sv, fb, ts, ti, and aut). These variables are frequently investigated in association with work engagement or the motivating potential of a workplace. Abbreviations are: 
  a.	Sv: Subjective value of the work a person does. Example: The tasks I do at work are simple and repetitive.
  b.	Fb: Feedback through work. Example: When working, I can see the quality of my work.
  c.	Ts: Task significance. Example: The way I do my work influences many others.
  d.	Ti: task independence. Example: At work, I can do tasks from start to finish.
  e.	Aut: Autonomy. Example: My work gives me the opportunity to decide by myself how I do my work.

Calculate multiple regression models for the added variables separately and for all of them combined

6.	Investigate confidence intervals for the R²

7.	Select the optimal model based on forward selecting variables into the model

8.  Interpret additional regression models from the literature. You can see the tables below if you connected your folder correctly.

Regression models from the literature. 

Le, H., Oh, I. S., Robbins, S. B., Ilies, R., Holland, E., & Westrick, P. (2011). Too much of a good thing: curvilinear relationships between personality traits and job performance. Journal of Applied Psychology, 96(1), 113.

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Le et al 2011_Correlation Matrix.png)


![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Le et al 2011_Regression Table.png)
Questions on Le et al. (2011): 
-	What are the dependent variables in the different models?
-	Are the different concepts measured as variables or as scales? And are reliabilities OK?
-	Which variables are significant predictors of job performance?
-	Why is there a negative relationship between Conscientiousness and CWB while the relationship is positive with OCB?
-	Is Conscientiousness correlated with Job Complexity? Which information can you use to find out?
-	Does the introduction of the quadratic term of Conscientiousness lead to variance inflation? Why or why not?




```{r Session 6, eval = FALSE, echo = FALSE}

# Session 6: Multiple linear regression



# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("dplyr")){install.packages("dplyr")}; library(dplyr)
if(!require("tidyr")){install.packages("tidyr")}; library(tidyr)
if(!require("car")){install.packages("car")}; library(car)
if(!require("broom")){install.packages("broom")}; library(broom)
if(!require("sjPlot")){install.packages("sjPlot")}; library(sjPlot)
if(!require("apaTables")){install.packages("apaTables")}; library(apaTables)
if(!require("MBESS")){install.packages("MBESS")}; library(MBESS)
if(!require("olsrr")){install.packages("olsrr")}; library(olsrr)



#### Task 1 ####
#Task 1: Read the data from the previous practices; assemble the multiple regression table from last practice

#load the data
data <- read_excel("data_session3.xlsx")


#this practice, we want to build large multiple regression models. 
# Therefore we need cases to be complete for a long list of variables.

data2<-data[complete.cases(data$pi, data$com, data$Alter, data$Gesch, data$sv, data$fb, data$ts,
                           data$ti, data$aut),]

data2 <- data %>% 
  drop_na(pi, com, Alter, Gesch, sv, fb, ts, ti, aut)


#From last session: Build a regression model with personal initiative, gender ("Gesch") and age ("Alter")

simple.reg<-lm(com~pi, data2)
summary(simple.reg)
mult.reg.1<-lm(com~pi + Alter, data2)
summary(mult.reg.1)
mult.reg.2<-lm(com~pi + Alter + Gesch, data2)
summary(mult.reg.2)

#a multiple regression table
apa.reg.table(simple.reg, mult.reg.1, mult.reg.2, filename = "reg_simp_mult1_mult2")



#### Task 2 ####
#Task 2: Investigate how correlations and (standardized) predictors in a multiple regression model differ.
#correlations
correl<-data2[,c("com","pi", "Alter", "Gesch")]
apa.cor.table(correl)


#for comparison: The standardized model
mult.reg.stand<-lm(scale(com)~scale(pi) + scale(Alter) + scale(Gesch),data2)
summary(mult.reg.stand)

tab_model(mult.reg.2, mult.reg.stand)


#if we want to save the file
tab_model(mult.reg.2, mult.reg.stand, file = "regtable.doc")



#### Task 4 #### 
#Task 4: Diagnose the multiple regression model

#Diagnose the model
par(mfrow = c(2, 2))  # to display the four plots in a 2x2 array
plot(simple.reg)      # this produces the four plots for "simple.reg"
plot(mult.reg.2)      # this produces the four plots for "mult.reg"


#investigate variance inflation factors (VIF)
?vif                  # this shows the "vif" function from the car package
vif(mult.reg.2)


#We now want to work with more variables.



#### Task 5 ####
#Task 5. Calculate multiple multiple regression models

#starting point is mult.reg.2, our 'baseline model'
baseline<-lm(com~pi + Alter + Gesch, data2)
summary(baseline)

#models with added predictors
baseline.sv<-lm(com~pi + Alter + Gesch + sv, data2)
summary(baseline.sv)

baseline.fb<-lm(com~pi + Alter + Gesch + fb, data2)
summary(baseline.fb)

baseline.ts<-lm(com~pi + Alter + Gesch + ts, data2)
summary(baseline.ts)

baseline.ti<-lm(com~pi + Alter + Gesch + ti, data2)
summary(baseline.ti)

baseline.aut<-lm(com~pi + Alter + Gesch + aut, data2)
summary(baseline.aut)

#Which variable do you consider most useful for our regression model?

#end point is a model with all variables, the 'full model'
full<-lm(com~pi + Alter + Gesch + sv + fb + ts + ti + aut,data2,na.action = na.exclude)

plot(full)

tab_model(baseline, baseline.sv, baseline.fb, baseline.ti, baseline.ts, baseline.aut, full , auto.label = FALSE)



#### Task 6 ####
#Task 6: Calculate confidence intervals for the R²

#select models for the regression table
apa.reg.table(baseline, full, filename = "newtable")


#alternative to calculate confidence intervals (MBESS):
ci.R2(R2 = .2196, df.1 = 9, df.2 = 432, conf.level =.95,Random.Predictors=FALSE)



#### Task 7 ####
#Task 7: Automated selection of variables into the model

#Comparing all possible models (olsrr):

?olsrr

ols_step_all_possible(baseline)
ols_step_all_possible(full)
#Automatic forward selection of variables
ols_step_forward_p(full)

ols_step_best_subset(full)



```

### Session 7: Dichotomous and nominal data in regression

1.	Run simple linear regression on commitment with a dichotomous independent variable (gender) and compare this result from simple linear regression with the difference between the means for commitment for women and for men.

2.	Add a few variables (e.g. personal initiative (pi) or age) and see, how this changes the regression weights for gender in relation to the mean difference.

3.	Use ‘Abteilung’, which is a nominal variable (use ‘as.factor’), and interpret results for the different roles within a business. Also, take a look at the data table to see, how ‘Abteilung’ is entered in the data table.

4.	Run a logistic regression model with production employee (Yes/no) as a dependent variable (glm). Interpret results.

5.  Interpret results from the literature


Pogacar, R., Angle, J., Lowrey, T. M., Shrum, L. J., & Kardes, F. R. (2021). Is Nestlé a lady? The feminine brand name advantage. Journal of Marketing, 85(6), 101-117.

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Pogacar et al. 2021_Regression Tables.png)


Questions on Pogacar et al. (2021): 

-	What are the dependent variables in the different models? 
-	What is the difference between the dependent variable in the above model versus the below model?
-	Which variables are significant predictors of the different dependent variables? 
-	What is the predictor the authors are most interested in in this analysis and which variables are ‘control variables’?
-	How would you interpret the results of regression modeling in light of the title of the article?


```{r Session 7, eval = FALSE, echo = FALSE}

# Session 7: Dichotomous and nominal data in regression



# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("apaTables")){install.packages("apaTables")}; library(apaTables)
if(!require("dplyr")){install.packages("dplyr")}; library(dplyr)
if(!require("ggplot2")){install.packages("ggplot2")}; library(ggplot2)
if(!require("car")){install.packages("car")}; library(car)
if(!require("sjPlot")){install.packages("sjPlot")}; library(sjPlot)



#load the data
data <- read_excel("data_session3.xlsx")


#### Task 1 ####
#Task 1: Regression with a dichotomous IV

#Correlations
correl<-data[,c("Alter" ,"Gesch", "com", "pi")]
apa.cor.table(correl)


#Use gender as independent variable
reg2<-lm(com~Gesch,data,na.action = na.exclude)
summary(reg2)


#Check the model diagnostics
par(mfrow = c(2, 2))
plot(reg2)



#Compare the b with the difference in means
options(pillar.sigfig=5) 
#to see what I want you to see, we need to see more digits after the comma
group_by(data, Gesch) %>%
  summarise(
    count = n(),
    mean = mean(com, na.rm = TRUE),
    sd = sd(com, na.rm = TRUE)
  )

# this adds a column to the output which already contains the difference in means 
data %>%
  group_by(Gesch) %>%
  summarise(
    count = n(),
    mean = mean(com, na.rm = TRUE),
    sd = sd(com, na.rm = TRUE)
  ) %>%
  filter(!is.na(Gesch)) %>% 
  mutate(mean_diff = mean[Gesch == 1] - mean[Gesch == 2])



#### Task 2 ####
#Task 2 
#Multiple regression with the dichotomous variable
mult.reg<-lm(com~pi + Alter + Gesch ,data,na.action = na.exclude)
summary(mult.reg)



#### Task 3 ####
# Task 3 Use 'Abteilung', which is a nominal variable

# Labels:
# value label
# 1 Produktion
# 2 Verwaltung
# 3 Forschung und Entwicklung
# 5 Personal
# 6 Rechnungswesen und Controlling
# 7 Marketing und Vertrieb
# 8 Logistik und Materialwirtschaft
# 9 Anderes


#Create a barplot to see the frequencies of the different Labels:
data$Abteilung <- haven::zap_labels(data$Abteilung)
barplot(table(data$Abteilung), ylab = "Freq")

# alternative with ggplot 
ggplot(data, aes(Abteilung)) +
  geom_bar()


# table of commitment scores by Abteilung
group_by(data, Abteilung) %>%
  summarise(
    count = n(),
    mean = mean(com, na.rm = TRUE)
  )

# Abteilung is a character vector and not a factor vector
# so we need to change that
data$Abteilung <- factor(data$Abteilung)

# now we can relevel
# releveling determines what all other roles are compared to
data$Abteilung_lvld_1 <- relevel(data$Abteilung, "1")

mult.reg.factor1<-lm(com~Abteilung_lvld_1 + Alter + Gesch ,data, na.action = na.exclude)
summary(mult.reg.factor1)


#If we wanted to compare everything with, 'Verwaltung' instead, we could change the leveling:
# Use code '2' as the reference category
data$Abteilung_lvld_2 <- relevel(data$Abteilung, "2")


#Last, rerun the regression
mult.reg.factor2<-lm(com~Abteilung_lvld_2 + Alter + Gesch  ,data,na.action = na.exclude)
summary(mult.reg.factor2)

#Again, investigate model diagnostics
par(mfrow = c(2, 2))
plot(mult.reg.factor2)

tab_model(mult.reg.factor1, mult.reg.factor2, auto.label = TRUE)



#### Task 4 ####
#Task 4


#Use a dichotomous variable as a dependent variable?
#Change a variable to produce a dichotomous variable

data$abt <- haven::zap_labels(data$Abteilung) 
#remove the labels in order to continue with the next command


data$prod <- car::recode(data$abt, "1 = 'yes'; 2:9 = 'no'; else = NA")


# alternative
data$prod_alt1 <- data$abt %>% 
  as.numeric() %>% 
  case_match(1 ~ "yes", 2:9 ~ "no")


# Check the scatter plot to see relations of the new variable with com and pi
plot(data$com, data$prod)
abline(lm(data$prod~data$com),col = "green")

plot(data$pi, data$prod)
abline(lm(data$prod~data$pi),col = "green")


#Run the GLM model
glm.model1<-glm(prod~com + pi, data, na.action=na.exclude,family = "binomial")
summary(glm.model1)

# you can add additional variables to the glm from last practice (workplace-related).
glm.model2<-glm(prod~com + pi + sv + fb + ts + ti + aut, data, na.action=na.exclude,family = "binomial")
summary(glm.model2)


#for easier interpretation, get odds ratios for the independent variables
exp(cbind(OR = coef(glm.model2), confint(glm.model2)))


#Alternative: Use SjPlot
sjPlot::tab_model(glm.model1, glm.model2, auto.label = FALSE)

#Even odds ratios are often a bit tricky to interpret. Some support can be found online, for example here:
#https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
```

### Session 8: Moderation and nonlinearity

Task 1:	Investigate direct effects of variables of interest, for example personal initiative, autonomy and task significance as predictors of commitment

Task 2:	Analyze if there exists an interaction effect between the independent variables 

Task 3:	Investigate the interactive relationship graphically

This is what you need for analyzing moderation in the term paper. Task 4 and task 5 are a bit more complex and go beyond what is needed for the term paper.

Task 4:	Investigate a third variable in the relationship of personal initiative and commitment, in the example I used task significance and task independence

Task 5:	Investigate a nonlinear relationship


```{r Session 8, eval = FALSE, echo = FALSE}

# Session 8: Moderation and nonlinearity



# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("apaTables")){install.packages("apaTables")}; library(apaTables)
if(!require("interactions")){install.packages("interactions")}; library(interactions)
if(!require("sandwich")){install.packages("sandwich")}; library(sandwich)
if(!require("ggplot2")){install.packages("ggplot2")}; library(ggplot2)



#load the data
data <- read_excel("data_session3.xlsx")



#### Task 1 ####
#Task 1: Investigate direct effects of variables of interest, for example personal initiative, 
#        autonomy and task significance as predictors of commitment

mult.reg<-lm(com~Alter + Gesch + pi + aut + ts ,data,na.action = na.exclude)
summary(mult.reg)



#### Task 2 ####
#Task 2: Assume there may be an interaction between personal initiative and the two workplace-related indicators 
#        in predicting commitment

mult.reg.mod1 <- lm(com~Alter + Gesch + pi + aut + ts + pi*ts ,data,na.action = na.exclude)
summary(mult.reg.mod1)
mult.reg.mod2 <- lm(com~Alter + Gesch + pi + aut + ts + pi*aut ,data,na.action = na.exclude)
summary(mult.reg.mod2)

apa.reg.table(mult.reg, mult.reg.mod2, filename = "moderation")



#### Task 3 ####
#Task 3: Investigate the interaction effect of personal initiative and task significance

## interactions ##

interact_plot(
  mult.reg.mod1,
  pred = pi,
  modx = ts,
  x.label = "Personal initiative",
  y.label = "Commitment",
  legend.main = "Task significance")

interact_plot(
  mult.reg.mod2,
  pred = pi,
  modx = aut,
  x.label = "Personal initiative",
  y.label = "Commitment",
  legend.main = "Autonomy")


interact_plot(mult.reg.mod1,
  pred = pi,
  modx = ts,
  plot.points = TRUE)

interact_plot(mult.reg.mod1,
  pred = pi,
  modx = ts,
  interval = TRUE,
  int.width = 0.8)


## sandwich ##

# simple slope analyses
sim_slopes(mult.reg.mod1, pred = pi, modx = ts, johnson_neyman = FALSE)
sim_slopes(mult.reg.mod2, pred = pi, modx = aut, johnson_neyman = FALSE)
sim_slopes(mult.reg.mod1, pred = pi, modx = ts, johnson_neyman = TRUE)
johnson_neyman(mult.reg.mod1, pred = pi, modx = ts, alpha = .05)


#Work with the code: Change the input to investigate the interaction of personal initiative and autonomy instead.

#This is what you need for analyzing moderation in the term paper. Task 4 and task 5 are a bit more complex and go beyond what is needed for the term paper.



#### Task 4 ####
#Task 4: Investigate a three-way interaction effect

#Step 1
mult.reg.mod3<-lm(com~Alter + Gesch + pi + ti + ts ,data,na.action = na.exclude)
summary(mult.reg.mod3)

#Step 2
# long 
mult.reg.mod4_long <- lm(com ~ Alter + Gesch + pi + ti + ts + pi:ti + pi:ts + ti:ts,data,na.action = na.exclude)
summary(mult.reg.mod4_long)
# short 
mult.reg.mod4_short <- lm(com ~ Alter + Gesch + pi*ti + pi*ts + ti*ts,data,na.action = na.exclude)
summary(mult.reg.mod4_short)

# pi - main effect
# pi:ti - interaction effect
# pi*ti = pi, pi:ti - main effect and interaction effect 
# hence, short and long model are the same 

#Step 3
mult.reg.mod5<-lm(com~Alter + Gesch + pi*ti*ts ,data,na.action = na.exclude)
summary(mult.reg.mod5)

# pi*ti*ts = pi, ti, ts, pi:ti, pi:ts, ti:ts, pi:ti:ts
# includes all main and all interaction effects


#Plot the interaction effect
interact_plot(mult.reg.mod5, pred = pi, modx = ti, mod2 = ts, interval = FALSE, int.width = 0.8)
#Produce the table for the three models
apa.reg.table(mult.reg.mod3, mult.reg.mod4_long, mult.reg.mod5, filename = "threewayinteraction.")



#### Task 5 ####
#Task 5: Investigate non-linear relationships

#Autonomy as a predictor of commitment
reg1 <- lm(com ~ aut, data, na.action = na.exclude)
summary(reg1)

poly.reg1<-lm(com~aut + I(aut^2) ,data,na.action = na.exclude)
summary(poly.reg1)


#Investigate the plot
ggplot(data, aes(aut, com) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

#Add a higher-order polynom
poly.reg2 <- lm(com~poly(aut, 3, raw=TRUE) ,data,na.action = na.exclude)
summary(poly.reg2)

#Investigate the plot
ggplot(data, aes(aut, com) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE))


```

### Session 9: Mediation and process macro

Tasks:

In this practice session, we want to investigate this model:

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Mediation Model.png)
 
Predictor is Personal Initiative (pi). 
Mediators are Feedback (fb) and Autonomy (aut). 
The outcome is Commitment (com)

Task 1:	Use the regression based approach to test mediation (Baron & Kenny method).

Task 2:	Use the process macro to test mediation.

Download the process macro: https://haskayne.ucalgary.ca/CCRAM/resource-hub

Open process macro in R:  
Run process macro in R:

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Run Process Macro.png)

This may take a few minutes.

Select the correct model from the manual. With two mediators but no additional paths, the correct model is Model 4:

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Model 4.png)
 

Think about an alternative model and try to test it

Age could be a moderator in the relationship between PI and the mediators
This could be the theoretical model (Model 8):

![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Model 8.png)

This is what you need for analyzing mediation in the term paper. Task 3 and task 4 are a bit more complex and go beyond what is needed for the term paper.

Task 3:	Use an SEM to test the path model.

Information on lavaan: https://lavaan.ugent.be/index.html

Task 4:	Interpret the results of the different approaches. 

Please note that coefficients may not be exactly equal across all methods. For a summary on the different estimation methods, see for example:   MacKinnon, D. P., Lockwood, C. M., Hoffman, J. M., West, S. G., & Sheets, V. (2002). A comparison of methods to test mediation and other intervening variable effects. Psychological methods, 7(1), 83-104.


```{r Session 9, eval = FALSE, echo = FALSE}

# Session 9: Mediation and process macro



# setting your working directory 
setwd("A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
# loading packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("apaTables")){install.packages("apaTables")}; library(apaTables)
if(!require("lavaan")){install.packages("lavaan")}; library(lavaan)
if(!require("ggplot2")){install.packages("ggplot2")}; library(ggplot2)



#load the data
data <- read_excel("data_session3.xlsx")



#### Task 1####
#Task 1. The regression based approach - a series of linear regressions

mult.reg.m1<-lm(com~pi ,data,na.action = na.exclude)
summary(mult.reg.m1)

mult.reg.m2.a<-lm(aut~pi ,data,na.action = na.exclude)
summary(mult.reg.m2.a)

mult.reg.m2.b<-lm(fb~pi ,data,na.action = na.exclude)
summary(mult.reg.m2.b)

mult.reg.m3<-lm(com~aut + fb + pi ,data,na.action = na.exclude)
summary(mult.reg.m3)



#### Task 2 ####
#Task 2. Testing mediation using the process macro

#Select the correct model from the manual. With two mediators but no additional paths, the correct model is Model 4:
process(data = data, y = "com", x = "pi", m = c("fb", "aut"), model = 4)

#Think about an alternative model and try to test it
#Age could be a moderator in the relationship between PI and the mediators
#This could be the theoretical model (Model 8):
process (data = data, y = "com", x = "pi",w = "aut", model = 1)

# This is what you need for analyzing mediation in the term paper. 
# Task 3 and task 4 are a bit more complex and go beyond what is needed for the term paper.



#### Task 3 ####
#Task 3. Testing mediation using the structural equation modeling approach


#Specify the mediation model in lavaan
#Signs ‘ signal the beginning and the end of model specification

med.model <- '
#relationship between com and pi, we call the relationship ‘c’.
#com ~ c*pi

#relationship between com and aut/ts, we call the relationships ‘b1’ and ‘b2’.
com ~ b1*aut + b2*fb

#relationships between pi and the mediators, we call them ‘a1’ and ‘a2’.
aut ~ a1*pi
fb ~ a2*pi

#Indirect effects
indei.aut := a1*b1
indei.fb := a2*b2
#Total effect:
# totei:= indei.aut + indei.fb + c
'
#Test the model versus the data
fit <- sem(med.model, data = data)
summary(fit)
#Investigate model fit
fitMeasures(fit)
modindices(fit)

#Investigate the correlation matrix
cormatrix<-lavInspect(fit, "cor.all")
View(cormatrix)



#### Task 4 ####
#Task 4. Interpret the results of the different approaches.



```

### Session 10: Structural equation modeling with lavaan

Tasks:

In this practice session (again), we want to investigate this model:


![](A:/Module und Vorlesungen/Modul Quantitative Methods SS 2025/Practice/Images/Mediation Model.png)

 
Predictor is Personal Initiative (ei). 
Mediators are Feedback (fb) and Autonomy (aut). 
The outcome is Commitment (com)
This time, we want to build a latent variable model/SEM
•	Step 1: The measurement model
•	Step 2: The structural model
•	Step 3: Investigate if this is mediation or partial mediation


```{r Session 10, eval = FALSE, echo = FALSE}

# Session 10: Structure equation modeling with lavaan



# setting your working directory 
setwd("Z:/Lehre/Experimentelles Praktikum/SoSe 25_Quantitative Methoden")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("lavaan")){install.packages("lavaan")}; library(lavaan)



#load the data
data <- read_excel("data_session3.xlsx")


#Specify the mediation model in lavaan



#### Step 1####
#Step 1: The measurement model

med.model.lat.meas <- '

#Measurement model: How are the latent variables defined?
#No need to worry about recoded items any more - the model can take care of that

eiL =~ ei1 + ei2 + ei3 + ei4 + ei5 
comL =~ com_1 + com_2 + com_3r + com_4 + com_5
autL =~ jd_16_aut1 + jd_17_aut2_r 
fbL =~ jd_13_fb1 + jd_18_fb2_r

#DV on IV

#DV on mediator

#Mediator on IV

#Indirect effects

#Total effects

'

fit.meas <- sem(med.model.lat.meas, data = data, std.lv=FALSE, meanstructure = TRUE )

#the model summary
summary(fit.meas)

#the assessment of model fit
fitMeasures(fit.meas)

#additional information, for example the standardized results
standardizedsolution(fit.meas)

#We can produce a number of additional outputs from the model. See: https://lavaan.ugent.be/tutorial/inspect.html

#The difference between model implied and observed variance/covariance-matrices (the residuals) 
# can be obtained using this command:
residuals(fit.meas)

#We can investigate Items that show large residuals and, if fit was not ideal, try to exclude them. 
# In this case, model fit is good - you can try adding more variables and see if fit changes


#If fit is not sufficient, we can also investigate modification indices.
modindices(fit.meas)
#mi[mi$op == "~~",]
#Modification indices indicate paths that are currently not specified in the model but that would reduce misfit if they were.



#### Step 2 #### 
#Step 2: The structural model

med.model.lat.str <- '

#Measurement model: How are the latent variables defined?
#No need to worry about recoded items any more - the model can take care of that

eiL =~ ei1 + ei2 + ei3 + ei4 + ei5
comL =~ com_1 + com_2 + com_3r + com_4 + com_5
autL =~ jd_16_aut1 + jd_17_aut2_r 
fbL =~ jd_13_fb1 + jd_18_fb2_r


#DV on IV

comL ~ c*eiL

#DV on mediator

comL ~ b1*autL + b2*fbL

#Mediator on IV

autL ~ a1*eiL
fbL ~ a2*eiL

#Indirect effects

indei.aut := a1*b1
indei.fb := a2*b2

#Total effects

totei := indei.aut + indei.fb + c

'

fit.str <- sem(med.model.lat.str, data = data, std.lv=FALSE, meanstructure = TRUE )

summary(fit.str)
#please note that we see negative regression weights now where weights previously were positive. 
# This relates to the anchor-item in the measurement model.

fitMeasures(fit.str)
#fit is still good.



#### Step 3 ####
#Step 3: Investigate if this is mediation or partial mediation


#we need to remove the direct effect

med.model.lat.str.nodir <- '

#Measurement model: How are the latent variables defined?
#No need to worry about recoded items any more - the model can take care of that

eiL =~ ei1 + ei2 + ei3 + ei4 + ei5 
comL =~ com_1 + com_2 + com_3r + com_4 + com_5
autL =~ jd_16_aut1 + jd_17_aut2_r 
fbL =~ jd_13_fb1 + jd_18_fb2_r


#DV on IV

#comL ~ c*eiL --> Direct effect removed!

#DV on mediator

comL ~ b1*autL + b2*fbL

#Mediator on IV

autL ~ a1*eiL
fbL ~ a2*eiL

#Indirect effects

indei.aut := a1*b1
indei.fb := a2*b2

#Total effects

#totei := indei.aut + indei.fb + c #--> as we dont estimate the direct path c, we cannot calculate this


'

fit.str.nodir <- sem(med.model.lat.str.nodir, data = data, std.lv=FALSE, meanstructure = TRUE )

summary(fit.str.nodir)
#please note that we see negative regression weights now where weights previously were positive. 
# This relates to the anchor-item in the measurement model.

fitMeasures(fit.str.nodir)

#compare both models to assess the change in Chi² - is it full or partial mediation?
anova(fit.str, fit.str.nodir)


```


### Session 11: Multilevel modeling with the multilevel package - a primer

You can find an excellent overview on multilevel modeling here:
https://cran.r-project.org/doc/contrib/Bliese_Multilevel.pdf



```{r Session 11, eval = FALSE, echo = FALSE}

# Session 11: Multilevel modeling with the multilevel package - a primer



# setting your working directory 
setwd("Z:/Lehre/Experimentelles Praktikum/SoSe 25_Quantitative Methoden")

# this clears the environment (any loaded or created data(frames)) so we can start fresh
rm(list = ls())

# loading packages
if(!require("readxl")){install.packages("readxl")}; library(readxl)
if(!require("multilevel")){install.packages("multilevel")}; library(multilevel)
if(!require("lattice")){install.packages("lattice")}; library(lattice)
if(!require("sjPlot")){install.packages("sjPlot")}; library(sjPlot)



#load the data
data <- read_excel("data_session3.xlsx")



#Multilevel modeling: Company (Unt) identifies the individuals who are nested within different companies

#Look at an item that way explicitly asked for the organization (referent shift consensus model)

data$fk<-(data$fk_1 + data$fk_2 + data$fk_4 + data$fk_5 + data$fk_7 + data$fk_8)/6

#to simplify things, lets use complete cases only
data2<-data[complete.cases(data$pi, data$com, data$fk),]



#take a look at how much variation 'resides' at the organization level

#For personal initiative
icc_calculation_pi<-aov(pi~as.factor(Unt), data2)
ICC1(icc_calculation_pi)

#for commitment
icc_calculation_com<-aov(com~as.factor(Unt), data2)
ICC1(icc_calculation_com)

#for error management culture
icc_calculation_fk<-aov(fk~as.factor(Unt), data2)
ICC1(icc_calculation_fk)

#--> It the ICC1 is .12, indicating that 12% of the variation between individual data points
# is due to variation between businesses

#Again, our model is to explain commitment via two variables, personal initiative and error management culture, 
# which reside at different levels

#Step 1: We can test, whether a model which takes the multilevel data structure into account 
# fits the data significantly better than a model without the multilevel data structure:

model_no_mult<-gls(com~1, data2, control = list(opt="optim"), na.action = na.exclude)

model_mult<-lme(com~1, random=~1|Unt, data2, control = list(opt="optim"), na.action = na.exclude)

anova(model_no_mult, model_mult)

#--> the model is slightly better if we take the company level into account


#Investigating cohesiveness among individuals for the company level variable error management culture
rwg.j_fk<-rwg.j(data2[,c("fk_1", "fk_2", "fk_4", "fk_5", "fk_7", "fk_8")], data2$Unt, ranvar = 2)

summary(rwg.j_fk)
rwg.j_fk


#how does the relation of the lower level variables look like?

xyplot(
  com ~ pi |
    as.factor(Unt),
  data = data2,
  type = c("p", "g", "r"),
  col = "dark blue",
  col.line = "black",
  xlab = "Personal Initiative",
  ylab = "Commitment"
)

#lets see whether we can explain more variation in the dependent variable if we use a multilevel model


## the single level model##

lm.model<-lm(com~pi + fk, data2, na.action = na.exclude)
summary(lm.model)


## the multilevel model ##

#aggregate error management culture to the organizational level
untdata<-aggregate(data2[,c("fk")],list(data2$Unt),mean, na.rm=T)

#combine comapany data with individual data
names(untdata)<-c("Unt","fk_unt")

data3 = merge(data2,untdata, by=c("Unt"))

#some issue with data$pi - hence I renamed it
data3$pi_pers<-data3$pi


#Finally: Modelling - first, random intercept models - the intercept can vary randomly between companies
lme.model.1<-lme(com~pi_pers, random=~1|Unt, data3, control = list(opt="optim"), na.action = na.exclude)

VarCorr(lme.model.1)

lme.model.2<-lme(com~pi_pers + fk_unt, random=~1|Unt, data3, control = list(opt="optim"), na.action = na.exclude)

VarCorr(lme.model.2)
#--> less 'leftover' variance for the intercept. Hence, some variation is explained by fk_unt

library(sjPlot)

tab_model(lme.model.1, lme.model.2)

#what is a marginal and conditional R²?
# Marginal R2 and describes the proportion of variance explained by the fixed factor(s) alone;
# Conditional R2, which describes the proportion of variance explained by both the fixed and random factors

#could there be an interaction effect?
lme.model.3<-lme(com~pi_pers + fk_unt + pi_pers*fk_unt, random=~1|Unt, data3, control = list(opt="optim"), na.action = na.exclude)
tab_model(lme.model.1, lme.model.2, lme.model.3)


#we can also test if the fk_unt can explain slope differences between companies
lme.model.4<-lme(com~pi_pers + fk_unt, random=~1 + pi_pers|Unt, data3, control = list(opt="optim"), na.action = na.exclude)
VarCorr(lme.model.4)
tab_model(lme.model.1, lme.model.2, lme.model.4)


lme.model.5<-lme(com~pi_pers * fk_unt, random=~1 + pi_pers|Unt, data3, control = list(opt="optim"), na.action = na.exclude)
VarCorr(lme.model.5)
tab_model(lme.model.1, lme.model.2, lme.model.4, lme.model.5)


```

