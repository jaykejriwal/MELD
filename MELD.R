#Import csv file
dat = read.csv('D:\\D drive\\MELD corpus\\output\\MELD_Dyadic_ap_pairdistance.csv',sep='\t')
dat$col3= (dat$Emotion==dat$Sentiment)
table(dat['col3'])
#1362 entries have different classes when compared to 8937

library(palmerpenguins)
library(ggstatsplot)
library(tibble)
library(ggpubr)
str(dat)
dat$Emotion <- as.factor(dat$Emotion)  
x <- which(names(dat) == "Emotion") # name of grouping variable
y <- which(names(dat) == "Mean.Pitch.distance" # names of variables to test
|names(dat) == "Min.Pitch.distance" |
 names(dat) == "Max.Pitch.distance" |
 names(dat) == "Mean.Intensity.distance" |
  names(dat) == "Min.Intensity.distance" |
  names(dat) == "Max.Intensity.distance" |
  names(dat) == "Jitter.distance" |
  names(dat) == "Shimmer.distance" |
  names(dat) == "Mean.Nhr.distance" |
  names(dat) == "Speechrate.distance")
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("positive-negative", "negative-positive"), c("positive-neutral", "neutral-positive"), c("neutral-negative", "negative-neutral"),c("negative-negative", "positive-positive")) # comparisons for post-hoc tests
# Edit until here


# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(dat,
      x = colnames(dat[j]), y = colnames(dat[i]),
      color = colnames(dat[j]),
      legend = "none",
      palette = "npg",
      add = "jitter"
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
        method = method1, label.y = max(dat[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))# remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}

#######################

data = read.csv('D:\\D drive\\MELD corpus\\output\\MELD_Dyadic_ap_pairdistance1.csv')
dat <- subset(data, Emotion == "neutral-positive"|Emotion == "neutral-negative")
dat$Emotion <- factor(dat$Emotion)
str(dat)
for (i in 6:13) { # variables to compare are variables 1 to 4
  boxplot(dat[, i] ~ dat$Emotion, # draw boxplots by group
          ylab = names(dat[i]), # rename y-axis with variable's name
          xlab = "Emotion"
  )
  print(t.test(dat[, i] ~ dat$Emotion)) # print results of t-test
}

x <- which(names(dat) == "Emotion") # name of grouping variable
y <- which(names(dat) == "Mean.Pitch.distance" # names of variables to test
           |
             names(dat) == "Max.Pitch.distance" |
             names(dat) == "Mean.Intensity.distance" |
             names(dat) == "Max.Intensity.distance" |
             names(dat) == "Jitter.distance" |
             names(dat) == "Shimmer.distance" |
             names(dat) == "Mean.Nhr.distance" |
             names(dat) == "Speechrate.distance")
method <- "t.test" # one of "wilcox.test" or "t.test"
paired <- FALSE # if paired make sure that in the dataframe you have first all individuals at T1, then all individuals again at T2
# Edit until here


# Edit at your own risk
for (i in y) {
  for (j in x) {
    ifelse(paired == TRUE,
           p <- ggpaired(dat,
                         x = colnames(dat[j]), y = colnames(dat[i]),
                         color = colnames(dat[j]), line.color = "gray", line.size = 0.4,
                         palette = "npg",
                         legend = "none",
                         xlab = colnames(dat[j]),
                         ylab = colnames(dat[i]),
                         add = "jitter"
           ),
           p <- ggboxplot(dat,
                          x = colnames(dat[j]), y = colnames(dat[i]),
                          color = colnames(dat[j]),
                          palette = "npg",
                          legend = "none",
                          add = "jitter"
           )
    )
    #  Add p-value
    print(p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                                 method = method,
                                 paired = paired,
                                 # group.by = NULL,
                                 ref.group = NULL
    ))
  }
}
###########25-04
#dat = read.csv('D:\\D drive\\MELD corpus\\output\\MELD_Dyadic_ap_pairdistance.csv',sep='\t')
#dat$emotion_type <- ifelse(dat$Emotion=='positive-positive' | dat$Emotion=='neutral-neutral'| dat$Emotion=='negative-negative', "same", "different")

dat = read.csv('D:\\D drive\\MELD corpus\\output\\MELD_Dyadic_ap_pairdistance.csv',sep='\t')
dat$col3= (dat$Emotion==dat$Emotion)
table(dat['col3'])
#1362 entries have different classes when compared to 8937
library(palmerpenguins)
library(ggstatsplot)
library(tibble)
library(ggpubr)
str(dat)
dat$Emotion <- as.factor(dat$Emotion)  
x <- which(names(dat) == "Emotion") # name of grouping variable
y <- which(names(dat) == "Mean.Pitch.distance" # names of variables to test
           |names(dat) == "Min.Pitch.distance" |
             names(dat) == "Max.Pitch.distance" |
             names(dat) == "Mean.Intensity.distance" |
             names(dat) == "Min.Intensity.distance" |
             names(dat) == "Max.Intensity.distance" |
             names(dat) == "Jitter.distance" |
             names(dat) == "Shimmer.distance" |
             names(dat) == "Mean.Nhr.distance" |
             names(dat) == "Speechrate.distance")
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("positive-positive", "negative-negative"), c("positive-positive", "neutral-neutral"), c("negative-negative", "neutral-neutral")) # comparisons for post-hoc tests
# Edit until here


# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(dat,
                   x = colnames(dat[j]), y = colnames(dat[i]),
                   color = colnames(dat[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter"
    )
    print(
      p + stat_compare_means(aes(label = paste0(after_stat(method), ", p-value = ", after_stat(p.format))),
                             method = method1, label.y = max(dat[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))# remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}

#two.way <- aov(Emotion ~ Mean.Pitch.distance + Min.Pitch.distance, data = dat)
data = read.csv('D:\\D drive\\MELD corpus\\output\\MELD_Dyadic_ap_pairdistance.csv',sep='\t')
dat <- subset(data, Emotion == "positive-positive"|Emotion == "neutral-neutral") #|Emotion =="negative-negative" 
dat$Emotion <- factor(dat$Emotion)
str(dat)
for (i in 6:15) { # variables to compare are variables 1 to 4
  boxplot(dat[, i] ~ dat$Emotion, # draw boxplots by group
          ylab = names(dat[i]), # rename y-axis with variable's name
          xlab = "Emotion"
  )
  print(t.test(dat[, i] ~ dat$Emotion)) # print results of t-test
}
#Pos-pos and neg-neg no diff in means
#Pos-pos and neu-neu no diff in means
#neg-neg and neu-neu no diff in means
############

data = read.csv('D:\\D drive\\MELD corpus\\output\\MELD_Dyadic_ap_pairdistance_abs.csv',sep='\t')
dat <- subset(data, Emotion == "positive-positive"|Emotion == "neutral-neutral"|Emotion == "negative-negative")
library(palmerpenguins)
str(dat)
dat$Emotion <- as.factor(dat$Emotion)
library(ggstatsplot)
library(tibble)
x<-"Emotion"
cols <- 6:15
type <- "parametric" # given the large number of observations, we use the parametric version
paired <- FALSE 
plotlist <-
  purrr::pmap(
    .l = list(
      data = list(as_tibble(dat)),
      x = x,
      y = as.list(colnames(dat)[cols]),
      plot.type = "box", # for boxplot
      type = type, # parametric or nonparametric
      pairwise.comparisons = TRUE, # to run post-hoc tests if more than 2 groups
      pairwise.display = "significant", # show only significant differences
      bf.message = FALSE, # remove message about Bayes Factor
      centrality.plotting = FALSE # remove central measure
    ),
    .f = ifelse(paired, # automatically use ggwithinstats if paired samples, ggbetweenstats otherwise
                ggstatsplot::ggwithinstats,
                ggstatsplot::ggbetweenstats
    )
  )

# print all plots together with statistical results
for (i in 1:length(plotlist)) {
  print(plotlist[[i]])
}
