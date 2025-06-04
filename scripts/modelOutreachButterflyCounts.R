library(tidyverse)
library(splines)
library(pracma)
library(googlesheets4)

### read in data from google sheet
df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1XUPfa1KrvSBAnLsmHMcQ1Ckwxdm_3mgCtlzyDfYk1Os/edit?gid=0#gid=0")

#model fit to all data and data from the first five individuals
m_old <- glm(Count ~ sigmoid(Effort), data = df[1:5,], family = "poisson")
m_new <- glm(Count ~ sigmoid(Effort), data = df, family = "poisson")

### prediction at 100 minutes for both first five individuals and with all observers
oldPrediction <- predict.lm(m_old, data.frame(Effort = 100), interval = 'confidence')
newPrediction <- predict.lm(m_new, data.frame(Effort = 100), interval = 'confidence')


# plot butterfly estimates with confidence interval for prediciton with only
# 5 observers and with 25 observers
ggplot() +
  geom_point(mapping = aes(x = 5, y = exp(oldPrediction[1]))) +
  geom_errorbar(mapping = aes(x = 5,
                              y = exp(oldPrediction[1]), 
                              ymin = exp(oldPrediction[2]), 
                              ymax = exp(oldPrediction[3]))) +
  geom_point(mapping = aes(x = nrow(df), y = exp(newPrediction[1])),
             color = "Blue") +
  geom_errorbar(mapping = aes(x = nrow(df),
                              y = exp(newPrediction[1]), 
                              ymin = exp(newPrediction[2]), 
                              ymax = exp(newPrediction[3])),
                color = "Blue") +
  labs(x = "Number of Counters", y = "Butterfly Estimate") +
  theme_classic() +
  theme(text = element_text(size=rel(5)))

ggsave(filename = "modelOutputFigure/MorePreciseEstiamte.png",
       width = 5, height = 3, dpi = 444)
        