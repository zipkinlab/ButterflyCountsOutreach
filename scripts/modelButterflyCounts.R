library(tidyverse)
library(splines)


# Model counts given effort
n <- 50
trueNumber <- 80
effort <- runif(n, min = 0.1, max = 6)

# effort <- 1:50

count <- trueNumber / (1+exp(-effort))
# Add noise
count <- count * rnorm(n, mean = 1, sd = 0.1)

plot(effort, count)

library(pracma)

m <- glm(count ~ sigmoid(effort), family = "poisson")

predict(m, data.frame(effort = 1:7)) %>% exp(.)

plot(1:7, exp(predict(m, data.frame(effort = 1:7)) ))


# Model with increasing number of users -----------------------------------

i <- 10
myResults <- lapply(3:n, function(i) {
  m_i <- glm(count[1:i] ~ sigmoid(effort[1:i]), family = "poisson")
  
  # Prediction at 100 minuts:
  est_i <- predict(m_i, data.frame(effort = 100), se = T)
  
  out <- predict.lm(m_i,  data.frame(effort = 100), interval = "confidence") %>% 
    as.data.frame() %>% 
    {.[1,]} %>% 
    data.frame(n = i, .)
  
  # out <- confint(m_i) %>% 
  #   {data.frame(., param = row.names(.))} %>% 
  #   pivot_wider(names_from = param, values_from = 1:2) %>% 
  #   data.frame(n = i, est_i, .)
  return(out)
}) %>% 
  bind_rows()


ggplot(myResults) +
  geom_ribbon(aes(x = n, ymin = exp(lwr), ymax = exp(upr)), alpha = 0.5, color = NA) +
  geom_path(aes(x = n, y = exp(fit))) +
  xlab("Participant number") +
  ylab("Predicted Count")


