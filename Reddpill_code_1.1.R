# Knitr and kable options
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.align="center", out.width="80%")
options(kableExtra.latex.load_packages = FALSE)

# Open required packages
library(tidyverse)
library(tidytext)
library(syuzhet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(RedditExtractoR)
library(kableExtra)
library(stringr)
library(textclean)
library(gbm)
library(e1071)
library(gtrendsR)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(syuzhet)) install.packages("syuzhet", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(RedditExtractoR)) install.packages("RedditExtractoR", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(textclean)) install.packages("textclean", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org") #SVM modelling with e1071 package
if(!require(gtrendsR)) install.packages("gtrendsR", repos = "http://cran.us.r-project.org")

# Create ggplot2 theme
plot_theme <- theme(plot.caption = element_text(size = 11, face = "italic"), axis.title = element_text(size = 11))


# Load clean dataset
load(file="~/.RData")

# Establish ggplot theme
plot_theme <- theme(plot.caption = element_text(size = 11, face = "italic"), axis.title = element_text(size = 11))

# Pushishift data extraction attempt, currently non-functional due to API instability, if attempted would also require post-processing to work consistently with the remainder of the code in this report (attempt with caution)
# library(jsonlite)
# if(!require("jsonlite")) install.packages("jsonlite", repos = "http://cran.us.r-project.org")

# dat <- fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1609430400&before=1612108800&q=pregabalin&size=1000"),flatten = T)
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1612108800&before=1614528000&q=pregabalin&size=1000"),flatten = T))
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1614528000&before=1617206400&q=pregabalin&size=1000"),flatten = T))
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1617206400&before=1619798400&q=pregabalin&size=1000"),flatten = T))
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1619798400&before=1622476800&q=pregabalin&size=1000"),flatten = T))
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1622476800&before=1625068800&q=pregabalin&size=1000"),flatten = T))
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1625068800&before=1627747200&q=pregabalin&size=1000"),flatten = T))
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1627747200&before=1630425600&q=pregabalin&size=1000"),flatten = T))
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1630425600&before=1633017600&q=pregabalin&size=1000"),flatten = T))
# dat <- dat %>% rbind(fromJSON(url("https://api.pushshift.io/reddit/submission/search?html_decode=true&after=1633017600&before=1635696000&q=pregabalin&size=1000"),flatten = T))


# Extract reddit posts with Reddit ExtractoR - run this function from .R file or load provided dataset, gives 429 Error when used inline with .Rmd file
dat <- RedditExtractoR::find_thread_urls(
keywords = "pregabalin", sort_by = "top", period = "year")

# Plot by subreddit
dat %>% 
  count(subreddit) %>%
  slice_max(n, n=25) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(subreddit, -n), y = n)) +
  xlab("Subreddit") +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Convert date_utc from character to date
dat <- dat %>%
  mutate(date = as_datetime(date_utc))

# Round to the nearest week  
dat$date <- round_date(dat$date, unit = "month")

#Plot date
dat %>%
  count(date) %>%
  ggplot(aes(x=date, y=n)) +
  geom_line() +
  labs(x = "Date", y = "No. of submissions") + plot_theme

# Plot Google Trends data for pregabalin
trend <- gtrends("pregabalin", time = "today 12-m")

data.frame(trend$interest_over_time) %>%
  ggplot(aes(x=trend$interest_over_time$date, y=trend$interest_over_time$hits )) +
  geom_line() +
  labs(x = "Date", y = "Search hits") + plot_theme

# Append title to text
dat <- dat %>% mutate(full_text = paste(title, "-", text ))

# Perform text cleaning
dat <- dat %>% 
  mutate(text_clean = full_text %>% 
           replace_non_ascii() %>% 
           replace_html(symbol = F) %>% # remove html tag
           str_replace_all("[0-9]", " ") %>% 
           str_replace_all("[-|]", " ") %>% # replace "-" with space
           tolower() %>% #lowercase
           replace_symbol() %>%
           replace_contraction() %>% 
           replace_word_elongation() %>%  # lengthen shortened word
           str_replace_all("[[:punct:]]", " ") %>% # remove punctuation
           str_replace_all(" dr ", " doctor ") %>% 
           make_plural() %>%
           str_replace_all(" s ", " ") %>%  
           str_squish() %>% # remove double whitespace
           str_trim() # remove whitespace at the start and end of the text
  )

# Remove temporary column
dat <- dat %>% select(-full_text)

# Calculate sentiment score and plot sentiment distribution
dat <- dat %>% 
  mutate(sent = get_sentiment(text_clean, method="afinn"))

dat %>%
  ggplot(aes(x=sent)) +
  geom_histogram(bins=30, color = I("black")) +
  labs(x = "Sentiment", y = "No. of submissions") + plot_theme

# Convert strings to factors
col_names <- names(dat[1:9])
dat[col_names] <- lapply(dat[col_names] , factor)

# Partition data into test and train sets
set.seed(1)
test_index <- createDataPartition(y = dat$sent, times = 1, p = 0.5, list = FALSE)
train <- dat[-test_index,]
test <- dat[test_index,]


# Perform gbm training
simple_model_gbm <- gbm(sent ~ date + comments + subreddit, data=train, cv = 100)

# See results
y_pred = predict(simple_model_gbm, newdata = test)

simple_rmse_gbm <- RMSE(y_pred, test$sent)

# plot error curve
gbm.perf(simple_model_gbm, method = "OOB")

# find index for number trees with minimum CV error
best <- which.min(simple_model_gbm$cv.error)

# get MSE and compute RMSE
sqrt(simple_model_gbm$cv.error[best])

# Creat hyperparameter tuning grid for GBM
hyper_grid_gbm <- expand.grid(
  learning_rate = c(0.3, 0.1, 0.05, 0.01, 0.005),
  RMSE = NA,
  trees = NA,
  time = NA
)

# execute grid search
for(i in seq_len(nrow(hyper_grid_gbm))) {
  
  # fit gbm
  set.seed(123)  # for reproducibility
  train_time <- system.time({
    m <- gbm(
      formula = sent ~ date + comments + subreddit,
      data = train,
      distribution = "gaussian",
      n.trees = min(simple_model_gbm$cv.error), 
      shrinkage = hyper_grid_gbm$learning_rate[i], 
      interaction.depth = 3, 
      n.minobsinnode = 10,
      cv.folds = 10 
    )
  })
  
  # add SSE, trees, and training time to results
  hyper_grid_gbm$RMSE[i]  <- sqrt(min(m$cv.error))
  hyper_grid_gbm$trees[i] <- which.min(m$cv.error)
  hyper_grid_gbm$time[i]  <- train_time[["elapsed"]]
  
}

# Train a simple SVM model
simple_model_svm <- svm(sent ~ date + comments + subreddit, dat=train)

# Create hyperparameter tune grid for SVM
hyper_grid_svm <- expand.grid(
  gamma = c(0.3, 0.1, 0.05, 0.01),
  cost = c(10, 100, 1000, 10000),
  RMSE = NA,
  time = NA
)

# Create a function for training SVM model
# execute grid search
for(i in seq_len(nrow(hyper_grid_svm))) {
  set.seed(123)  # for reproducibility
  train_time <- system.time({
    ## train SVM model 
    m <- svm( 
      sent ~ date + comments + subreddit, 
      data = train,
      cost = hyper_grid_svm$cost[i], 
      gamma = hyper_grid_svm$gamma[i], 
      type = "eps-regression", 
      kernel = "radial")
  })
  
  # add RMSE and training time to results
  hyper_grid_svm$RMSE[i]  <- sqrt(min(mean(m$residuals^2)))
  hyper_grid_svm$time[i]  <- train_time[["elapsed"]]
}


# Fit model onto test data
y_pred = predict(simple_model_gbm, newdata = test)

# Predict RMSE
simple_rmse_gbm <- RMSE(y_pred, test$sent)

# Add results to final result table
rmse_results <- data.frame(Method = "Simple GBM", RMSE = simple_rmse_gbm)
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Fit optimised GBM model
optimised_model_gbm <- gbm(
  formula = sent ~ date + comments + subreddit,
  data = train,
  distribution = "gaussian",
  n.trees = min(simple_model_gbm$cv.error), 
  shrinkage = hyper_grid_gbm$learning_rate[which.min(hyper_grid_gbm$RMSE)], 
  interaction.depth = 3, 
  n.minobsinnode = 10,
  cv.folds = 10 
)

# Use model onto test data
y_pred = predict(optimised_model_gbm, newdata = test)

# Predict RMSE
optimised_rmse_gbm <- RMSE(y_pred, test$sent)

# Add results to final result table
rmse_results <- rmse_results %>% rbind(Method = "Optimised GBM", RMSE = optimised_rmse_gbm)
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Use simple SVMmodel ont test data
y_pred = predict(simple_model_svm, newdata = test)

# Predict RMSE
optimised_rmse_svm <- RMSE(y_pred, test$sent)

# Add results to final result table
rmse_results <- rmse_results %>% rbind(Method = "Simple SVM", RMSE = simple_rmse_svm)
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Plot SVM optimisation results
hyper_grid_svm %>% 
  ggplot(aes(x=gamma, y=RMSE))+
  geom_line()+
  facet_grid(rows=vars(cost), labeller = label_both)
plot_theme

# Fit optimised SVM model
optimised_model_svm <- svm( 
  sent ~ date + comments + subreddit, 
  data = train,
  cost = hyper_grid_svm$cost[which.min(hyper_grid_svm$RMSE)], 
  gamma = hyper_grid_svm$gamma[which.min(hyper_grid_svm$RMSE)], 
  type = "eps-regression", 
  kernel = "radial")

# Use model onto test data
y_pred = predict(optimised_model_svm, newdata = test)

# Predict RMSE
optimised_rmse_svm <- RMSE(y_pred, test$sent)

# Add results to final result table
rmse_results <- rmse_results %>% rbind(Method = "Optimised SVM", RMSE = optimised_rmse_svm)
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Save data for RMD knitting
save(dat, file = "ReddPill_data.RData")
