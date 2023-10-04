?mean
a <- 5
b <- 6
a + b
sum(a,b)

ages <- c(5,6)
sum(ages)

names <- c('John','James')
friends <- data.frame(names,ages) 
View(friends)
str(friends)
friends$ages
sum(friends$ages)
friends[1,1]
friends[1,]
friends[,2]

install.packages("tidyverse")
library('tidyverse')
data()
View(starwars)

starwars %>%
  filter(height > 150 & mass < 200) %>%
  mutate(height_in_meters = height/100)%>%
  select(height_in_meters,mass)%>%
  arrange(height_in_meters)%>%
  #view()
  plot()

View(msleep)
glimpse(msleep)
head(msleep)
class(msleep$name)
length(msleep)
length(msleep$name)
names(msleep)
unique(msleep$vore)
missing <- !complete.cases(msleep)
msleep[missing,]

glimpse(starwars)
starwars%>%
  select(name,height,mass)
starwars%>%
  select(1:3)
starwars%>%
  select(matches('color'))

starwars%>%
  select(name,height,mass,everything())%>%
  View()
starwars%>%
  rename("characters"="name")%>%
  head()

class(starwars$hair_color)
starwars$hair_color <- as.character(starwars$hair_color)%>%
  glimpse()

starwars%>%
  mutate(hair_color= as.character(hair_color))%>%
  glimpse()

df <- starwars
df$sex <- as.factor(df$sex)
levels(df$sex)

df <- df%>%
  mutate(sex = factor(sex,
                      levels=c("male","female","hermaphroditic","none")))
levels(df$sex)
starwars%>%
  select(name,mass,sex)%>%
  filter(mass < 55 & sex == "male")

starwars%>%
  select(sex)%>%
  mutate(sex = recode(sex,"male"="man","female"="woman"))

mean(starwars$height,na.rm = TRUE)

names <- c("Peter","Andrew","Peter")
ages <- c(22,33,22)
friends <- data.frame(names,ages)
friends%>%
  distinct()
distinct(friends)

starwars%>%
  mutate(height_m = height/100)%>%
  select(name,height,height_m)%>%
  mutate(tallness =
           if_else(height_m <1,
                   "short",
                   "tall"))
install.packages("gapminder")
library(gapminder)
View(gapminder)
glimpse(gapminder)
data <- select(gapminder,country,year,lifeExp)
View(data)

# pivot_wider
wide_data <- data%>%
  pivot_wider(names_from = year,values_from=lifeExp)
wide_data
# pivot_longer
long_data <- pivot_longer(wide_data,2:13,
               names_to = "year",
               values_to = "lifeExp")
long_data
# range and spread
min(msleep$awake)
max(msleep$awake)
IQR(msleep$awake)

#centrality
mean(msleep$awake)
median(msleep$awake)

#variance
var(msleep$awake)

summary(msleep$awake)

#summarize your data
msleep%>%
  drop_na(vore)%>%
  group_by(vore)%>%
  summarise(Lower = min(sleep_total),
                        Average=mean(sleep_total),
                        Upper = max(sleep_total),
                        Difference = max(sleep_total) - min(sleep_total))%>%
  arrange(Average)%>%
  View()

#tables
table(msleep$vore)


# Visualise
plot(pressure)


ggplot(data=starwars,mapping = aes(x = gender)) + geom_bar()


#histogram
starwars%>%
  drop_na(height)%>%
  ggplot(mapping=aes(x=height))+geom_histogram()


# Load the required packages
library(GA)
library(PortfolioAnalytics)

# Load the data for stocks or assets you want to include in the portfolio
data(edhec)
returns <- edhec[, 1:5]

# Define the fitness function
fitness <- function(weights) {
  mean_returns <- colMeans(returns) %*% weights
  stddev_returns <- sqrt(diag(weights %*% cov(returns) %*% t(weights)))
  return(-mean_returns + stddev_returns)
}

# Define the portfolio optimization problem
portfolio.spec <- portfolio.spec(assets=colnames(returns))

# Define the objectives for the portfolio optimization
portfolio.spec <- add.objective(portfolio.spec, type="return", name="mean")
portfolio.spec <- add.objective(portfolio.spec, type="risk", name="stddev")

# Define the portfolio optimization constraints
portfolio.spec <- add.constraint(portfolio.spec, type="weight_sum")
portfolio.spec <- add.constraint(portfolio.spec, type="weight", min=0.1)
portfolio.spec <- add.constraint(portfolio.spec, type="weight", max=0.4)

# Define the lower and upper range of values for the decision variables
lower_bound <- rep(0, ncol(returns))
upper_bound <- rep(1, ncol(returns))

# Define the GA optimization algorithm
genetic.algorithm <- ga(type="real-valued", fitness=fitness, lower=lower_bound, upper=upper_bound, popSize=100)
# Get the optimal portfolio weights
weights <- attr(genetic.algorithm, "weights")
