library('ggplot2')

# Read Data
prices <- read.csv('./data/stock_prices.csv',stringsAsFactors = FALSE)
prices[1, ]

# Date Stock Close
#1 2011-05-25 DTE 51.12

# Transform Date
library('lubridate')
prices <- transform(prices, Date = ymd(Date))
prices[1, ]

# Date Stock Close
# 1 2011-05-25   DTE 51.12

# Reshape Data
library('reshape')
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

# Data Subset
prices <- subset(prices, Date != ymd('2002-02-01'))
prices <- subset(prices, Stock != 'DDR')

date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

# Bivariate Analysis - Correlation
cor.matrix <- cor(date.stock.matrix[, 2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

ggplot(data.frame(Correlation = correlations),
       aes(x = Correlation, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

# Principal Components
(pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)]))



# PCA Loadings
principal.component <- pca$loadings[, 1]
loadings <- as.numeric(principal.component)

ggplot(data.frame(Loading = loadings),
       aes(x = Loading, fill = 1)) +
  geom_density() +
  theme(legend.position = 'none')

# Predict on PCA
market.index <- predict(pca)[, 1]


# Read Data
dji.prices <- read.csv('./data/DJI.csv', stringsAsFactors = FALSE)
dji.prices <- transform(dji.prices, Date = ymd(Date))

# Subset Data
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

# Reverse Sort Data
dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

# Comparison Data for MI and DJI
comparison <- data.frame(Date = dates,
                         MarketIndex = market.index,
                         DJI = dji)

ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Transform data
comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)

# Sixteenth code snippet
ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Reshape  (melt) comparison data
alt.comparison <- melt(comparison, id.vars = 'Date')

names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison,
       aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()

# Scalled Data
comparison <- transform(comparison, MarketIndex = scale(MarketIndex))
comparison <- transform(comparison, DJI = scale(DJI))

alt.comparison <- melt(comparison, id.vars = 'Date')

names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) +
  geom_point() +
  geom_line()
