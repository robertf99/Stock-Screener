#--------------------
# global constants
# ------------------
what_metrics <- c('Previous Close',
                  'Change & Percent Change',
                  '52-week Low',
                  '52-week High',
                  'P/E Ratio',
                  "Price/EPS Estimate Current Year",
                  "Price/EPS Estimate Next Year",
                  "PEG Ratio",
                  'Price/Book',
                  "Price/Sales",
                  'Average Daily Volume',
                  'EBITDA',
                  "Market Capitalization",
                  "Dividend Yield", 
                  'Dividend/Share',
                  'Earnings/Share',
                  'EPS Estimate Current Year',
                  'EPS Estimate Next Quarter',
                  'Percent Change From 52-week Low',
                  'Change From 52-week Low',
                  'Percent Change From 52-week High',
                  'Change From 52-week High')

#---------------------
# done once everyday
#---------------------

# check ASX symbols

# setwd("D:/Personal/R projects/stock")

# url <- 'http://www.asx.com.au/asx/research/ASXListedCompanies.csv'
# csv <- rawToChar(getURLContent(url, binary=TRUE))
# df <- read.csv(textConnection(csv), skip = 2)
# colnames(df) = c('company', 'ASX', 'industry')
# df$ASX = paste0(df$ASX, '.AX')
# df = data.frame(lapply(df, function(x) as.character(x)), stringsAsFactors = 'FALSE')
# df[df$ASX == 'BUB.AX', 'industry'] = 'Food, Beverage & Tobacco'
# saveRDS(df, file = 'symbols.rds')
# 
# # download symbol financials
# financials = cbind(company = df$company,
#                    industry = df$industry,
#                    getQuote(df$ASX, what = yahooQF(what_metrics)))
# financials[['Trade Time']] = as.character(as.Date(financials[['Trade Time']]))
# numericIdx = which(names(financials) %in% c('P. Close','PEG Ratio',
#                                             'Price/EPS Estimate Current Year',
#                                             'P/E Ratio', "Price/Book", "Price/Sales",
#                                             'Ave. Daily Volume'))
# financials[, numericIdx] = apply(financials[, numericIdx], 2, as.numeric)
# 
# saveRDS(financials, file = 'asx.rds')

###########################################################

shinyServer(function(input, output) {

  callModule(mod_ASX, 'mod_asx')

})