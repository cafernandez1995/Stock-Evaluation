#ALPFA UCF - Technical Development Commitee
#Author: Christopher Fernandez
#Data Analytics Project 03.09.2020

#Checking for normality

#set working directory / location of work files
#change this to be wherever our input files are kept locally
setwd("C:/Users/Chris/Documents/ALPFA/Data Analytics Project")

#read in data and save as tables
valuation<-read.csv("valuation_multiples.csv", na.strings = "--")
lsp<-read.csv("liquidity_solvency_profitability.csv", na.strings = "--")
#The tables we are going to be working with will be "valuation" and "lsp"
#"na.strings" defines every "--" as a missing or N/A value
#The CSV files referenced above are copies of of the excel files of the same name available on the GitHub


#==========Valuation Multiples==========

#Summary - includes min, max, median, and mean of each variable
#we want to check these of the numerical variables
summary(valuation)
#all five numerical variables have an unusually high maximum
#a lot of N/A values for each which are potentially influencing the accuracy of our results

#retrieving variable names of our data tables
names(valuation)
#the explanatory variables are P.FCF.LF., P.B.LF., P.S.LF., & P.E.LF.
#the response variable is Price

#Frequency Histograms
#we want to see a bell curve for each numerical variable to confirm normality
hist(valuation$Price)
hist(valuation$P.FCF.LF.)
hist(valuation$P.B.LF.)
hist(valuation$P.S.LF.)
hist(valuation$P.E.LF.)
#none are bell curves, HIGHLY skewed right due to unusually large values in each
#should consider removing the highly-valued outlier(s) that seem to be skewing the data

#QQ Plots
#Used to visualize relationship between Price & explanatory variables
#We want the see a strong (straight) linear correlation
qqplot(valuation$P.FCF.LF.,valuation$Price,xlab = "P/FCF LF", ylab = "Price")
#appears linear exepct for two unusually large values at the right tail
qqplot(valuation$P.B.LF.,valuation$Price,xlab = "P/B LF", ylab = "Price")
#appears linear with stronger correlation at the left tail
qqplot(valuation$P.S.LF.,valuation$Price,xlab = "P/S LF", ylab = "Price")
#semi-linear with stronger correlation at the left tail
qqplot(valuation$P.E.LF.,valuation$Price,xlab = "P/E LF", ylab = "Price")
#appears linear except for an unusually large value at the right tail

#Shapiro Test for Normality
#Null Hypothesis: data is normally distributed
#Alternative Hypothesis: data is NOT normally distributed
#Reject the null hypothesis if p-value is less than significance level (typically 0.05)
shapiro.test(valuation$Price)
#sample size not sufficient to run normality test on response variable
shapiro.test(valuation$P.FCF.LF.)
shapiro.test(valuation$P.B.LF.)
shapiro.test(valuation$P.S.LF.)
shapiro.test(valuation$P.E.LF.)
#Same conclusion for all explanatory variables
#p-value<0.05 => reject null => data is NOT normal

#Names of potential outliers by unusually LARGE variable values:
#Price: NVR Inc, Seaboard Corp
#P.FCF.LF.: OptimizeRx Corp (2)
#P.B.LF.: Sally Beauty Holdings Inc, Insperity Inc, Moxian Inc, Akari Therapeutics PLC
#P.S.LF.: UroGen Pharma Ltd, Intra-Cellular Therapies Inc (2), Mudrick Capital Acquisition Corp
#P.E.LF.: Extraction Oil & Gas Inc, Atlassian Corp PLC (2)


#==========Liquidity, Solvency, Profitability==========

#Summary - includes min, max, median, and mean of each variable
#we want to check these of the numerical variables
summary(lsp)
#in our numerical variables we see a large change in min and max values cmompared to mean
#a lot of N/A values for each which are potentially influencing the accuracy of our results

#retrieving variable names of our data tables
names(lsp)
#the explanatory variables are ROIC.LF., ROE.LF., Free.Cash.Flow.Margin., T12M.EBITDA.Mrgn.LF.,
#Net.Mrgn.5Yr.Avg.LF., OPM.LF., GM.LF., Interest.Coverage.Ratio., Debt.Equity.LF., Debt.Assets.LF.,
#Modified.Working.Capital.Turnover.LF., Inv.Turnover.LF., A.R.Trnovr.LF., Cash.Convserion.Cycle.LF.,
#Cash.Ratio.LF., Quick.Ratio.LF., & Curr.Ratio.LF.
#the response variable is Price

#Frequency Histograms
#we want to see a bell curve for each numerical variable to confirm normality
hist(lsp$Price)
hist(lsp$Interest.Coverage.Ratio.)
hist(lsp$Debt.Equity.LF.)
hist(lsp$Debt.Assets.LF.)
hist(lsp$Modified.Working.Capital.Turnover.LF.)
hist(lsp$Inv.Turnover.LF.)
hist(lsp$A.R.Trnovr.LF.)
hist(lsp$Cash.Ratio.LF.)
hist(lsp$Quick.Ratio.LF.)
hist(lsp$Curr.Ratio.LF.)
#these are all highly skewed RIGHT
hist(lsp$ROIC.LF.)
hist(lsp$ROE.LF.)
hist(lsp$Free.Cash.Flow.Margin.)
hist(lsp$T12M.EBITDA.Mrgn.LF.)
hist(lsp$Net.Mrgn.5Yr.Avg.LF.)
hist(lsp$OPM.LF.)
#these are all highly skewed LEFT
hist(lsp$GM.LF.)
#failed to run due to data entry error
hist(lsp$Cash.Conversion.Cycle.LF.)
#not skewed? should examine min and max
#min and max appear evenly separated about the mean
#approximately normal

#none are bell curves, HIGHLY skewed in either direction due to unusual values in each
#should consider removing the outlier(s) that seem to be skewing the data

#QQ Plots
#Used to visualize relationship between Price & explanatory variables
#We want the see a strong (straight) linear correlation
qqplot(lsp$ROIC.LF.,lsp$Price,xlab = "ROIC LF", ylab = "Price")
qqplot(lsp$ROE.LF.,lsp$Price,xlab = "ROE LF", ylab = "Price")
qqplot(lsp$Free.Cash.Flow.Margin.,lsp$Price,xlab = "Free Cash Flow Margin", ylab = "Price")
qqplot(lsp$T12M.EBITDA.Mrgn.LF.,lsp$Price,xlab = "T12M EBITDA Mrgn LF", ylab = "Price")
qqplot(lsp$Net.Mrgn.5Yr.Avg.LF.,lsp$Price,xlab = "Net Margin 5Yr Avg LF", ylab = "Price")
qqplot(lsp$OPM.LF.,lsp$Price,xlab = "OPM LF", ylab = "Price")
qqplot(lsp$Interest.Coverage.Ratio.,lsp$Price,xlab = "Interest Coverage Ratio", ylab = "Price")
qqplot(lsp$Debt.Equity.LF.,lsp$Price,xlab = "Debt/Equity LF", ylab = "Price")
qqplot(lsp$Debt.Assets.LF.,lsp$Price,xlab = "Debt/Assets LF", ylab = "Price")
#stronger correlation above
qqplot(lsp$Modified.Working.Capital.Turnover.LF.,lsp$Price,xlab = "Modified Working Capital Turnover LF", ylab = "Price")
qqplot(lsp$Inv.Turnover.LF.,lsp$Price,xlab = "Inv Turnover LF", ylab = "Price")
qqplot(lsp$A.R.Trnovr.LF.,lsp$Price,xlab = "A/R Trnovr LF", ylab = "Price")
qqplot(lsp$Cash.Conversion.Cycle.LF.,lsp$Price,xlab = "Cash Coversion Cycle LF", ylab = "Price")
#weaker linear relationship above due to inconsistent slope
qqplot(lsp$Cash.Ratio.LF.,lsp$Price,xlab = "Cash Ratio LF", ylab = "Price")
qqplot(lsp$Quick.Ratio.LF.,lsp$Price,xlab = "Quick Ratio LF", ylab = "Price")
qqplot(lsp$Curr.Ratio.LF.,lsp$Price,xlab = "Curr Ratio LF", ylab = "Price")
#would appear linear except for apparent outliers at the RIGHT tail for all plots above
qqplot(lsp$GM.LF.,lsp$Price,xlab = "GM LF", ylab = "Price")
#would appear linear if not for outliers near LEFT tail for above line of code

#Shapiro Test for Normality
#Null Hypothesis: data is normally distributed
#Alternative Hypothesis: data is NOT normally distributed
#Reject the null hypothesis if p-value is less than significance level (typically 0.05)
shapiro.test(lsp$Price)
shapiro.test(lsp$ROIC.LF.)
shapiro.test(lsp$Free.Cash.Flow.Margin.)
shapiro.test(lsp$Interest.Coverage.Ratio.)
shapiro.test(lsp$Debt.Assets.LF.)
#sample size not sufficient to run normality test on above variables
shapiro.test(lsp$ROE.LF.)
shapiro.test(lsp$T12M.EBITDA.Mrgn.LF.)
shapiro.test(lsp$Net.Mrgn.5Yr.Avg.LF.)
shapiro.test(lsp$OPM.LF.)
shapiro.test(lsp$GM.LF.)
shapiro.test(lsp$Debt.Equity.LF.)
shapiro.test(lsp$Modified.Working.Capital.Turnover.LF.)
shapiro.test(lsp$Inv.Turnover.LF.)
shapiro.test(lsp$A.R.Trnovr.LF.)
shapiro.test(lsp$Cash.Conversion.Cycle.LF.)
shapiro.test(lsp$Cash.Ratio.LF.)
shapiro.test(lsp$Quick.Ratio.LF.)
shapiro.test(lsp$Curr.Ratio.LF.)
#Same conclusion for all above explanatory variables
#p-value<0.05 => reject null => data is NOT normal

#Data would mostly appear normal aside from extreme outliers causing skewing
#Recommended to remove outliers of each variable one by one to obtain normality
#Alternatively could trasform data for modelling purposes