#ALPFA UCF - Technical Development Commitee
#Author: Christopher Fernandez
#Data Analytics Project 03.11.2020

#Retrieving Mean and Standard Deviations per Industry/Sector

#set working directory / location of work files
#change this to be wherever our input files are kept locally
setwd("C:/Users/Chris/Documents/ALPFA/Data Analytics Project")

#read in data and save as tables
valuation<-read.csv("valuation_multiples.csv", na.strings = "--")
lsp<-read.csv("liquidity_solvency_profitability.csv", na.strings = "--")
#The tables we are going to be working with will be "valuation" and "lsp"
#"na.strings" defines every "--" as a missing or N/A value
#The CSV files referenced above are copies of of the excel files of the same name available on the GitHub


#==========Valuation==========
#checking overall summary statistics and frequency tables for industry/sector
summary(valuation)
table(valuation$GICS.Ind.Name.)
table(valuation$GICS.Sector.)

#Averages by Sector:
#Price
avg_price_s<-aggregate(x=valuation$Price, FUN = mean, by=list(valuation$GICS.Sector.),na.rm=TRUE)
names(avg_price_s)<-c("Sector","Average Price")
avg_price_s
#P/FCF LF
avg_pfcflf_s<-aggregate(x=valuation$P.FCF.LF., FUN = mean, by=list(valuation$GICS.Sector.), na.rm=TRUE)
names(avg_pfcflf_s)<-c("Sector","Average P/FCF LF")
avg_pfcflf_s
#P/B LF
avg_pblf_s<-aggregate(x=valuation$P.B.LF., FUN = mean, by=list(valuation$GICS.Sector.),na.rm=TRUE)
names(avg_pblf_s)<-c("Sector","Average P/B LF")
avg_pblf_s
#P/S LF
avg_pslf_s<-aggregate(x=valuation$P.S.LF., FUN = mean, by=list(valuation$GICS.Sector.),na.rm=TRUE)
names(avg_pslf_s)<-c("Sector","Average P/S LF")
avg_pslf_s
#P/E LF
avg_pelf_s<-aggregate(x=valuation$P.E.LF., FUN = mean, by=list(valuation$GICS.Sector.),na.rm=TRUE)
names(avg_pelf_s)<-c("Sector","Average P/E LF")
avg_pelf_s

#Averages by Industry:
#Price
avg_price_i<-aggregate(x=valuation$Price, FUN = mean, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(avg_price_i)<-c("Industry","Average Price")
avg_price_i
#P/FCF LF
avg_pfcflf_i<-aggregate(x=valuation$P.FCF.LF., FUN = mean, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(avg_pfcflf_i)<-c("Industry","Average P/FCF LF")
avg_pfcflf_i
#P/B LF
avg_pblf_i<-aggregate(x=valuation$P.B.LF., FUN = mean, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(avg_pblf_i)<-c("Industry","Average P/B LF")
avg_pblf_i
#P/S LF
avg_pslf_i<-aggregate(x=valuation$P.S.LF., FUN = mean, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(avg_pslf_i)<-c("Industry","Average P/S LF")
avg_pslf_i
#P/E LF
avg_pelf_i<-aggregate(x=valuation$P.E.LF., FUN = mean, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(avg_pelf_i)<-c("Industry","Average P/E LF")
avg_pelf_i

#Standard Deviations by Sector:
#Price
sd_price_s<-aggregate(x=valuation$Price, FUN = sd, by=list(valuation$GICS.Sector.),na.rm=TRUE)
names(sd_price_s)<-c("Sector","Std Dev of Price")
sd_price_s
#P/FCF LF
sd_pfcflf_s<-aggregate(x=valuation$P.FCF.LF., FUN = sd, by=list(valuation$GICS.Sector.), na.rm=TRUE)
names(sd_pfcflf_s)<-c("Sector","Std Dev of P/FCF LF")
sd_pfcflf_s
#P/B LF
sd_pblf_s<-aggregate(x=valuation$P.B.LF., FUN = sd, by=list(valuation$GICS.Sector.),na.rm=TRUE)
names(sd_pblf_s)<-c("Sector","Std Dev of P/B LF")
sd_pblf_s
#P/S LF
sd_pslf_s<-aggregate(x=valuation$P.S.LF., FUN = sd, by=list(valuation$GICS.Sector.),na.rm=TRUE)
names(sd_pslf_s)<-c("Sector","Std Dev of P/S LF")
sd_pslf_s
#P/E LF
sd_pelf_s<-aggregate(x=valuation$P.E.LF., FUN = sd, by=list(valuation$GICS.Sector.),na.rm=TRUE)
names(sd_pelf_s)<-c("Sector","Std Dev of P/E LF")
sd_pelf_s

#Standard Deviations by Industry:
#Price
sd_price_i<-aggregate(x=valuation$Price, FUN = sd, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(sd_price_i)<-c("Industry","Std Dev of Price")
sd_price_i
#P/FCF LF
sd_pfcflf_i<-aggregate(x=valuation$P.FCF.LF., FUN = sd, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(sd_pfcflf_i)<-c("Industry","Std Dev of P/FCF LF")
sd_pfcflf_i
#P/B LF
sd_pblf_i<-aggregate(x=valuation$P.B.LF., FUN = sd, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(sd_pblf_i)<-c("Industry","Std Dev of P/B LF")
sd_pblf_i
#P/S LF
sd_pslf_i<-aggregate(x=valuation$P.S.LF., FUN = sd, by=list(valuation$GICS.Ind.Name.),na.rm=TRUE)
names(sd_pslf_i)<-c("Industry","Std Dev of P/S LF")
sd_pslf_i


#==========Liquidity, Solvency, Profitability==========
#Averages by Sector:
#Price
avg_prices_s<-aggregate(x=lsp$Price, FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_prices_s)<-c("Sector","Mean Price")
avg_prices_s
#ROIC LF
avg_roiclf_s<-aggregate(x=lsp$ROIC.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_roiclf_s)<-c("Sector","Mean ROIC LF")
avg_roiclf_s
#ROE LF
avg_roelf_s<-aggregate(x=lsp$ROE.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_roelf_s)<-c("Sector","Mean ROE LF")
avg_roelf_s
#Free Cash Flow Margin
avg_fcfm_s<-aggregate(x=lsp$Free.Cash.Flow.Margin., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_fcfm_s)<-c("Sector","Mean Free Cash Flow Margin")
avg_fcfm_s
#T12M EBITDA Mrgn LF
avg_t12emlf_s<-aggregate(x=lsp$T12M.EBITDA.Mrgn.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_t12emlf_s)<-c("Sector","Mean T12M EBITDA Mrgn LF")
avg_t12emlf_s
#Net Mrgn 5Yr Avg LF
avg_nm5alf_s<-aggregate(x=lsp$Net.Mrgn.5Yr.Avg.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_nm5alf_s)<-c("Sector","Mean Net Mrgn 5Yr Avg LF")
avg_nm5alf_s
#OPM LF
avg_opmlf_s<-aggregate(x=lsp$OPM.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_opmlf_s)<-c("Sector","Mean OPM LF")
avg_opmlf_s
#GM LF
avg_gmlf_s<-aggregate(x=lsp$GM.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_gmlf_s)<-c("Sector","Mean GM LF")
avg_gmlf_s
#Interest Coverage Ratio
avg_icr_s<-aggregate(x=lsp$Interest.Coverage.Ratio., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_icr_s)<-c("Sector","Mean Interest Coverage Ratio")
avg_icr_s
#Debt/Equity LF
avg_delf_s<-aggregate(x=lsp$Debt.Equity.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_delf_s)<-c("Sector","Mean Debt/Equity LF")
avg_delf_s
#Debt/Assets LF
avg_dalf_s<-aggregate(x=lsp$Debt.Assets.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_dalf_s)<-c("Sector","Mean Debt/Assets LF")
avg_dalf_s
#Modified Working Capital Turnover LF
avg_mwctlf_s<-aggregate(x=lsp$Modified.Working.Capital.Turnover.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_mwctlf_s)<-c("Sector","Mean Modified Working Capital Turnover LF")
avg_mwctlf_s
#Inv Turnover LF
avg_itlf_s<-aggregate(x=lsp$Inv.Turnover.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_itlf_s)<-c("Sector","Mean Inv Turnover LF")
avg_itlf_s
#A/R Trnovr LF
avg_artlf_s<-aggregate(x=lsp$A.R.Trnovr.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_artlf_s)<-c("Sector","Mean A/R Trnovr LF")
avg_artlf_s
#Cash Conversion Cycle LF
avg_ccclf_s<-aggregate(x=lsp$Cash.Conversion.Cycle.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_ccclf_s)<-c("Sector","Mean Cash Conversion Cycle LF")
avg_ccclf_s
#Cash Ratio LF
avg_carlf_s<-aggregate(x=lsp$Cash.Ratio.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_carlf_s)<-c("Sector","Mean Cash Ratio LF")
avg_carlf_s
#Quick Ratio LF
avg_qrlf_s<-aggregate(x=lsp$Quick.Ratio.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_qrlf_s)<-c("Sector","Mean Quick Ratio LF")
avg_qrlf_s
#Curr Ratio LF
avg_curlf_s<-aggregate(x=lsp$Debt.Equity.LF., FUN = mean, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(avg_curlf_s)<-c("Sector","Mean Curr Ratio LF")
avg_curlf_s

#Averages by Industry:
#Price
avg_prices_i<-aggregate(x=lsp$Price, FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_prices_i)<-c("Industry","Mean Price")
avg_prices_i
#ROIC LF
avg_roiclf_i<-aggregate(x=lsp$ROIC.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_roiclf_i)<-c("Industry","Mean ROIC LF")
avg_roiclf_i
#ROE LF
avg_roelf_i<-aggregate(x=lsp$ROE.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_roelf_i)<-c("Industry","Mean ROE LF")
avg_roelf_i
#Free Cash Flow Margin
avg_fcfm_i<-aggregate(x=lsp$Free.Cash.Flow.Margin., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_fcfm_i)<-c("Industry","Mean Free Cash Flow Margin")
avg_fcfm_i
#T12M EBITDA Mrgn LF
avg_t12emlf_i<-aggregate(x=lsp$T12M.EBITDA.Mrgn.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_t12emlf_i)<-c("Industry","Mean T12M EBITDA Mrgn LF")
avg_t12emlf_i
#Net Mrgn 5Yr Avg LF
avg_nm5alf_i<-aggregate(x=lsp$Net.Mrgn.5Yr.Avg.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_nm5alf_i)<-c("Industry","Mean Net Mrgn 5Yr Avg LF")
avg_nm5alf_i
#OPM LF
avg_opmlf_i<-aggregate(x=lsp$OPM.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_opmlf_i)<-c("Industry","Mean OPM LF")
avg_opmlf_i
#GM LF
avg_gmlf_i<-aggregate(x=lsp$GM.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_gmlf_i)<-c("Industry","Mean GM LF")
avg_gmlf_i
#Interest Coverage Ratio
avg_icr_i<-aggregate(x=lsp$Interest.Coverage.Ratio., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_icr_i)<-c("Industry","Mean Interest Coverage Ratio")
avg_icr_i
#Debt/Equity LF
avg_delf_i<-aggregate(x=lsp$Debt.Equity.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_delf_i)<-c("Industry","Mean Debt/Equity LF")
avg_delf_i
#Debt/Assets LF
avg_dalf_i<-aggregate(x=lsp$Debt.Assets.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_dalf_i)<-c("Industry","Mean Debt/Assets LF")
avg_dalf_i
#Modified Working Capital Turnover LF
avg_mwctlf_i<-aggregate(x=lsp$Modified.Working.Capital.Turnover.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_mwctlf_i)<-c("Industry","Mean Modified Working Capital Turnover LF")
avg_mwctlf_i
#Inv Turnover LF
avg_itlf_i<-aggregate(x=lsp$Inv.Turnover.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_itlf_i)<-c("Industry","Mean Inv Turnover LF")
avg_itlf_i
#A/R Trnovr LF
avg_artlf_i<-aggregate(x=lsp$A.R.Trnovr.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_artlf_i)<-c("Industry","Mean A/R Trnovr LF")
avg_artlf_i
#Cash Conversion Cycle LF
avg_ccclf_i<-aggregate(x=lsp$Cash.Conversion.Cycle.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_ccclf_i)<-c("Industry","Mean Cash Conversion Cycle LF")
avg_ccclf_i
#Cash Ratio LF
avg_carlf_i<-aggregate(x=lsp$Cash.Ratio.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_carlf_i)<-c("Industry","Mean Cash Ratio LF")
avg_carlf_i
#Quick Ratio LF
avg_qrlf_i<-aggregate(x=lsp$Quick.Ratio.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_qrlf_i)<-c("Industry","Mean Quick Ratio LF")
avg_qrlf_i
#Curr Ratio LF
avg_curlf_i<-aggregate(x=lsp$Debt.Equity.LF., FUN = mean, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_curlf_i)<-c("Industry","Mean Curr Ratio LF")
avg_curlf_i

#Standard Deviations by Sector:
#Price
sd_prices_s<-aggregate(x=lsp$Price, FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_prices_s)<-c("Sector","Std Dev of Price")
sd_prices_s
#ROIC LF
sd_roiclf_s<-aggregate(x=lsp$ROIC.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_roiclf_s)<-c("Sector","Std Dev of ROIC LF")
sd_roiclf_s
#ROE LF
sd_roelf_s<-aggregate(x=lsp$ROE.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_roelf_s)<-c("Sector","Std Dev of ROE LF")
sd_roelf_s
#Free Cash Flow Margin
sd_fcfm_s<-aggregate(x=lsp$Free.Cash.Flow.Margin., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_fcfm_s)<-c("Sector","Std Dev of Free Cash Flow Margin")
sd_fcfm_s
#T12M EBITDA Mrgn LF
sd_t12emlf_s<-aggregate(x=lsp$T12M.EBITDA.Mrgn.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_t12emlf_s)<-c("Sector","Std Dev of T12M EBITDA Mrgn LF")
sd_t12emlf_s
#Net Mrgn 5Yr Avg LF
sd_nm5alf_s<-aggregate(x=lsp$Net.Mrgn.5Yr.Avg.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_nm5alf_s)<-c("Sector","Std Dev of Net Mrgn 5Yr Avg LF")
sd_nm5alf_s
#OPM LF
sd_opmlf_s<-aggregate(x=lsp$OPM.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_opmlf_s)<-c("Sector","Std Dev of OPM LF")
sd_opmlf_s
#GM LF
sd_gmlf_s<-aggregate(x=lsp$GM.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_gmlf_s)<-c("Sector","Std Dev of GM LF")
sd_gmlf_s
#Interest Coverage Ratio
sd_icr_s<-aggregate(x=lsp$Interest.Coverage.Ratio., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_icr_s)<-c("Sector","Std Dev of Interest Coverage Ratio")
sd_icr_s
#Debt/Equity LF
sd_delf_s<-aggregate(x=lsp$Debt.Equity.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_delf_s)<-c("Sector","Std Dev of Debt/Equity LF")
sd_delf_s
#Debt/Assets LF
sd_dalf_s<-aggregate(x=lsp$Debt.Assets.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_dalf_s)<-c("Sector","Std Dev of Debt/Assets LF")
sd_dalf_s
#Modified Working Capital Turnover LF
sd_mwctlf_s<-aggregate(x=lsp$Modified.Working.Capital.Turnover.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_mwctlf_s)<-c("Sector","Std Dev of Modified Working Capital Turnover LF")
sd_mwctlf_s
#Inv Turnover LF
sd_itlf_s<-aggregate(x=lsp$Inv.Turnover.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_itlf_s)<-c("Sector","Std Dev of Inv Turnover LF")
sd_itlf_s
#A/R Trnovr LF
sd_artlf_s<-aggregate(x=lsp$A.R.Trnovr.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_artlf_s)<-c("Sector","Std Dev of A/R Trnovr LF")
sd_artlf_s
#Cash Conversion Cycle LF
sd_ccclf_s<-aggregate(x=lsp$Cash.Conversion.Cycle.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_ccclf_s)<-c("Sector","Std Dev of Cash Conversion Cycle LF")
sd_ccclf_s
#Cash Ratio LF
sd_carlf_s<-aggregate(x=lsp$Cash.Ratio.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_carlf_s)<-c("Sector","Std Dev of Cash Ratio LF")
sd_carlf_s
#Quick Ratio LF
sd_qrlf_s<-aggregate(x=lsp$Quick.Ratio.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_qrlf_s)<-c("Sector","Std Dev of Quick Ratio LF")
sd_qrlf_s
#Curr Ratio LF
sd_curlf_s<-aggregate(x=lsp$Debt.Equity.LF., FUN = sd, by=list(lsp$GICS.Sector.),na.rm=TRUE)
names(sd_curlf_s)<-c("Sector","Std Dev of Curr Ratio LF")
sd_curlf_s

#Standard Deviations by Industry:
#Price
sd_prices_i<-aggregate(x=lsp$Price, FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_prices_i)<-c("Industry","Std Dev of Price")
sd_prices_i
#ROIC LF
sd_roiclf_i<-aggregate(x=lsp$ROIC.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_roiclf_i)<-c("Industry","Std Dev of ROIC LF")
sd_roiclf_i
#ROE LF
sd_roelf_i<-aggregate(x=lsp$ROE.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_roelf_i)<-c("Industry","Std Dev of ROE LF")
sd_roelf_i
#Free Cash Flow Margin
sd_fcfm_i<-aggregate(x=lsp$Free.Cash.Flow.Margin., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_fcfm_i)<-c("Industry","Std Dev of Free Cash Flow Margin")
sd_fcfm_i
#T12M EBITDA Mrgn LF
sd_t12emlf_i<-aggregate(x=lsp$T12M.EBITDA.Mrgn.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_t12emlf_i)<-c("Industry","Std Dev of T12M EBITDA Mrgn LF")
sd_t12emlf_i
#Net Mrgn 5Yr Avg LF
sd_nm5alf_i<-aggregate(x=lsp$Net.Mrgn.5Yr.Avg.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_nm5alf_i)<-c("Industry","Std Dev of Net Mrgn 5Yr Avg LF")
sd_nm5alf_i
#OPM LF
sd_opmlf_i<-aggregate(x=lsp$OPM.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_opmlf_i)<-c("Industry","Std Dev of OPM LF")
sd_opmlf_i
#GM LF
sd_gmlf_i<-aggregate(x=lsp$GM.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_gmlf_i)<-c("Industry","Std Dev of GM LF")
sd_gmlf_i
#Interest Coverage Ratio
sd_icr_i<-aggregate(x=lsp$Interest.Coverage.Ratio., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_icr_i)<-c("Industry","Std Dev of Interest Coverage Ratio")
sd_icr_i
#Debt/Equity LF
sd_delf_i<-aggregate(x=lsp$Debt.Equity.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_delf_i)<-c("Industry","Std Dev of Debt/Equity LF")
sd_delf_i
#Debt/Assets LF
sd_dalf_i<-aggregate(x=lsp$Debt.Assets.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_dalf_i)<-c("Industry","Std Dev of Debt/Assets LF")
sd_dalf_i
#Modified Working Capital Turnover LF
sd_mwctlf_i<-aggregate(x=lsp$Modified.Working.Capital.Turnover.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_mwctlf_i)<-c("Industry","Std Dev of Modified Working Capital Turnover LF")
sd_mwctlf_i
#Inv Turnover LF
sd_itlf_i<-aggregate(x=lsp$Inv.Turnover.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_itlf_i)<-c("Industry","Std Dev of Inv Turnover LF")
sd_itlf_i
#A/R Trnovr LF
sd_artlf_i<-aggregate(x=lsp$A.R.Trnovr.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_artlf_i)<-c("Industry","Std Dev of A/R Trnovr LF")
sd_artlf_i
#Cash Conversion Cycle LF
sd_ccclf_i<-aggregate(x=lsp$Cash.Conversion.Cycle.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_ccclf_i)<-c("Industry","Std Dev of Cash Conversion Cycle LF")
sd_ccclf_i
#Cash Ratio LF
sd_carlf_i<-aggregate(x=lsp$Cash.Ratio.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_carlf_i)<-c("Industry","Std Dev of Cash Ratio LF")
sd_carlf_i
#Quick Ratio LF
sd_qrlf_i<-aggregate(x=lsp$Quick.Ratio.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(sd_qrlf_i)<-c("Industry","Std Dev of Quick Ratio LF")
sd_qrlf_i
#Curr Ratio LF
avg_curlf_i<-aggregate(x=lsp$Debt.Equity.LF., FUN = sd, by=list(lsp$GICS.Ind.Name.),na.rm=TRUE)
names(avg_curlf_i)<-c("Industry","Std Dev of Curr Ratio LF")
avg_curlf_i

