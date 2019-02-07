#ABOD of HighDimOut'
#
#
#


install.packages("HighDimOut")
library("HighDimOut")
library(ggplot2)


#data read from excel sheet
#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み
data <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09282018_ABOD_for_DrOISHIdata/Bonferroni_results09282018.xlsx",
                     #                     sheet=1)
                     
                     #rocdata <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09062018_ABOD_for_DrOISHIdata/matlabABODresults09062018.xlsx",
                     sheet=1)

forabod = data

res.ABOD = Func.ABOD(data = forabod, basic = FALSE, perc = F)
