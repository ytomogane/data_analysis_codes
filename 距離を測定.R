#距離を測定

#\と/の置き換え
pathPrep <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}
data=read.csv('C:/Users/yousu/Google Drive/for_analyza_12202017/analyze12152017__4-7yr_subject_June2015_noJHID_110717_newM2_MergedWithVolume5_fixed_original12092017_forAnalyze.csv')

data=read.csv('C:/Users/yousu/Google Drive/for_analyze_02062018/analyze2062018__4-7yr_subject_June2015_noJHID_110717_newM2_MergedWithVolume5_fixed_original12092017_forAnalyze.csv')
for.distance=data[1:1034,41:453]


distance.each=dist(x=for.distance, method = "euclidean", diag = TRUE, upper = FALSE)
write.csv(x=as.matrix(distance.each), file = "analyze02062018__4-7yr_EUCdistance.csv")

