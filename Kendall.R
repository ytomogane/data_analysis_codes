#Kendall distance

install.packages("RMallow")
library("RMallow", lib.loc="~/R/win-library/3.4")



data=read.csv('C:/Users/yousu/Google Drive/for_analyze_02062018/analyze2062018__4-7yr_subject_June2015_noJHID_110717_newM2_MergedWithVolume5_fixed_original12092017_forAnalyze.csv')
for.distance=data[1:1034,41:453]


AKEN=data[,2:408]

## Validations are volumes and sex, age, race
##sex; male=0, female=1, age; z-score, race; 
##white or black or asian respectively 0 or 1
#volumes and R/L ratio; z-score

AResult=AllKendall(for.distance,for.distance)

write.table(AResult,file="analyze02062018_A_kendall_distance.csv", sep=",")
