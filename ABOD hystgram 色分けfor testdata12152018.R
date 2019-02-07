#ABOD hystgram 色分けfor testdata
#http://d.hatena.ne.jp/Rion778/?of=164


#pile graph script


library(ggplot2)

#data read from excel sheet
#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み
abod.mfeat <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_11132018_ABOD_for_DrOISHIdata/forABOD hystogram detailed LV3 testdataset for ABOD, Bonferroni12072018.xlsx",
                        sheet=1)
# データのサイズ
dim(abod.mfeat)
select(.data=abod.mfeat,ABOD )

X=3 #1~5,41

J=X 
LVV=paste("ABOD")
DISEASES=paste("Disesase")
file.name=sprintf("hystogram of test data ABOD.pdf",J)

# 14列目にABODLV%のスコアが記録されている
head(DISEASES)
# スコアを抽出して数値に変換
#abod.mfeat <- abod.mfeat %>% mutate(V14 = as.numericV14)
# スコアのヒストグラムのプロット
#aesの代わりにaes_を使うとx,y軸の変数を文字列で指定することができます
#文字列はas.nameで囲いましょう
#MRIの機種ごとの色分け
p <- ggplot(abod.mfeat, aes(ABOD,fill=Disease)) + geom_histogram(binwidth = 0.025) +
  scale_x_log10() + xlab("log10(ABOD scores)") + ylab("sample numbers") +
  theme_bw()+
  labs(title=sprintf("LV%01dABOD",J))+
  coord_cartesian(xlim=c(5e-11,2e-5))
#coord_cartesian(xlim = NULL, ylim = NULL)

#color変更
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually

p=p + scale_fill_manual(values=c("grey20", "grey60"))

print(p)

pdf(file.name, width=10, height=10)
plot(p)
dev.off()
