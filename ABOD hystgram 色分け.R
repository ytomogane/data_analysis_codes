#ABOD hystgram 色分け
#http://d.hatena.ne.jp/Rion778/?of=164


#pile graph script


library(ggplot2)

#data read from excel sheet
#xlsx の読み込みpackage
install.packages("openxlsx")
library(openxlsx)

# データの読み込み
abod.mfeat <- read.xlsx("C:/Users/yousu/Google_Drive/for_analyze_09282018_ABOD_for_DrOISHIdata/matlabABODresults09282018.xlsx",
                        sheet=1)
# データのサイズ
dim(abod.mfeat)
select(.data=abod.mfeat, LV5,LV4,LV3,LV2,LV1,LV41)

X=3 #1~5,41

  J=X 
  LVV=paste("LV",J,sep="")
  MRI=paste("MRI")
  file.name=sprintf("MRImachines of LV%01dABOD.pdf",J)
  
  # 14列目にABODLV%のスコアが記録されている
  head(abo)
  # スコアを抽出して数値に変換
  #abod.mfeat <- abod.mfeat %>% mutate(V14 = as.numericV14)
  # スコアのヒストグラムのプロット
  #aesの代わりにaes_を使うとx,y軸の変数を文字列で指定することができます
  #文字列はas.nameで囲いましょう
  #MRIの機種ごとの色分け
  p <- ggplot(data = abod.mfeat, aes_(x = as.name(LVV),fill=as.name('MRI'))) + geom_histogram(binwidth = 0.025) +
    scale_x_log10() + xlab("log10(ABOD scores)") + ylab("sample numbers") +
    theme_bw()+
    labs(title=sprintf("LV%01dABOD",J))+
  coord_cartesian(xlim=c(5e-7,2e-5))
  #coord_cartesian(xlim = NULL, ylim = NULL)
  
  #color変更
  #http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
  
  p=p + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
  
  print(p)
  
  pdf(file.name, width=10, height=10)
  plot(p)
  dev.off()
