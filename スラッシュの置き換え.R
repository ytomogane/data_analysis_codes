
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

#pathPrep()実行でクリップボードのスラッシュが変換される。Functionにはいっている。
#例；"C:\Users\yousu\Google_Drive\for_analyze_03152018\June2015.csv"
#実行後
#例；[1] "\"C:/Users/yousu/Google_Drive/for_analyze_03152018/June2015.csv\""クリップボード内にある。
#パスコピーpathPrep()実行、ペースト。これでいける

pathPrep()

