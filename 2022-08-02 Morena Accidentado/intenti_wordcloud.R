library(wordcloud2)
library(webshot)
# figPath = system.file("examples/t.png",package = "wordcloud2")
a <- wordcloud2(demoFreq)
a
class(a)

htmlwidgets::saveWidget(a, "a.html")
webshot("a.html", "a1.png")


#Create Palette
redPalette <- c("#5c1010", "#6f0000", "#560d0d", "#c30101", "#940000")
redPalette

#Download images for plotting
url = "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/twitter_wordcloud/handmaiden.jpeg"
handmaiden <- "handmaiden.jpg"
download.file(url, handmaiden) # download file
url = "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/twitter_wordcloud/resistance.jpeg"
resistance <- "resistance.jpeg"
download.file(url, resistance) # download file

#plots
b <- wordcloud2(demoFreq, size=1.6, color=rep_len( redPalette, nrow(demoFreq) ) )
b <- wordcloud2(demoFreq, size=1.6, figPath = resistance, color=rep_len( redPalette, nrow(demoFreq) ) )
b <- wordcloud2(demoFreq, size=1.6, figPath = resistance, color="#B20000")

htmlwidgets::saveWidget(b, "a.html")
