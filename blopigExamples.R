#-- Load Theme
source("masterTheme.R")

#-- Data
snacks <- rpois(40, 7)
words <- rnorm(40, snacks*100, 250)
isGood <- words>=750
Lyuba <- data.frame(snacks, words, isGood)

#-- A basic plot
p <- ggplot(Lyuba, aes(x=snacks, y=words)) + themeThesis

#-- Different aesthetics
p1a <- p + geom_point(aes(shape=isGood), size=3)
p1b <- p + geom_point(aes(colour=isGood), size=3)
plotMultiple(p1a, p1b, ncol=2)

#-- Different geometries
p2a <- p +
  geom_point(size=3) +
  scale_y_continuous(limits=c(-250, 2000))
p2b <- p +
  geom_smooth(size=3) +
  scale_y_continuous(limits=c(-250, 2000))
p2c <- p +
  geom_smooth(size=3) +
  geom_point(size=3) +
  scale_y_continuous(limits=c(-250, 2000))
plotMultiple(p2a, p2b, p2c, ncol=3)

#-- Different themes
p <- ggplot(Lyuba, aes(x=snacks, y=words, colour=isGood)) + geom_point(size=3)
p3a <- p + themeThesis
p3b <- p + theme_economist_white()
p3c <- p + theme_dark()
plotMultiple(p3a, p3b, p3c, ncol=3)

#-- Thesis figure
p <- ggplot(Lyuba, aes(x=snacks, y=words, colour=isGood)) + themeThesis
p + geom_point(size=3) +
  scale_x_continuous("Snacks eaten", limits=c(0, 15), breaks=seq(0, 15, 5)) +
  scale_y_continuous("Words written", limits=c(0, 2000), breaks=seq(0, 2000, 500)) +
  ggtitle("Daily productivity") +
  scale_color_manual("Day", labels=c("Bad", "Good"), values=unname(thesisCols("blue", "red")))

                     