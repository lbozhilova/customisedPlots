##############################################
##### Thesis GGtheme and colour palettes #####
##############################################

#----- Packages -----#
require("ggplot2")
require("ggthemes")
require("grid")
require("gridExtra")

#----- Theme -----#
themeThesis <- theme_minimal(base_size = 18) +
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), 
        legend.direction = "horizontal",
        legend.justification = 0.1, 
        legend.text = element_text(size = 18, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), 
        axis.title.y = element_text(vjust = 2), 
        axis.ticks.x = element_line(color="gray40", size=0.3),
        axis.ticks.y = element_blank(), 
        axis.line = element_line(color = "gray40", size = 0.3),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
  )

#----- Colour palette -----#
# Colours
thesisColours <- c(
  `blue` = "#0084ff",
  `teal` = "#44bec7",
  `yellow` = "#ffc300",
  `red` = "#fa3c4c",
  `pink` = "#d696bb",
  `coral` = "#cd5b45"
  )
# Function to extract colours
thesisCols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (thesisColours)
  thesisColours[cols]
}
# Palettes
thesisPalettes <- list(
  `main` = thesisCols("blue", "yellow", "red", "pink"),
  `mainShort` = thesisCols("blue", "yellow", "red"),
  `alt` = thesisCols("coral", "teal"),
  `all` = thesisCols("blue", "yellow", "red", "pink", "coral", "teal")
)
# Function to access palettes
# Example usage: thesisPal("alt")(10)
thesisPal <- function(palette="main", reverse=FALSE, ...){
  pal<- thesisPalettes[[palette]]
  if (reverse)
    pal <- rev(pal)
  colorRampPalette(pal, ...)
}

#----- Multiple plots in a single figure -----#
plotMultiple <- function(..., ncol=2){
  tgLetter <- function(Lt) textGrob(Lt, x=unit(0, "npc"), y=unit(1, "npc"), just=c("left","top"),
                                    gp=gpar(col="black", fontsize=18, fontfamily="Helvetica"))
  plotList <- list(...)
  plotList <- lapply(1:length(plotList), function(idx){
    arrangeGrob(plotList[[idx]], top=tgLetter(LETTERS[idx]))
  })
  grid.arrange(grobs=plotList, ncol=ncol)
}
