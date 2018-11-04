library(officer)
library(magrittr)
library(grid)
library(data.table)
gdxChoice <- "AfricanAgFutures"  # needs to be created above nutrientModFunctions.R
source("R/nutrientModFunctions.R")
imageFilePrefix <- paste0("graphics/", gdxChoice, "/final/")
dateNtime <- Sys.Date()
# see https://cran.r-project.org/web/packages/officer/vignettes/powerpoint.html for details on using officer to create powerpoints
presentationShape <- "wide" # or "tall"

my_pres.AfricanAgFutures <- copy(read_pptx("presentations/blank.pptx"))

 if (presentationShape %in% "wide") my_pres.AfricanAgFutures <- copy(read_pptx("presentations/blank1.pptx"))
graphsListHolder <- getNewestVersion(fileShortName = "graphsListHolder.var", directory = fileloc("gDir"), fileType = "rds")

# ph_with_text(type = "ctrTitle", str = "Comparing Nutrient Modeling Results") %>%
# add_slide(layout="Section Header", master="Office Theme")  %>%
# ph_with_text(type = "title", str = "Adequacy Ratios") %>%
# add_slide(layout="Title and Content", master="Office Theme")  %>%
# ph_with_img_at( src = img.file)

imageList <- names(graphsListHolder)

# pdfDimensions <- getNewestVersion("pdfDimensions", fileloc("gDir"))
# pdfDimensions[, width := as.numeric(width)]
# pdfDimensions[, height := as.numeric(height)]
# 
# pdfDimensions[, fileName := gsub(imageFilePrefix, "", fileName)]
# imageList.wb <- paste0("imageList_", gdxChoice, ".wb")
# pdfDimensions.wb <- pdfDimensions[fileName %in% get(imageList.wb)]
# 
# pdfDimensions.wb[, newOrder := match(fileName, get(imageList.wb))]
# setorder(pdfDimensions.wb, newOrder)
# pdfDimensions.wb[, newOrder := NULL]

# # add path back onto filename
# pdfDimensions.wb[, fileName := paste0(imageFilePrefix, fileName)]
# oldwd <- getwd()
# setwd("graphics/AfricanAgFutures")

my_pres.AfricanAgFutures <- add_slide(x = my_pres.AfricanAgFutures, layout='Title Slide', master='Office Theme')
contentString <- paste0("African Agricultural Futures")
titleContentString <- paste0("ph_with_text(x = my_pres.AfricanAgFutures, type = 'ctrTitle', str =  '", contentString, "')")
my_pres.AfricanAgFutures <- eval(parse(text = titleContentString))
subTitleText <- paste0("created ", Sys.Date())
my_pres.AfricanAgFutures <- ph_with_text(x = my_pres.AfricanAgFutures, type = "subTitle", str = subTitleText)

pageStart <- "add_slide(x = my_pres.AfricanAgFutures, layout='Title and Content', master='Office Theme') %>% "
titleListPre <-  "ph_with_text(type = 'title', str = '"
titleListPost <- "') %>% "
imageListPre <- paste0("ph_with_img(src = fileNameComplete, height = height, width = width, type = 'body')")
str1 <- "The title of each of the following slides is the name of the png file displayed in the slide." 
strList <- c(str1)
#my_pres.AfricanAgFutures <- eval(parse(text = titleSlide.wb))
my_pres.AfricanAgFutures <- add_slide(x = my_pres.AfricanAgFutures, layout="Title and Content", master="Office Theme")
my_pres.AfricanAgFutures <- ph_with_text(x = my_pres.AfricanAgFutures, type = "title", str = "File name explanation")
my_pres.AfricanAgFutures <- ph_with_ul(x = my_pres.AfricanAgFutures, type = "body", index = 1,
                         str_list = strList,
                         level_list = c(1) )
width = 7
height = 6
for (i in 3:length(imageList)) { # 3 to get rid of the legends
#  fileNameComplete <- grid.draw(graphsListHolder[[i]])
  fileNameComplete <- paste0(getwd(), "/", "graphics/AfricanAgFutures/" ,imageList[i], ".png")
  fileNameForHeader <- gsub("AfricanAgFutures_", "", imageList[i])
  fileNameForHeader <- gsub(".var", "", fileNameForHeader)
  cat("\nfileName:", fileNameComplete, "\nfileNameforHeader:", fileNameForHeader, "\nwidth:", width, "\nheight:", height)
  titleString <- paste0(titleListPre, fileNameForHeader, titleListPost)
  imageString <- imageListPre
  finalString <- paste0(pageStart, titleString, imageString)
  cat("\nfinalString:", finalString, "\n")
  my_pres.AfricanAgFutures <- eval(parse(text = finalString))
}
my_pres.AfricanAgFutures <- add_slide(x = my_pres.AfricanAgFutures, layout="Title and Content", master="Office Theme") %>%
  ph_with_text(type = 'title', str = 'The End')

# my_pres.AfricanAgFutures <- remove_slide(x = my_pres.AfricanAgFutures, index = 1)

if (presentationShape %in% "tall") targetDirNfileName.AfricanAgFutures <- paste0(getwd(),"/presentations/",gdxChoice, "/", "AfricanAgFuturesGraphics", ".pptx" )
if (presentationShape %in% "wide") targetDirNfileName.AfricanAgFutures <- paste0(getwd(),"/presentations/",gdxChoice, "/", "AfricanAgFuturesGraphics.wide", ".pptx" )
print(my_pres.AfricanAgFutures, target = targetDirNfileName.AfricanAgFutures)

#setwd(oldwd)

# knitr::kable(layout_summary(my_pres)
# knitr::kable(layout_properties ( x = my_pres, layout = "Title and Content",
#               master = "Office Theme" )
# knitr::kable(layout_properties ( x = my_pres, layout = "Section Header",
#               master = "Office Theme" )
# knitr::kable(layout_properties ( x = my_pres, layout = "Title Only",
#               master = "Office Theme" )



