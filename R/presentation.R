library(officer)
library(magrittr)
library(data.table)
source("R/nutrientModFunctions.R")
gdxChoice <- getGdxChoice()
imageFilePrefix <- paste0("graphics/", gdxChoice, "/final/")
dateNtime <- Sys.Date()

# first do World Bank income aggregations
my_pres.wb <- copy(read_pptx("presentations/blank.pptx"))


# ph_with_text(type = "ctrTitle", str = "Comparing Nutrient Modeling Results") %>%
# add_slide(layout="Section Header", master="Office Theme")  %>%
# ph_with_text(type = "title", str = "Adequacy Ratios") %>%
# add_slide(layout="Title and Content", master="Office Theme")  %>%
# ph_with_img_at( src = img.file)

imageList_SSPs.wb <- c("fig1.budgetShare_WB.base.png", "fig1.budgetShare_WB.var.png", "fig1.budgetShare_WB.varFort.png",
                       "fig2.adequacy.macro_WB.base.png", "fig2.adequacy.macro_WB.var.png", "fig2.adequacy.macro_WB.varFort.png",
                       "fig3.AMDRhiLo_WB.base.png", "fig3.AMDRhiLo_WB.var.png", "fig3.AMDRhiLo_WB.varFort.png",
                       "fig4.1.adequacy.vits_WB.base.png", "fig4.1.adequacy.vits_WB.var.png", "fig4.1.adequacy.vits_WB.varFort.png",
                       "fig4.2.adequacy.vits_WB.base.png", "fig4.2.adequacy.vits_WB.var.png", "fig4.2.adequacy.vits_WB.varFort.png",
                       "fig4.3.adequacy.minrls_WB.base.png", "fig4.3.adequacy.minrls_WB.var.png", "fig4.3.adequacy.minrls_WB.varFort.png",
                       "fig5.compDINB_WB.base.png", "fig5.compDINB_WB.var.png", "fig5.compDINB_WB.varFort.png",
                       "fig6.facetMapRR_2010.base.png", "fig6.facetMapRR_2010.var.png", "fig6.facetMapRR_2010.varFort.png",
                       "fig7.facetMapRR_IncDelta.base.png", "fig7.facetMapRR_IncDelta.var.png", "fig7.facetMapRR_IncDelta.varFort.png",
                       "fig8.facetMapRR_CCDelta.base.png", "fig8.facetMapRR_CCDelta.var.png", "fig8.facetMapRR_CCDelta.varFort.png",
                       "figS1.foodavail.1_WB.base.png", "figS1.foodavail.1_WB.var.png", "figS1.foodavail.1_WB.varFort.png",
                       "figS1.foodavail.2_WB.base.png", "figS1.foodavail.2_WB.var.png", "figS1.foodavail.2_WB.varFort.png",
                       "figS1.foodavail.3_WB.base.png", "figS1.foodavail.3_WB.var.png", "figS1.foodavail.3_WB.varFort.png",
                       "figS3.nutavail.zinc.base.png", "figS3.nutavail.zinc.var.png", "figS3.nutavail.zinc.varFort.png",
                       "figS4.RaoNenergyShareNonStaples_WB.base.png", "figS4.RaoNenergyShareNonStaples_WB.var.png", "figS4.RaoNenergyShareNonStaples_WB.varFort.png",
                       "figS6.badRatios_WB.base.png", "figS6.badRatios_WB.var.png", "figS6.badRatios_WB.varFort.png")
# removes CC files and var and varFort Files
imageList_USAIDPrdNhance.wb <- c("fig1.budgetShare_WB.base.png",
                                  "fig2.adequacy.macro_WB.base.png",
                                  "fig3.AMDRhiLo_WB.base.png",
                                  "fig4.1.adequacy.vits_WB.base.png", "fig4.2.adequacy.vits_WB.base.png", "fig4.3.adequacy.minrls_WB.base.png",
                                  "fig5.compDINB_WB.base.png",
                                  "fig6.facetMapRR_2010.base.png",
                                  "fig7.facetMapRR_IncDelta.base.png",
                                  "figS1.foodavail.1_WB.base.png",  "figS1.foodavail.2_WB.base.png",  "figS1.foodavail.3_WB.base.png",
                                  "figS3.nutavail.zinc.base.png",
                                  "figS4.RaoNenergyShareNonStaples_WB.base.png",
                                  "figS6.badRatios_WB.base.png")

imageList_SSPs.tenregions <- c("figS8.foodavail.1_tenregions.base.png", "figS8.foodavail.1_tenregions.var.png", "figS8.foodavail.1_tenregions.varFort.png",
                               "figS8.foodavail.2_tenregions.base.png", "figS8.foodavail.2_tenregions.var.png", "figS8.foodavail.2_tenregions.varFort.png",
                               "figS8.foodavail.3_tenregions.base.png", "figS8.foodavail.3_tenregions.var.png", "figS8.foodavail.3_tenregions.varFort.png",
                               "figS10.1.adequacy.vits_tenregions.base.png", "figS10.1.adequacy.vits_tenregions.var.png", "figS10.1.adequacy.vits_tenregions.varFort.png",
                               "figS10.2.adequacy.vits_tenregions.base.png", "figS10.2.adequacy.vits_tenregions.var.png", "figS10.2.adequacy.vits_tenregions.varFort.png",
                               "figS10.3.adequacy.minrls_tenregions.base.png", "figS10.3.adequacy.minrls_tenregions.var.png", "figS10.3.adequacy.minrls_tenregions.varFort.png")
imageList_USAIDPrdNhance.tenregions <- c("figS8.foodavail.1_tenregions.base.png",
                                          "figS8.foodavail.2_tenregions.base.png",
                                          "figS8.foodavail.3_tenregions.base.png",
                                          "figS10.1.adequacy.vits_tenregions.base.png",
                                          "figS10.2.adequacy.vits_tenregions.base.png",
                                          "figS10.3.adequacy.minrls_tenregions.base.png",
                                          "figS10.4.adequacy.macro_tenregions.base.png")

pdfDimensions <- getNewestVersion("pdfDimensions", fileloc("gDir"))
pdfDimensions[, width := as.numeric(width)]
pdfDimensions[, height := as.numeric(height)]

pdfDimensions[, fileName := gsub(imageFilePrefix, "", fileName)]
imageList.wb <- paste0("imageList_", gdxChoice, ".wb")
pdfDimensions.wb <- pdfDimensions[fileName %in% get(imageList.wb)]

pdfDimensions.wb[, newOrder := match(fileName, get(imageList.wb))]
setorder(pdfDimensions.wb, newOrder)
pdfDimensions.wb[, newOrder := NULL]

# add path back onto filename
pdfDimensions.wb[, fileName := paste0(imageFilePrefix, fileName)]

# if (gdxChoice %in% "SSPs") imageList.wb <- imageList_SSPs.wb; imageList.tenregions <- imageList_SSPs.tenregions
# if (gdxChoice %in% "USAIDPrdNhance") imageList.wb <- imageList_USAIDPrdNhance.wb; imageList.tenregions <- imageList_USAIDPrdNhance.tenregions
my_pres.wb <- add_slide(x = my_pres.wb, layout='Title Slide', master='Office Theme')
contentString.wb <- paste0("Nutrient Modeling Results Comparison, World Bank aggregations for ", gdxChoice)
titleContentString.wb <- paste0("ph_with_text(x = my_pres.wb, type = 'ctrTitle', str =  '", contentString.wb, "')")
my_pres.wb <- eval(parse(text = titleContentString.wb))
subTitleText <- paste0("created ", Sys.Date())
my_pres.wb <- ph_with_text(x = my_pres.wb, type = "subTitle", str = subTitleText)


pageStart.wb <- "add_slide(x = my_pres.wb, layout='Title and Content', master='Office Theme') %>% "
titleListPre <-  "ph_with_text(type = 'title', str = '"
titleListPost <- "') %>% "
imageListPre.wb <- paste0("ph_with_img(src = fileName, height = height, width = width, type = 'body')")
str1 <- "The title of each of the following slides is the name of the png file displayed in the slide. The list below shows where the data come from."
str2 <- "Base - A 'base' file uses the original data set for nutrient composition."
str3 <- "Var - A file with 'var' in its name uses country-specific nutrient composition values for maize, rice, and wheat. This information is in countryCropVariety.xlsx"
str4 <- "VarFort - Results from a 'Var' file with 'Fort'ification of selected food items. This information is in '...fortValues_[date created].xlsx'"
strList <- c(str1, str2, str3, str4)
#my_pres.wb <- eval(parse(text = titleSlide.wb))
my_pres.wb <- add_slide(x = my_pres.wb, layout="Title and Content", master="Office Theme")
my_pres.wb <- ph_with_text(x = my_pres.wb, type = "title", str = "File name explanation")
my_pres.wb <- ph_with_ul(x = my_pres.wb, type = "body", index = 1,
                         str_list = strList,
                         level_list = c(1, 2, 2, 2) )

for (i in 1: length(get(imageList.wb))) {
  fileName <- pdfDimensions.wb$fileName[i]
  width <- pdfDimensions.wb$width[i]
  height <- pdfDimensions.wb$height[i]
  fileNameForHeader <- get(imageList.wb)[i]
  cat("\nfileName:", fileName, "fileNameforHeader", fileNameForHeader, "width:", width, "height:", height)
  titleString <- paste0(titleListPre, fileNameForHeader, titleListPost)
  imageString <- imageListPre.wb
  finalString <- paste0(pageStart.wb, titleString, imageString)
  cat("\nfinalString:", finalString, "\n")
  my_pres.wb <- eval(parse(text = finalString))
}
my_pres.wb <- add_slide(x = my_pres.wb, layout="Title and Content", master="Office Theme") %>%
  ph_with_text(type = 'title', str = 'The End')

# my_pres.wb <- remove_slide(x = my_pres.wb, index = 1)

targetDirNfileName.wb <- paste0("presentations/",gdxChoice, "/", "comparisons.WB", ".pptx" )
print(my_pres.wb, target = targetDirNfileName.wb)

# now work on ten regions -----
my_pres.tenregions <- copy(read_pptx("presentations/blank.pptx"))
imageList.tenregions <- paste0("imageList_", gdxChoice, ".tenregions")
pdfDimensions.tenregions <- pdfDimensions[fileName %in% get(imageList.tenregions)]
pdfDimensions.tenregions[, newOrder := match(fileName, get(imageList.tenregions))]
setorder(pdfDimensions.tenregions, newOrder)
pdfDimensions.tenregions[, newOrder := NULL]
pdfDimensions.tenregions[, fileName := paste0(imageFilePrefix, fileName)]

my_pres.tenregions <- add_slide(x = my_pres.tenregions, layout='Title Slide', master='Office Theme')
contentString.tenregions <- paste0("Nutrient Modeling Results Comparison, ten regions for ", gdxChoice)
titleContentString.tenregions <- paste0("ph_with_text(x = my_pres.tenregions, type = 'ctrTitle', str =  '", contentString.tenregions, "')")
my_pres.tenregions <- eval(parse(text = titleContentString.tenregions))
subTitleText <- paste0("created ", Sys.Date())
my_pres.tenregions <- ph_with_text(x = my_pres.tenregions, type = "subTitle", str = subTitleText)

pageStart.tenregions <- "add_slide(x = my_pres.tenregions, layout='Title and Content', master='Office Theme') %>% "
titleListPre <-  "ph_with_text(type = 'title', str = '"
titleListPost <- "') %>% "
imageListPre.tenregions <- paste0("ph_with_img(src = fileName, height = height, width = width, type = 'body')")
str1 <- "The title of each of the following slides is the name of the png file displayed in the slide. The list below shows where the data come from."
str2 <- "Base - A 'base' file uses the original data set for nutrient composition."
str3 <- "Var - A file with 'var' in its name uses country-specific nutrient composition values for maize, rice, and wheat. This information is in countryCropVariety.xlsx"
str4 <- "VarFort - Results from a 'Var' file with 'Fort'ification of selected food items. This information is in '...fortValues_[date created].xlsx'"
strList <- c(str1, str2, str3, str4)

#my_pres.tenregions <- eval(parse(text = titleSlide.tenregions))
my_pres.tenregions <- eval(parse(text = titleContentString.tenregions))
my_pres.tenregions <- add_slide(x = my_pres.tenregions, layout="Title and Content", master="Office Theme")
my_pres.tenregions <- ph_with_text(x = my_pres.tenregions, type = "title", str = "File name explanation")
my_pres.tenregions <- ph_with_ul(x = my_pres.tenregions, type = "body", index = 1,
                                 str_list = strList,
                                 level_list = c(1, 2, 2, 2) )

for (i in 1: length(get(imageList.tenregions))) {
  fileName <- pdfDimensions.tenregions$fileName[i]
  width <- pdfDimensions.tenregions$width[i]
  height <- pdfDimensions.tenregions$height[i]
  fileNameForHeader <- get(imageList.tenregions)[i]
  cat("\nfileName", fileName, "fileNameforHeader", fileNameForHeader, "width:", width, "height:", height)
  titleString <- paste0(titleListPre, fileNameForHeader, titleListPost)
  imageString <- imageListPre.tenregions
  finalString <- paste0(pageStart.tenregions, titleString, imageString)
  my_pres.tenregions <- eval(parse(text = finalString))

}
#my_pres.tenregions <- remove_slide(x = my_pres.tenregions, index = 1)

targetDirNfileName.tenregions <- paste0("presentations/",gdxChoice, "/", "comparisons.tenregions", ".pptx" )
print(my_pres.tenregions, target = targetDirNfileName.tenregions)

# knitr::kable(layout_summary(my_pres)
# knitr::kable(layout_properties ( x = my_pres, layout = "Title and Content",
#               master = "Office Theme" )
# knitr::kable(layout_properties ( x = my_pres, layout = "Section Header",
#               master = "Office Theme" )
# knitr::kable(layout_properties ( x = my_pres, layout = "Title Only",
#               master = "Office Theme" )



