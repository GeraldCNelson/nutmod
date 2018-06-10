library(officer)
library(magrittr)
source("R/nutrientModFunctions.R")
gdxChoice <- getGdxChoice()

my_pres.wb <- read_pptx("presentations/blank.pptx")
my_pres.tenregions <- read_pptx("presentations/blank.pptx")

# ph_with_text(type = "ctrTitle", str = "Comparing Nutrient Modeling Results") %>%
# add_slide(layout="Section Header", master="Office Theme")  %>%
# ph_with_text(type = "title", str = "Adequacy Ratios") %>%
# add_slide(layout="Title and Content", master="Office Theme")  %>%
# ph_with_img_at( src = img.file)

imageList_SSPs.wb <- c("fig1.budgetShare_WB.base.pdf", "fig1.budgetShare_WB.var.pdf", "fig1.budgetShare_WB.varFort.pdf",
                       "fig2.adequacy.macro_WB.base.pdf", "fig2.adequacy.macro_WB.var.pdf", "fig2.adequacy.macro_WB.varFort.pdf",
                       "fig3.AMDRhiLo_WB.base.pdf", "fig3.AMDRhiLo_WB.var.pdf", "fig3.AMDRhiLo_WB.varFort.pdf",
                       "fig4.1.adequacy.vits_WB.base.pdf", "fig4.1.adequacy.vits_WB.var.pdf", "fig4.1.adequacy.vits_WB.varFort.pdf",
                       "fig4.2.adequacy.vits_WB.base.pdf", "fig4.2.adequacy.vits_WB.var.pdf", "fig4.2.adequacy.vits_WB.varFort.pdf",
                       "fig4.3.adequacy.minrls_WB.base.pdf", "fig4.3.adequacy.minrls_WB.var.pdf", "fig4.3.adequacy.minrls_WB.varFort.pdf",
                       "fig5.compDINB_WB.base.pdf", "fig5.compDINB_WB.var.pdf", "fig5.compDINB_WB.varFort.pdf",
                       "fig6.facetMapRR_2010.base.pdf", "fig6.facetMapRR_2010.var.pdf", "fig6.facetMapRR_2010.varFort.pdf",
                       "fig7.facetMapRR_IncDelta.base.pdf", "fig7.facetMapRR_IncDelta.var.pdf", "fig7.facetMapRR_IncDelta.varFort.pdf",
                       "fig8.facetMapRR_CCDelta.base.pdf", "fig8.facetMapRR_CCDelta.var.pdf", "fig8.facetMapRR_CCDelta.varFort.pdf",
                       "FigS1.foodavail.1_WB.base.pdf", "FigS1.foodavail.1_WB.var.pdf", "FigS1.foodavail.1_WB.varFort.pdf",
                       "FigS1.foodavail.2_WB.base.pdf", "FigS1.foodavail.2_WB.var.pdf", "FigS1.foodavail.2_WB.varFort.pdf",
                       "FigS1.foodavail.3_WB.base.pdf", "FigS1.foodavail.3_WB.var.pdf", "FigS1.foodavail.3_WB.varFort.pdf",
                       "figS3.nutavail.zinc.base.pdf", "figS3.nutavail.zinc.var.pdf", "figS3.nutavail.zinc.varFort.pdf",
                       "figS4.RaoNenergyShareNonStaples_WB.base.pdf", "figS4.RaoNenergyShareNonStaples_WB.var.pdf", "figS4.RaoNenergyShareNonStaples_WB.varFort.pdf",
                       "figS6.badRatios_WB.base.pdf", "figS6.badRatios_WB.var.pdf", "figS6.badRatios_WB.varFort.pdf")
# removes CC files and var and varFort Files
imageList_USAIDPriorities.wb <- c("fig1.budgetShare_WB.base.pdf",
                                  "fig2.adequacy.macro_WB.base.pdf",
                                  "fig3.AMDRhiLo_WB.base.pdf",
                                  "fig4.1.adequacy.vits_WB.base.pdf",
                                  "fig4.2.adequacy.vits_WB.base.pdf",
                                  "fig4.3.adequacy.minrls_WB.base.pdf",
                                  "fig5.compDINB_WB.base.pdf",
                                  "fig6.facetMapRR_2010.base.pdf",
                                  "fig7.facetMapRR_IncDelta.base.pdf",
                                  "FigS1.foodavail.1_WB.base.pdf",
                                  "FigS1.foodavail.2_WB.base.pdf",
                                  "FigS1.foodavail.3_WB.base.pdf",
                                  "figS3.nutavail.zinc.base.pdf",
                                  "figS4.RaoNenergyShareNonStaples_WB.base.pdf",
                                  "figS6.badRatios_WB.base.pdf")

imageList_SSPs.tenregions <- c("figS8.foodavail.1_tenregions.base.pdf", "figS8.foodavail.1_tenregions.var.pdf", "figS8.foodavail.1_tenregions.varFort.pdf",
                               "figS8.foodavail.2_tenregions.base.pdf", "figS8.foodavail.2_tenregions.var.pdf", "figS8.foodavail.2_tenregions.varFort.pdf",
                               "figS8.foodavail.3_tenregions.base.pdf", "figS8.foodavail.3_tenregions.var.pdf", "figS8.foodavail.3_tenregions.varFort.pdf",
                               "figS10.1.adequacy.vits_tenregions.base.pdf", "figS10.1.adequacy.vits_tenregions.var.pdf", "figS10.1.adequacy.vits_tenregions.varFort.pdf",
                               "figS10.2.adequacy.vits_tenregions.base.pdf", "figS10.2.adequacy.vits_tenregions.var.pdf", "figS10.2.adequacy.vits_tenregions.varFort.pdf",
                               "figS10.3.adequacy.minrls_tenregions.base.pdf", "figS10.3.adequacy.minrls_tenregions.var.pdf", "figS10.3.adequacy.minrls_tenregions.varFort.pdf")
imageList_USAIDPriorities.tenregions <- c("figS8.foodavail.1_tenregions.base.pdf",
                                          "figS8.foodavail.2_tenregions.base.pdf",
                                          "figS8.foodavail.3_tenregions.base.pdf",
                                          "figS10.1.adequacy.vits_tenregions.base.pdf",
                                          "figS10.2.adequacy.vits_tenregions.base.pdf",
                                          "figS10.3.adequacy.minrls_tenregions.base.pdf",
                                          "figS10.4.adequacy.macro_tenregions.base.pdf")

if (gdxChoice %in% "SSPs") imageList.wb <- imageList_SSPs.wb; imageList.tenregions <- imageList_SSPs.tenregions
if (gdxChoice %in% "USAIDPriorities") imageList.wb <- imageList_USAIDPriorities.wb; imageList.tenregions <- imageList_USAIDPriorities.tenregions
titleSlide.wb <- "add_slide(x = my_pres.wb, layout='Title Slide', master='Office Theme')"
titleContentString.wb <- paste0('Nutrient Modeling Results Comparison, World Bank aggregations, created ', Sys.Date())
titleSlideContent.wb <- "ph_with_text(x = my_pres.wb, type = 'ctrTitle', str = 'Nutrient Modeling Results Comparison')"
pageStart.wb <- "add_slide(x = my_pres.wb, layout='Title and Content', master='Office Theme') %>% "
pageStart.tenregions <- "add_slide(x = my_pres.tenregions, layout='Title and Content', master='Office Theme') %>% "
titleListPre <-  "ph_with_text(type = 'title', str = '"
titleListPost <- "') %>% "
imageListPre <- paste0("ph_with_img(src = 'graphics/", gdxChoice, "/final/")
imageListPost <- "', type = 'body', width = NULL, height = NULL)"
str1 <- "The title of each of the following slides is the name of the pdf file displayed in the slide. The list below shows where the data come from."
str2 <- "Base - A 'base' file uses the original data set for nutrient composition."
str3 <- "Var - A file with 'var' in its name uses country-specific nutrient composition values for maize, rice, and wheat. This information is in countryCropVariety.xlsx"
str4 <- "VarFort - Results from a 'Var' file with 'Fort'ification of selected food items. This information is in '...fortValues_[date created].xlsx'"
strList <- c(str1, str2, str3, str4)
my_pres.wb <- eval(parse(text = titleSlide.wb))
my_pres.wb <- eval(parse(text = titleSlideContent.wb))
my_pres.wb <- add_slide(x = my_pres.wb, layout="Title and Content", master="Office Theme")
my_pres.wb <- ph_with_text(x = my_pres.wb, type = "title", str = "File name explanation")
my_pres.wb <- ph_with_ul(x = my_pres.wb, type = "body", index = 1,
                         str_list = strList,
                         level_list = c(1, 2, 2, 2) )


for (i in imageList.wb) {
  titleString <- paste0(titleListPre, i, titleListPost)
  imageString <- paste0(imageListPre, i, imageListPost)
  finalString <- paste0(pageStart.wb, titleString, imageString)
  my_pres.wb <- eval(parse(text = finalString))
}
my_pres.wb <- remove_slide(x = my_pres.wb, index = 1)

titleSlide.wb <- "add_slide(x = my_pres.wb, layout='Title Slide', master='Office Theme')"
titleContentString.tenregions <- paste0('Nutrient Modeling Results Comparison, ten regions, created', Sys.Date())
titleSlideContent.tenregions <- "ph_with_text(x = my_pres.tenregions, type = 'ctrTitle', str = 'Nutrient Modeling Results Comparison')"
my_pres.tenregions <- add_slide(x = my_pres.tenregions, layout="Title and Content", master="Office Theme")
my_pres.tenregions <- ph_with_text(x = my_pres.tenregions, type = "title", str = "File name explanation")
my_pres.tenregions <- ph_with_ul(x = my_pres.tenregions, type = "body", index = 1,
                                 str_list = strList,
                                 level_list = c(1, 2, 2, 2) )

for (i in imageList.tenregions) {
  titleString <- paste0(titleListPre, i, titleListPost)
  imageString <- paste0(imageListPre, i, imageListPost)
  finalString <- paste0(pageStart.tenregions, titleString, imageString)
  my_pres.tenregions <- eval(parse(text = finalString))
}
my_pres.tenregions <- remove_slide(x = my_pres.tenregions, index = 1)

targetDirNfileName.wb <- paste0("presentations/",gdxChoice, "/", "comparisons.WB", ".pptx" )
targetDirNfileName.tenregions <- paste0("presentations/",gdxChoice, "/", "comparisons.tenregions", ".pptx" )

print(my_pres.tenregions, target = targetDirNfileName.tenregions)
print(my_pres.wb, target = targetDirNfileName.wb)

# knitr::kable(layout_summary(my_pres)
# knitr::kable(layout_properties ( x = my_pres, layout = "Title and Content",
#               master = "Office Theme" )
# knitr::kable(layout_properties ( x = my_pres, layout = "Section Header",
#               master = "Office Theme" )
# knitr::kable(layout_properties ( x = my_pres, layout = "Title Only",
#               master = "Office Theme" )
