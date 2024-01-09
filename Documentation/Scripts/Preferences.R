
################################################################################
#------------------------------------------------------------------------------#
#   PROJECT PREFERENCES                                                        #
#------------------------------------------------------------------------------#
################################################################################


options(digits = 15)

vc_MainSubgroups <- c("Cancer without HIV" = "CancerWithoutHIV",
                      "Cancer with HIV" = "CancerWithHIV",
                      "HIV without Cancer" = "HIVWithoutCancer")




# palette_CCP_Categorical <- qualitative_hcl(n = 5,
#                                            h = c(0, 212),
#                                            c = 97,
#                                            l = 59)

color_LightGrey <- "#EDEDED"
color_MediumGrey <- "#D0D0D0"
color_DarkGrey <- "#595959"


color_Primary <- "#054996"
color_PrimaryLight <- "#05499650"
color_Secondary <- "#8e1e39"
color_SecondaryLight <- "#8e1e3950"
color_Tertiary <- "#2B8C88"
color_TertiaryLight <- "#2B8C8850"
color_Accent <- "#960551"
color_AccentLight <- "#96055150"

color_Purple <- "#7f5cb7"
color_Yellow <- "#f9f871"

color_CancerOnly <- color_Primary
color_HIVCancer <- color_Secondary
color_HIVOnly <- color_DarkGrey

vc_FillPalette_Subgroup <- c("Cancer without HIV" = color_CancerOnly,
                             "Cancer with HIV" = color_HIVCancer,
                             "HIV without Cancer" = color_HIVOnly)

vc_AlphaPalette_2 <- c(0.5, 0.9)
vc_AlphaPalette_3 <- c(0.2, 0.5, 0.9)
vc_AlphaPalette_4 <- c(0.3, 0.5, 0.7, 0.9)
vc_AlphaPalette_5 <- c(0.4, 0.5, 0.6, 0.7, 0.8)

