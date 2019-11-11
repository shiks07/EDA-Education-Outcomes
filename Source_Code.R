# Creating the data frame that contains the different rates for individual ethnicities 
# and creating the plotting function

library(ggplot2)
library(dplyr)
library(tidyr)
library(naniar)
library(plotly)
library(corrplot)

#importing the data frame
schoolData <- read.csv(file = "CRDC 2015-16 School Data.csv")
PA <- subset(schoolData, schoolData$LEA_STATE == "PA")
PA$KEY <- paste(PA$LEAID, PA$SCHID) 

#cleaning the data
PA <- replace_with_na_all(data = PA, condition = ~.x %in% c(-9, -7, -5, -2)) 

PA <- subset(PA, SCH_FTETEACH_TOT > 1)

PA <- subset(PA, SCH_SAL_TOTPERS_WFED < 100000000)
PA <- subset(PA, SCH_NPE_WFED < 100000000)


## Create fields for school characteristics:

PA$SAL_PER_TEACH <- with(PA, SCH_SAL_TEACH_WOFED / SCH_FTE_TEACH_WOFED)

PA$RATE_LEP <- with(PA, ifelse((TOT_ENR_M + TOT_ENR_F)>(SCH_ENR_LEP_M + SCH_ENR_LEP_F),
                               (SCH_ENR_LEP_M + SCH_ENR_LEP_F)/(TOT_ENR_M + TOT_ENR_F), NA))

PA$STU_TEA_RATIO <- with(PA, (TOT_ENR_M + TOT_ENR_F) / SCH_FTETEACH_TOT)

PA$TEA_ABSENT <- with(PA, ifelse(SCH_FTETEACH_TOT>SCH_FTETEACH_ABSENT, 
                                 SCH_FTETEACH_ABSENT / SCH_FTETEACH_TOT, NA))

PA$EXP_STU <- with(PA, (SCH_SAL_TOTPERS_WFED + SCH_NPE_WFED) / (TOT_ENR_M + TOT_ENR_F))

## Create fields for overall rates:
PA$RATE_ABSENT <- with(PA, ifelse((TOT_ENR_M + TOT_ENR_F)>=(TOT_ABSENT_M + TOT_ABSENT_F),
                                  (TOT_ABSENT_M + TOT_ABSENT_F)/(TOT_ENR_M + TOT_ENR_F), NA))

PA$RATE_ISS <- with(PA, ifelse((TOT_ENR_M + TOT_ENR_F)>=(TOT_DISCWODIS_ISS_M + TOT_DISCWODIS_ISS_F),
                               (TOT_DISCWODIS_ISS_M + TOT_DISCWODIS_ISS_F)/(TOT_ENR_M + TOT_ENR_F), NA))

PA$RATE_BULLY <- with(PA, ifelse((TOT_ENR_M + TOT_ENR_F)>=(TOT_HBREPORTED_RAC_M + TOT_HBREPORTED_RAC_F),
                                 (TOT_HBREPORTED_RAC_M + TOT_HBREPORTED_RAC_F)/(TOT_ENR_M + TOT_ENR_F), NA))

## Create data frame with school characteristics and overall rates:
PA_compare <- subset(PA, select = 
                       c(SAL_PER_TEACH, RATE_LEP, STU_TEA_RATIO, TEA_ABSENT, EXP_STU,
                         RATE_ABSENT, RATE_ISS, RATE_BULLY))


## create correlation matrix for listed variables, using only complete observations without NAs
corr_matrix <- cor(PA_compare, use = "complete.obs")


## create a visualization of the correlation matrix

corrplot(corr_matrix,tl.col = "black", method = "color", order = "hclust", addCoef.col = "black",
         sig.level   = 0.01, insig = "blank",type = "upper", diag = FALSE)


## Create rates of each indicator by ethnicity and gender, 
## where there are more than 5 students in each subgroup and 
##the number enrolled is greater than the numerator 
## (to eliminate misreported data with rates over 100%):

## Enrollment by ethnicity
PA <- PA %>% mutate(SCH_ENR_HI = SCH_ENR_HI_M + SCH_ENR_HI_F)
PA <- PA %>% mutate(SCH_ENR_BL = SCH_ENR_BL_M + SCH_ENR_BL_F)
PA <- PA %>% mutate(SCH_ENR_WH = SCH_ENR_WH_M + SCH_ENR_WH_F)
PA <- PA %>% mutate(SCH_ENR_AS = SCH_ENR_AS_M + SCH_ENR_AS_F)

## Bullying rates
PA <- PA %>% mutate(SCH_BULLY_HI = SCH_HBREPORTED_RAC_HI_M + SCH_HBREPORTED_RAC_HI_F)
PA <- PA %>% mutate(SCH_BULLY_BL = SCH_HBREPORTED_RAC_BL_M + SCH_HBREPORTED_RAC_BL_F)
PA <- PA %>% mutate(SCH_BULLY_WH = SCH_HBREPORTED_RAC_WH_M + SCH_HBREPORTED_RAC_WH_F)
PA <- PA %>% mutate(SCH_BULLY_AS = SCH_HBREPORTED_RAC_AS_M + SCH_HBREPORTED_RAC_AS_F)

PA <- PA %>% mutate(RATE_BULLY_HI = ifelse(SCH_ENR_HI>=SCH_BULLY_HI & SCH_ENR_HI>=5, 
                                           SCH_BULLY_HI/SCH_ENR_HI, NA))
PA <- PA %>% mutate(RATE_BULLY_BL = ifelse(SCH_ENR_BL>=SCH_BULLY_BL & SCH_ENR_BL>=5, 
                                           SCH_BULLY_BL/SCH_ENR_BL, NA))
PA <- PA %>% mutate(RATE_BULLY_WH = ifelse(SCH_ENR_WH>=SCH_BULLY_WH & SCH_ENR_WH>=5, 
                                           SCH_BULLY_WH/SCH_ENR_WH, NA))
PA <- PA %>% mutate(RATE_BULLY_AS = ifelse(SCH_ENR_AS>=SCH_BULLY_AS & SCH_ENR_AS>=5, 
                                           SCH_BULLY_AS/SCH_ENR_AS, NA))
PA <- PA %>% mutate(RATE_BULLY_M = ifelse(TOT_ENR_M>=TOT_HBREPORTED_SEX_M & TOT_ENR_M>=5, 
                                          TOT_HBREPORTED_SEX_M/TOT_ENR_M, NA))
PA <- PA %>% mutate(RATE_BULLY_F = ifelse(TOT_ENR_F>TOT_HBREPORTED_SEX_F & TOT_ENR_F>5, 
                                          TOT_HBREPORTED_SEX_F/TOT_ENR_F, NA))

## ISS rates
PA <- PA %>% mutate(SCH_ISS_HI = SCH_DISCWODIS_ISS_HI_M + SCH_DISCWODIS_ISS_HI_F)
PA <- PA %>% mutate(SCH_ISS_BL = SCH_DISCWODIS_ISS_BL_M + SCH_DISCWODIS_ISS_BL_F)
PA <- PA %>% mutate(SCH_ISS_WH = SCH_DISCWODIS_ISS_WH_M + SCH_DISCWODIS_ISS_WH_F)
PA <- PA %>% mutate(SCH_ISS_AS = SCH_DISCWODIS_ISS_AS_M + SCH_DISCWODIS_ISS_AS_F)

PA <- PA %>% mutate(RATE_ISS_HI = ifelse(SCH_ENR_HI>=SCH_ISS_HI & SCH_ENR_HI>=5, 
                                         SCH_ISS_HI/SCH_ENR_HI, NA))
PA <- PA %>% mutate(RATE_ISS_BL = ifelse(SCH_ENR_BL>=SCH_ISS_BL & SCH_ENR_BL>=5, 
                                         SCH_ISS_BL/SCH_ENR_BL, NA))
PA <- PA %>% mutate(RATE_ISS_WH = ifelse(SCH_ENR_WH>=SCH_ISS_WH & SCH_ENR_WH>=5, 
                                         SCH_ISS_WH/SCH_ENR_WH, NA))
PA <- PA %>% mutate(RATE_ISS_AS = ifelse(SCH_ENR_AS>=SCH_ISS_AS & SCH_ENR_AS>=5, 
                                         SCH_ISS_AS/SCH_ENR_AS, NA))
PA <- PA %>% mutate(RATE_ISS_M = ifelse(TOT_ENR_M>=TOT_DISCWODIS_ISS_M & TOT_ENR_M>=5, 
                                        TOT_DISCWODIS_ISS_M/TOT_ENR_M, NA))
PA <- PA %>% mutate(RATE_ISS_F = ifelse(TOT_ENR_F>TOT_DISCWODIS_ISS_F & TOT_ENR_F>5, 
                                        TOT_DISCWODIS_ISS_F/TOT_ENR_F, NA))
## Absent rates
PA <- PA %>% mutate(SCH_ABSENT_HI = SCH_ABSENT_HI_M + SCH_ABSENT_HI_F)
PA <- PA %>% mutate(SCH_ABSENT_BL = SCH_ABSENT_BL_M + SCH_ABSENT_BL_F)
PA <- PA %>% mutate(SCH_ABSENT_WH = SCH_ABSENT_WH_M + SCH_ABSENT_WH_F)
PA <- PA %>% mutate(SCH_ABSENT_AS = SCH_ABSENT_AS_M + SCH_ABSENT_AS_F)

PA <- PA %>% mutate(RATE_ABSENT_HI = ifelse(SCH_ENR_HI>=SCH_ABSENT_HI & SCH_ENR_HI>=5, 
                                            SCH_ABSENT_HI/SCH_ENR_HI, NA))
PA <- PA %>% mutate(RATE_ABSENT_BL = ifelse(SCH_ENR_BL>=SCH_ABSENT_BL & SCH_ENR_BL>=5, 
                                            SCH_ABSENT_BL/SCH_ENR_BL, NA))
PA <- PA %>% mutate(RATE_ABSENT_WH = ifelse(SCH_ENR_WH>=SCH_ABSENT_WH & SCH_ENR_WH>=5, 
                                            SCH_ABSENT_WH/SCH_ENR_WH, NA))
PA <- PA %>% mutate(RATE_ABSENT_AS = ifelse(SCH_ENR_AS>=SCH_ABSENT_AS & SCH_ENR_AS>=5, 
                                            SCH_ABSENT_AS/SCH_ENR_AS, NA))
PA <- PA %>% mutate(RATE_ABSENT_M = ifelse(TOT_ENR_M>=TOT_ABSENT_M & TOT_ENR_M>=5, 
                                           TOT_ABSENT_M/TOT_ENR_M, NA))
PA <- PA %>% mutate(RATE_ABSENT_F = ifelse(TOT_ENR_F>=TOT_ABSENT_F & TOT_ENR_F>=5, 
                                           TOT_ABSENT_F/TOT_ENR_F, NA))

PA_bully_rates <- subset(PA, select = c(KEY, RATE_BULLY_HI, RATE_BULLY_BL, RATE_BULLY_WH, RATE_BULLY_AS,
                                        RATE_BULLY_M, RATE_BULLY_F))
PA_iss_rates <- subset(PA, select = c(KEY, RATE_ISS_HI, RATE_ISS_BL, RATE_ISS_WH, RATE_ISS_AS,
                                      RATE_ISS_M, RATE_ISS_F))
PA_abs_rates <- subset(PA, select = c(KEY, RATE_ABSENT_HI, RATE_ABSENT_BL, RATE_ABSENT_WH, RATE_ABSENT_AS,
                                      RATE_ABSENT_M, RATE_ABSENT_F))

## Keep rows with fewer than 3 NAs (at least 2 ethnicities to compare):
PA_bully_rates <- PA_bully_rates[rowSums(is.na(PA_bully_rates)) < 3, ]
PA_iss_rates <- PA_iss_rates[rowSums(is.na(PA_iss_rates)) < 3, ]
PA_abs_rates <- PA_abs_rates[rowSums(is.na(PA_abs_rates)) < 3, ]

## Data frame with sums of each ethnicity
enroll <- subset(PA, select = c(SCH_ENR_HI, SCH_ENR_BL, SCH_ENR_WH, SCH_ENR_AS, TOT_ENR_M, TOT_ENR_F))
enroll_sums <- sapply(enroll, sum)

## combine rates into one data frame
PA_rates <- merge(merge(PA_bully_rates, PA_iss_rates, by="KEY", all.x=TRUE, all.Y=TRUE), 
                  PA_abs_rates, by="KEY", all.x=TRUE, all.y=TRUE)


#function to create plots


my_plot <- function(hist1, var1, color1, hist2, var2, color2,x) 
{
  p <- plot_ly(alpha = 0.6) %>% 
    add_histogram(name = hist1,x =var1, color = I(color1)) %>% 
    add_histogram( name = hist2, x =var2,color = I(color2)) %>% 
    layout(barmode = "overlay", xaxis = list(title = x ), yaxis = list(title = "No. Of Schools") )
  return(p)  
}
