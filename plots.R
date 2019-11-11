library(plotly)

#function to create plots

my_plot <- function(hist1, var1, color1, hist2, var2, color2,x) 
{
p <- plot_ly(alpha = 0.6) %>% 
     add_histogram(name = hist1,x =var1, color = I(color1)) %>% 
     add_histogram( name = hist2, x =var2,color = I(color2)) %>% 
     layout(barmode = "overlay", xaxis = list(title = x ), yaxis = list(title = "No. Of Schools") )
return(p)  
}


#absentiesm rates for different ethnicities
abs_WH_BL <- my_plot("White", PA_abs_rates$RATE_ABSENT_WH,"purple", "Black", PA_abs_rates$RATE_ABSENT_BL,"yellow","Absenteeism Rates")

abs_WH_HI <- my_plot("White", PA_abs_rates$RATE_ABSENT_WH,"purple", "Hispanic", PA_abs_rates$RATE_ABSENT_HI,"blue","Absenteeism Rates")

abs_WH_AS <- my_plot("White", PA_abs_rates$RATE_ABSENT_WH,"purple", "Asian", PA_abs_rates$RATE_ABSENT_AS,"green","Absenteeism Rates")

abs_BL_HI <- my_plot("Black", PA_abs_rates$RATE_ABSENT_BL,"yellow", "Hispanic", PA_abs_rates$RATE_ABSENT_HI,"blue","Absenteeism Rates")

abs_AS_HI <- my_plot("Asian", PA_abs_rates$RATE_ABSENT_AS,"green", "Hispanic", PA_abs_rates$RATE_ABSENT_HI,"blue","Absenteeism Rates")

abs_BL_AS <- my_plot("Black", PA_abs_rates$RATE_ABSENT_BL,"yellow", "Asian", PA_abs_rates$RATE_ABSENT_AS,"green","Absenteeism Rates")


#in school suspension (ISS) rates

iss_WH_BL <- my_plot("White", PA_iss_rates$RATE_ISS_WH,"purple", "Black", PA_iss_rates$RATE_ISS_BL,"yellow","In School Suspension(ISS) Rates")

iss_WH_HI <- my_plot("White", PA_iss_rates$RATE_ISS_WH,"purple", "Hispanic", PA_iss_rates$RATE_ISS_HI,"blue","In School Suspension(ISS) Rates")

iss_WH_AS <- my_plot("White", PA_iss_rates$RATE_ISS_WH,"purple", "Asian", PA_iss_rates$RATE_ISS_AS,"green","In School Suspension(ISS) Rates")

iss_BL_HI <- my_plot("Black", PA_iss_rates$RATE_ISS_BL,"yellow", "Hispanic", PA_iss_rates$RATE_ISS_HI,"blue","In School Suspension(ISS) Rates")

iss_AS_HI <- my_plot("Asian", PA_iss_rates$RATE_ISS_AS,"green", "Hispanic", PA_iss_rates$RATE_ISS_HI,"blue","In School Suspension(ISS) Rates")

iss_BL_AS <- my_plot("Black", PA_iss_rates$RATE_ISS_BL,"yellow", "Asian", PA_iss_rates$RATE_ISS_AS,"green","In School Suspension(ISS) Rates")



#bullying rates of different ethnicities

bully_WH_BL <- my_plot("White", PA_bully_rates$RATE_BULLY_WH,"purple", "Black", PA_bully_rates$RATE_BULLY_BL,"yellow","Bullying Rates")

bully_WH_HI <- my_plot("White", PA_bully_rates$RATE_BULLY_WH,"purple", "Hispanic", PA_bully_rates$RATE_BULLY_HI,"blue","Bullying Rates")

bully_WH_AS <- my_plot("White", PA_bully_rates$RATE_BULLY_WH,"purple", "Asian", PA_bully_rates$RATE_BULLY_AS,"green","Bullying Rates")

bully_BL_HI <- my_plot("Black", PA_bully_rates$RATE_BULLY_BL,"yellow", "Hispanic", PA_bully_rates$RATE_BULLY_HI,"blue","Bullying Rates")

bully_AS_HI <- my_plot("Asian", PA_bully_rates$RATE_BULLY_AS,"green", "Hispanic", PA_bully_rates$RATE_BULLY_HI,"blue","Bullying Rates")

bully_BL_AS <- my_plot("Black", PA_bully_rates$RATE_BULLY_BL,"yellow", "Asian", PA_bully_rates$RATE_BULLY_AS,"green","Bullying Rates")



#male and female 

bully_M_F <- my_plot("Male", PA_bully_rates$RATE_BULLY_M,"black", "Female", PA_bully_rates$RATE_BULLY_F,"red","Bullying Rates")

iss_M_F <- my_plot("Male", PA_iss_rates$RATE_ISS_M,"black", "Female", PA_iss_rates$RATE_ISS_F,"red","In School Suspension (ISS) Rates")

abs_M_F <- my_plot("Male", PA_abs_rates$RATE_ABSENT_M,"black", "Female", PA_abs_rates$RATE_ABSENT_F,"red","Absenteeism Rates")

