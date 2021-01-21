##########################
# Clean data for FIGURE 3
##########################
### Clean finalMatrix
fittedCasesPlot_data <- subset(finalMatrix, select=c(state,population,
                                                     corona_cases_apr,
                                                     corona_cases_jul,
                                                     corona_deaths_apr_fit,
                                                     corona_deaths_jul_fit))
fittedCasesPlot_data <- gather(fittedCasesPlot_data,Label1,corona_cases,corona_cases_apr:corona_cases_jul,factor_key=TRUE)
fittedCasesPlot_data <- gather(fittedCasesPlot_data,Label2,corona_deaths,corona_deaths_apr_fit:corona_deaths_jul_fit,factor_key=TRUE)
fittedCasesPlot_data <- subset(fittedCasesPlot_data,
                               (Label1=="corona_cases_jul" & 
                                  Label2=="corona_deaths_jul_fit") |
                                 (Label1=="corona_cases_apr" & 
                                    Label2=="corona_deaths_apr_fit"))
fittedCasesPlot_data <- subset(fittedCasesPlot_data, 
                               state!="District of Columbia" & 
                                 state!="Hawaii" &
                                 state!="Alasks")