library(caTools)
library(cowplot)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(latex2exp)
library(openxlsx)
library(progress)
library(Rcpp)
library(reshape)
library(reshape2)
library(tidyr)
library(zoo)



plot_hospital<- function(initial_report= 1000,
                           final_report = 10000,
                           distribution= "ramp",
                           young=.24,
                           medium=.6){
  
      hospital <- hospital_queues(initial_report=initial_report,
                                  final_report = final_report,
                                  distribution= distribution,
                                  young=.24,
                                  medium=.6)
      
      hospital$totaldead<- hospital$Dead_at_ICU + hospital$Dead_in_ED + hospital$Dead_on_Floor+ hospital$Dead_waiting_for_Floor+ hospital$Dead_waiting_for_ICU+ hospital$Dead_with_mild_symptoms
      hospital$totalWC<- hospital$WC1 + hospital$WC2 + hospital$WC3
      hospital$totalWF<- hospital$WF1 + hospital$WF2 + hospital$WF3
      
      hospital_melt<- hospital %>% gather(variable, value, -time);  
      
  
      p1 <-ggplot(hospital,
                  aes(x=time, y=reports))+
        geom_bar(size=1.5, stat="identity")+
      theme_bw(base_size=16) +
        labs(x="Time (Day)", y="Patients")+
        ggtitle("Initial emergency department visits")+
        theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))      #geom_line(aes(y=avg_alpha_min, x=as.Date(time, origin = "1900-01-01")), size=1.5, color="red", linetype = "twodash")

      
      p2 <-ggplot(hospital_melt,
                  aes(x=time,y=value, color=variable))+
        geom_line(data= hospital_melt[hospital_melt$variable %in% c("Number_seen_at_ED", "totaldead"),],size=1.5)+
        theme_bw(base_size=16) +
        scale_color_manual( name="Legend", values=c("black", "red"), labels=c("Number_seen_at_ED"="ED throughput", "totaldead"="Deaths"))+
        labs(x="Time (Day)", y="Patients")+
        ggtitle("Emergency department cumulative triages and deaths")+
        theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))
       
      p3 <-ggplot(hospital_melt,
                  aes(x=time,y=value, fill=variable))+
        geom_area(data= hospital_melt[hospital_melt$variable %in% c("Dead_at_ICU", "Dead_waiting_for_ICU", "Dead_on_Floor", "Dead_waiting_for_Floor", "Dead_with_mild_symptoms", "Dead_in_ED"),],size=1.5)+
        theme_bw(base_size=16)+
        scale_fill_manual( name="Legend", values=(c("black", "yellow", "red",  "pink", "grey", "orange")), labels=c("Dead_at_ICU"="In ICU", "Dead_waiting_for_ICU"="Waiting for ICU beds", "Dead_on_Floor"= "On floor", "Dead_waiting_for_Floor"="Waiting for floor beds", "Dead_with_mild_symptoms"="Post discharge from ED", "Dead_in_ED"="In ED"))+
        labs(x="Time (Day)", y="Patients")+
        ggtitle("Deaths by location")+
        theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) 
  
      
      p4 <-ggplot(hospital_melt,
                  aes(x=time,y=value, color=variable))+
        geom_line(data= hospital_melt[hospital_melt$variable %in% c("CTotal", "FTotal", "totalWC", "totalWF"),],size=1.5)+
        theme_bw(base_size=16) +
        scale_color_manual( name="Legend", values=c("black", "red", "grey", "pink"), labels=c("CTotal"="In ICU", "FTotal"= "On floor", "totalWC" ="Waiting for ICU beds", "totalWF"="Waiting for floor beds"))+
        labs(x="Time (Day)", y="Patients")+
        ggtitle("ICU and floor utilization and queues")+
        theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))
      
      
      plot_grid(p1, p2,p3,p4, nrow=2, ncol=2, labels=c('A', 'B', 'C', 'D'), align="hv")

}
