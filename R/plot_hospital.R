
#' @export
plot_hospital<- function( doprotocols=0, dynamicModel=0, params, ...){
  
          hospital_input <- hospital_input_generation(dynamicModel=dynamicModel, params=params)
          
          if (dynamicModel==1){
            params$t= length(hospital_input)
          }
          
          
          
          if(doprotocols== 0 ) {
            params$M_final=params$M
            params$L_final=params$L
          }
          

          
          floor_capacity_function <- floor_capacity_timeseries(params=params, doprotocols)
            
            
          icu_capacity_function <- icu_capacity_timeseries( params=params, doprotocols)   

          hospital <- hospital_queues(params=params,
                                      doprotocols=doprotocols,
                                      floor_capacity_timeseries=floor_capacity_function,
                                      icu_capacity_timeseries=icu_capacity_function,
                                      ed_visits_timeseries=hospital_input
                                      )
      


      hospital$totaldead<- hospital$Dead_at_ICU + hospital$Dead_in_ED + hospital$Dead_on_Floor+ hospital$Dead_waiting_for_Floor+ hospital$Dead_waiting_for_ICU+ hospital$Dead_with_mild_symptoms
      hospital$totalWC<- hospital$WC1 + hospital$WC2 + hospital$WC3
      hospital$totalWF<- hospital$WF1 + hospital$WF2 + hospital$WF3
      

      
      hospital_melt<- hospital %>% gather(variable, value, -time);  
      
  
      p1 <-ggplot(hospital,
                  aes(x=time, y=reports))+
        geom_bar(size=1.5, stat="identity")+
      theme_bw(base_size=14) +
        labs(x="Time (Day)", y="Patients")+
        ggtitle("COVID19+ presentations to the health system")+
        theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))      
      
     
      
      p2 <-ggplot(hospital_melt,
                  aes(x=time,y=value, color=variable))+
        geom_line(data= hospital_melt[hospital_melt$variable %in% c("Number_seen_at_ED", "totaldead"),],size=1.5)+
        theme_bw(base_size=14) +
        scale_color_manual( name=element_blank(), values=c("black", "red"), labels=c("Number_seen_at_ED"="ED throughput", "totaldead"="Deaths"))+
        labs(x="Time (Day)", y="Patients")+
        ggtitle("Cumulative COVID19+ presentations and deaths")+
        theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),  legend.position = c(0.25, 0.75), legend.text=element_text(size=11),    legend.title=element_text(size=8),legend.background = element_rect(fill="transparent"))
       
      p3 <-ggplot(hospital_melt,
                  aes(x=time,y=value, fill=variable))+
        geom_area(data= hospital_melt[hospital_melt$variable %in% c("Dead_at_ICU", "Dead_waiting_for_ICU", "Dead_on_Floor", "Dead_waiting_for_Floor", "Dead_with_mild_symptoms", "Dead_in_ED"),],size=1.5)+
        theme_bw(base_size=14)+
        scale_fill_manual( name=element_blank(), values=(c("black", "yellow", "red",  "pink", "grey", "orange")), labels=c("Dead_at_ICU"="In ICU", "Dead_waiting_for_ICU"="Waiting for ICU beds", "Dead_on_Floor"= "On floor", "Dead_waiting_for_Floor"="Waiting for floor beds", "Dead_with_mild_symptoms"="Post discharge from ED", "Dead_in_ED"="In ED"))+
        labs(x="Time (Day)", y="Patients")+
        ggtitle("Cumulative deaths of COVID19+ patients by location")+
        theme(panel.border = element_blank(), axis.line = element_line(colour = "black"), legend.position = c(0.25, 0.65), legend.text=element_text(size=11),    legend.title=element_text(size=8),legend.background = element_rect(fill="transparent")) 
  
      
      p4 <-ggplot(hospital_melt,
                  aes(x=time,y=value, color=variable))+
        geom_line(data= hospital_melt[hospital_melt$variable %in% c("CTotal", "FTotal", "totalWC", "totalWF"),],size=1.5)+
        theme_bw(base_size=14) +
        scale_color_manual( name=element_blank(), values=c("black", "red", "grey", "pink"), labels=c("CTotal"="In ICU", "FTotal"= "On floor", "totalWC" ="Waiting for ICU beds", "totalWF"="Waiting for floor beds"))+
        labs(x="Time (Day)", y="Patients")+
        ggtitle("ICU and floor utilization by COVID19+ patients")+
        theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),  legend.position = c(0.25, 0.85), legend.text=element_text(size=11),    legend.title=element_text(size=8),legend.background = element_rect(fill="transparent"))+
        #geom_hline(yintercept=M, linetype="dashed", color = "black", size=1.5)+
        #geom_hline(yintercept=L, linetype="dashed", color = "red", size=1.5)
        geom_line(data= hospital_melt[hospital_melt$variable == "capacity_L",],size=1.5, linetype="dashed", color = "orangered4", alpha=0.9)+
        geom_line(data= hospital_melt[hospital_melt$variable == "capacity_M",],size=1.5, linetype="dashed",  color = "gray41", alpha=0.9)
      ### determine when the hospital exceeds capacity
      
      #ICU queue 
      
      ICUover = (hospital$WC1+hospital$WC2+hospital$WC3>=0.001)
      
      
      #floor queue
      
      floorover = (hospital$WF1+hospital$WF2+hospital$WF3>=1)
      
      
      if(sum(floorover)>0){
        floorover<- min(which(floorover))
        p4 <- p4 +annotate(geom="label", x=floorover, y=hospital$capacity_L[floorover], label=paste("Day", as.character(floorover)), size=4, color="red")
      }

      if(sum(ICUover)>0){
        ICUover<- min(which(ICUover))
        p4 <- p4 +annotate(geom="label", x=ICUover, y=hospital$capacity_M[ICUover], label=paste("Day", as.character(ICUover)), size=4)
      }
      list(p1, p2, p3, p4, ICUover, floorover)
      

}
