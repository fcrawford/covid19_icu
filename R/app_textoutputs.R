#' @export
text_hospital = function(doprotocols=0, dynamicModel=0, params, ...){
  
  
  
  
    hospital_input <- hospital_input_generation (dynamicModel=dynamicModel, params=params)
    
    if (dynamicModel==1){
      params$t= length(hospital_input)
    }
    
    if(doprotocols==0) {
      params$M_final=params$M
      params$L_final=params$L
    }
    
    floor_capacity_function <- floor_capacity_timeseries (params=params, doprotocols=doprotocols)
    
    
    icu_capacity_function <- icu_capacity_timeseries( params=params, doprotocols=doprotocols)   

    hospital <- hospital_queues(doprotocols=doprotocols,
                                dynamicModel=dynamicModel,
                                params=params,
                                floor_capacity_timeseries=floor_capacity_function,
                                icu_capacity_timeseries=icu_capacity_function,
                                ed_visits_timeseries= hospital_input)
    

    hospital$totaldead<- hospital$Dead_at_ICU + hospital$Dead_in_ED + hospital$Dead_on_Floor+ hospital$Dead_waiting_for_Floor+ hospital$Dead_waiting_for_ICU+ hospital$Dead_with_mild_symptoms
    hospital$totalWC<- hospital$WC1 + hospital$WC2 + hospital$WC3
    hospital$totalWF<- hospital$WF1 + hospital$WF2 + hospital$WF3
    
    #ICU queue
    ICUover = (hospital$WC1+hospital$WC2+hospital$WC3>=0.001)


    #floor queue
    floorover = (hospital$WF1+hospital$WF2+hospital$WF3>=1)

    #day that you run out of beds
    if(sum(floorover)>0){
      floorover<- min(which(floorover))
    } else{floorover = "No shortage"}

    if(sum(ICUover)>0){
      ICUover<- min(which(ICUover))
    } else{ICUover = "No shortage"}

    #max number that are in the queue at any given time
    ICU_WC = max(hospital$totalWC)
    floor_WF = max(hospital$totalWF)
    
    #initial number of patients in the ICU and on the floor
    pt_init = sum((hospital %>% select(C1, C2, C3, FL1, FL2, FL3))[1,])
    
    ###calculate initial utilization by percentage
    M_inf = hospital$CTotal[1] + max(hospital$Number_seen_at_ED)
    M_occupied_inf = params$M*params$M_occupied/(M_inf)
    L_inf = hospital$FTotal[1] + max(hospital$Number_seen_at_ED)
    L_occupied_inf = params$L*params$L_occupied/(L_inf)
    
    L_temp = params$L
    M_temp = params$M
    
    params_inf = params
    
    params_inf$M= M_inf
    params_inf$L= L_inf
    params_inf$M_occupied= M_occupied_inf;
    params_inf$L_occupied= L_occupied_inf;
    params_inf$M_final = params_inf$M
    params_inf$L_final = params_inf$L
    
    floor_capacity_function_inf <- floor_capacity_timeseries( params=params_inf, doprotocols=doprotocols)
    
    
    icu_capacity_function_inf <- icu_capacity_timeseries( params=params_inf, doprotocols=doprotocols)   
    
    
    ###calculate max utilization
    hospital_inf <- hospital_queues(doprotocols=0,
                                    dynamicModel=dynamicModel,
                                    params=params_inf,
                                    floor_capacity_timeseries=floor_capacity_function_inf,
                                    icu_capacity_timeseries=icu_capacity_function_inf,
                                    ed_visits_timeseries= hospital_input)
    
    if(max(hospital_inf$CTotal)-M_temp>0){
      C_needed = max(hospital_inf$CTotal) - M_temp
    } else {C_needed = 0}
    
    
    if(max(hospital_inf$FTotal)-L_temp>0){
      F_needed = max(hospital_inf$FTotal) - L_temp
    } else {F_needed = 0}
    


    text = data.frame(Variable = c("Total number of COVID19+ presentations to the health system",
                                    "Total deaths of COVID19+ patients",
                                    "Case fatality ratio for COVID19+ patients",
                                    "Deaths of COVID19+ patients in the ICU",
                                    "Deaths of COVID19+ patients on the floor",
                                    "Deaths of COVID19+ patients waiting for ICU beds",
                                    "Deaths of COVID19+ patients waiting for floor beds",
                                    "Days to floor overflow", 
                                   "Days to ICU overflow", 
                                   "Extra floor beds needed for COVID19+ patients", 
                                   "Extra ICU beds needed for COVID19+ patients"),
                      Value = c(ceiling(tail(hospital$Number_seen_at_ED, n=1)),
                                ceiling(tail(hospital$totaldead, n=1)),
                                paste(signif(tail(hospital$totaldead, n=1)/(pt_init + tail(hospital$Number_seen_at_ED, n=1)), digits=3)*100, "%"),
                                ceiling(tail(hospital$Dead_at_ICU, n=1)),
                                ceiling(tail(hospital$Dead_on_Floor, n=1)),
                                ceiling(tail(hospital$Dead_waiting_for_ICU,n=1)),
                                ceiling(tail(hospital$Dead_waiting_for_Floor, n=1)),
                                floorover, 
                                ICUover, 
                                format(ceiling(F_needed), scientific=FALSE), 
                                format(ceiling(C_needed), scientific = FALSE)))

}

####################################

#' @export
text_parameters = function( doprotocols, dynamicModel, params, ...){
  
  

df = data.frame(t=c("Time horizon", params$t),
                young=c("Proportion COVID+ admissions <18 years", params$young),
                medium=c("Proportion COVID+ admissions 18-65 years",params$medium),
                I_init=c("Initial infections per day", params$I_init),
                     I_final=c("Final infections per day", params$I_final),
                     distribution=c("ED presentation curve", params$distribution),
                     doublingtime=c("Doubling time (exponential growth)", params$doublingtime),
                     rampslope=c("Ramp slope (linear growth)", params$rampslope),
                     M=c("Initial ICU capacity", params$M),
                     L=c("Initial Floor capacity", params$L),
                     L_occupied=c("Initial Floor occupancy %", params$L_occupied),
                     M_occupied=c("Initial ICU occupancy %", params$M_occupied),
                     L_final=c("Target floor capacity", params$L_final),
                     Lramp=c("Floor ramp", paste(params$floorcapramp1,"--",params$floorcapramp2)),
                     M_final=c("Target ICU capacity", params$M_final),
                     Mramp=c("ICU ramp", paste(params$icucapramp1,"--",params$icucapramp2)),
                     avg_LOS_ICU=c("Average time in ICU for COVID+ patients", params$avg_LOS_ICU),
                     avg_LOS_Floor=c("Average time on floor for COVID+ patiens", params$avg_LOS_Floor),
                     p_death_ICU2=c("Probability of death in ICU, 18-65 years, given time in ICU", params$p_death_ICU2),
                     p_death_ICU3=c("Probability of death in ICU, 65+ years, given time in ICU", params$p_death_ICU3),
                     p_death_floor2=c("Probability of death on floor, 18-65 years, given time on floor", params$p_death_Floor2),
                     p_death_floor3=c("Probability of death on floor, 65+ years, given time on floor", params$p_death_Floor3))


df = t(df)


}





