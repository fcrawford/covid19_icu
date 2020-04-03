#' @export

text_hospital = function(t,
                                       young,
                                       medium,
                                       #######################
                                       I_init,
                                       I_final,
                                       distribution,
                                       doublingtime,
                                       rampslope,
                                       #######################
                                       M,
                                       L,
                                       L_occupied,
                                       M_occupied,
                                       Lfinal,
                                       Lramp,
                                       Mfinal,
                                       Mramp,
                                       ######################
                                       avg_LOS_ICU,
                                       avg_LOS_Floor,
                                       #####################
                                       p_death_ICU2,
                                       p_death_ICU3,
                                       p_death_floor2,
                                       p_death_floor3,
                                       #####################
                                       slope,
                                       doprotocols=0,
                                       ...){

    hospital <- hospital_queues(t=t,
                                              young=young,
                                              medium=medium,
                                              #######################
                                              I_init=I_init,
                                              I_final=I_final,
                                              distribution=distribution,
                                              doublingtime=doublingtime,
                                              rampslope=rampslope,
                                              #######################
                                              M=M,
                                              L=L,
                                              L_occupied=L_occupied,
                                              M_occupied=M_occupied,
                                              Lfinal=Lfinal,
                                              Lramp=Lramp,
                                              Mfinal=Mfinal,
                                              Mramp= Mramp,
                                              ######################
                                              avg_LOS_ICU=avg_LOS_ICU,
                                              avg_LOS_Floor=avg_LOS_Floor,
                                              #####################
                                              p_death_ICU2=p_death_ICU2,
                                              p_death_ICU3=p_death_ICU3,
                                              p_death_floor2=p_death_floor2,
                                              p_death_floor3=p_death_floor3,
                                              #####################
                                              slope=slope,
                                              doprotocols=doprotocols)
    
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


    text = data.frame(Variable = c("Total number of COVID19+ presentations to the health system",
                                    "Total deaths of COVID19+ patients",
                                    "Case fatality ratio for COVID19+ patients",
                                    "Deaths of COVID19+ patients in the ICU",
                                    "Deaths of COVID19+ patients on the floor",
                                    "Deaths of COVID19+ patients waiting for ICU beds",
                                    "Deaths of COVID19+ patients waiting for floor beds",
                                    "Days to floor overflow", 
                                   "Days to ICU overflow", 
                                   "Floor beds needed to accomodate all COVID19+ patients", 
                                   "ICU beds needed to accomodate all COVID19+ patients"),
                      Value = c(ceiling(tail(hospital$Number_seen_at_ED, n=1)),
                                ceiling(tail(hospital$totaldead, n=1)),
                                paste(signif(tail(hospital$totaldead, n=1)/(pt_init + tail(hospital$Number_seen_at_ED, n=1)), digits=3)*100, "%"),
                                ceiling(tail(hospital$Dead_at_ICU, n=1)),
                                ceiling(tail(hospital$Dead_on_Floor, n=1)),
                                ceiling(tail(hospital$Dead_waiting_for_ICU,n=1)),
                                ceiling(tail(hospital$Dead_waiting_for_Floor, n=1)),
                                floorover, 
                                ICUover, 
                                format(ceiling(floor_WF), scientific=FALSE), 
                                format(ceiling(ICU_WC), scientific = FALSE)))

}



