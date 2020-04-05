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
    
    ###calculate initial utilization by percentage
    M_inf = 3e15
    M_occupied_inf = M*M_occupied/(100*M_inf)
    L_inf = 3e15
    L_occupied_inf = L*L_occupied/(100*L_inf)
    
    
    ###calculate max utilization
    hospital_inf <- hospital_queues(t=t,
                                young=young,
                                medium=medium,
                                #######################
                                I_init=I_init,
                                I_final=I_final,
                                distribution=distribution,
                                doublingtime=doublingtime,
                                rampslope=rampslope,
                                #######################
                                M=M_inf,
                                L=L_inf,
                                L_occupied=L_occupied_inf,
                                M_occupied=M_occupied_inf,
                                Lfinal=L_inf,
                                Lramp=c(0,0),
                                Mfinal=M_inf,
                                Mramp=c(0,0),
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
                                doprotocols=0)
    
    if(max(hospital_inf$CTotal)-M>0){
      C_needed = max(hospital_inf$CTotal) - M
    } else {C_needed = 0}
    
    
    if(max(hospital_inf$FTotal)-L>0){
      F_needed = max(hospital_inf$FTotal) - L
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
text_parameters = function(t,
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
                                       doprotocols=doprotocols,
                                       ...){

df = data.frame(t=c("Time horizon", t),
                young=c("<18 yo", young),
                medium=c("18-65 yo",medium),
                I_init=c("Initial infections per day", I_init),
                     I_final=c("Final infections per day", I_final),
                     distribution=c("ED presentation curve", distribution),
                     doublingtime=c("doubling time", doublingtime),
                     rampslope=c("Ramp slope", rampslope),
                     M=c("ICU capacity", M),
                     L=c("Floor capacity", L),
                     L_occupied=c("Floor occupancy %", L_occupied),
                     M_occupied=c("ICU occupancy &", M_occupied),
                     Lfinal=c("Target floor capacity", Lfinal),
                     Lramp=c("Floor ramp", paste(Lramp[1],"--",Lramp[2])),
                     Mfinal=c("ICU target capacity", Mfinal),
                     Mramp=c("ICU ramp", paste(Mramp[1],"--",Mramp[2])),
                     avg_LOS_ICU=c("Avg ICU LOS", avg_LOS_ICU),
                     avg_LOS_Floor=c("Avg Floor LOS", avg_LOS_Floor),
                     p_death_ICU2=c("Probability of death in ICU 18-65yo", p_death_ICU2),
                     p_death_ICU3=c("Probability of death in ICU 65+ yo", p_death_ICU3),
                     p_death_floor2=c("Probability of death on floor 18-65 yo", p_death_floor2),
                     p_death_floor3=c("Probability of death on floor 65+ yo", p_death_floor3))


df = t(df)


}





