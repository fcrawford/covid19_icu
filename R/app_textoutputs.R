#' @export

text_hospital = function(initial_report= 1000,
                         final_report = 10000,
                         distribution= "exponential",
                         young=.24,
                         medium=.6,
                         M=352,
                         L=1781,
                         t = 14,
                         avg_LOS_ICU=10,
                         avg_LOS_Floor=7,
                         growth_rate=1,
                         p_death_ICU1,
                         p_death_ICU2 ,
                         p_death_ICU3,
                         p_death_floor2 ,
                         p_death_floor3,
                         rampslope=1.2,
                         Cinit = .25,
                         Finit = .5,
                         Lfinal=1781,
                         Lramp=c(0,0),
                         Mfinal=352,
                         Mramp=c(0,0),
                         doprotocols=0){

    hospital <- hospital_queues_new(initial_report=initial_report,
                                final_report = final_report,
                                distribution= distribution,
                                young=young,
                                medium=medium,
                                M=M,
                                L=L,
                                t=t,
                                avg_LOS_ICU=avg_LOS_ICU,
                                avg_LOS_Floor=avg_LOS_Floor,
                                growth_rate=growth_rate,
                                p_death_ICU1 = p_death_ICU1,
                                p_death_ICU2 = p_death_ICU2,
                                p_death_ICU3 = p_death_ICU3,
                                p_death_floor2=p_death_floor2,
                                p_death_floor3=p_death_floor3,
                                rampslope=rampslope,
                                Cinit = Cinit,
                                Finit = Finit,
                                Lfinal=Lfinal,
                                Lramp=Lramp,
                                Mfinal=Mfinal,
                                Mramp=Mramp,
                                doprotocols=doprotocols)
    
    hospital$totaldead<- hospital$Dead_at_ICU + hospital$Dead_in_ED + hospital$Dead_on_Floor+ hospital$Dead_waiting_for_Floor+ hospital$Dead_waiting_for_ICU+ hospital$Dead_with_mild_symptoms
    hospital$totalWC<- hospital$WC1 + hospital$WC2 + hospital$WC3
    hospital$totalWF<- hospital$WF1 + hospital$WF2 + hospital$WF3
    
    #ICU queue
    ICUover = (hospital$WC1+hospital$WC2+hospital$WC3>=0.1)


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
    pt_init = sum((hospital %>% select(C1, C2, C3, F1, F2, F3))[1,])


    text = data.frame(Variable = c("Total ED visits",
                                    "Total deaths",
                                    "Case fatality ratio",
                                    "Deaths in the ICU",
                                    "Deaths on the floor",
                                    "Deaths waiting for ICU beds",
                                    "Deaths waiting for floor beds",
                                    "Days to floor overflow", 
                                   "Days to ICU overflow", 
                                   "Floor beds needed", 
                                   "ICU beds needed"),
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



