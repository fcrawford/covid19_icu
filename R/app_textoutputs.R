#' @export

text_hospital = function(initial_report= 1000,
                         final_report = 10000,
                         distribution= "exponential",
                         young=.24,
                         medium=.6,
                         M=352,
                         L=1781,
                         t = 60,
                         chi_C=0.1,
                         chi_L=.142857,
                         growth_rate=1,
                         mu_C1 = .1,
                         mu_C2 = .1,
                         mu_C3 = .1,
                         rampslope=1.2,
                         Cinit = .25,
                         Finit = .5,
                         Lfinal=1781,
                         Lramp=c(0,0),
                         Mfinal=352,
                         Mramp=c(0,0),
                         doprotocols=0){

    hospital <- hospital_queues(initial_report=initial_report,
                            final_report = final_report,
                            distribution= distribution,
                            young=young,
                            medium=medium,
                            M=M,
                            L=L,
                            t=t,
                            chi_C=chi_C,
                            chi_L=chi_L,
                            growth_rate=growth_rate,
                            mu_C1 = mu_C1,
                            mu_C2 = mu_C2,
                            mu_C3 = mu_C3,
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
    ICUover = (hospital$WC1+hospital$WC2+hospital$WC3>=1)
    
    
    #floor queue
    floorover = (hospital$WF1+hospital$WF2+hospital$WF3>=1)
    
    #day that you run out of beds
    if(sum(floorover)>0){
      floorover<- min(which(floorover))
    }
    else(floorover = "No shortage")
    
    if(sum(ICUover)>0){
      ICUover<- min(which(ICUover))
    }
    else(floorover = "No shortage")
    
    #max number that are in the queue at any given time
    ICU_WC = max(hospital$WC1 + hospital$WC2 + hospital$WC3)
    floor_WF = max(hospital$WF1 + hospital$WF2 + hospital$WF3)
    

    text = data.table(floorover, ICUover, 
                      format(ceiling(floor_WF), scientific=FALSE), 
                      format(ceiling(ICU_WC), scientific = FALSE))
    names(text) = c("Days to floor overflow", "Days to ICU overflow", "Floor beds needed", "ICU beds needed")
    
    text = xtable(text)
}