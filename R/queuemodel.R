
#*************************** COVID-19 Hospital Queueing MODEL *****************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#************************************* MODEL FUNCTIONS ************************************#

# lambda = rate of presenting for care
# M = number of ICU beds
# L = number of Floor beds
# eta = rate of movement to an ICU bed from queue
# zeta = rate of movement to a floor bed from queue



############## run the queuing model


#' @export
hospital_queues<- function(params, doprotocols=0, floor_capacity_timeseries, icu_capacity_timeseries, ed_visits_timeseries, ...){
  


  ############## SET INITIAL CONDITIONS
  
  init       <- c(I=rep(0.0, times=3),
                  P=c(params$young, params$medium, params$old) * params$I_init, 
                  MS=rep(0.0, times=3),
                  WC=rep(0.0, times=3),
                  C=c(params$young, params$medium, params$old) * params$M * params$M_occupied/100,
                  WF=rep(0.0, times=3),
                  FL=c(params$young, params$medium, params$old) * params$L * params$L_occupied/100,
                  WF=rep(0.0, times=3),
                  FL=rep(0.0, times=3),
                  R=rep(0.0, times=3),
                  D=rep(0.0, times=3),
                  Dead_at_ICU =0,
                  Dead_on_Floor =0,
                  Dead_waiting_for_ICU =0,
                  Dead_waiting_for_Floor=0,
                  Dead_with_mild_symptoms=0,
                  Dead_in_ED=0,
                  Number_seen_at_ED=0,
                  CTotal= params$M * params$M_occupied/100,
                  FTotal= params$L * params$L_occupied/100
  )
  
  ### functions for reports and ramping
  
  reports <- approxfun(
    ed_visits_timeseries,
    rule=2)
  
  capacity_L <- approxfun(
    floor_capacity_timeseries,
    rule=2);
  
  capacity_M <- approxfun(
    icu_capacity_timeseries,
    rule=2);
  
  icu_transfers<- approxfun(
    params$icu_transfers_timeseries,
    rule=2);
  
  floor_transfers<- approxfun(
    params$floor_transfers_timeseries,
    rule=2);
  
  
  model <- function(time, state, parameters) {
    with(as.list(c(state, parameters)), {
      I = state[1:3]
      P = state[4:6]
      MS = state[7:9]
      WC = state[10:12]
      C = state[13:15]
      WF = state[16:18]
      FL = state[19:21]
      R = state[22:24]
      D = state[25:27]
      Dead_at_ICU= state[28];
      Dead_on_Floor= state[29];
      Dead_waiting_for_ICU= state[30];
      Dead_waiting_for_Floor= state[31];
      Dead_with_mild_symptoms= state[32];
      Dead_in_ED= state[33];
      Number_seen_at_ED= state[34];
      CTotal= state[35]
      FTotal= state[36]
      
      C_capped = 1-1/(1+exp(slope*(CTotal -capacity_M(time))))
      F_capped = 1-1/(1+exp(slope*(FTotal -capacity_L(time))))
      
      floor_trans = floor_transfers(time)
      icu_trans = icu_transfers(time)
      
      dI  <- c(0,0,0) # - lambda* I - phi_I * I - mu_I * I 
      dP  <- -(sigma_MS+sigma_C+sigma_F+mu_P)*P + xi_MS * MS + age * reports(time) # + lambda_I * I
      dMS <- sigma_MS * P - (phi + mu_MS + xi_MS)* MS
      dWC <- (theta_WF * WF + theta_F * FL + sigma_C * P + age *icu_trans)*C_capped - eta*WC *(1-C_capped) -mu_WC * WC
      dC  <- (theta_WF * WF + theta_F * FL + sigma_C * P + eta*WC + age *icu_trans)* (1-C_capped) - mu_C*C - chi_C *C
      dWF <- (sigma_F* P + age *floor_trans) * F_capped - zeta * WF * (1-F_capped) - (mu_WF+ theta_WF+chi_LQ)* WF
      dFL <- (sigma_F* P + zeta*WF+ age *floor_trans) * (1-F_capped) - (mu_F + theta_F+chi_L)* FL
      dWF2 <- (chi_C*C) * F_capped - zeta * WF2 * (1-F_capped) - (mu_WF+chi_LQ)* WF
      dFL2 <- (chi_C*C+zeta*WF2) * (1-F_capped) - (mu_F + chi_L)* FL2
      dR  <- phi* MS + chi_L* FL + + chi_L* FL2 + phi_I * I + chi_LQ * WF + chi_LQ * WF2
      dD  <- mu_C * C+ mu_F * FL + mu_F * FL2 + mu_I * I + mu_MS *MS + mu_WF * WF+ mu_WF * WF2 + mu_WC * WC + mu_P * P

      dDead_at_ICU = mu_C %*% C;
      dDead_on_Floor = mu_F %*% FL + mu_F %*% FL2 ;
      dDead_waiting_for_ICU = mu_WC %*% WC;
      dDead_waiting_for_Floor =  mu_WF %*% WF + mu_WF %*% WF2;
      dDead_with_mild_symptoms = mu_MS %*%MS;
      dDead_in_ED = mu_P %*% P;
      dNumber_seen_at_ED = reports(time)  +xi_MS %*% MS;
      
      dCTotaldt = sum((theta_WF * WF + theta_F * FL + sigma_C * P + eta*WC+ age*icu_trans)* (1-C_capped) - mu_C*C - chi_C *C)
      dFTotaldt = sum((sigma_F* P + chi_C*C + zeta*WF+ zeta* WF2+age*floor_trans) * (1-F_capped) - (mu_F+ theta_F+chi_L)* FL- (mu_F + chi_L)* FL2) 
      
      return(
        list(
          c(dI,dP,dMS, dWC,dC, 
            dWF, dFL, dWF2, dFL2 dR, dD, 
            dDead_at_ICU, dDead_on_Floor,dDead_waiting_for_ICU, 
            dDead_waiting_for_Floor,dDead_with_mild_symptoms, dDead_in_ED, 
            dNumber_seen_at_ED, dCTotaldt, dFTotaldt
          )
      
        )
      )
    })
  }
  
  
  out <- as.data.frame(ode(y=init, times= c(0:params$t), func=model, parms=params, method="lsodes"))
  names(out)[2:ncol(out)] = names(init)
  #print(proc.time() - ptm)
  
  out=out[out$time %in% 1:params$t,]
  
  out$reports <- reports(1:params$t);
  out$capacity_L <- capacity_L(1:params$t);
  out$capacity_M <- capacity_M(1:params$t);
  
  
  return(out)
  
}
