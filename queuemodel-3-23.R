####Here is the ODE equation code I started coding with, but the queue parts (M*chi_M, L*chi_L, etc; the most important parts!) still aren't correct. I took out how I had it and just left in the "M" and "L". The S, E, and A compartments are the same as the epi model. I started with just coding the "medium" age: 


start_time = Sys.time()

#****************************** AGE-STRATIFIED COVID19 MODEL ******************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#************************************* MODEL FUNCTIONS ************************************#


# k_inf = relative infectiousness for kids
# k_susp = relative susceptibility for kids
# k_report =relative testing rate for kids

# lambda = rate of presenting for care
# M = number of ICU beds
# L = number of Floor beds
# eta = rate you can be moved to an ICU bed from queue
# zeta = rate you can be moved to an ICU bed from queue

############## Reporting rate function determines who shows up to the ED 3/23
report_rate<-function(t,
                      initial_report, 
                      final_report, 
                      distribution="uniform",
                      growth_rate=1){
  
  report_rate<-rep(0,t)
  if (distribution=="uniform"){
    report_rate<-rep(initial_report,t)
  }
  if (distribution=="logistic"){
    z <- log(1/0.005-1)
    zz  <- seq(-z*(1+(2/(t-3))),
               z*(1+(2/(t-3))),
               by=(2*z)/(t-3))
    zzz  <- as.numeric(final_report-initial_report)/(1+exp(-zz))
    report_rate<-zzz+initial_report
  }
  if (distribution=="ramp"){
    report_rate<- seq(initial_report, final_report, length.out= t+1)[2:(t+1)]	
  }
  
  if (distribution=="geometric"){
    geometric_factor<- exp(1/t* log(final_report/initial_report))
    report_rate<- (geometric_factor^(1:t))*initial_report	
  }
  
  if (distribution=="exponential"){
    report_rate<- (exp(growth_rate*(1:t)))*initial_report	
  }
  
  try(if (length(report_rate) != t)(stop("reporting rate time scale does not match inputted timescale")))
  
  return(report_rate)
}


############## STRATIFIED MODEL


############## RUN ODE


############## IMPORT PARAMETERS

# libraries
library(tidyverse)
library(deSolve)

# set working directory
#setwd("~/R/Covid-19")



hospital_queues<- function(initial_report= 1000,
            final_report = 10000,
            distribution= "exponential",
            young=.24,
            medium=.6,
            slope=50,
            M=352,
            L=1781,
		        t = 60,
		        chi_C=0.1,
		        chi_L=.142857,
		        growth_rate=1
		        ){
  

      # read in parameters
      params = read.csv("queueinputs1.csv", stringsAsFactors=FALSE)[1,]
      #attach(params)
      
      ############## SET INITIAL CONDITIONS
      
      # percentages in reporting to ED?
      old = 1- young - medium
      
      params$young = young;
      
      params$medium = medium;
      
      
      params$old = 1- young - medium;
      
      params$slope = slope;
      ##########
      
      
      
      x = data.frame(
        # initial conditions young
        I1= 0,
        P1= initial_report*young,
        MS1 = 0,
        WC1= 0,
        C1 =0,
        WF1 =0,
        F1 = 0,
        R1=0,
        D1=0,
        
        # initial conditions medium
        I2= 0,
        P2 = initial_report*medium,
        MS2 = 0,
        WC2= 0,
        C2 = 0,
        WF2 = 0,
        F2= 0,
        R2=0,
        D2 =0,
        
        # initial conditions old
        I3 =0,
        P3= initial_report*old,
        MS3 = 0,
        WC3= 0,
        C3 = 0,
        WF3= 0,
        F3 = 0,
        R3 =0,
        D3 =0,
      
      
      
        Dead_at_ICU =0,
        Dead_on_Floor =0,
        Dead_waiting_for_ICU =0,
        Dead_waiting_for_Floor=0,
        Dead_with_mild_symptoms=0,
        Dead_in_ED=0,
        Number_seen_at_ED=0,
      
        FTotal=0,
        CTotal=0
      )
      
      ############## RUN MODEL
      
      reports <- approxfun(report_rate(t = t, initial_report = initial_report, final_report = final_report, distribution=distribution, growth_rate=growth_rate), rule=2)
      
      
      model_strat <- function (t, x , pars,...) {
        
        
        # initial conditions young ###CHANGE YOUND PARAMS ######
        I1<- x[1];
        P1 <- x[2];
        MS1 <- x[3];
        WC1 <- x[4];
        C1 <- x[5];
        WF1 <- x[6];
        F1 <- x[7];
        R1 <- x[8];
        D1 <-x[9];
        
        # initial conditions medium
        I2 <- x[10];
        P2 <- x[11];
        MS2 <- x[12];
        WC2 <- x[13];
        C2 <- x[14];
        WF2 <- x[15];
        F2 <- x[16];
        R2 <- x[17];
        D2 <-x[18];
        
        # initial conditions old ##### CHANGE OLD PARAM
        I3 <- x[19];
        P3 <- x[20];
        MS3 <- x[21];
        WC3 <- x[22];
        C3 <- x[23];
        WF3 <- x[24];
        F3 <- x[25];
        R3 <- x[26];
        D3 <-x[27];
        
        
        Dead_at_ICU <- x[28];
        Dead_on_Floor <- x[29];
        Dead_waiting_for_ICU <- x[30];
        Dead_waiting_for_Floor<- x[31];
        Dead_with_mild_symptoms<- x[32];
        Dead_in_ED<- x[33];
        Number_seen_at_ED<- x[34];
        
        FTotal<- x[35]
        CTotal<- x[36]
        
        ##################################### initialize parameters
        
        
        phi_I1=pars$phi_I1
        phi1 =pars$phi1
        sigma_MS1=pars$sigma_MS1
        sigma_C1=pars$sigma_C1
        sigma_F1=pars$sigma_F1
        chi_C1=chi_C#pars$chi_C1
        chi_L1=chi_L#=pars$chi_L1
        theta_F1=pars$theta_F1
        eta1=pars$eta1
        zeta1=pars$zeta1
        xi_MS1=pars$xi_MS1
        mu_I1=pars$mu_I1
        mu_P1=pars$mu_P1
        mu_MS1=pars$mu_MS1
        mu_C1=pars$mu_C1
        mu_F1=pars$mu_F1
        mu_WC1=pars$mu_WC1
        mu_WF1=pars$mu_WF1
        lambda1=pars$lambda1
        theta_WF1=pars$theta_WF1
        
        
        phi_I2=pars$phi_I2
        phi2 =pars$phi2
        sigma_MS2=pars$sigma_MS2
        sigma_C2=pars$sigma_C2
        sigma_F2=pars$sigma_F2
        chi_C2=chi_C#=pars$chi_C2
        chi_L2=chi_L#=pars$chi_L2
        theta_F2=pars$theta_F2
        eta2=pars$eta2
        zeta2=pars$zeta2
        xi_MS2=pars$xi_MS2
        mu_I2=pars$mu_I2
        mu_P2=pars$mu_P2
        mu_MS2=pars$mu_MS2
        mu_C2=pars$mu_C2
        mu_F2=pars$mu_F2
        mu_WC2=pars$mu_WC2
        mu_WF2=pars$mu_WF2
        lambda2=pars$lambda2
        theta_WF2=pars$theta_WF2
        
        
        phi_I3=pars$phi_I3
        phi3 =pars$phi3
        sigma_MS3=pars$sigma_MS3
        sigma_C3=pars$sigma_C3
        sigma_F3=pars$sigma_F3
        chi_C3=chi_C#pars$chi_C3
        chi_L3=chi_L#pars$chi_L3
        theta_F3=pars$theta_F3
        eta3=pars$eta3
        zeta3=pars$zeta3
        xi_MS3=pars$xi_MS3
        mu_I3=pars$mu_I3
        mu_P3=pars$mu_P3
        mu_MS3=pars$mu_MS3
        mu_C3=pars$mu_C3
        mu_F3=pars$mu_F3
        mu_WC3=pars$mu_WC3
        mu_WF3=pars$mu_WF3
        lambda3=pars$lambda3
        theta_WF3=pars$theta_WF3
        
        
        #M = pars$M;
        #L = pars$L;
        
        young =pars$young
        medium = pars$medium
        old = pars$old
        
        slope=pars$slope
        
        #

        #attach parameters into workspace
        #attach(params)
        
        ######################### Equations ##############################
        ### YOUNG
        
        dI1dt = 0 #- lambda1 * I1 -phi_I1 * I1 - mu_I1 * I1  #(1-alpha2)*delta*E2
        
        dP1dt =  xi_MS1 * MS1 - (sigma_MS1 + sigma_C1 + sigma_F1 + mu_P1) *P1 +young* reports(t)# + presenting for care - lambda2 *I1 +
        
        dMS1dt = sigma_MS1*P1 - (phi1 + mu_MS1 + xi_MS1)*MS1 
        
        dWC1dt =  - (sigma_C1 * P1 + theta_F1 * F1 + theta_WF1 * WF1 +eta1 * WC1) *(1/(1+exp(slope*(CTotal -M)))) + (sigma_C1 * P1 + theta_F1 * F1 + theta_WF1 * WF1) -  (mu_WC1)*WC1 # icu queue
        
        dC1dt = (sigma_C1 * P1 + theta_F1 * F1 + theta_WF1 * WF1 +eta1*WC1) *(1/(1+exp(slope*(CTotal -M)))) -  (mu_C1)*C1 - chi_C1 * C1 # icu
        
        dWF1dt = (sigma_F1 * P1 + chi_C1*C1) *(1- 1/(1+exp(slope*(FTotal -L))))  - zeta1 * WF1 *(1/(1+exp(slope*(CTotal -L)))) - (mu_WF1+ theta_WF1)*WF1 # floor queue
        
        dF1dt = (sigma_F1 *P1 + zeta1* WF1+ chi_C1 * C1) *(1/(1+exp(slope*(FTotal -L))))  - (chi_L1 + mu_F1 + theta_F1)*F1 # floor bed
        
        dR1dt = phi1*MS1+ chi_L1 * F1 +  phi_I1 * I1
        
        dD1dt = mu_C1 * C1+ mu_F1 * F1 + mu_I1 * I1 + mu_MS1 *MS1 + mu_WF1 * WF1 + mu_WC1 * WC1 + mu_P1 * P1
        
        ### MEDIUM
        
        
        dI2dt = 0 #- lambda2 * I2 -phi_I2 * I2 - mu_I2 * I2  #(1-alpha2)*delta*E2
        
        dP2dt =  xi_MS2 * MS2 - (sigma_MS2 + sigma_C2 + sigma_F2 + mu_P2) *P2 + medium* reports(t)# presenting for care - lambda2 *I2 + # +
        
        dMS2dt = sigma_MS2*P2 - (phi2 + mu_MS2 + xi_MS2)*MS2 
        
        dWC2dt =  (sigma_C2 * P2 + theta_F2 * F2 + theta_WF2 * WF2) -  (mu_WC2)*WC2- (sigma_C2 * P2 + theta_F2 * F2 + theta_WF2 * WF2 +eta2 * WC2) *(1/(1+exp(slope*(CTotal -M)))) # icu queue
        
        dC2dt = (sigma_C2 * P2 + theta_F2 * F2 + theta_WF2 * WF2 +eta2*WC2) *(1/(1+exp(slope*(CTotal -M)))) -  (mu_C2)*C2- chi_C2 * C2 # icu
        
        dWF2dt = (sigma_F2 * P2 + chi_C2*C2)  - (mu_WF2+ theta_WF2)*WF2   - (zeta2 * WF2 + sigma_F2 * P2 + chi_C2*C2) *(1/(1+exp(slope*(FTotal -L))))# floor queue
        
        dF2dt = (sigma_F2 *P2 + zeta2* WF2+ chi_C2 * C2) *(1/(1+exp(slope*(FTotal -L))))  - (chi_L2 + mu_F2 + theta_F2)*F2 # floor bed
        
        dR2dt =phi2*MS2+ chi_L2 * F2 +  phi_I2 * I2
        
        dD2dt =mu_C2 * C2+ mu_F2 * F2 + mu_I2 * I2 + mu_MS2 *MS2+ mu_WF2 * WF2 + mu_WC2 * WC2 + mu_P2 * P2
        
        ### OLD
        
        dI3dt = 0 #- lambda3 * I3 -phi_I3 * I3 - mu_I3 * I3  #(1-alpha2)*delta*E2
        # 
        dP3dt =   xi_MS3 * MS3 - (sigma_MS3 + sigma_C3 + sigma_F3 + mu_P3)*P3 + old* reports(t)# presenting for care - lambda3 *I3 #
        # 
        dMS3dt = sigma_MS3 * P3 - (phi3 + mu_MS3 + xi_MS3)*MS3 
        # 
        dWC3dt = (sigma_C3 * P3 + theta_F3 * F3 + theta_WF3 * WF3) *(1- 1/(1+exp(slope*(CTotal -M)))) -  (mu_WC3)*WC3 - eta3 * WC3 *(1/(1+exp(slope*(CTotal -M)))) # icu queue
        # 
        dC3dt =  (sigma_C3 * P3 + theta_F3 * F3 + theta_WF3 * WF3 + eta3*WC3) *(1/(1+exp(slope*(CTotal -M)))) -  (mu_C3)*C3- chi_C3 * C3 # icu
        # 
        dWF3dt = (sigma_F3 * P3 + chi_C3*C3) - (mu_WF3+ theta_WF3)*WF3  - (sigma_F3 * P3 + chi_C3*C3+ zeta3 * WF3) *(1/(1+exp(slope*(FTotal -L))))  # floor queue
        # 
        dF3dt = (sigma_F3 *P3 + zeta3* WF3+ chi_C3 * C3) *(1/(1+exp(slope*(FTotal -L))))  - (chi_L3 + mu_F3 + theta_F3)*F3 # floor bed
        # 
        dR3dt = phi3*MS3 + chi_L3 * F3 +  phi_I3 * I3
        # 
        dD3dt = mu_C3 * C3 + mu_F3 * F3 + mu_I3 * I3 + mu_MS3 * MS3+ mu_WF3 * WF3 + mu_WC3 * WC3 + mu_P3 * P3
        # 
        # 
        dFTotaldt =   (sigma_F1 *P1 + zeta1* WF1+ chi_C1 * C1 + sigma_F2 *P2 + zeta2* WF2+ chi_C2 * C2 + sigma_F3 *P3 + zeta3* WF3+ chi_C3 * C3) *(1/(1+exp(slope*(FTotal -L))))  - (chi_L1 + mu_F1 + theta_F1)*F1   - (chi_L2 + mu_F2 + theta_F2)*F2 + - (chi_L3 + mu_F3 + theta_F3)*F3 ;
        
        dCTotaldt =  (sigma_C1 * P1 + theta_F1 * F1 + theta_WF1 * WF1 +eta1*WC1 + sigma_C2 * P2 + theta_F2 * F2 + theta_WF2 * WF2 +eta2*WC2 +sigma_C3 * P3 + theta_F3 * F3 + theta_WF3 * WF3 + eta3*WC3) *(1/(1+exp(slope*(CTotal -M)))) -  (mu_C1)*C1 - chi_C1 * C1 -  (mu_C2)*C2- chi_C2 * C2 -  (mu_C3)*C3- chi_C3 * C3;
        
        
        
        
        
        
        dDead_at_ICUdt = mu_C1 * C1 + mu_C2 * C2+ mu_C3 * C3;
        dDead_on_Floordt = mu_F1 * F1 + mu_F2 * F2 + mu_F3 * F3;
        dDead_waiting_for_ICUdt = mu_WC1 * WC1 + mu_WC2 * WC2+ mu_WC3 * WC3;
        dDead_waiting_for_Floordt =  mu_WF1 * WF1 + mu_WF2 * WF2 + mu_WF3 * WF3;
        dDead_with_mild_symptomsdt = mu_MS1 *MS1+ mu_MS2 *MS2+mu_MS3 *MS3;
        dDead_in_EDdt = mu_P1 * P1 + mu_P2 * P2+ mu_P3 * P3;
        # 
        dNumber_seen_at_EDdt = reports(t)  +xi_MS1 * MS1 +xi_MS2 * MS2 +xi_MS3 * MS3 ;
        
        
        ###################################
        # results
        output <- c(dI1dt, dP1dt, dMS1dt, dWC1dt,dC1dt,dWF1dt, dF1dt, dR1dt, dD1dt,
                    dI2dt, dP2dt, dMS2dt, dWC2dt,dC2dt,dWF2dt, dF2dt, dR2dt, dD2dt,
                    dI3dt, dP3dt, dMS3dt, dWC3dt,dC3dt,dWF3dt, dF3dt, dR3dt, dD3dt,
                    dDead_at_ICUdt ,dDead_on_Floordt ,dDead_waiting_for_ICUdt,
                    dDead_waiting_for_Floordt,dDead_with_mild_symptomsdt,dDead_in_EDdt,dNumber_seen_at_EDdt, dFTotaldt, dCTotaldt)
        
        # list it!
        list(output)
      }
      
      
      
      run_model <- function(func, xstart, times, params, method = "lsodes") {
        return(as.data.frame(ode(func = func, y = xstart, times = times, parms = params, method = method)))
      }

      test = run_model(model_strat, xstart = as.numeric(x), times = c(1:t), params, method = "lsodes")
      names(test)[2:ncol(test)] = names(x)
      
      test$reports <- reports(1:t);
      

      
      return(test)


}

end_time = Sys.time()

