
#update_inputs_csv <- function() {

##### Conversion of data to model inputs

########TOTAL NUMBER OF BEDS AVAILABLE ###########

#Number of ICU beds available
M = 352

#Number of floor beds available
L = 1781

################ <18 years #######################

#####INPUTS

###Required inputs

#Proportion of symptomatic infectives presenting to ED
p_lambda1 = .106

#Proportion of patients that step up from the floor queue to the ICU
ptheta_WF1 = .1

#Expected time to recovery
d_phi1 = 14

#Expected time to death
d_mu1 = 12

#Expected proportion which recover without treatment
p_phi1 = .85

###solving set of linear equations for rate of stepping up from floor to ICU and
###death rate from floor

#Proportion of patients stepping up from floor to ICU
ptheta_F1 = .05

#Proportion of patients dying on the floor
pmu_F1 = .134

#Rate of stepping down from the floor (discharge) (1/avg # days spent on the floor)
chi_L1 = .142857

M11 = matrix(0, 2,2)
M11[1,1] = 1-ptheta_F1
M11[1,2] = -ptheta_F1
M11[2,1] = -pmu_F1
M11[2,2] = 1-pmu_F1

out11 = solve(M11)%*% matrix(chi_L1*c(ptheta_F1, pmu_F1), 2,1)

###solving set of linear equations for rate of returning to ED and 
###death rate of patients triaged as having mild symptoms

#proportion of patients returning to the ED after being triaged as mild
pxi_MS1 = .15

#proportion of patients dying after after being triaged as mild
pmu_MS1 = .0066

#recovery rate (1/days required to recovery if triaged as mild)
phi1 = 1/14

M12 = matrix(0,2,2)
M12[1,1] = 1-pxi_MS1
M12[1,2] = -pxi_MS1
M12[2,1] = -pmu_MS1
M12[2,2] = 1-pmu_MS1

out12 = solve(M12)%*% matrix(phi1*c(pxi_MS1, pmu_MS1), 2,1)

#####OUTPUTS

parameters1 = data.frame(
  
  #Rate of recovery for symptomatic infectives with mild symptoms
  phi_I1 = 1/(p_phi1*d_phi1 + (1-p_phi1)*d_mu1),
  
  #Rate of recovery of patients triaged as having mild symptoms
  phi1 = phi1,
  
  #Rate at which ED patients are triaged as having mild symptoms
  sigma_MS1 = .984,
  
  #Rate at which ED patients are triaged to the ICU
  sigma_C1 = 0,
  
  #Rate at which ED patients are triaged to the floor
  sigma_F1 = .016,
  

  #Rate of stepping down from the ICU to the floor
  chi_C1 = .1,
  
  #Rate of stepping down from the floor (discharge)
  chi_L1 = chi_L1,
  
  #Rate of stepping up from the floor to the ICU
  theta_F1 = out11[1],
  
  #Rate at which patients are moved to the ICU from the ICU queue
  eta1 = .041667,
  
  #Rate at which patients are moved to the floor from the floor queue
  zeta1 = .041667,
  
  #Rate at which patients with mild symptoms return to the ED
  xi_MS1 = out12[1],
  
  #Death rate of symptomatic infectives prior to presenting to the ED
  mu_I1 = 0,
  
  #Death rate of symptomatic infectives at ED prior to triage
  mu_P1 = 0,
  
  #Death rate of patients triaged as having mild symptoms
  mu_MS1 = out12[2],
  
  #Death rate of patients in the ICU
  mu_C1 = .1,
  
  #Death rate of patients on the floor
  mu_F1 = out11[2],
  
  #Death rate of patients waiting for an ICU bed
  mu_WC1 = 2.272727,
  
  #Death rate of patients waiting for a floor bed
  mu_WF1 = out11[2])


  
  ###parameters which require previous values for calculation
  
  lambda1 = (parameters1$phi_I1 + parameters1$mu_I1)*p_lambda1/(1-p_lambda1)
  
  theta_WF1 = ptheta_WF1*parameters1$mu_WF1/(1-ptheta_WF1)
  
param1 = cbind(parameters1, lambda1 = lambda1, theta_WF1= theta_WF1)

################ 18-64 years ######################
#####INPUTS

###Required inputs

#Proportion of symptomatic infectives presenting to ED
p_lambda2 = .364

#Proportion of patients that step up from the floor queue to the ICU
ptheta_WF2 = .1

#Expected time to recovery
d_phi = 14

#Expected time to death
d_mu = 12

#Expected proportion which recover without treatment
p_phi = .85

###solving set of linear equations for rate of stepping up from floor to ICU and
###death rate from floor

#Proportion of patients stepping up from floor to ICU
ptheta_F2 = .05

#Proportion of patients dying on the floor
pmu_F2 = .134

#Rate of stepping down from the floor (discharge) (1/avg # days spent on the floor)
chi_L2 = .142857

M21 = matrix(0, 2,2)
M21[1,1] = 1-ptheta_F2
M21[1,2] = -ptheta_F2
M21[2,1] = -pmu_F2
M21[2,2] = 1-pmu_F2

out21 = solve(M21)%*% matrix(chi_L2*c(ptheta_F2, pmu_F2), 2,1)

###solving set of linear equations for rate of returning to ED and 
###death rate of patients triaged as having mild symptoms

#proportion of patients returning to the ED after being triaged as mild
pxi_MS2 = .15

#proportion of patients dying after after being triaged as mild
pmu_MS2 = .0066

#recovery rate (1/days required to recovery if triaged as mild)
phi2 = 1/14

M22 = matrix(0,2,2)
M22[1,1] = 1-pxi_MS2
M22[1,2] = -pxi_MS2
M22[2,1] = -pmu_MS2
M22[2,2] = 1-pmu_MS2

out22 = solve(M22)%*% matrix(phi2*c(pxi_MS2, pmu_MS2), 2,1)

#####OUTPUTS

parameters2 = data.frame(

  #Rate of recovery for symptomatic infectives with mild symptoms
  phi_I2 = 1/(p_phi*d_phi + (1-p_phi)*d_mu),

  #Rate of recovery of patients triaged as having mild symptoms
  phi2 = phi2,

  #Rate at which ED patients are triaged as having mild symptoms
  sigma_MS2 = .725,

  #Rate at which ED patients are triaged to the ICU
  sigma_C2 = .058,

  #Rate at which ED patients are triaged to the floor
  sigma_F2 = .217,
  
  #Rate of stepping down from the ICU to the floor
  chi_C2 = .1,
  
  #Rate of stepping down from the floor (discharge)
  chi_L2 = chi_L2,

  #Rate of stepping up from the floor to the ICU
  theta_F2 = out21[1],
  
  #Rate at which patients are moved to the ICU from the ICU queue
  eta2 = .041667,
  
  #Rate at which patients are moved to the floor from the floor queue
  zeta2 = .041667,
  
  #Rate at which patients with mild symptoms return to the ED
  xi_MS2 = out22[1],
  
  #Death rate of symptomatic infectives prior to presenting to the ED
  mu_I2 = 0,
  
  #Death rate of symptomatic infectives at ED prior to triage
  mu_P2 = 0,
  
  #Death rate of patients triaged as having mild symptoms
  mu_MS2 = out22[2],
  
  #Death rate of patients in the ICU
  mu_C2 = .1,
  
  #Death rate of patients on the floor
  mu_F2 = out21[2],
  
  #Death rate of patients waiting for an ICU bed
  mu_WC2 = 2.272727,
  
  #Death rate of patients waiting for a floor bed
  mu_WF2 = out21[2])
  
  ###parameters which require previous values for calculation
  
  lambda2 = (parameters2$phi_I2 + parameters2$mu_I2)*p_lambda2/(1-p_lambda2)
  
  theta_WF2 = ptheta_WF2*parameters2$mu_WF2/(1-ptheta_WF2)

param2 = cbind(parameters2, lambda2 = lambda2, theta_WF2 = theta_WF2)

################ 65+ years ######################

#####INPUTS

###Required inputs

#Proportion of symptomatic infectives presenting to ED
p_lambda3 = .612

#Proportion of patients that step up from the floor queue to the ICU
ptheta_WF3 = .1

#Expected time to recovery
d_phi3 = 14

#Expected time to death
d_mu3 = 12

#Expected proportion which recover without treatment
p_phi3 = .85

###solving set of linear equations for rate of stepping up from floor to ICU and
###death rate from floor

#Proportion of patients stepping up from floor to ICU
ptheta_F3 = .05

#Proportion of patients dying on the floor
pmu_F3 = .134

#Rate of stepping down from the floor (discharge) (1/avg # days spent on the floor)
chi_L3 = .142857

M31 = matrix(0, 2,2)
M31[1,1] = 1-ptheta_F3
M31[1,2] = -ptheta_F3
M31[2,1] = -pmu_F3
M31[2,2] = 1-pmu_F3

out31 = solve(M31)%*% matrix(chi_L3*c(ptheta_F3, pmu_F3), 2,1)

###solving set of linear equations for rate of returning to ED and 
###death rate of patients triaged as having mild symptoms

#proportion of patients returning to the ED after being triaged as mild
pxi_MS3 = .15

#proportion of patients dying after after being triaged as mild
pmu_MS3 = .0066

#recovery rate (1/days required to recovery if triaged as mild)
phi3 = 1/14

M32 = matrix(0,2,2)
M32[1,1] = 1-pxi_MS3
M32[1,2] = -pxi_MS3
M32[2,1] = -pmu_MS3
M32[2,2] = 1-pmu_MS3

out32 = solve(M32)%*% matrix(phi3*c(pxi_MS3, pmu_MS3), 2,1)

#####OUTPUTS

parameters3 = data.frame(
  
  #Rate of recovery for symptomatic infectives with mild symptoms
  phi_I3 = 1/(p_phi3*d_phi3 + (1-p_phi3)*d_mu3),
  
  #Rate of recovery of patients triaged as having mild symptoms
  phi3 = phi3,
  
  #Rate at which ED patients are triaged as having mild symptoms
  sigma_MS3 = .425,
  
  #Rate at which ED patients are triaged to the ICU
  sigma_C3 = .163,
  
  #Rate at which ED patients are triaged to the floor
  sigma_F3 = .412,
  
  #Rate of stepping down from the ICU to the floor
  chi_C3 = .1,
  
  #Rate of stepping down from the floor (discharge)
  chi_L3 = chi_L3,
  
  #Rate of stepping up from the floor to the ICU
  theta_F3 = out31[1],
  
  #Rate at which patients are moved to the ICU from the ICU queue
  eta3 = .041667,
  
  #Rate at which patients are moved to the floor from the floor queue
  zeta3 = .041667,
  
  #Rate at which patients with mild symptoms return to the ED
  xi_MS3 = out32[1],
  
  #Death rate of symptomatic infectives prior to presenting to the ED
  mu_I3 = 0,
  
  #Death rate of symptomatic infectives at ED prior to triage
  mu_P3 = 0,
  
  #Death rate of patients triaged as having mild symptoms
  mu_MS3 = out32[2],
  
  #Death rate of patients in the ICU
  mu_C3 = .1,
  
  #Death rate of patients on the floor
  mu_F3 = out31[2],
  
  #Death rate of patients waiting for an ICU bed
  mu_WC3 = 2.272727,
  
  #Death rate of patients waiting for a floor bed
  mu_WF3 = out31[2])
  
  ###parameters which require previous values for calculation
  
  lambda3 = (parameters3$phi_I3 + parameters3$mu_I3)*p_lambda3/(1-p_lambda3)
  
  theta_WF3 = ptheta_WF3*parameters3$mu_WF3/(1-ptheta_WF3)
  
param3 = cbind(parameters3, lambda3 = lambda3, theta_WF3 = theta_WF3)
  
############ COMBINE ALL PARAMETERS AND SAVE

param_all = cbind(param1, param2, param3, M = M, L = L)

  
#write.csv(param_all,file.path(system.file("content/", package='covid19icu'), "queueinputs1.csv"))

#}
