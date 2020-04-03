**<h3> MODEL INPUTS </h4>**

<br/>

<h4> **Notice:** This model is intended to represent the movements of COVID19+ patients through a hospital system.
Inputs for number of presentations to the health system, capacity, capacity expansion, and length of stay should be specific to COVID19+ patients
and exclude presentations, admissions, beds, and lengths of stay for patients who do not have COVID19. </h>

<br/>

#### **Use the "Scenario" tab to specify a local infection scenario.**

**Time horizon (days)**: the number of days for which you wish to see projections.

**Change in the number of COVID19+ presentations to the health system**: the shape of the curve which describes how infections will increase. Select "Exponential", "Linear", "Saturated", or "Flat". 
	Control the rate of new infections using the following parameters.

* **Doubling time for COVID19+ presentations to the health system per day (exponential infection curve)**: if cases are increasing exponentially, the number of days it will take for the number of cases presenting to the ED daily to double.

* **Rate of increase in new COVID19+ presentations to the health system per day (linear infection curve)**: if cases are increasing linearly, the rate at which cases increase.

* **Peak number of COVID19+ presentations to the health system (saturated infection curve)**: if cases are increasing exponentially and peaking, the number of cases you expect to see at the peak.

* **Flat curve**: the number of cases per day is the same throughout the scenario.

**Initial cases per day (patients/day)**: the number of COVID+ patients presenting daily at the beginning of the simulation .



<br/>
  
#### **Use the "Capacity" tab to specify the current capacity of your hospital or hospital system for COVID19+ patients.**

**ICU capacity for COVID19+ patients**

* **Initial ICU capacity for COVID19+ patients (beds)**: the total number of ICU beds available to COVID19+ patients at the beginning of the simulation, including those already occupied by COVID+ patients.  This excludes ICU beds that are occupied by patients who do not have COVID19.

* **% of ICU capacity for COVID19+ patients occupied at time 0 (%)**: the percentage of the initial ICU capacity specified above which is occupied by COVID19+ patients at the beginning of the simulation.  This excludes occupancy by patients who do not have COVID19.

**Floor capacity**

* **Initial floor capacity for COVID19+ patients (beds)**: the total number of floor beds available to COVID19+ patients at the beginning of the simulation, including those already occupied by COVID+ patients.  This excludes floor beds that are occupied by patients who do not have COVID19.

* **% of floor capacity for COVID19+ patients occupied at time 0 (%)**: the percentage of the initial floor capacity specified above which is occupied by COVD19+ patients at the beginning of the simulation.  This excludes occupancy by patients who do not have COVID19.

<br/>
  
#### **Use the "Strategy" tab to specify changes to hospital capacity which may affect utilization of floor and ICU beds.**

**Expansion of ICU capacity**:

* **Target ICU capacity for COVID19+ patients (beds)**: the target number of ICU beds for COVID19+ patients.

* **ICU capacity scale-up (days)**: the time frame within the simulation during which expansion of ICU capacity would occur.

**Expansion of floor capacity**:
  
* **Target floor capacity for COVID19+ patients (beds)**: the target number of floor beds.

* **Floor capacity scale-up (days)**: the time frame within the simulation during which expansion of floor capacity would occur.



<br/>
  
#### **Inputs on the "Parameters" tab allow you to specify features of your population which influence results of the model.**
  
 **Average time on floor for COVID19+ patients**: the average length of stay on the floor for all COVID19+ patients, including those who die, step up to the ICU, or are discharged.

**Average time in ICU for COVID19+ patients**: the average length of stay in the ICU for all COVID19+ patients, including those who die, step down from the ICU, or are discharged.

**Probability of death on the floor for COVID19+ patients (age range)**: the probability that a COVID19+ patient entering the floor will die while on the floor, given the average time on the floor.  Reducing average time on the floor while keeping this probability constant increases the per-day risk of death on the floor.

**Probability of death in the ICU for COVID19+ patients (age range)**: the probability that a COVID19+ patient entering the ICU will die while in the ICU, given the average time in the ICU.  Reducing average time in the ICU while keeping this probability constant increases the per-day risk of death in the ICU.

