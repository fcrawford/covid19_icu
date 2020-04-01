**<h3> MODEL INPUTS </h4>**

<br/>

<h4> **Notice:** This model is intended to represent the movements of COVID19+ patients through a hospital system.
Inputs for ED visits, capacity, capacity expansion, and length of stay should be specific to COVID19+ patients
and exclude ED visits, beds, and lengths of stay for patients who do not have COVID19. </h>

<br/>

#### **Use the "Scenario" tab to specify a local infection scenario.**

**Time horizon (days)**: the number of days for which you wish to see projections.

**Infection curve**: the shape of the curve which describes how infections will increase. Select "Exponential", "Linear", "Saturated", or "Flat". 
	Control the rate of new infections using the following parameters.

* **Doubling time (exponential infection curve)**: if cases are increasing exponentially, the number of days it will take for the number of cases presenting to the ED daily to double.

* **Rate of increase in new cases per day (linear infection curve)**: if cases are increasing linearly, the rate at which cases increase.

* **Peak number of cases (saturated infection curve)**: if cases are increasing exponentially and peaking, the number of cases you expect to see at the peak.

* **Flat curve**: the number of cases per day is the same throughout the scenario.

**Initial cases per day (patients/day)**: the number of COVID+ patients presenting daily at the beginning of the simulation .



<br/>
  
#### **Use the "Capacity" tab to specify the current capacity of your hospital or hospital system for COVID19+ patients.**

**ICU capacity**

* **Initial ICU capacity (beds)**: the total number of ICU beds available to COVID19+ patients at the beginning of the simulation, including those already occupied by COVID+ patients.

* **% of ICU capacity occupied at time 0 (%)**: the percentage of total ICU beds occupied by COVID19+ patients at the beginning of the simulation.

**Floor capacity**

* **Initial floor capacity (beds)**: the total number of floor beds available to COVID19+ patients at the beginning of the simulation, including those already occupied by COVID+ patients.

* **% of floor capacity occupied at time 0 (%)**: the percentage of total floor beds occupied by COVD19+ patients at the beginning of the simulation.

<br/>
  
#### **Use the "Strategy" tab to specify changes to hospital capacity which may affect utilization of floor and ICU beds.**

**Expansion of ICU capacity**:

* **Target ICU capacity (beds)**: the target number of ICU beds.

* **ICU capacity scale-up (days)**: the time frame within the simulation during which expansion of ICU capacity would occur.

**Expansion of floor capacity**:
  
* **Target floor capacity (beds)**: the target number of floor beds.

* **Floor capacity scale-up (days)**: the time frame within the simulation during which expansion of floor capacity would occur.



<br/>
  
#### **Inputs on the "Parameters" tab allow you to specify features of your population which influence results of the model.**
  
 **Average time on floor**: the average length of stay on the floor for all patients, including those who die, step up to the ICU, or are discharged.

**Average time in ICU**: the average length of stay in the ICU for all patients, including those who die, step down from the ICU, or are discharged.

**Death rate in the ICU (age range)**: the average number of deaths per day which occur in the ICU in each age range.


