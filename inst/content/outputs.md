**<h3> MODEL OUTPUTS </h3>**

<br/>

<h4> **Notice:** Model outputs are completely dependent on user-defined inputs and depend on assumptions encoded in the model structure. 
Please familiarize yourself with the model structure and inputs for accurate interpretation of these outputs. <h4/>

<br/> 

#### **Plot A** 

This plot depicts  the **COVID19+ cases presenting to the ED per day** during the days included in the simulation.  For example, on day 10, the plot
	gives the number of COVID19+ cases which presented to the ED on day 10.

<br/> 

#### **Plot B**

This plot depicts **cumulative ED visits and deaths**.

* **Cumulative ED visits (black curve)**: the total number of ED visits which occured in the simulation since time 0.  For example,
	on day 10, the black curve gives the total number of ED visits that occurred between day 0 and day 10.

* **Cumulative deaths (red curve)**: the total number of deaths observed in the simulation since time 0.  For example, on day 10, the red curve
	gives the total number of deaths that occurred between day 0 and day 10. 

<br/> 
 
#### **Plot C** 

This plot depicts **cumulative deaths by location**, the total number of deaths observed from each compartment in the model. On a given day, the difference between the minimum
	and maximum values spanned by an area of a particular color represents the number of total deaths which occurred in that compartment
	between time 0 and that day.

<br/> 

#### **Plot D**

This plot depicts **ICU and floor utilization** on a given day. 

* **Maximum ICU capacity (black dashed):** the maximum ICU capacity of your health system. A tag appears with the day on which caacity is exceeded when the number of patients requiring ICU care in the simulation
	exceeds the maximum ICU capacity.  Maximum capacity changes depending on the capacity expansion specified by the user.

* **In ICU (black):** the number of patients who are in the ICU on a given day.

* **Waiting for ICU beds (gray):** the number of patients who are in the ICU queue on a given day.

* **On floor (red):** the number of patients who are on the floor on a given day.

* **Maximum floor capacity (red dashed):** the maximum floor capacity of your health system. A tag appears with the day on which caacity is exceeded when the number of patients requiring floor beds in the simulation
	exceeds the maximum floor capacity.  Maximum capacity changes depending on the capacity expansion specified by the user.

* **Waiting for floor beds (pink):** the number of patients who are in the floor queue on a given day.