# Bayesian_inference
Obtain the HPD post-distribution of Bayesian inference using prior information and confirm the importance of prior information and Bayesian inference compared to general confidence intervals


Data from 2011 to 2020 were used for the measurement period using data from phase 3 clinical trials of new drugs related to infectious diseases in the United States. In addition, data published in 2016 to use appropriate prior information (Biomedtracker. (2015). Clinical Development Success Rates 11, 2006-2015) will be checked and adopted as prior information, analyzed Bayesian inference, and predicted future clinical trial results.

#### prior data from "Clinical Development Success Rates 2006-2015" published in 2016 due to lack of prior information

#### Set the prior-distribution to Beta (36, 14) meaningfully

#### After that, the following posterior distribution appears.
![image](https://user-images.githubusercontent.com/105573554/236973916-58377ffd-2ef8-4424-bebe-4aca1dc3c8fe.png)
![image](https://user-images.githubusercontent.com/105573554/236974020-37461332-128b-43e2-a55d-b4ef152b13fb.png)

#### This allows estimation of theta and variance
![image](https://user-images.githubusercontent.com/105573554/236974087-7b2eac8a-16d4-4f48-8891-52c0ecdc249c.png)

#### One of the important features of Bayesian inference is that the predictive distribution of future practices can be obtained by relying only on previous data without relying on estimates of parameters.


#### The difference between the classical confidence interval and the post-interval
![image](https://user-images.githubusercontent.com/105573554/236974410-1da3ca56-a7d8-4554-bce2-46eada969af6.png)
