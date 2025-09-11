### CCJS 710 - Advanced Statistics Methods: Limited Dependent Variables (Fall 2025)

* Course Catalog Description: Application of advanced data analysis strategies to criminological and criminal justice problems, with specific focus on limited dependent variables.
* Instructor: Robert Brame
* Office: LeFrak 2139
* Course Meets: Thursdays 4-6:45 in LeFrak 1220
* Office Hours: Tuesdays and Thursdays from 10-11:00
* Readings will be assigned throughout the semester.
* Course-related policies: In all matters, the class will follow University guidance as outlined [here](https://academiccatalog.umd.edu/graduate/policies/course-related-policies/).
* Accessibility accommodations: If you think you might need one or more academic accommodations, please contact the Accessibility and Disability Service Office ([link](https://ads.umd.edu)) for guidance and assistance. Please contact me to set up an appointment to discuss any accommodations that are authorized. 
* Letter grades: At the end of the semester, letter grades will be assigned on a 100-point scale (A+ = 98 and higher; A = 92-97; A- = 90-91; B+ = 88-89; B = 82-87; B- = 80-81; C+ = for 78-79; C = 72-77; C- = 70-71; D+ = 68-69; D = 62-67; D- = 60-61; and F = any grade less than 60). All numeric grades (including the final numeric grade in the class at the end of the semester) will be rounded off to the nearest 1 point (for example, a 78.5 would be rounded to a 79 and a 78.4 would be rounded to a 78).
* Numeric grades in this class will be based on 3 in-class exams; each exam will contribute equally to your final grade and each will be graded on a 100-point scale.
* Exam questions will be short-answer format and will require you to report and interpret calculations. I will give you practice questions as the exam dates approach.
* On exams, you must turn off your internet connection and not consult any artificial intelligence or any other support services for help. You will also be asked to write the University honor pledge on your exam and sign it.
* We will be using R statistical software ([link](https://www.r-project.org)) during class sessions; I will post relevant R code on this webpage.

#### Course Outline

1. syllabus review and R primer (comparing homicide rates of 2 cities)
2. review of point estimation and confidence intervals
3. state-level changes in homicide rates

exam 1 (Thursday 10/16)

4. estimating the effect of arrest in a domestic violence experiment
5. studying the prevalence of crime victimization in a survey
6. descriptive analysis of murder clearance rate trends
7. survival time studies of criminal recidivism

exam 2 (Thursday 11/20)

8. measuring the association between static risk factors and recidivism
9. observational studies of treatment effects with limited dv outcomes
10. seat belts and injuries in car crashes
 
exam 3 (TBA)

#### R Code for Thursday 9/4/25

* Larry Wasserman's R primer ([link](https://www.stat.cmu.edu/~larry/all-of-statistics/=R/Rintro.pdf)).
* Brown et al., + discussion (2001; [link](https://projecteuclid.org/journals/statistical-science/volume-16/issue-2/Interval-Estimation-for-a-Binomial-Proportion/10.1214/ss/1009213286.full)).
* chapter 20 of Weisburd and Britt (2007; [link](https://link.springer.com/book/10.1007/978-0-387-34113-2)).

##### Script #1

```R
# coin 1

h1 <- 12
f1 <- 22
h1/f1

# coin 2

h2 <- 15
f2 <- 31
h2/f2

# build confidence interval

r1 <- rbeta(n=1e5,shape1=1/2+h1,shape2=1/2+f1-h1)
r2 <- rbeta(n=1e5,shape1=1/2+h2,shape2=1/2+f2-h2)
hist(r2-r1)
quantile(r2-r1,c(0.025,0.975))
```

##### Script #2

```R
set.seed(381)

trap <- vector()

for(i in 1:3000){
  x1 <- rbinom(n=1,size=22,p=0.5)
  x2 <- rbinom(n=1,size=31,p=0.5)
  r1 <- rbeta(n=1e5,shape1=1/2+x1,shape2=1/2+22-x1)
  r2 <- rbeta(n=1e5,shape1=1/2+x2,shape2=1/2+31-x2)
  d <- r2-r1
  lcl.d <- quantile(d,0.025)
  ucl.d <- quantile(d,0.975)
  trap[i] <- ifelse(lcl.d<0 & ucl.d>0,1,0)
  }

table(trap)
mean(trap)
```

##### Script #3

```R
set.seed(433)

# st. louis 2024

h1 <- 150
p1 <- 277294
h1/p1*100000

# washington dc 2024

h2 <- 179
p2 <- 702250
h2/p2*100000

# build confidence interval

r1 <- 100000*rbeta(n=1e5,shape1=1/2+h1,shape2=1/2+p1-h1)
r2 <- 100000*rbeta(n=1e5,shape1=1/2+h2,shape2=1/2+p2-h2)
hist(r2-r1)
quantile(r2-r1,c(0.025,0.975))
```

#### R Code for Thursday 9/11/25

* This is mostly based on the material on pages 615-615 of WB.
* Let's begin by supposing that there is a population of persons released from prison in the United States in any given year.
* Now, let's further suppose that the recidivism rate for people in this population is 67.5% (i.e., the population parameter).
* If we draw simple random samples from this population and estimate the recidivism rate in each sample, we should get estimates that are *close* to the population parameter value.
* Here is an example:

<p align="center">
<img src="/gfiles/fig1.png" width="750px">
</p>

* Let's look at a single sample:

```R
set.seed(602)
u <- runif(n=50,min=0,max=1)
y <- ifelse(u>0.675,0,1)
table(y,exclude=NULL)
```

* How should we use the information in the sample to estimate the population recidivism rate?

```R
N <- length(y)
N
1/N*sum(y)
mean(y)
```

* How can we calculate the standard error of this estimate?

```
N <- length(y)
N
p <- mean(y)
p
q <- 1-p
q
std.err <- sqrt(p*q/N)
std.err
```

* How could we obtain a 93% confidence interval for this estimate?
* We *consider* the idea of a sampling distribution of proportions.
* We *assume* that the standardized version of this sampling distribution can be approximated by the standard normal distribution.
* Next, we find the 3.5th and 96.5th percentiles of this normal distribution.

```R
qnorm(p=0.035)
qnorm(p=0.965)
```

* So, the 93% confidence interval for the sample proportion is:

```R
lower.limit <- p-1.811911*std.err
lower.limit
upper.limit <- p+1.811911*std.err
upper.limit
```

* Does the interval include the true population parameter value of 0.675?
* If so, the confidence interval "trapped" the true population value; if not, it didn't.
* Notice that whether a population parameter value is trapped is a yes or no question.
* Interpretation: if this is a valid procedure for calculating a 93% confidence interval, then if we drew many thousands of samples and used this same procedure to calculate the 93% confidence interval for each sample, then 93% of the sample intervals would contain the true population parameter value.
* How do we know whether it is valid?

```R
trap <- vector()

for(i in 1:1e5){
  u <- runif(n=50,min=0,max=1)
  y <- ifelse(u>0.675,0,1)
  N <- length(y)
  p <- mean(y)
  q <- 1-p
  std.err <- sqrt(p*q/N)
  lower.limit <- p-1.811911*std.err
  upper.limit <- p+1.811911*std.err
  trap[i] <- ifelse(lower.limit<0.675 & upper.limit>0.675,1,0)
  }

table(trap,exclude=NULL)
```

* Everything looks reasonable here.
* Now, let's consider a different case.
* Suppose there is a population of 18 year olds and that 90.3% of them would acknowledge involvement in at least one act that could be considered to be an act of "delinquency" within the past 3 years -- if they were asked about it in a survey. So, this is a population value.
* Let's draw samples of size 80 and see whether the procedure is valid.
* How would you modify the code above to check on whether the 93% confidence interval procedure still works as it should?
* What is the *coverage rate* of the confidence interval for this example?
* What else could we do?
* Let's consider our prison release and recidivism example from earlier.
* Instead of assuming that the sample p's come from a normal sampling distribution (think about how reasonable that is), let's assume that the sample p's come from a beta sampling distribution (this is from the Brown et al. paper, p. 113).

```R
trap <- vector()

for(i in 1:1e5){
  u <- runif(n=50,min=0,max=1)
  y <- ifelse(u>0.675,0,1)
  sy <- sum(y)
  N <- length(y)
  lower.limit <- qbeta(p=0.035,shape1=sy,shape2=1+N-sy)
  upper.limit <- qbeta(p=0.965,shape1=1+sy,shape2=N-sy)
  trap[i] <- ifelse(lower.limit<0.675 & upper.limit>0.675,1,0)
  }

table(trap,exclude=NULL)
```

* This interval is called the Clopper-Pearson or *exact* interval.
* Let's apply the CP interval to the juvenile delinquency example. What is its coverage rate?
* Next, we remind ourselves that these are simulations to check coverage rates.
* Let's consider the single sample from our population of people released from prison.
* Recall that we had 37 people who recidivated and 13 people who didn't.
* This implies a sample estimated recidivism rate of 74%.
* The 93% confidence interval (based on the normal distribution) around this estimate is [0.628,0.852].
* What is the 93% confidence interval based on the Clopper-Pearson procedure?
* Compare the widths of the 2 confidence intervals in the single sample. What do you see?
* Now, consider a single sample of N = 80 18-year-olds who are asked about the delinquency involvement over the previous 3 years; for this sample, we observe that 71 of the 80 people acknowledged they had engaged in delinquency while 9 said they had not.
* What is the normal 95% confidence interval for this sample?
* What is the Clopper-Pearson 95% confidence interval for this sample?
* Compare the widths of the 2 confidence intervals. What do you see?
