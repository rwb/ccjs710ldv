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

---

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
  trap[i] <- ifelse(lcl.d<=0 & ucl.d>=0,1,0)
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

---

#### R Code for Thursday 9/11/25

* Let's begin by supposing that there is a population of persons released from prison in the United States in any given year.
* Now, let's further suppose that the recidivism rate for people in this population is 67.5% (i.e., the population parameter).
* If we draw simple random samples from this population and estimate the recidivism rate in each sample, we should get estimates that are *close* to the population parameter value.
* Here is an example:

<p align="center">
<img src="/gfiles/fig1.png" width="750px">
</p>

##### Script #1

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

---
* Interpretation: if this is a valid procedure for calculating a 93% confidence interval, then if we drew many thousands of samples and used this same procedure to calculate the 93% confidence interval for each sample, then 93% of the sample intervals would contain the true population parameter value.
* How do we know whether it is valid?

```R
trap <- vector()
pvec <- vector()

for(i in 1:1e5){
  u <- runif(n=50,min=0,max=1)
  y <- ifelse(u>0.675,0,1)
  N <- length(y)
  pvec[i] <- mean(y)
  q <- 1-pvec[i]
  std.err <- sqrt(pvec[i]*q/N)
  lower.limit <- pvec[i]-1.811911*std.err
  upper.limit <- pvec[i]+1.811911*std.err
  trap[i] <- ifelse(lower.limit<=0.675 & upper.limit>=0.675,1,0)
  }

table(trap,exclude=NULL)
hist(pvec)
```

* Everything looks reasonable here.
* Central limit theorem?
  
---

##### Script #2

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
  trap[i] <- ifelse(lower.limit<=0.675 & upper.limit>=0.675,1,0)
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

---

* Note that there is yet another option called the equal-tailed Jeffreys interval.
* Let's calculate the Jeffreys interval for our prison release/recidivism example dataset:

```R
N <- 50
sy <- 37
lower.limit <- qbeta(p=0.035,shape1=1/2+sy,shape2=1/2+N-sy)
lower.limit
upper.limit <- qbeta(p=0.965,shape1=1/2+sy,shape2=1/2+N-sy)
upper.limit
```

* Check on the coverage of this interval for both the prison release/recidivism and 18-year-old delinquency problems.
* What do you conclude?

---

##### Script #3

* Now, let's consider a new example. The following dataset contains the entire population of people released from prison in NC for fiscal year 1978; for each person, the age at the time of release from prison is recorded.

```R
age <- c(rep(16,19),rep(17,161),rep(18,492),rep(19,480),rep(20,624),
  rep(21,599),rep(22,580),rep(23,468),rep(24,537),rep(25,443),rep(26,432),
  rep(27,338),rep(28,415),rep(29,292),rep(30,324),rep(31,254),rep(32,234),
  rep(33,179),rep(34,187),rep(35,167),rep(36,177),rep(37,132),rep(38,152),
  rep(39,117),rep(40,119),rep(41,93),rep(42,113),rep(43,102),rep(44,85),
  rep(45,75),rep(46,90),rep(47,72),rep(48,86),rep(49,62),rep(50,78),
  rep(51,61),rep(52,57),rep(53,50),rep(54,44),rep(55,49),rep(56,55),
  rep(57,34),rep(58,34),rep(59,25),rep(60,21),rep(61,18),rep(62,19),
  rep(63,11),rep(64,16),rep(65,7),rep(66,5),rep(67,13),rep(68,5),rep(69,3),
  rep(70,1),rep(71,3),rep(72,5),rep(73,3),rep(74,4),rep(75,2),rep(77,2),rep(78,2))

N <- length(age)
N
```

* Let's create a chart.

```R
barplot(table(age),
  xlab="Age (in years) at Time of Release",
  ylab="Number of People",
 main="Age at Release from Prison (1978 NCDOC)")
```

* Since the data are skewed, we might expect the mean and the median to be different.
* Which one should be greater?

```R
mean(age)
median(age)
```

* Let's repeatedly sample from this population to see the Central Limit Theorem in action:

```R
mna <- vector()
mda <- vector()

for(i in 1:1e5){
  s <- sample(1:N,size=300,replace=T)
  sa <- age[s]
  mna[i] <- mean(sa)
  mda[i] <- median(sa)
  }

par(mfrow=c(1,3))
hist(mna)
hist(mda)
boxplot(mna,mda)
mean(mna)
sd(mna)
```

* Now, let's work with 3 individual samples from this population:

```R
s <- sample(1:N,size=300,replace=T)
ys <- age[s]
mean(ys)
sd(ys)/sqrt(300)

s <- sample(1:N,size=300,replace=T)
ys <- age[s]
mean(ys)
sd(ys)/sqrt(300)

s <- sample(1:N,size=300,replace=T)
ys <- age[s]
mean(ys)
sd(ys)/sqrt(300)
```

* Then, we draw a sample of size N = 35 and calculate a 90% confidence interval.

```R
s <- sample(1:N,size=35,replace=T)
ys <- age[s]
mean(ys)
std.err <- sd(ys)/sqrt(35)
std.err
qt(p=0.05,df=35-1)
qt(p=0.95,df=35-1)
lower.limit <- mean(ys)-1.690924*std.err
lower.limit
upper.limit <- mean(ys)+1.690924*std.err
upper.limit
```

* Did the 90% confidence interval trap the true population parameter value?
* Now, let's repeatedly sample with samples of size N = 20 and check on the coverage rate for this procedure.

```R
qt(p=0.05,df=20-1)
qt(p=0.95,df=20-1)

trap <- vector()

for(i in 1:1e5){
  s <- sample(1:N,size=20,replace=T)
  ys <- age[s]
  std.err <- sd(ys)/sqrt(20)
  lower.limit <- mean(ys)-1.729133*std.err
  upper.limit <- mean(ys)+1.729133*std.err
  trap[i] <- ifelse(lower.limit<=29.32787 & upper.limit>=29.32787,1,0)
  }

table(trap)
```

* What happens if we increase the sample size to 300?

```R
qt(p=0.05,df=300-1)
qt(p=0.95,df=300-1)
qnorm(p=0.05)
qnorm(p=0.95)

trap <- vector()

for(i in 1:1e5){
  s <- sample(1:N,size=300,replace=T)
  ys <- age[s]
  std.err <- sd(ys)/sqrt(300)
  lower.limit <- mean(ys)-1.649966*std.err
  upper.limit <- mean(ys)+1.649966*std.err
  trap[i] <- ifelse(lower.limit<=29.32787 & upper.limit>=29.32787,1,0)
  }

table(trap)
```

---
* Try the above exercises with this new dataset comprised of the 1980 age-at-release data from the NC Department of Corrections:

```R
age <- c(rep(15,1),rep(16,20),rep(17,224),rep(18,504),rep(19,472),rep(20,626),
  rep(21,517),rep(22,601),rep(23,516),rep(24,565),rep(25,407),rep(26,495),
  rep(27,302),rep(28,397),rep(29,291),rep(30,298),rep(31,261),rep(32,330),
  rep(33,224),rep(34,231),rep(35,163),rep(36,194),rep(37,157),rep(38,149),
  rep(39,125),rep(40,129),rep(41,116),rep(42,100),rep(43,88),rep(44,105),
  rep(45,88),rep(46,80),rep(47,72),rep(48,60),rep(49,68),rep(50,67),
  rep(51,64),rep(52,50),rep(53,47),rep(54,51),rep(55,47),rep(56,42),
  rep(57,28),rep(58,39),rep(59,12),rep(60,29),rep(61,12),rep(62,13),
  rep(63,8),rep(64,19),rep(65,12),rep(66,9),rep(67,2),rep(68,5),rep(69,3),
  rep(70,6),rep(71,1),rep(73,2),rep(74,2),rep(75,1),rep(77,1),rep(79,1))
```

#### R Code for Thursday 9/18/25

##### Review Time

* You are given a random sample of 380 people who were all released from prison 3 years ago.
* Each of these people has been followed up and we have determined that 237 of them "failed" or "recidivated."
* Calculate the sample estimate of the recidivism rate.
* Calculate a 93% confidence interval for the recidivism rate using the normal approximation to the binomial or Wald formula.
* Calculate a 93% confidence interval for the recidivism rate using the Clopper-Pearson approach.
* Calculate a 93% confidence interval using the Jeffreys prior approach.
* What should we consider when making a decision about which interval to report?
* Conduct a coverage rate analysis for each of the 3 intervals with the knowledge that the population recidivism rate is 65%. How does this affect your decision about which interval to report?

```R
trials <- 380
events <- 237

recidivism.rate <- events/trials
recidivism.rate

# normal approximation to binomial/Wald (discussed in WB)

p <- recidivism.rate
p
q <- 1-p
q
se.p <- sqrt(p*q/trials)
se.p

qnorm(p=0.035)
z <- qnorm(p=0.965)
z

lcl <- p-z*se.p
lcl
ucl <- p+z*se.p
ucl

# Clopper-Pearson approach (discussed in the Brown paper)

lcl <- qbeta(p=0.035,shape1=events,shape2=1+trials-events)
lcl
ucl <- qbeta(p=0.965,shape1=1+events,shape2=trials-events)
ucl

# Jeffreys prior approach (discussed in the Brown paper)

lcl <- qbeta(p=0.035,shape1=1/2+events,shape2=1/2+trials-events)
lcl
ucl <- qbeta(p=0.965,shape1=1/2+events,shape2=1/2+trials-events)
ucl
```

---

* Now, we check on the actual performance of the 3 different types of confidence intervals.

```R
# simulate 10,000 datasets

pop.p <- 0.65
trials <- 380

events <- vector()

for(i in 1:1e4){
  events[i] <- rbinom(n=1,size=trials,p=pop.p)
  }

events[1:12]
events[9995:10000]

# calculate normal approximation to binomial confidence intervals (93%)
# and trap rate

qnorm(p=0.035)
z <- qnorm(p=0.965)
z

p.est <- events/trials
hist(p.est,main="sampling distribution of recidivism rate")
q.est <- 1-p.est
se.p <- sqrt(p.est*q.est/trials)
lcl.na <- p.est-z*se.p
ucl.na <- p.est+z*se.p
trap.na <- ifelse(lcl.na<pop.p & ucl.na>pop.p,1,0)
table(trap.na)

# calculate clopper-pearson confidence intervals
# and trap rate

lcl.cp <- qbeta(p=0.035,shape1=events,shape2=1+trials-events)
ucl.cp <- qbeta(p=0.965,shape1=1+events,shape2=trials-events)
trap.cp <- ifelse(lcl.cp<=pop.p & ucl.cp>=pop.p,1,0)
table(trap.cp)

# calculate jeffreys prior confidence intervals
# and trap rate

lcl.jp <- qbeta(p=0.035,shape1=1/2+events,shape2=1/2+trials-events)
ucl.jp <- qbeta(p=0.965,shape1=1/2+events,shape2=1/2+trials-events)
trap.jp <- ifelse(lcl.jp<=pop.p & ucl.jp>=pop.p,1,0)
table(trap.jp)
```

---

* Now, let's suppose we are looking at the prevalence of self-reported delinquency; our sample size is 52 and our population p is 0.92. How well do the 87% confidence intervals perform in this context?

```R
# simulate 10,000 datasets

pop.p <- 0.92
trials <- 52

events <- vector()

for(i in 1:1e4){
  events[i] <- rbinom(n=1,size=trials,p=pop.p)
  }

events[1:12]
events[9995:10000]

# calculate normal approximation to binomial confidence intervals (87%)
# and trap rate

qnorm(p=0.065)
z <- qnorm(p=0.935)
z

p.est <- events/trials
hist(p.est,main="sampling distribution of delinquency rate")
q.est <- 1-p.est
se.p <- sqrt(p.est*q.est/trials)
lcl.na <- p.est-z*se.p
ucl.na <- p.est+z*se.p
trap.na <- ifelse(lcl.na<=pop.p & ucl.na>=pop.p,1,0)
table(trap.na)

# calculate clopper-pearson confidence intervals
# and trap rate

lcl.cp <- qbeta(p=0.065,shape1=events,shape2=1+trials-events)
ucl.cp <- qbeta(p=0.935,shape1=1+events,shape2=trials-events)
trap.cp <- ifelse(lcl.cp<pop.p & ucl.cp>pop.p,1,0)
table(trap.cp)

# calculate jeffreys prior confidence intervals
# and trap rate

lcl.jp <- qbeta(p=0.065,shape1=1/2+events,shape2=1/2+trials-events)
ucl.jp <- qbeta(p=0.935,shape1=1/2+events,shape2=1/2+trials-events)
trap.jp <- ifelse(lcl.jp<=pop.p & ucl.jp>=pop.p,1,0)
table(trap.jp)
```

* What conclusion can you draw about the reliability of the different confidence interval procedures?

---

* Next, we work with the dataset we studied at the end of last week's class.
* Note that we are still reviewing confidence intervals (emphasis on chapter 20 of WB)

##### Script #1

```R
# enter the data

age <- c(rep(15,1),rep(16,20),rep(17,224),rep(18,504),rep(19,472),rep(20,626),
  rep(21,517),rep(22,601),rep(23,516),rep(24,565),rep(25,407),rep(26,495),
  rep(27,302),rep(28,397),rep(29,291),rep(30,298),rep(31,261),rep(32,330),
  rep(33,224),rep(34,231),rep(35,163),rep(36,194),rep(37,157),rep(38,149),
  rep(39,125),rep(40,129),rep(41,116),rep(42,100),rep(43,88),rep(44,105),
  rep(45,88),rep(46,80),rep(47,72),rep(48,60),rep(49,68),rep(50,67),
  rep(51,64),rep(52,50),rep(53,47),rep(54,51),rep(55,47),rep(56,42),
  rep(57,28),rep(58,39),rep(59,12),rep(60,29),rep(61,12),rep(62,13),
  rep(63,8),rep(64,19),rep(65,12),rep(66,9),rep(67,2),rep(68,5),rep(69,3),
  rep(70,6),rep(71,1),rep(73,2),rep(74,2),rep(75,1),rep(77,1),rep(79,1))

barplot(table(age))
mean(age)
median(age)
N <- length(age)

# we draw 3 samples of size N=300 from the population
# and calculate mean, std error, and 95% confidence interval for each sample

qt(p=0.025,df=300-1)
t <- qt(p=0.975,df=300-1)
t

s <- sample(1:N,size=300,replace=T)
ys <- age[s]
mean(ys)
stderr <- sd(ys)/sqrt(300)
stderr
lcl <- mean(ys)-t*stderr
lcl
ucl <- mean(ys)+t*stderr
ucl

s <- sample(1:N,size=300,replace=T)
ys <- age[s]
mean(ys)
stderr <- sd(ys)/sqrt(300)
stderr
lcl <- mean(ys)-t*stderr
lcl
ucl <- mean(ys)+t*stderr
ucl

s <- sample(1:N,size=300,replace=T)
ys <- age[s]
mean(ys)
stderr <- sd(ys)/sqrt(300)
stderr
lcl <- mean(ys)-t*stderr
lcl
ucl <- mean(ys)+t*stderr
ucl
```

##### Script #2

* Now, let's consider a sample of size N = 35 and the 92% confidence interval for the mean age.

```R
s <- sample(1:N,size=35,replace=T)
ys <- age[s]
mean(ys)
std.err <- sd(ys)/sqrt(35)
std.err

qt(p=0.04,df=35-1)
t <- qt(p=0.96,df=35-1)
t

lower.limit <- mean(ys)-t*std.err
lower.limit
upper.limit <- mean(ys)+t*std.err
upper.limit
```
* Did we trap the true population parameter value?

##### Script #3

* Let's draw repeated samples of size N = 15 from the population and calculate 93% confidence intervals in each sample.

```R
qt(p=0.035,df=15-1)
t <- qt(p=0.965,df=15-1)
t

trap <- vector()
mys <- vector()

for(i in 1:1e4){
  s <- sample(1:N,size=15,replace=T)
  ys <- age[s]
  mys[i] <- mean(ys)
  std.err <- sd(ys)/sqrt(15)
  lower.limit <- mean(ys)-t*std.err
  upper.limit <- mean(ys)+t*std.err
  trap[i] <- ifelse(lower.limit<=29.17824 & upper.limit>=29.17824,1,0)
  }

table(trap)
hist(mys)
```

* What fraction of the confidence intervals contained the true population parameter?
* Does this confidence interval undercover or overcover?

##### Script #4

* Let's draw repeated samples of size N = 200 from the population and calculate 91% confidence intervals in each sample.

```R
qt(p=0.045,df=200-1)
t <- qt(p=0.955,df=200-1)
t

trap <- vector()

for(i in 1:1e4){
  s <- sample(1:N,size=200,replace=T)
  ys <- age[s]
  std.err <- sd(ys)/sqrt(200)
  lower.limit <- mean(ys)-t*std.err
  upper.limit <- mean(ys)+t*std.err
  trap[i] <- ifelse(lower.limit<=29.17824 & upper.limit>=29.17824,1,0)
  }

table(trap)
```

* What fraction of the confidence intervals contained the true population parameter?
* Does this confidence interval undercover or overcover?

#### R Code for Thursday 9/25/25

* In today's class, we consider the issue of using confidence intervals to study the uncertainty of estimating the difference between two sample means (when the populaton difference is known).
* Next, we study confidence intervals as a measure of uncertainty for the sample median.
* Then, we examine the issue of uncertainty in assessing differences between medians and proportions.

##### Script #1

* We now consider a more complicated analysis wherein we estimate the *difference* between the mean age values for the two populations (1978 and 1980 releasees). We will calculate a 80% confidence interval for the sample estimate of the difference between the 2 cohort means.

```R
age78 <- c(rep(16,19),rep(17,161),rep(18,492),rep(19,480),rep(20,624),
  rep(21,599),rep(22,580),rep(23,468),rep(24,537),rep(25,443),rep(26,432),
  rep(27,338),rep(28,415),rep(29,292),rep(30,324),rep(31,254),rep(32,234),
  rep(33,179),rep(34,187),rep(35,167),rep(36,177),rep(37,132),rep(38,152),
  rep(39,117),rep(40,119),rep(41,93),rep(42,113),rep(43,102),rep(44,85),
  rep(45,75),rep(46,90),rep(47,72),rep(48,86),rep(49,62),rep(50,78),
  rep(51,61),rep(52,57),rep(53,50),rep(54,44),rep(55,49),rep(56,55),
  rep(57,34),rep(58,34),rep(59,25),rep(60,21),rep(61,18),rep(62,19),
  rep(63,11),rep(64,16),rep(65,7),rep(66,5),rep(67,13),rep(68,5),rep(69,3),
  rep(70,1),rep(71,3),rep(72,5),rep(73,3),rep(74,4),rep(75,2),rep(77,2),rep(78,2))
 
age80 <- c(rep(15,1),rep(16,20),rep(17,224),rep(18,504),rep(19,472),rep(20,626),
  rep(21,517),rep(22,601),rep(23,516),rep(24,565),rep(25,407),rep(26,495),
  rep(27,302),rep(28,397),rep(29,291),rep(30,298),rep(31,261),rep(32,330),
  rep(33,224),rep(34,231),rep(35,163),rep(36,194),rep(37,157),rep(38,149),
  rep(39,125),rep(40,129),rep(41,116),rep(42,100),rep(43,88),rep(44,105),
  rep(45,88),rep(46,80),rep(47,72),rep(48,60),rep(49,68),rep(50,67),
  rep(51,64),rep(52,50),rep(53,47),rep(54,51),rep(55,47),rep(56,42),
  rep(57,28),rep(58,39),rep(59,12),rep(60,29),rep(61,12),rep(62,13),
  rep(63,8),rep(64,19),rep(65,12),rep(66,9),rep(67,2),rep(68,5),rep(69,3),
  rep(70,6),rep(71,1),rep(73,2),rep(74,2),rep(75,1),rep(77,1),rep(79,1))

N78 <- length(age78)
N78
N80 <- length(age80)
N80
 
# population parameter
 
pop.delta <- mean(age80)-mean(age78)
pop.delta

# draw a simple random sample of 300 cases
# from each year's population

s78 <- sample(1:N78,size=300,replace=T)
s80 <- sample(1:N80,size=300,replace=T)
 
ys78 <- age78[s78]
ys80 <- age80[s80]

# calculate the difference between the 2 sample means:
 
delta.hat <- mean(ys80)-mean(ys78)
delta.hat

# calculate the 80% confidence interval for the difference between the 2 sample means (WB, page 617)

# calculate the critical value of t

qt(p=0.1,df=600-2)
tc <- qt(p=0.9,df=600-2)
tc

# method 1: separate variance method

v80 <- var(ys80)
v78 <- var(ys78)
n80 <- 300
n78 <- 300
se.delta <- sqrt(v80/(n80-1)+v78/(n78-1))

lower.limit <- delta.hat-tc*se.delta
lower.limit

upper.limit <- delta.hat+tc*se.delta
upper.limit

# method 2: pooled variance method

p1 <- n80*v80+n78*v78
p2 <- n80+n78-2
p3 <- (n80+n78)/(n80*n78)
se.delta <- sqrt(p1/p2)*sqrt(p3)

lower.limit <- delta.hat-tc*se.delta
lower.limit

upper.limit <- delta.hat+tc*se.delta
upper.limit
```

* Did we trap the true population parameter value?
* Let's see how the separate variance *procedure* performs across repeated samples:

```R
qt(p=0.1,df=600-2)
tc <- qt(p=0.9,df=600-2)
tc

trap <- vector()
 
for(i in 1:1e5){
  s78 <- sample(1:N78,size=300,replace=T)
  s80 <- sample(1:N80,size=300,replace=T)
  ys78 <- age78[s78]
  ys80 <- age80[s80]
  delta.hat <- mean(ys80)-mean(ys78)
  v80 <- var(ys80)
  v78 <- var(ys78)
  n80 <- 300
  n78 <- 300
  se.delta <- sqrt(v80/(n80-1)+v78/(n78-1))
  lower.limit <- delta.hat-tc*se.delta
  upper.limit <- delta.hat+tc*se.delta
  trap[i] <- ifelse(lower.limit<=pop.delta & upper.limit>=pop.delta,1,0)
  }

table(trap)
```

* What conclusion do we reach about the performance of this confidence interval procedure?
* I will leave it as an exercise for you to see what results you get if you use the pooled variance procedure.
---

##### Script #2

* We now consider the problem of estimating a confidence interval for the sample median.
* To investigate this, we will use the 1978 NCDOC data:

```R
set.seed(381)
 
age78 <- c(rep(16,19),rep(17,161),rep(18,492),rep(19,480),rep(20,624),
  rep(21,599),rep(22,580),rep(23,468),rep(24,537),rep(25,443),rep(26,432),
  rep(27,338),rep(28,415),rep(29,292),rep(30,324),rep(31,254),rep(32,234),
  rep(33,179),rep(34,187),rep(35,167),rep(36,177),rep(37,132),rep(38,152),
  rep(39,117),rep(40,119),rep(41,93),rep(42,113),rep(43,102),rep(44,85),
  rep(45,75),rep(46,90),rep(47,72),rep(48,86),rep(49,62),rep(50,78),
  rep(51,61),rep(52,57),rep(53,50),rep(54,44),rep(55,49),rep(56,55),
  rep(57,34),rep(58,34),rep(59,25),rep(60,21),rep(61,18),rep(62,19),
  rep(63,11),rep(64,16),rep(65,7),rep(66,5),rep(67,13),rep(68,5),rep(69,3),
  rep(70,1),rep(71,3),rep(72,5),rep(73,3),rep(74,4),rep(75,2),rep(77,2),rep(78,2))

N <- length(age78)

# calculate the population median

median(age78)

# draw a sample and calculate the sample median

s <- sample(1:N,size=300,replace=T)
ys <- age78[s]
median(ys)
```

* Now, let's use the bootstrap to calculate the 87% confidence interval for the sample median.
* Note that this approach is described in Wasserman (2004:109-110).
* This approach assumes that the *bootstrap distribution* of the sample median is "close to normal."

```R
qt(p=0.065,df=300-1)
tc <- qt(p=1-0.065,df=300-1)
tc

smd <- vector()
 
for(i in 1:3000){
  b <- sample(1:300,size=300,replace=T)
  yb <- ys[b]
  smd[i] <- median(yb)
  }

# confidence interval for the median

median(ys)-tc*sd(smd)
median(ys)+tc*sd(smd)

# create a histogram of the bootstrapped medians

hist(smd)
```

* If we want to check on the actual coverage performance of this confidence interval procedure, then we use the following approach.
* We will draw 3000 samples and then draw 3000 bootstrap samples for each of the 1000 samples.

```R
trap.median <- vector()

for(i in 1:3000){
  s <- sample(1:N,size=300,replace=T)
  yrs <- age78[s]

  smd <- vector()
 
  for(j in 1:3000){
    b <- sample(1:300,size=300,replace=T)
    yb <- yrs[b]
    smd[j] <- median(yb)
    }

  lcl.median <- median(yrs)-tc*sd(smd)
  ucl.median <- median(yrs)+tc*sd(smd)

  trap.median[i] <- ifelse(lcl.median<=median(age78) & ucl.median>=median(age78),1,0)
  }

table(trap.median)
```

* What is our trap (coverage) rate from this study?

---

##### Script #3

* Wasserman (2004:110-112) introduces the concept of the *pivotal bootstrap* which makes weaker assumptions than the normal bootstrap above.
* Here, the key assumption is that the bootstrapped distribution of sample medians is a good approximation to the actual sampling distribution of sample medians.

```R
smd <- vector()
 
for(i in 1:10000){
  b <- sample(1:300,size=300,replace=T)
  yb <- ys[b]
  smd[i] <- median(yb)
  }

# confidence interval for the median

lcl <- 2*median(ys)-quantile(smd,0.935)
lcl
ucl <- 2*median(ys)-quantile(smd,0.065)
ucl

# create a boxplot showing the bootstrapped distribution of sample medians alongside
# the (approximate) sampling distribution of the sample medians

mdrs <- vector()

for(i in 1:10000){
  s <- sample(1:N,size=300,replace=T)
  ys <- age78[s]
  mdrs[i] <- median(ys)
  }

boxplot(smd,mdrs)
```

* Check on the coverage rate.

```R
trap.median <- vector()

for(i in 1:3000){
  s <- sample(1:N,size=300,replace=T)
  yrs <- age78[s]

  smd <- vector()
 
  for(j in 1:3000){
    b <- sample(1:300,size=300,replace=T)
    yb <- yrs[b]
    smd[j] <- median(yb)
    }

  lcl.median <- 2*median(yrs)-quantile(smd,0.935)
  ucl.median <- 2*median(yrs)-quantile(smd,0.065)

  trap.median[i] <- ifelse(lcl.median<=median(age78) & ucl.median>=median(age78),1,0)
  }

table(trap.median)
```

##### Script #4

* We now consider the *percentile interval*
* Here is the R code:

```R
smd <- vector()
 
for(i in 1:3000){
  b <- sample(1:300,size=300,replace=T)
  yb <- ys[b]
  smd[i] <- median(yb)
  }

# confidence interval for the median

lcl <- quantile(smd,0.065)
lcl
ucl <- quantile(smd,0.935)
ucl
```

* Let's check on the coverage rate:

```R
trap.median <- vector()

for(i in 1:3000){
  s <- sample(1:N,size=300,replace=T)
  yrs <- age78[s]

  smd <- vector()
 
  for(j in 1:3000){
    b <- sample(1:300,size=300,replace=T)
    yb <- yrs[b]
    smd[j] <- median(yb)
    }

  lcl.median <- quantile(smd,0.065)
  ucl.median <- quantile(smd,0.935)

  trap.median[i] <- ifelse(lcl.median<=median(age78) & ucl.median>=median(age78),1,0)
  }

table(trap.median)
```

##### Script #6

* Since the median is the 50% percentile of a distribution, the probability an observation drawn at random is above or below the median is 0.5.
* Step 1: consider the number of cases in the dataset; for our example, the sample size is 300.
* Step 2: find the 6.5th and 93.5th percentiles of the binomial distribution with 300 trials and success probability 0.5 (since we are studying the median).
* Step 3: with 300 cases and success probability 0.5, the endpoints of the confidence interval will be at positions 137 and 163.
* Step 4: sort the dataset.
* Step 5: find the value of the variable being studied in positions 137 and 163.
* This is a so-called *exact* procedure.

```R
ns <- 300
qbinom(p=0.065,size=300,prob=0.5)
qbinom(p=0.935,size=300,prob=0.5)
sort.ys <- sort(ys)
lcl <- sort.ys[137]
lcl
ucl <- sort.ys[163]
ucl
```

* Let's check on the coverage rate of the exact procedure:

```R
trap.median <- vector()

for(i in 1:3000){
  s <- sample(1:N,size=300,replace=T)
  yrs <- age78[s]
  sort.yrs <- sort(yrs)
  lcl.median <- sort.yrs[137]
  ucl.median <- sort.yrs[163]
  trap.median[i] <- ifelse(lcl.median<=median(age78) & ucl.median>=median(age78),1,0)
  }

table(trap.median)
```

##### Script #7

* Let's return to the question of proportions, except now we will consider the difference between 2 proportions.
* 
