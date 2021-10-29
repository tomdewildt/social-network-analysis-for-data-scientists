# Homeplay for week 9

## ERGM I
hookups <- ina::Hookups

### 1. Explore the network (N, E, attributes, D or U) and run the necessary
###    descriptive statistics (remember to check the class and type of each
###    attribute).
hookups

### 2. What are the hypotheses formulated in the book for this example?

#### 1. Characters hookup with other characters of similar age
#### 2. Characters hookup with other characters in the same job postion
#### 3. Characters hookup with other characters of the same race
#### 4. Characters hookup with other characters of different sex
#### 5. Younger characters have more hookups
#### 6. Characters in the show for a short me had less opportunity for hook

### 3. Open page 73 and insert the terms specified in the example in a series
###    of nested models. The model on page 73 is your final complete model—nest
###    at least 3 of them to explore the effects of the terms one by one.
m0 <- ergm::ergm(hookups ~ edges)
m1 <- ergm::ergm(hookups ~ edges + absdiff("birthyear"))
m2 <- ergm::ergm(hookups ~ edges + nodecov("birthyear"))
m3 <- ergm::ergm(hookups ~ edges + nodematch("position"))
m4 <- ergm::ergm(hookups ~ edges + nodematch("race"))
m5 <- ergm::ergm(hookups ~ edges + nodematch("sex"))
m6 <- ergm::ergm(hookups ~ edges + nodecov("season"))

### 3a. Which is the best model?
texreg::screenreg(list(m0, m1, m2, m3, m4, m5, m6))

#### By analyzing the results we can see that model 3 is the best. Because the
#### AIC (371.58) and BIC (381.28) score is the highest of all models.

### 3b. Can you tell whether each model is dyadic (inter)dependent or
###     independent?

#### All models are dyadic independent.

### 3c. Is each of these variables exogenous or endogenous?

#### All the variables are exogenous. Because they are all related to attributes
#### in the network and outside the structure of the network.

### 3d. Comment on the results. What is the substantive meaning of each of
###     these terms? Are the hypotheses supported? Comment on the odd ratios
###     too.
m7 <- ergm::ergm(hookups ~ edges + absdiff("birthyear") + nodecov("birthyear") +
                   nodematch("position") + nodematch("race") + nodematch("sex") +
                   nodecov("season"))
texreg::screenreg(list(m7))

#### 1. Characters hookup with other characters of similar age:
####    This hypothesis is supported, the probability of hookup increases with
####    an difference in age (absdiff.birthyear < 0 && stat. signf.)

#### 2. Characters hookup with other characters in the same job position
####    This hypothesis is supported, the probability of hookup increases with
####    when characters share the same job (nodematch.position > 0 && stat.
####    signf.)

#### 3. Characters hookup with other characters of the same race
####    This hypothesis is supported, the probability of hookup increases with
####    with characters of same race (nodematch.race > 0 && stat. signf.)

#### 4. Characters hookup with other characters of different sex
####    This hypothesis is supported, the probability of hookup increases with
####    with characters of different sex (nodematch.sex < 0 && stat. signf.)

#### 5. Younger characters have more hookups
####    This hypothesis is not supported.

#### 6. Characters in the show for a short me had less opportunity for hook
####    This hypothesis is supported, the probability of hookup decreases for
####    characters that entered the show in later seasons (nodecov.season < 0
####    && stat. signf.)

### 3e. What is the difference between absdif and nodecov estimated on
###     birthyear?

#### nodecov: predicts ties as variable increases (e.g. older more hookups)
#### absdiff: predicts ties according to similairty in variable (e.g.
#### similarity in age)

### 4. Formulate one additional hypothesis on the network’s structure and then
###    nest another model, adding one variable to explore the network’s
###    structure and test this final hypothesis.
