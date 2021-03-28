Thesis outline proposal
========================================================
author: Steven Wallaert
date: 07 / 12 / 2020
autosize: true

General outline
========================================================
- Intro
  - aim and scope
  - literature review / survey
- Methods
  - Inclusion of methods
  - Data generating mechanism
  - Data preprocessing & model selection procedures
  - Estimands
  - Analysis

Rationale
========================================================
- AI & ML tools highly popular because of their proven strengths

- Scientists in pre-clinical pharmacological research want to apply ML 

- Little is known about performance in these specific contexts
  - High dimensional
  - Very small sample sizes (10 to 50, 100 at most)
  
- Therefore there is need for a __neutral comparison study__ 

- Findings of this study should ultimately result in guidelines

- Finally, an analysis of a real data set should be included as a demonstration of how guidelines can be punt into practice



Aim and scope
========================================================

Aim:

To identify
- the **best performing methods** for every scenario in the simulation study
  - needs definition of 'best performing'
- **limitations** and **weaknesses** of every method under study, 'soft limits'
- **conditions** that 'break' the considered methods in one way or another, 'hard limits'

So we can inform

- method users
- method designers

***

Scope:


- Pre-clinical pharmacological research 
- High dimensional data 
- (Extremely) small sample sizes (or sample size to feature ratios)
- Supervised ML methods (prediction), where ML is defined broadly



Inclusion of methods
========================================================

Selection should be as neutral as possible.

Weber and colleagues advise to be _"as comprehensive as possible"_.

Proposal:

- Next to input from Janssen Pharmaceuticals
- Small literature review / survey

Inclusion of methods (continued)
========================================================

Benefits of performing small literature review / survey:

- Relevance:
  - results will be applicable because all methods are effectively used in recent research
  - avoids risk of excluding key methods
  - avoids unnecessary comparisons of e.g. obsolete methods
- Efficiency:
  - Only 'real' candidates will be considered
  - No obviously invalid or unused methods will be considered
- Less subjective:
  - Inclusion of methods subject to 'reasonable' criteria, e.g. k most frequently used methods, or top k best performing methods


Literature review / survey
========================================================

- selection of search terms: 
"machine learning", "small data", "small sample size", "high dimensional data", "supervised learning", "prediction", "predictive modeling" ?
- which databases, when, what time period
- inclusion criteria
  - English
  - accessible
  - high dimensional data
  - prediction using ML (broadly defined)
  - sample size <= 100
  - field? --> What about other fields with similar data set structures? e.g. SL on (f)MRI data
- title screening
- abstract screening

Output:

- list of used methods
- optional: reported performance

Data generating mechanism(s)
========================================================

Simulated data

- Should be as realistic as possible
  - This should be checked / demonstrated using inspection of key empirical summaries
    - Which summaries? --> requires domain knowledge
- Needs a wide variety of conditions or scenarios

***

Which factors to vary?
- sample size
- number of features (absolute or relative)
- number of predictive features (absolute or relative)
- correlation structure of features
- response balance
- how responses are determined
- Other, perhaps domain specific, factors?

Data preprocessing & model selection procedures
========================================================
Shoul mimic 'real' analyses as much as possible.

- How are these data typically preprocessed?
  - literature review
  - Janssen Pharmaceuticals
- Model building / selection
  - train-test split
  - hyper-parameter tuning through CV, select model with minimal CV-error? or AIC, BIC etc. ?


Estimands: Performance criteria
========================================================
Performance metric(s)
- Which one(s) to use? --> review / survey?
- Calculated on
  - test split (unbiased, high variance)
  - very large sample from 'population' (unbiased, low variance)

Selection / Detection rate

Analysis of results
========================================================

- Exploratory
- Formal: full factorial design analyzed by means of Beta regression (?)
  - Of interest: main / interaction effects of "methods"

References
========================================================

Boulesteix, A.‐L., Hoffmann, S., Charlton, A. and Seibold, H. (2020), A replication crisis in methodological research?. Significance, 17: 18-21. https://doi.org/10.1111/1740-9713.01444

Weber, L.M., Saelens, W., Cannoodt, R. et al. Essential guidelines for computational method benchmarking. Genome Biol 20, 125 (2019). https://doi.org/10.1186/s13059-019-1738-8

Vabalas A, Gowen E, Poliakoff E, Casson AJ (2019) Machine learning algorithm validation with a limited sample size. PLOS ONE 14(11): e0224365. https://doi.org/10.1371/journal.pone.0224365

Boulesteix AL. Ten simple rules for reducing overoptimistic reporting in methodological computational research. PLoS Comput Biol. 2015;11(4):e1004191. Published 2015 Apr 23. doi:10.1371/journal.pcbi.1004191

 Morris, TP, White, IR, Crowther, MJ. Using simulation studies to evaluate statistical methods. Statistics in Medicine. 2019; 38: 2074– 2102. https://doi.org/10.1002/sim.8086 
