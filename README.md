# project_rlddm

## Implementation of Reinforcement Learning Drift Diffusion Model in R Stan with Go/No-Go Task Data

This repository includes R scripts and stan codes for my 2019 Spring Computational Modeling final project. <br>

### Directories
[1_analysis](https://github.com/Jihyuncindyhur/project_rlddm/tree/master/1_analysis): hierarchical Bayesian analysis of RLDDM <br>
[2_param_recovery](https://github.com/Jihyuncindyhur/project_rlddm/tree/master/2_param_recovery): parameter recovery of three suggested models <br>
[3_simulation](https://github.com/Jihyuncindyhur/project_rlddm/tree/master/3_simulation): generating fake datasets with true parameters <br>
[4_plotting](https://github.com/Jihyuncindyhur/project_rlddm/tree/master/4_plotting): plotting parameter recovery results <br>
[5_data](https://github.com/Jihyuncindyhur/project_rlddm/tree/master/4_plotting): a list of datasets

### Files
- rlddm_allsubj: hierarchical Bayesian analysis results with the entire subject data
- rlddm_final_hur: pdf slides for the final presentation

### Data Reference

Millner, A. J., den Ouden, H. E., Gershman, S. J., Glenn, C. R., Kearns, J. C., Bornstein, A. M., Marx, B. P., Keane, T. M., & Nock, M. K. (2019). Suicidal thoughts and behaviors are associated with an increased decision-making bias for active responses to escape aversive states. *Journal of Abnormal Psychology,* 128(2):106-118.


### Plans
1. Model comparison with the whole 129 subjects
2. Parameter recoveries with multiple combinations of true parameters
3. RLDDM for a task with two alternative choices (e.g. probabilistic selection task) 
