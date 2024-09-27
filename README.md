<p align="center">
  <img src="https://github.com/robertocerinaprojects/PoSSUM/assets/51832016/c6aab730-ec8c-4bda-a9a3-2024594b0ec8" alt="PossumLogo" width="200" />
</p>


# PoSSUM
A Protocol for Surveying Social-media Users with Multimodal LLMs

## How to use this repository 
This repository contains the following information: 
- The [Technical Report](./PoSSUM_Technical_Report.pdf) is a detailed note on how the protocol works (*please download it and view it on your own machine as the GitHub preview doesn't display properly*). It contains a detailed description of the algorithmic routines used, as well as the statistical models applied to weight the data. It also presents granular crosstab-level comparisons of PoSSUM's estimates against those of other pollsters in the field during similar dates;
- The [survey_objects](./survey_objects) folder contains anonymized, dated, survey-like microdata generated at each round of PoSSUM polling for the 2024 US election;
- The [press_releases](./press_releases) folder (*please download these and view them on your own machine as the GitHub preview doesn't display properly*) contains topline and crosstab summaries for each PoSSUM poll conducted during the 2024 US election;
- The [R scripts](./R%20scripts) folder contains every R script necessary to run the PoSSUM protocol and the weighting procedure. The code is a bit rough at this stage—the intention is to slowly clean it up as time allows and make it accessible to the wider public, but for now, only savvy users who understand the technical report will be able to run it on their own machines. Each sub-directory's scripts are presented according to execution order. The code is divided into three parts:
    + Part 1. Involves the preparation of a `Stratification Frame', which is necessary to weight the data and generate quotas;
    + Part 2. Is the running of the poll itself;
    + Part 3. Describes the weighting according to MrP;
    + [README_BEFORE_NEW_POLL.txt](./R%20scripts/README_BEFORE_NEW_POLL.txt) describes scripts that have to be tweaked to reflect the pollster's desires before re-running each poll;
- The [utils](./utils) folder presents a series of helpful functions to execute the protocol;
- The [exe](./exe.R) script runs the poll from top to bottom. This should only be executed after the parameters described in [README_BEFORE_NEW_POLL.txt](./R%20scripts/README_BEFORE_NEW_POLL.txt) are updated. 

## PoSSUM Estimates 
Note: The MrP fits a random-walk temporal effect to the data. The values reported below are a projection of the public's preferences on exactly the last day of the fieldwork period -- not the average across fieldwork days. 


### Topline estimates 

| Population | Vote2024                       | 15/08 to  23/08       | 07/09 to 11/09        |
|------------|--------------------------------|-----------------------|-----------------------|
| Voters     | Kamala Harris (D)              |                       |                       |
| Voters     | Donald Trump (R)               |                       |                       |
| Voters     | Robert F. Kennedy Jr. (Ind.)   |                       |                       |
| Voters     | Jill Stein (G)                 |                       |                       |
| Voters     | Cornel West (Ind.)             |                       |                       |
| Voters     | Chase Oliver (L)               |                       |                       |
| Adults     | Stay Home                      |                       |                       |
| Adults     | Turnout                        |                       |                       |

### (DEPRECIATED) Initial Topline estimates (15/08/2024 to 23/08/2024)
The initial PoSSUM estimates, fit to a ballot-access map prior to RFK Jr. endorsig Trump and withrdrawing from key states, and weighted with an MrP model which did not take into account temporal dynamics. This is **depreciated**, and should not be used to analyse poll-to-poll swings. It is retained here for transparency.  

| Population | Vote2024                      | Topline               |
|------------|--------------------------------|-----------------------|
| Voters     | Kamala Harris (D)              | 45.8 (43.8, 47.8)     |
| Voters     | Donald Trump (R)               | 45.6 (43.3, 47.2)     |
| Voters     | Robert F. Kennedy Jr. (Ind.)   | 5.5 (3.8, 7.5)        |
| Voters     | Jill Stein (G)                 | 0.9 (0.5, 2.6)        |
| Voters     | Cornel West (Ind.)             | 0.8 (0.4, 1.8)        |
| Voters     | Chase Oliver (L)               | 1 (0.4, 1.9)          |
| Adults     | Stay Home                      | 28.3 (26, 30.5)       |
| Adults     | Turnout                        | 71.7 (69.5, 74)       |


### Cross-Pollsters Comparison

07/09/2024 to 11/09/2024

<p align="center">
  <img src="https://github.com/user-attachments/files/17166602/vote2024_population_2024-09-07_2024-09-12.pdf"
alt="CrossPollsters_Topline_Comparison" width="750" />
</p>


15/08/2024 to 23/08/2024

<p align="center">
  <img src="https://github.com/user-attachments/files/17166596/vote2024_population_2024-08-15_2024-08-23.pdf"
alt="CrossPollsters_Topline_Comparison" width="750" />
</p>

### Aggregate MrP (Updated as of 11/09/2024)

#### State-level Projections 

#### Distribution of Projected Electoral College Votes 

## 


## How to cite
```bibtex
@misc{PoSSUM,
  author = {Roberto Cerina},
  title = {PoSSUM: A Protocol for Surveying Social-media Users with Multimodal LLMs},
  year = {2024},
  howpublished = {\url{https://github.com/robertocerinaprojects/PoSSUM}},
  note = {Version: alpha 1.0.0}
}
```

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact
If you have questions, please send me an email at r.cerina@uva.nl.
