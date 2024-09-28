
<p align="center">
  <kbd>
  <img src="https://github.com/robertocerinaprojects/PoSSUM/assets/51832016/c6aab730-ec8c-4bda-a9a3-2024594b0ec8" alt="PossumLogo" width="200" />
  </kbd>
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

### Topline estimates 

| Population | Vote2024                       | 15/08 to 23/08         | 07/09 to 11/09        |
|------------|--------------------------------|------------------------|-----------------------|
| Voters     | Kamala Harris (D)              | 46.4 (44.2, 48.3)      | 47.6 (45.4, 50)       |
| Voters     | Donald Trump (R)               | 47.2 (45.1, 49.3)      | 46.8 (44.4, 49.6)     |
| Voters     | Robert F. Kennedy Jr. (Ind.)   | 3.7 (2.4, 5.3)         | 3 (1.7, 4.8)          |
| Voters     | Jill Stein (G)                 | 1.1 (0.4, 2.5)         | 0.4 (0.1, 1)          |
| Voters     | Cornel West (Ind.)             | 0.2 (0, 0.7)           | 0.8 (0.2, 2.1)        |
| Voters     | Chase Oliver (L)               | 1 (0.5, 2)             | 0.9 (0.4, 1.7)        |
| Adults     | Stay Home                      | 30 (27.6, 32.2)        | 24.6 (21.4, 27.6)     |
| Adults     | Turnout                        | 70 (67.8, 72.4)        | 75.4 (72.4, 78.6)     |

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

#### 2<sup>nd</sup> Poll

<p align="center">
  <img src="https://github.com/user-attachments/files/17166732/vote2024_population_2024-09-07_2024-09-12.pdf"
alt="CrossPollsters_Topline_Comparison_2nd_poll" width="850" />
</p>

#### 1<sup>st</sup> Poll

<p align="center">
  <img src="https://github.com/user-attachments/files/17166741/vote2024_population_2024-08-15_2024-08-23.pdf"
alt="CrossPollsters_Topline_Comparison_1st_poll" width="850" />
</p>

### Aggregate MrP (Updated as of 12/09/2024)
Note: State-level projections are derived from an MrP model which leverages all of the PoSSUM polls fielded during the campaign. Variation between polls is accounted for via random-walk poll-level effects. The projections reflect the most likely estimates as of the field-dates of the most recent available poll. 

<p align="center">
  <img src="https://github.com/user-attachments/files/17172878/vote_share_map_2024-08-15_2024-09-12.pdf"
alt="CrossPollsters_Topline_Comparison" width="650" />
</p>

<figure>
<p align="center">
  <img src="https://github.com/user-attachments/files/17172879/probability_map_2024-08-15_2024-09-12.pdf"
alt="CrossPollsters_Topline_Comparison" width="650" />
</p>
  <figcaption align="center">Geographic distribution of difference in probability of winning a given state between Donald Trump (R, in red) and Kamala Harris (D, in blue). </figcaption>
</figure>

<figure style="text-align: center;">
  <img src="https://github.com/user-attachments/files/17172877/EC_votes_histogram_2024-08-15_2024-09-12.pdf"
       alt="CrossPollsters_Topline_Comparison" width="650" />
  <figcaption style="max-width: 650px; margin: 0 auto;">
    Figure 5: Expected distribution of Popular and Electoral College votes by party. The legends present the likelihood of various scenarios.
  </figcaption>
</figure>

<div style="width: 500px; margin: 0 auto; text-align: center;">
  <p><b>Figure 5:</b> Expected distribution of Popular and Electoral College votes by party. The legends present the likelihood of various scenarios.</p>
</div>

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
