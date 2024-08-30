<p align="center">
  <img src="https://github.com/robertocerinaprojects/PoSSUM/assets/51832016/c6aab730-ec8c-4bda-a9a3-2024594b0ec8" alt="PossumLogo" width="200" />
</p>


# PoSSUM
A protocol to poll social-media users unobtrusively and inexpensively using multimodal Large Language Models (LLMs).

## How to use this repository 
This repository contains the following information: 
- The [Technical Report](./PoSSUM_Technical_Report) is a detail note on how the protocol works. It contains a detailed description of the algorithmic routines used within, as well as the statistical models used to weight the data. It also presents granular crosstab-level comparisons of PoSSUM's estimates against those of other pollsters in the field during similar dates; 
- The [survey_objects](./survey_objects) folder contains anonymised, dated, survey-like microdata generated at each round of PoSSUM polling for the 2024 US election;
- The [R scripts](./R_scripts) folder contains every R script necessary to run the PoSSUM protocol and the weighting procedue. The code is a bit rough at this stage - the intention is to slowly clean it up as time allows and make it accessible to the wider public, but for now only savy users who understand the technical report will be able to run it on their own machines. Each sub-directory's scripts are presented according to execution order. The code is divided in three parts:
    + Part 1. Involves the preparation of a `Stratification Frame', which is necessary to weight the data and generate quotas;
    + Part 2. Is the running of the poll itself;
    + Part 3. Describes the weighting according to MrP;
    + [README_BEFORE_NEW_POLL.txt](./README_BEFORE_NEW_POLL.txt) describes scripts that have to be tweaked to reflect the pollster's desires before re-running each poll;
- The [utils](./utils) presents a series of helpful functions to execute the protocol. 
  
## How to cite
@misc{PoSSUM,
  author = {Roberto Cerina},
  title = {PoSSUM: A Protocol for Surveying Social-media Users with Multimodal LLMs},
  year = {2024},
  howpublished = {\url{https://github.com/robertocerinaprojects/PoSSUM}},
  note = {Version: alpha 1.0.0}
}

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact
If you have questions, please send me an email at r.cerina@uva.nl.
