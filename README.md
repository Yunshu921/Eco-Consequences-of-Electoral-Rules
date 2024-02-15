# Revisiting the the Economic Consequences of Electoral Rules in Brazil

## Overview

This repo contains the code and data to produce 1) a paper which is a reproduction of Maya Chin (2023)'s paper, "When Do Politicians Appeal Broadly? The Economic Consequences of Electoral Rules in Braziland" and 2) replications of appendix figures B1, B2a, B2b, and B3 of this paper.


## File Structure

The repo is structured as:

-   `inputs/data` contains the data file to be used for analysis and in appendix figures..
-   `inputs/llm` contains the chat history with Chat-GPT4.
-   `inputs/literature` contains the original paper and appendix which we make a reproduction.
-   `outputs/paper` contains the files used to generate the paper, including the Quarto document - paper.qmd, the PDF of the reproduction paper - paper.pdf, and reference bibliography file - references.bib.
-   `replication/scripts` contains the code used to replicate each of the figures.
-   `replication/plots` contains the replicated figures.
-   `replication/helper_functions` contains essential preprocessing functions and specifies the regression model as described in the original study.

## LLM Usage

In the development of this paper, a Large Language Model, specifically Chat-GPT4, was employed for providing a idea of the coding components and a first draft of narrative composition. The entire chat history can be found in `inputs/llm/usage.txt`.