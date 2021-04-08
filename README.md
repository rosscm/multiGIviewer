<div align="center">

# multiGIviewer

An interactive web application built with R Shiny to view multi-screen genetic interaction data

<div align="left">

# Contents

- [Usage](#usage)
- [Inputs](#inputs)
- [Outputs](#outputs)

# Usage

Watch this short demo on how to use `multiGIviewer` to view replicated genetic
interactions (qGI scores) across two or more CRISPR screens of interest.

![](inst/demo.gif)

# Inputs

| Parameters              | Values     | Properties |
| ----------------------- | ---------- | ---------- |
| **Dataset**             | GIN_20210406, GIN_20210107, GIN_20201129, CHEM_20210218 | qGI scores for 280 (GIN_20210406), 257 (GIN_20210107), 247 (GIN_20201129), 68 (CHEM_20210218) screens
| **Screens**             | screens IDs | list of all screens in selected dataset; two or more screens need to be selected to view replicated interactions
| **Media conditions(s)** | min, rich and/or pyro (GIN); DMSO (CHEM)  | media type used to define wildtype single mutant fitness; more than one can be selected if screens were done in different media types
| **FDR threshold**       | value from 0 to 1 | threshold to define significant genetic interactions; interactions must pass threshold in at least two screens to be shown
| **Plot labels**         | comma separated gene symbols (character sensitive) | by default top ten positive and negative interactions are plotted; any label(s) can be chosen
| **Label type**          | text or padded box | by default plot labels are drawn as plain text; padded box wraps text in a white box to better visualize labels
| **Reference line(s)**   | y=x, x=0 and/or y=0 | any selection of reference lines can be chosen


# Outputs

#### Plot

| Elements                                    | Properties |
| ---------------------------------------     | ---------- |
| **Fitness HAP1 wildtype [LFC] (x-axis)**    | wildtype dropout effect (log2-foldchange) in selected media condition(s)
| **Fitness HAP1 knockout [LFC] (y-axis)**    | knockout dropout effect (log2-foldchange) in selected screens
| **Genetic interaction in n screens (fill)** | number of times positive or negative interaction is significant across selected screens
| **Mean qGI score (size)**                   | mean qGI score of interaction across selected screens

#### Table

| Columns        | Properties |
| -------------- | ---------- |
| **gene**       | gene interaction
| **mean_qGI**   | mean qGI score of interaction across selected screens
| **min_FDR**    | minimum qGI FDR value of interaction across selected screens
| **mean_wtLFC** | mean wildtype dropout effect (log2-foldchange) across selected media condition(s)
| **mean_koLFC** | mean knockout dropout effect (log2-foldchange) across selected screens
| **n_sig**      | number of times positive or negative interaction is significant across selected screens
