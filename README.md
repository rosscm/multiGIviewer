
<div align="center">

# multiGIviewer

An interactive R {shiny} web application built with the {golem}
framework to view multi-screen genetic interaction data

<div align="left">

# Contents

-   [Usage](#usage)
-   [Inputs](#inputs)
-   [Outputs](#outputs)

# Usage

Watch this short demo on how to use `multiGIviewer` to view replicated
genetic interactions (qGI scores) across two or more CRISPR screens of
interest.

![](inst/demo.gif)

# Inputs

| Parameters              | Values                                                                                     | Properties                                                                                                                                                                              |
|-------------------------|--------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Dataset**             | GIN\_20210406, GIN\_20210107, GIN\_20201129, CHEM\_20210218, CHEM\_20210218\_GIN\_20210406 | qGI scores for 280 (GIN\_20210406), 257 (GIN\_20210107), 247 (GIN\_20201129), 68 (CHEM\_20210218), 348 (CHEM\_20210218) & (GIN\_20210406) screens \[*required*; default GIN\_20210406\] |
| **Screens**             | screens IDs                                                                                | list of all screens in selected dataset; two or more screens need to be selected to view replicated interactions \[*required*\]                                                         |
| **Media conditions(s)** | min, rich, pyro and/or DMSO                                                                | media type used to define wildtype single mutant fitness; more than one can be selected if screens were done in different media types \[*required*\]                                    |
| **FDR threshold**       | value from 0 to 1                                                                          | threshold to define significant genetic interactions; interactions must pass threshold in at least two screens to be shown \[*required*; default 0.2\]                                  |
| **Positive colour**     | HEX colour codes                                                                           | colour to fill darkest positive points \[*optional*; default `#FAE057`\]                                                                                                                |
| **Negative colour**     | HEX colour codes                                                                           | colour to fill darkest negative points \[*optional*; default `#61C2FA`\]                                                                                                                |
| **Plot labels**         | gene symbols                                                                               | comma separated (character sensitive) list of genes to label on plot \[*optional*; default top ten positive and negative interactions\]                                                 |
| **Label type**          | Text or Padded box                                                                         | method to draw plot labels; padded box wraps text in a white box to better visualize labels \[*optional*; default Text\]                                                                |
| **Reference line(s)**   | y=x, x=0 and/or y=0                                                                        | selection of reference lines to draw on plot \[*optional*\]                                                                                                                             |

# Outputs

#### Plot

| Elements                                    | Properties                         |
|---------------------------------------------|------------------------------------|
| **Fitness HAP1 wildtype \[LFC\] (x-axis)**  | corresponds to `mean_wtLFC` column |
| **Fitness HAP1 knockout \[LFC\] (y-axis)**  | corresponds to `mean_koLFC` column |
| **Genetic interaction in n screens (fill)** | corresponds to `n_sig` column      |
| **Mean qGI score (size)**                   | corresponds to `mean_qGI` column   |

#### Table

| Columns         | Properties                                                                              |
|-----------------|-----------------------------------------------------------------------------------------|
| **gene**        | gene interaction                                                                        |
| **mean\_qGI**   | mean qGI score of interaction across selected screens                                   |
| **min\_FDR**    | minimum qGI FDR value of interaction across selected screens                            |
| **mean\_wtLFC** | mean wildtype dropout effect (log2-foldchange) across selected media condition(s)       |
| **mean\_koLFC** | mean knockout dropout effect (log2-foldchange) across selected screens                  |
| **n\_sig**      | number of times positive or negative interaction is significant across selected screens |
| **screen\_sig** | list of screens where interaction is significant                                        |
