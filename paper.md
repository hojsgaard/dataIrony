---
title: 'dataIrony: Online ironing of data until they are smooth enough'
authors:
- affiliation: 1
  name: Søren Højsgaard
  orcid: 0000-0002-0234-0266
date: "21 February 2019"
bibliography: paper.bib
tags:
- smoothing
- exponential smoothing
- double exponential smoothing
affiliations:
- index: 1
  name: Department of Mathematical Sciences, Aalborg University, Denmark
---

# Summary

This R [@R] package, `dataIrony` contains functionality for smoothing
of online data.
The functionality has been used e.g.\ in
[@thorup:etal:2013] in connection with online estimation of live body
weight for dairy cattle.

Some characteristics of such data are that 1) data are very noisy, 2)
data are recorded at non--equidistant time points and 3) there is
often no well defined model for how the systematic component of data
evolves over time.  In many
applications, e.g. those envisioned in [@thorup:etal:2013], it is
essential that the methodology can be brought to practical use in an
online setting. Based in these requirements, we have implemented relatively
simple smoothing techniques. The name of the package derives from
"ironing data" until data are sufficiently smooth. 

The documentation of `dataIrony` consists of manual pages for the
various available functions, an article describing how to use the
package (*vignette*), and unit tests.

I would like to thank Mikkel Meyer Andersen for helpful discussions.

<!-- ![Simulation illustration.](paper-fig-simulation.png) -->

# References
