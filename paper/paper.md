---
title: 'teems and the teemsphere: Open-source tools for reproducible computable general equilibrium model runs in R'
tags:
  - R
  - computable general equilibrium
  - economic modeling
  - trade policy
  - climate policy
  - reproducibility
  - high-performance computing
authors:
  - name: Matthew Cantele
    orcid: 0000-0003-3729-1764
    corresponding: true
    affiliation: "1, 2, 3"
  - name: Martin Ingram
    orcid: 0000-0001-6269-8342
    affiliation: "4"
  - name: Tom Kompas
    orcid: 0000-0002-0665-0966
    affiliation: "1, 2, 3"
affiliations:
  - index: 1
    name: School of Agriculture, Food and Ecosystem Sciences, University of Melbourne, Australia
    ror: 01ej9dk98
  - index: 2
    name: Centre for Environmental and Economic Research (CEER), University of Melbourne, Australia
  - index: 3
    name: Centre of Excellence for Biosecurity Risk Analysis (CEBRA), University of Melbourne, Australia
  - index: 4
    name: School of BioSciences, University of Melbourne, Australia
    ror: 01ej9dk98
date: 23 November 2026
bibliography: paper.bib
---

# Summary

Computable general equilibrium (CGE) models represent national or global economies through systems of interlinked markets in which households, firms, and governments respond to changes in prices.
These models are a standard tool for examining the potential impacts of tariffs, carbon pricing, productivity shocks, and demographic trajectories on output, trade, incomes, and emissions, as well as providing the economic foundation for most integrated assessment models used within climate research [@Dixon2013; @Cantele2021].
Carrying out even a single simulation can be difficult given the many steps between the initial database and the final model output.
These include selecting and aggregating a database, choosing a model and closure, specifying shocks and solution methods, and solving a system which may contain millions of nonlinear equations.
In practice, these steps are often carried out across proprietary programs, graphical interfaces, and manually edited intermediate files, with published results rarely reproducible from the information provided within a manuscript.

The teemsphere is an open-source organization which develops and maintains a set of tools to carry out this process within a short script.
The core components are `teems` (Trade and Environment Equilibrium Modeling System), an R package which prepares and manages a model run, and `teems-solver`, a containerized solver written in C and Fortran.
Vetted model and closure files are provided through `teems-models`, while `teems-mappings` provides set aggregation mappings for each GTAP database version.

A model run begins by reading a Global Trade Analysis Project (GTAP) database [@Aguiar2022] and aggregating the data to the regions, sectors, and other sets associated with the chosen model.
The R package then parses the model file, applies closure swaps and shocks, and validates the full configuration before calling the solver.
Following the solution, results are returned as R objects.
Because the full configuration is retained within a script, the model run can be shared with colleagues, reviewers, and policy-makers, reproduced elsewhere, and modified through individual function arguments.

# Statement of need

The difficulty of understanding and reproducing CGE analyses has contributed to the perception of these models as black boxes [@Bohringer2003].
This challenge is compounded by the proprietary nature of prevalent data and software, as well as by the number of steps involved in preparing and solving a model.
Each step introduces the possibility of an error propagating through to the output without any obvious indication that this has taken place.
The lack of information pertaining to solution methods and software was evident within a systematic review of equilibrium-model studies, where two-thirds did not specify the software used to solve the model.
Of those that did, 71% used GAMS and 25% used GEMPACK, both commercial products [@Cantele2021].

In order to make modeling choices explicit, `teems` records every choice as a function argument within the R script.
This includes the mappings used to aggregate regions, the weights applied when aggregating behavioral parameters, the variables swapped between exogenous and endogenous, and the values assigned to shocks.
Solution and matrix methods, the number of steps, and the number of subintervals are specified in the same way.
The solver runs within a fixed container so that the same script and image produce nearly identical numerical results on another machine.

These tools are intended to serve three broad groups of users.
For CGE modelers, they provide a scripted and testable workflow for existing models, including large intertemporal models which require parallel matrix methods.
For scientists outside economics, particularly climate and land-use researchers seeking to incorporate an economy-wide model within a coupled framework, they provide an entry point to CGE modeling.
Finally, policy analysts can run and repeat scenarios under alternative aggregations or shocks within minutes, reducing the need to commission a new study for each configuration.

# State of the field

Two established software systems dominate CGE modeling [@Horridge2013].
GEMPACK [@Harrison1996; @Horridge2024] solves models written in the TABLO language by linearizing the equations and integrating the linearized system across the shock, following @Pearson1991.
The GTAP model and its many extensions are maintained within this system.
GAMS [@Bussieck2004], typically used in conjunction with MPSGE [@Rutherford1999], solves levels formulations using general-purpose nonlinear solvers.
Both GEMPACK and GAMS are commercial software.

teemsphere tools encompass all steps from database aggregation to numerical solution while retaining the ability to execute existing TABLO model files.
The large body of GEMPACK models, including the standard GTAP family and single-country models such as ORANI-G, can therefore be run as written while maintaining comparability with their published versions.
The solver is compiled and containerized, and builds on code used in a series of dimensionally large applications [@VanHa2015].
These include the parallel solution of intertemporal CGE models [@VanHa2016; @VanHa2017] and an analysis of climate change and trade barriers [@Kompas2019].
The bordered block-diagonal methods employed in these applications are available through the same R function used for a serial LU solve, allowing the workflow to be used on a personal laptop or a high-performance computing (HPC) server.

# Software design

![Structure of a model run. Three functions prepare the data, model, and shocks. The combined configuration is validated and written to text inputs by `ems_deploy()`, after which `ems_solve()` calls the containerized solver and `ems_compose()` returns labeled results.\label{fig:arch}](architecture.png){width="100%"}

**Components and their interfaces.**
Within the teemsphere, `teems` prepares and manages the run, while `teems-solver` carries out the numerical solution (\autoref{fig:arch}).
The `teems-models` and `teems-mappings` repositories hold the model files and aggregation mappings as data, so they can be versioned and extended independently.
Communication between the R package and the solver takes place through model, closure, shock, and command files written to a run directory by `ems_deploy()`.
We chose a text-file rather than an in-memory interface so that all intermediate files can be inspected and the workflow remains interoperable with GEMPACK.

**Validation and error reporting.**
Given the potential for persistent logic errors to survive through to the final output, the model configuration is validated before the solver is called.
Within this process, `ems_model()` rejects malformed TABLO files with one of 118 named errors, while `ems_deploy()` checks that the closure is square, shocks refer to existing elements, swaps are consistent, and mappings cover every element; the package carries 338 such messages in all.
The 162 solver failure patterns are classified as resource, size, specification, or interface errors and reported with an associated remedy.
The package contains 898 tests and every documented example is executed within the test suite.
In addition, every solver change must reproduce the solution files from a suite of reference runs byte for byte.

**Solution methods and computational performance.**
The solver implements solution methods including Johansen, Euler, Gragg, and explicit Runge-Kutta methods with adaptive step control [@Schiffmann2022] integrated in log-levels [@MuntheKaas1999].
Each linear system is factorized using the HSL sparse direct codes [@Duff1996; @HSL2026] under PETSc and MPI [@Balay2026].
This can be carried out serially using LU decomposition or in parallel following a bordered block-diagonal ordering by region (DBBD) or time period (SBBD, NDBBD).
Before solving, `ems_probe()` inspects the structure of a deployed model and recommends a matrix method and resources.
On a 32-core machine with 128 GB of memory, the single-step Johansen solution method takes approximately 12 minutes to solve a 250-million-equation intertemporal GTAP model using the parallel SBBD matrix method.
With the same machine and solution method, the uncondensed standard GTAP model at full regional, sectoral, and endowment resolution (163 regions, 65 sectors, 8 endowments; 40.5 million equations) takes 24 minutes using the parallel DBBD matrix method.

**Containerization and reproducibility.**
The solver is distributed as a Docker image [@Merkel2014] built on a published base image which fixes the toolchain, MPI, and PETSc versions.
Users on Linux, macOS, and Windows compile the solver from the same source against this base image at the same instruction-set level, with native arm64 support tested on Apple silicon.
Since the HSL license requires users to provide their own copy of the libraries, the image must be built locally rather than downloaded, which takes approximately five minutes.
With the current solver release, 98.4% of solution variables across 18 reference runs agreed to six significant digits between x86-64 and arm64 builds, with the remaining differences confined to near-zero results.

**Included models, mappings, and documentation.**
The `teems-models` repository provides six vetted models with their standard closures: the classic (6.2) and standard (7.1) GTAP models [@Hertel1997; @Corong2017], the GTAP-E energy and emissions extension [@Burniaux2002], GTAP-E with the GTAP-Power electricity detail [@Peters2016], the GTAP-AEZ land-use model [@Hertel2009], and the intertemporal rational-expectations model GTAP-RE [@VanHa2026].
These are accompanied by aggregation mappings for GTAP database versions 9 through 12 in `teems-mappings`, so that an initial run requires only the database files.
For this initial run, `ems_example()` produces a complete script for any vetted model with the user's data paths incorporated.
A user manual containing worked examples and benchmarks is maintained at <https://teemsphere.github.io/>.

# Research impact statement

`teems` has been available on CRAN since June 2026, with version 1.1.0 released in November 2026.
Approximately 2,000 downloads have been logged to date.
A precursor to the package was used to carry out the recursive-dynamic land-use and Shared Socioeconomic Pathway simulations within the first author's doctoral work [@Cantele2023].
`teems` has been presented to modeling and policy audiences at the International Conference on Computing in Economics and Finance, the Australian Agricultural and Resource Economics Society conference, CSIRO, the Centre of Excellence for Biosecurity Risk Analysis, the Australian Treasury, the University of Minnesota, and the Centre of Policy Studies at Victoria University.

# AI usage disclosure

The R package and solver were designed and written by the authors, with development beginning many years before the availability of AI tools.
From mid-2026, Anthropic's Claude models were used as a coding assistant during solver modernization and the 1.0.0 release cycle.
For the solver, this work involved refactoring and hardening C code, implementing additional functionalities and solution methods, and carrying out fuzzing campaigns.
For the R package, whose design and functions predate this period, assistance was primarily used to develop the test suite, including snapshot tests of error messages and tests which execute every documented example, as well as to audit the package code and user-facing messages prior to release.

Correctness was verified through the reference-run gate, test suite, and platform checks described above, as well as GEMPACK reference solutions where available for archived models.
An analytic oracle of small models with closed-form solutions, built from the core functional forms, checked that Gragg and Euler reproduced the exact optimum and Johansen its first-order response, with negative controls confirming that the comparison could fail.
The Runge-Kutta methods were checked against converged Gragg solutions and their theoretical convergence orders.
All AI-assisted changes were directed and reviewed by the first author.

# Acknowledgements

We thank the Global Trade Analysis Project at Purdue University and the Centre of Policy Studies at Victoria University, whose model files and software documentation helped guide development.
We would also like to thank the Science and Technology Facilities Council for making HSL libraries available for academic use.
Finally we would like to thank maintainers of open-source software, from PETSc and MPICH to the R packages we load, without which teemsphere could not exist.

# References
