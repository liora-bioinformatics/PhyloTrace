<img src="app/static/images/PhyloTrace_bw.png#gh-light-mode-only" width="70%"/>
<img src="app/static/images/PhyloTrace.png#gh-dark-mode-only" width="70%"/>

[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.10996423-659DA3)](https://doi.org/10.5281/zenodo.10996423)
[![License: GPLv3](https://img.shields.io/badge/License-GPLv3-659DA3)](https://www.gnu.org/licenses/gpl-3.0)
[![Release](https://img.shields.io/github/v/release/liora-bioinformatics/PhyloTrace?label=Release&color=659DA3)](https://github.com/liora-bioinformatics/PhyloTrace/releases)
![Linux](https://img.shields.io/badge/Linux-FCC624?style=flat&logo=linux&logoColor=black)

PhyloTrace is the next-generation platform for decentralized bacterial pathogen monitoring and
epidemiological surveillance. It is an open-source desktop application with a graphical interface
and team collaboration features, built around core-genome multilocus sequence typing (cgMLST) and
antimicrobial-resistance screening — all from one local, portable database under your full control.

*For research use only. PhyloTrace is not a medical device and is not intended for use in
diagnostic procedures, for guiding treatment decisions, or for the management of individual
patients.*

[liora-bioinformatics.com/phylotrace](https://liora-bioinformatics.com/phylotrace) \|
[phylotrace@liora-bioinformatics.com](mailto:phylotrace@liora-bioinformatics.com?subject=%5BGitHub%5D%20PhyloTrace)

<br>

### Core-Genome Multilocus Sequence Typing (cgMLST and MLST)

High-resolution strain typing using standardized cgMLST and classical MLST schemes. 40 bacterial
schemes are currently available via [cgMLST.org](https://www.cgmlst.org) and PubMLST, and are
continuously updated.

<img src="docs/readme/typing-run.png" width="100%"/>

### Resistance, Virulence and Point Mutation Screening

Screen genomic assemblies against curated resistance gene databases, virulence factor registries,
and point mutation panels to detect antimicrobial resistance mechanisms immediately.

<img src="docs/readme/amr-heatmap.png" width="100%"/>

### Integrated Database Management

A local database for scalable isolate storage, indexing and curation — a simple interface for
browsing, editing and managing entries without reliance on external utilities.

<img src="docs/readme/database-browser.png" width="100%"/>

### Collaborative Data Import and Export

Exchange data seamlessly between partner labs. Choose exactly which data to share — including
privacy-sensitive details. Because PhyloTrace runs locally without public servers, private data
stays secure.

<img src="docs/readme/import.png" width="100%"/>

### Network and Phylogenetic Tree Graphs

Track transmission patterns and population dynamics with interactive minimum spanning trees (MST)
and hierarchical trees (Neighbour-Joining, UPGMA) — fully customizable layouts, clustering and
variable mapping.

<p>
<img src="docs/readme/mst.png" width="49%"/>
<img src="docs/readme/tree.png" width="49%"/>
</p>

### Spatial and Temporal Mapping

Track transmission events in geographic and temporal context. Interactive, time-resolved map
overlays and epidemic curves help uncover patterns that might otherwise remain hidden in
spreadsheets.

<p>
<img src="docs/readme/map.png" width="49%"/>
<img src="docs/readme/epi-curve.png" width="49%"/>
</p>

### Support for Custom Data Annotation

Declare custom metadata of any kind — clinical, environmental, numerical or otherwise — to
dynamically group and enrich analyses with important context. Custom variables can be added to
every visualization.

<img src="docs/readme/custom-variables.png" width="100%"/>

### Dashboard for Reproducible Analyses

Manage and group routine surveillance or retrospective analyses. Subsetting isolates or
snapshotting the current database state makes analyses reproducible and shareable.

<img src="docs/readme/analysis-dashboard.png" width="100%"/>

<br>

## Get PhyloTrace

Installation binaries are provided free of charge, on request:

📧 **[phylotrace@liora-bioinformatics.com](mailto:phylotrace@liora-bioinformatics.com?subject=PhyloTrace%20installation%20request)**

## Support & Services

PhyloTrace stays free and open source. Alongside it we offer an optional open-source
service-and-support model for labs and institutions that want more — installation and deployment
help, staff training, priority bug fixing, custom scheme integration, and commissioned feature work.
Get in touch at
[phylotrace@liora-bioinformatics.com](mailto:phylotrace@liora-bioinformatics.com?subject=PhyloTrace%20support%20enquiry).

## Citation

If you use PhyloTrace for your paper or publication, cite us with

*Freisleben, M. & Paskali, F. (2024). PhyloTrace. Zenodo. DOI: 10.5281/zenodo.10996423.*

```
@software{Freisleben_Paskali_2024,
  author       = {Freisleben, Marian and Paskali, Filip},
  title        = {PhyloTrace},
  year         = {2024},
  publisher    = {Zenodo},
  doi          = {10.5281/zenodo.10996423},
  url          = {https://doi.org/10.5281/zenodo.10996423}
}
```

## License

PhyloTrace is licensed under the
[GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0) — see [LICENSE](LICENSE).

<br>

<img src="app/static/images/partners_logo_round.svg" width="50%"/>

<sup><sup>Developed in collaboration with Hochschule Furtwangen University (HFU) and Medical
University of Graz (MUG). Featured on ShinyConf 2024 and R/Medicine 2024.</sup></sup>
