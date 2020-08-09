# Hetindel: analyze heterozygous indels 
This repository holds source code for Hetindel App deployed on shinyapps.io

[https://ae-tue.shinyapps.io/hetindel_v4/](https://ae-tue.shinyapps.io/hetindel_v4/)

## Authors: Robert Deibel, Hannes Preiss, Anastasia Eskova
#### Max Planck Institute for Developmental Biology, Tübingen, Germany

The **Hetindel** package was developed to identify genomic insertions or deletions, so called indels, in heterozygous sequencing data where both alleles carry mutations. **Hetindel** combines the deconvolution of Sanger chromatogram peaks (*Hill et al., 2014*) with dynamic programming optimization (*Dmitriev et al., 2008*) to predict the constituting alleles. Furthermore, it allows for batch processing of ab1 sequencing data, getting rid of one of the most limiting bottlenecks in efficient mutation identification. For the identification of mutant sequences Hetindel analyzes an archive containing .ab1 chromatogram files and only requires a site specific Fasta reference file for the identification of up to two mutant allels in the same position.

## References
1. Dmitriev, D. A., & Rakitov, R. A. (2008). Decoding of superimposed traces produced by direct sequencing of heterozygous indels. PLoS Comput Biol, 4(7), e1000113. doi:10.1371/journal.pcbi.1000113
2. Hill, J. T., Demarest, B. L., Bisgrove, B. W., Su, Y. C., Smith, M., & Yost, H. J. (2014). Poly peak parser: Method and software for identification of unknown indels using sanger sequencing of polymerase chain reaction products. Dev Dyn, 243(12), 1632-1636. doi:10.1002/dvdy.24183# New Document
