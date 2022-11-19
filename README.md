# genoPlotR2

This repository is a modified version of the R package genoPlotR

It contains an additional pentagonGrob shape feature as well as the ability to label gene annotations directly on top of the shape feature

To use the pentagonGrob you can use the following designation in a dna_seg object:
`
gene_type= "pentagonGrob"
`

To adjust the gene label y-axis location you can use the `y` variable in the annotations function
For example the following would display the gene labels on top of the gene shapes:
`
annotations( ... y=-0.8)
`
