################################################################################
# Plot helpers: list and determine gene "types"
################################################################################
gene_types <- function(auto=TRUE){
  types <- c("arrows", "headless_arrows",
             "blocks", "bars", "points", "text", "lines", 
             "side_blocks", "side_bars", "side_points", "side_text",
             "side_lines",
             "introns", "exons", "side_exons")
  if (auto) types <- c("auto", types)
  types
}
auto_gene_type <- function(n_genes){
  if (max(n_genes) > 1000){
    gene_type <- "side_bars"
  } else if (max(n_genes) > 100) {
    gene_type <- "side_blocks"
  } else {
    gene_type <- "side_bars"
  }
  gene_type
}

pentagonGrob <- function(gene, ...) {
  head_len<-120
  glen<-gene$end - gene$start
  mid <-(gene$end - gene$start)/2
  if (gene$strand ==1) {
    x <- c(gene$start, gene$start,gene$end- head_len,gene$end, gene$end- head_len)
    y <- c(0,1,1,0.5,0)
  }
  else {
    x <- c(gene$start +head_len, gene$start, gene$start +head_len, gene$end ,gene$end)
    y <- c(0,0.5,1,1,0)
  }
  polygonGrob(x, y, gp=gpar(fill=gene$fill, col=gene$col, lty=gene$lty,
                            lwd=gene$lwd), default.units="native")
}
