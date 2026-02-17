#' Function to create TCRA coverage file from bams
#'
#' @param bamPath    Path to bam file
#' @param outPath    Path to output directory
#' @param vdj.seg    Location of TCRA file
#' @param formatFile Extension of the alignment file, either cram or bam. Default: bam
#' @name getCovFromBam
#' @export


getCovFromBam <- function(bamPath, outPath, vdj.seg, formatFile = "bam"){
  # Requires samtools to be installed and working!
  if(length(bamPath) != 1){
    stop('Please only input one bam file at a time')
  }

  if(!(file.exists(bamPath))){
    stop('Can not find bam file')
  }

  if(!formatFile %in% c("bam", "cram")){
    stop('formatFile needs to be one of "cram" or "bam"')
  }


  vdj.start <- vdj.seg[vdj.seg$segName == 'all','start']
  vdj.end <- vdj.seg[vdj.seg$segName == 'all','end']

  cov.name <- gsub(paste0('.',formatFile),'',basename(bamPath))
  cov.output.files <- paste0(outPath,cov.name, '_TCRA.txt')

  # Check and index file exist
  index.extension <- ifelse(formatFile == "bam", ".bai", ".crai")
  index.path <- paste0(bamPath,index.extension)
  index.path2 <- paste0(gsub('bam$','',bamPath),index.extension)
  if(!(file.exists(index.path ) | file.exists(index.path2))){
    stop('No index file found for cram or bam, please index first before proceeding')
  }

  # Check if bam has chr or not before
  samtools.chr.check <- paste0('samtools idxstats ', bamPath,' | head -n 2')
  chr.check.output <- system(samtools.chr.check, intern = TRUE, ignore.stderr = TRUE)[2]
  chr.check.output2 <- strsplit(chr.check.output,'\t')[[1]][1]
  chr.present <- grepl('chr',chr.check.output2)
  chr14 <- ifelse(chr.present, 'chr14:','14:')


  samtools.cmds <- paste0('samtools depth ',bamPath,
                          ' -q 20 -Q 20 -r ',chr14, vdj.start,'-',vdj.end,' > ',
                          cov.output.files)

  # Replace with processx!
  sapply(samtools.cmds, system)

  return(cov.output.files)
}
