import java.net.URL
import java.util.zip.GZIPInputStream
import scala.io.Source
import scala.collection.mutable
object codonFrequency {

  def main(args: Array[String]): Unit = {

    val genURL = "https://ftp.ncbi.nih.gov/genomes/refseq/"
    val species =
      "bacteria/Escherichia_coli/reference/GCF_000005845.2_ASM584v2/"
    val dna = "GCF_000005845.2_ASM584v2_genomic.fna.gz"
    val fileURL = new URL(genURL + species + dna)
    val is = fileURL.openStream()
    val gz = new GZIPInputStream(is)
    val seqDNA = Source.fromInputStream(gz).getLines().drop(1).mkString("")

    val nucleotides = List("A", "C", "T", "G")
    val possibleCodons = List
      .fill(3)(nucleotides)
      .flatten
      .combinations(3)
      .toList
      .map(_.permutations)
      .flatten
      .map(_.mkString)

    val possibleCodonsWithCount = mutable.Map.empty ++ possibleCodons
      .zip(List.fill(possibleCodons.length)(0))
      .toMap


    /*
      Number of nucleotides in the DNA sequence is not divisible by 3, so after splitting
      it into codons, there is one single nucleotide left. I have decided to delete the
      last nucleotide from the sequence, which is C - this is done via .filter
    */
    val seqDNASplitToCodons =
      seqDNA.grouped(3).toList.filter(n => n.length == 3) 
    
    
    val allCodonsInDna = seqDNASplitToCodons.length
    seqDNASplitToCodons foreach { n => possibleCodonsWithCount(n) += 1 }
    val possibleCodonsWithCountAndFrequency = possibleCodonsWithCount
      .map(n => (n._1, n._2, n._2.toDouble / allCodonsInDna))
      .toList
      .sortBy(_._2)
      .reverse

    println("(Codon, number of occurrences, relative frequency)")
    possibleCodonsWithCountAndFrequency foreach { println }

/*
    (Codon, number of occurrences, relative frequency)
    (CGC,38319,0.024766403161288947)
    (GCG,38202,0.0246907835164686)
    (TTT,36842,0.023811785935650913)
    (AAA,36172,0.023378750362748082)
    (CAG,34724,0.022442876467877487)
    (CTG,33831,0.02186571114459058)
    (GCA,32204,0.02081414565636236)
    (TGC,31708,0.020493570068064144)
    (GCC,31165,0.02014261735748767)
    (GGC,31026,0.020052778634154098)
    (CCG,29143,0.01883575477777196)
    (ATC,28959,0.01871683157566133)
    (CGG,28875,0.01866254054861083)
    (CCA,28846,0.018643797217843393)
    (GAT,28709,0.01855525113801102)
    (TGG,28322,0.018305124620528343)
    (TTC,28279,0.01827733278525249)
    (TGA,28127,0.0181790918791611)
    (GAA,28119,0.01817392130515629)
    (TCA,27739,0.01792831903992782)
    (AAT,27705,0.017906344100407378)
    (ATT,27527,0.017791298828800355)
    (GTT,27509,0.017779665037289533)
    (AAC,27363,0.01768530206170175)
    (AGC,27036,0.017473954849255145)
    (GCT,26987,0.017442285083475687)
    (CAT,25842,0.016702246679037264)
    (TTG,25656,0.016582030833425434)
    (ATG,25368,0.016395890169252275)
    (CAA,25191,0.016281491219395858)
    (ACC,24772,0.01601068240589394)
    (GGT,24717,0.01597513470961087)
    (ACG,24607,0.015904039317044733)
    (CGT,23955,0.015482637535652724)
    (TCG,23854,0.015417359038841998)
    (CGA,23729,0.015336568820016843)
    (TAA,23054,0.014900301638361006)
    (TTA,22989,0.014858290724571925)
    (CAC,22369,0.014457571239199156)
    (GTG,21764,0.014066546580085404)
    (ATA,21575,0.01394439176922177)
    (TAT,21100,0.01363738893768618)
    (CTT,21063,0.013613475032913935)
    (AAG,20798,0.013442199769004607)
    (ACA,19632,0.01268858860780356)
    (TGT,19593,0.01266338205953011)
    (AGA,19011,0.01228722280068019)
    (GGA,18859,0.0121889818945888)
    (TCC,18679,0.012072643979480577)
    (TCT,18361,0.011867113662789383)
    (GAC,18308,0.011832858610007517)
    (GTC,18214,0.011772104365451)
    (GTA,17708,0.011445065559646772)
    (TAC,17611,0.011382372349838452)
    (AGG,17063,0.011028188030508972)
    (CCT,17003,0.010989408725472898)
    (AGT,16571,0.010710197729213162)
    (ACT,16475,0.010648150841155442)
    (CCC,16043,0.010368939844895706)
    (GGG,15818,0.010223517451010427)
    (CTC,14339,0.009267607581871192)
    (GAG,14202,0.009179061502038822)
    (TAG,8970,0.00579750610289313)
    (CTA,8916,0.005762604728360663)   
*/
  }
}
