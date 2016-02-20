package by.scalalab

import java.nio.file.{Files, Paths}

import by.scalalab.ip._

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.{Failure, Try}

/**
  * @author Larionov A.
  */
object Main {

  def main(args: Array[String]) {

    // lazy IO
    val lines: Source => Stream[String] = source =>
      source.getLines.toStream append {
        source.close; Stream.empty[String]
      }

    val rangesIO = Try{ lines(Source.fromURL(getClass.getResource("/ranges.tsv"))) }
    val transactionsIO = Try { lines(Source.fromURL(getClass.getResource("/transactions.tsv"))) }

    // read, parse an IP, make ranges and segments
    val segmentsTry = for {
      stream <- rangesIO
    } yield {
      stream.par.map(_.split("""\t""")).filter(_.length == 2)
        .map(parts => (parts(0).split("""-"""), parts(1)))
        .map(pair => {
          val (ips, name) = pair
          for {
            ip1 <- mkIPAddress(ips(0))
            ip2 <- mkIPAddress(ips(1))
            range <- Try(IPAddressRange(ip1, ip2))
            segment <- Try(Segment(range, name))
          } yield segment
        })
        .flatMap(_.toOption).seq
    }

    // read, parse user Id, make transactions
    val transactionsTry = for {
      stream <- transactionsIO
    } yield {
      stream.par.map(_.split("""\t""")).filter(_.length == 2)
        .map(parts =>
          for {
            id <- Try(parts(0).toLong)
            ip <- mkIPAddress(parts(1))
          } yield Transaction(id, ip))
        .flatMap(_.toOption).seq
    }

    // search tree
    val treeTry = for {s <- segmentsTry} yield SegmentTree(s)

    // generation of the required information (userId -> list of segment's name)
    val dataTry = for {
      segments <- segmentsTry
      transactions <- transactionsTry
      tree <- treeTry
    } yield {
      for {action <- transactions} yield {
        val seq = tree.segments(action.ip)
        val res = if (seq.nonEmpty) seq.map(_.name).distinct else Seq("Unknown")

        (action.userId, res)
      }
    }

    // write data to file
    val writerTry = for {
      data <- dataTry
      writer <- Try { Files.newBufferedWriter(Paths.get("./output.tsv")) }
    } yield {
      try {
        for {
          elem <- data
          n <- elem._2
        } writer.write(s"${elem._1}\t$n\n")
      } finally writer.close()
      writer
    }

    // errors
    List(segmentsTry, transactionsTry, treeTry, dataTry, writerTry)
      .distinct.filter(_.isFailure) match {
      case Nil => println("Program successfully finished without errors.")
      case list => list.foreach(println)
    }
  }

  private val ipRegex = """^([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})\.([0-9]{1,3})$""".r

  /** Creates an [[by.scalalab.ip.IPAddress]] from any [[String]]. */
  def mkIPAddress(str: String): Try[IPAddress] = {

    def parse(str: String) = str match {
      case ipRegex(v1, v2, v3, v4) => Try(IPAddress(v1.toInt, v2.toInt, v3.toInt, v4.toInt))
      case s => Failure(new IllegalArgumentException(s"Malformed IP address: $s"))
    }

    for {res <- parse(str)} yield res
  }

}

