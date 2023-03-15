import com.github.tototoshi.csv.CSVWriter

import java.io.File
import com.github.tototoshi.csv.*
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.jsoup.{Connection, Jsoup}

import scala.collection.mutable

object YouthDatabase {

  def updatePlayer(id: String, l5p: Seq[Double]): String = {

    val yp = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=",id))

    val last5Games: (Seq[Double], String) = yp.last5Performances.get
    val lastGame = last5Games._2
    println(f" ${yp.name.get},${yp.id.get},${yp.age.get._1}%2.3f,${yp.since.get},${yp.availability.get.replaceAll(" --> ",",")},${yp.bestPerformances.get},${last5Games._1.mkString(",")}")
    println(f" ${yp.name.get},${yp.id.get},${yp.age.get._1}%2.3f,${yp.since.get},${yp.availability.get.replaceAll(" --> ",",")},${yp.bestPerformances.get},${last5Games._1.zip(l5p).map(x => math.max(x._1,x._2)).mkString(",")},$lastGame")

    f"${yp.name.get},${yp.id.get},${yp.age.get._1}%2.3f,${yp.since.get},${yp.availability.get.replaceAll(" --> ",",")},${yp.bestPerformances.get},${last5Games._1.zip(l5p).map(x => math.max(x._1,x._2)).mkString(",")},$lastGame,${yp.nationality.get}"
  }

  def create(pathToCsvFile: String, ids: Seq[String]): Unit = {

    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

    for(id <- ids) createRecords += updatePlayer(id,Seq.fill(6)(-1.0))

    val createdRecords = createRecords.result()

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = false)
    writer.writeRow(Seq("Player,Player ID,Years,Days,Days in Academy,WC X,Stage N,Description,Last Match Date,Season,Week,B_GK,B_CD,B_WB,B_IM,B_W,B_F,L_GK,L_CD,L_WB,L_IM,L_W,L_F,Last Match Details,Country"))
    createdRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()

  }

  def update(pathToCsvFile: String): Unit = {

    val bufferedSource = io.Source.fromFile(pathToCsvFile)
    val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for (line <- bufferedSource.getLines) {
      println(line)
      val cols = line.split(",").map(_.trim)
      //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(16)}|${cols(17)}|${cols(18)}|${cols(19)}|${cols(20)}|${cols(21)}")
      val l5p: Seq[Double] = Seq(cols(16),cols(17),cols(18),cols(19),cols(20),cols(21)).map(_.toDouble)
      //println(l5p)
      updateRecords += updatePlayer(cols(1),l5p)
    }
    bufferedSource.close

    val updatedRecords = updateRecords.result()

    println("--------------------------")
    println(updatedRecords)
    updatedRecords.foreach(println(_))
    println("--------------------------")

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = true)
    updatedRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()

  }

}

object createMyYouthDatabase extends App{

  YouthDatabase.create("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\myYouthTeam.csv",
    Seq("324566127", "323823466", "322690232", "324275438", "320694609", "319365421", "322197956",
      "321389556", "320411270", "319041595", "315311672", "317512515", "315950483", "315629965",
      "315519359", "310526039", "313916366", "111136029")
  )

}

object createCustomYouthClubDatabase extends App{

  val youthAcademyId = 678445

  val url = s"https://www.hattrick.org/Club/Players/YouthPlayers.aspx?YouthTeamID=$youthAcademyId"

  val connection: Connection = Jsoup.connect(url)
  val document: Document = connection.get()

  val playerIDs: Array[String] = document.select("td.hidden:not(.left)").text().split(" ")

  YouthDatabase.create("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\FCB_youthPlayerDatabase.csv", playerIDs)

}

object Testt extends App{

/*  val List_of_ids = Seq("324275438", "320411270", "317512515")

  //val p1 = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","317512515"))
  //val p1 = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","324275438"))
  val p1 = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=", "310526039"))
  //val p1 = new Youth(Array("https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=","310723669"))

  val file = new File("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\youthPlayerDatabase.csv")
  val writer = CSVWriter.open(file, append = false)
  List_of_ids.foreach(id => {

    val p = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=", s"$id"))
    if (p.exists && p.stillInYouthAcademy) {
      val l5p = p.last5Performances.get.mkString(",")
      println(l5p)
      writer.writeAll(List(
        List(s"${p.name.get},${p.id.get},${p.age.get._1},${p.since.get},${p.availability.get.replaceAll(" --> ", ",")},${p.bestPerformances.get},$l5p")
      ))
    }
  })
  writer.close()


  println(p1.exists)
  println(p1.has_club)
  println(p1.name.get)
  println(p1.age.get._1)
  println(p1.nationality.get)
  println(p1.since.get)
  println(p1.link.get)
  println(p1.availability.get)
  println(p1.id.get)
  println(s"Specjalność: ${p1.speciality.get}")
  if (p1.stillInYouthAcademy) println(p1.bestPerformances.get)
  if (p1.stillInYouthAcademy) println(p1.last5Performances.get)


  val p2 = new Senior(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?playerId=", "468663386"))

  println(p2.exists)
  println(p2.has_club)
  println(p2.name.get)
  println(p2.age.get._1)
  println(p2.nationality.get)
  println(s"Specjalność: ${p2.speciality.get}")
  println(p2.link.get)*/

  //YouthDatabase.update("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\youthPlayerDatabase.csv")



}
