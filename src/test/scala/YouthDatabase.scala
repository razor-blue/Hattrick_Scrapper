import com.github.tototoshi.csv.CSVWriter

import java.io.File
import com.github.tototoshi.csv.*
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.jsoup.{Connection, Jsoup}

import scala.collection.mutable

def JsoupConnection(url: String): Document = {
  val connection: Connection = Jsoup.connect(url)
  connection.get()
}



object YouthDatabase {

  def PlayerDatabasePathByYouthTeamID(id: Int): String = Map(
    678445 -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\FCB_youthPlayerDatabase.csv",
    2955119 -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\luka_w_systemie_youthPlayerDatabase.csv"
  )(id)

  def updateSeniorPlayer(id: String, line: String): String = {

    val sp = new Senior(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?PlayerID=",id))

    val age = sp.age

    val cols: Array[String] = line.split(",").map(_.trim)

    println(line)

    f"${sp.name.get},${sp.id.get},${sp.age.get._1}%2.3f,-2,${cols.drop(5).mkString(",")}"


  }

  def updateYouthPlayer(id: String, l5p: Seq[Double]): String = {

    val yp = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=",id))

    val last5Games: (Seq[Double], String) = yp.last5Performances.getOrElse((Seq(-1.0,-1.0,-1.0,-1.0,-1.0,-1.0),"-----"))
    val lastGame = last5Games._2
    val bestPerformances = yp.bestPerformances.getOrElse("-1.0,-1.0,-1.0,-1.0,-1.0,-1.0")
    println(f" ${yp.name.get},${yp.id.get},${yp.age.get._1}%2.3f,${yp.since.get},${yp.availability.get.replaceAll(" --> ",",")},$bestPerformances,${last5Games._1.mkString(",")}")
    println(f" ${yp.name.get},${yp.id.get},${yp.age.get._1}%2.3f,${yp.since.get},${yp.availability.get.replaceAll(" --> ",",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1,x._2)).mkString(",")},$lastGame")

    f"${yp.name.get},${yp.id.get},${yp.age.get._1}%2.3f,${yp.since.get},${yp.availability.get.replaceAll(" --> ",",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1,x._2)).mkString(",")},$lastGame,${yp.nationality.get}"
  }

  def createDatabase(pathToCsvFile: String, ids: Seq[String]): Unit = {

    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for(id <- ids) createRecords += updateYouthPlayer(id,Seq.fill(6)(-1.0))
    val createdRecords = createRecords.result()

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = false)
    writer.writeRow(Seq("Player,Player ID,Years,Days,Days in Academy,WC X,Stage N,Description,Last Match Date,Season,Week,B_GK,B_CD,B_WB,B_IM,B_W,B_F,L_GK,L_CD,L_WB,L_IM,L_W,L_F,Last Match Details,Country"))
    createdRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()

  }

  def updateDatabase(pathToCsvFile: String): Unit = {

    val bufferedSource = io.Source.fromFile(pathToCsvFile)
    val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for (line <- bufferedSource.getLines.drop(1)) {
      println(line)
      val cols = line.split(",").map(_.trim)
      println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(17)}|${cols(18)}|${cols(19)}|${cols(20)}|${cols(21)}|${cols(22)}")
      val l5p: Seq[Double] = Seq(cols(17),cols(18),cols(19),cols(20),cols(21),cols(22)).map(_.toDouble)
      //println(l5p)
      val newRecord: String = if (cols(4).toInt >= 0) updateYouthPlayer(cols(1), l5p) else updateSeniorPlayer(cols(1), line)
      updateRecords += newRecord
    }
    bufferedSource.close

    val updatedRecords = updateRecords.result()

    println("--------------------------")
    println(updatedRecords)
    updatedRecords.foreach(println(_))
    println("--------------------------")

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = false)
    writer.writeRow(Seq("Player,Player ID,Years,Days,Days in Academy,WC X,Stage N,Description,Last Match Date,Season,Week,B_GK,B_CD,B_WB,B_IM,B_W,B_F,L_GK,L_CD,L_WB,L_IM,L_W,L_F,Last Match Details,Country"))
    updatedRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()

  }

  def getYouthPlayerIDsFromYouthClubID(youthAcademyId: Int): Array[String] = {

    val url = s"https://www.hattrick.org/Club/Players/YouthPlayers.aspx?YouthTeamID=$youthAcademyId"

    val document = JsoupConnection(url)

    val playerIDs: Array[String] = document.select("td.hidden:not(.left)").text().split(" ")

    playerIDs

  }

  def getYouthPlayerIDsFromYouthDatabase(pathToCsvFile: String): Seq[String] = {

    val bufferedSource = io.Source.fromFile(pathToCsvFile)
    val IDsbuffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim)
      val youthPlayerId: String = cols(1)
      IDsbuffer += youthPlayerId
    }
    bufferedSource.close

    val youthPlayerIDs: Seq[String] = IDsbuffer.result()

    youthPlayerIDs

  }

  def addPlayerToDatabase(pathToCsvFile: String, currentPlayerIDs: Seq[String]): Unit = {

    val storedPlayerIDs: Seq[String] = getYouthPlayerIDsFromYouthDatabase(pathToCsvFile)

    val newPlayers: Seq[String] = currentPlayerIDs.diff(storedPlayerIDs)

    println(s"New players: $newPlayers")

    //////-----------------
    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

    for (id <- newPlayers) createRecords += updateYouthPlayer(id, Seq.fill(6)(-1.0))

    val createdRecords = createRecords.result()

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = true)
    createdRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()

  }

}

object createMyYouthDatabase extends App{

  YouthDatabase.createDatabase("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\myYouthTeam.csv",
    Seq("324566127", "323823466", "322690232", "324275438", "320694609", "319365421", "322197956",
      "321389556", "320411270", "319041595", "315311672", "317512515", "315950483", "315629965",
      "315519359", "310526039", "313916366", "111136029")
  )

}

object createCustomYouthClubDatabase extends App{

  //val youthAcademyId = 678445
  val youthAcademyId = 2955119

  val pathToDatabase: String = YouthDatabase.PlayerDatabasePathByYouthTeamID(youthAcademyId)

  val playerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthAcademyId)

  YouthDatabase.createDatabase(pathToDatabase, playerIDs)

}

object updateCustomYouthClubDatabase extends App {

  //val youthAcademyId = 678445
  val youthAcademyId = 2955119

  val pathToDatabase: String = YouthDatabase.PlayerDatabasePathByYouthTeamID(youthAcademyId)

  val currentPlayerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthAcademyId)

  YouthDatabase.updateDatabase(pathToDatabase)
  YouthDatabase.addPlayerToDatabase(pathToDatabase,currentPlayerIDs)

}

