import com.github.tototoshi.csv.CSVWriter

import java.io.{File, FileNotFoundException}
import com.github.tototoshi.csv.*
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.jsoup.{Connection, Jsoup}

import scala.collection.mutable
import scala.io.BufferedSource

def JsoupConnection(url: String): Document = {
  val connection: Connection = Jsoup.connect(url)
  connection.get()
}

def headline: Seq[String] = Seq("Player,Player ID,Age,Speciality,Days in Academy,WC X,Stage N,Description,Last Match Date,Season,Week,B_GK,B_CD,B_WB,B_IM,B_W,B_F,L_GK,L_CD,L_WB,L_IM,L_W,L_F,Last Match Details,Country")




object YouthDatabase {

  val TeamIDPlayerDatabasePath: Map[Int, String] = Map(
    678445 -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\FCB_youthPlayerDatabase.csv",
    2955119 -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\luka_w_systemie_youthPlayerDatabase.csv"
  )

  def PlayerDatabasePathByYouthTeamID(id: Int): String = TeamIDPlayerDatabasePath(id)

  def YouthTeamIDByPlayerDatabasePath(path: String): Int = TeamIDPlayerDatabasePath.map(_.swap)(path)

  def updateSeniorPlayer(id: String, line: String): String = {

    val sp = new Senior(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?PlayerID=",id))

    if (sp.exists)
      Senior.UpdateExistingPlayer(sp, line)
    else
      Senior.UpdateNonExistingPlayer(line)

  }

  def updateYouthPlayer(id: String, b5p: Seq[String], l5p: Seq[Double], line: String): String = {

    val yp = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=",id))

    if(yp.exists)
      Youth.UpdateExistingPlayer(yp, b5p, l5p)
    else
      Youth.UpdateNonExistingPlayer(line)


    }

  def createDatabase(pathToCsvFile: String, ids: Seq[String]): Unit = {

    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for(id <- ids) createRecords += updateYouthPlayer(id,Seq.fill(6)("-1.0"),Seq.fill(6)(-1.0),Seq.fill(6)("").mkString(","))
    val createdRecords = createRecords.result()

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = false)
    writer.writeRow(headline)
    createdRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()

  }

  def updateDatabase(pathToCsvFile: String): Unit = {

      val bufferedSource: Option[BufferedSource] = try {
        Some(io.Source.fromFile(pathToCsvFile))
      } catch {
        case _: FileNotFoundException => None
      }

      bufferedSource match {
        case Some(source) =>

          val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
          for (line <- source.getLines.drop(1)) {
            val cols: Array[String] = line.split(",").map(_.trim)
            val l5p: Seq[Double] = cols.slice(17, 23).toSeq.map(_.toDouble)
            val b5p: Seq[String] = cols.slice(11, 17).toSeq
            val newRecord: String = if (cols(4).toInt >= 0) updateYouthPlayer(cols(1), b5p, l5p, line) else updateSeniorPlayer(cols(1), line)
            updateRecords += newRecord
          }
          val updatedRecords = updateRecords.result()
          source.close()
          println(s"File $pathToCsvFile exists.")

          println("--------------------------")
          println(updatedRecords)
          updatedRecords.foreach(println(_))
          println("--------------------------")

          val file = new File(pathToCsvFile)
          val writer = CSVWriter.open(file, append = false)
          writer.writeRow(headline)
          updatedRecords.foreach(x => writer.writeRow(Seq(x)))
          writer.close()

        case None =>
          println(s"File $pathToCsvFile does not exists.")
          println(s"Create file $pathToCsvFile.")

          val playerIDs: Array[String] =
            YouthDatabase.getYouthPlayerIDsFromYouthClubID(YouthTeamIDByPlayerDatabasePath(pathToCsvFile))
          YouthDatabase.createDatabase(pathToCsvFile, playerIDs)
      }


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

    for (id <- newPlayers) createRecords += updateYouthPlayer(id, Seq.fill(6)("-1.0"), Seq.fill(6)(-1.0), Seq.fill(6)("").mkString(","))

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

/*object updateCustomYouthClubDatabase {

  //val youthAcademyId = 678445
  val youthAcademyId = 2955119

  val pathToDatabase: String = YouthDatabase.PlayerDatabasePathByYouthTeamID(youthAcademyId)

  val currentPlayerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthAcademyId)

  YouthDatabase.updateDatabase(pathToDatabase)
  YouthDatabase.addPlayerToDatabase(pathToDatabase,currentPlayerIDs)

}*/

class CustomYouthClubAnalysis {

  def this(youthAcademyId: Int) = {

    this()

    val pathToDatabase: String = YouthDatabase.PlayerDatabasePathByYouthTeamID(youthAcademyId)

    val currentPlayerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthAcademyId)

    YouthDatabase.updateDatabase(pathToDatabase)
    YouthDatabase.addPlayerToDatabase(pathToDatabase, currentPlayerIDs)

  }





}

object run extends App{

  new CustomYouthClubAnalysis(678445)
  new CustomYouthClubAnalysis(2955119)


}

