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

def tryBufferedSource(pathToFile: String): Option[BufferedSource] = {

  try {
    Some(io.Source.fromFile(pathToFile))
  } catch {
    case _: FileNotFoundException => None
  }

}


def headline: Seq[String] = Seq("Player,Player ID,Age,Speciality,Days in Academy,WC X,Stage N,Description,Last Match Date,Season,Week,B_GK,B_CD,B_WB,B_IM,B_W,B_F,L_GK,L_CD,L_WB,L_IM,L_W,L_F,Last Match Details,Country")

def read_config_db: Int = {

  val config_db_path = "src/data/config_db.dat"

  val bufferedSource: Option[BufferedSource] = tryBufferedSource(config_db_path)

  val firstPlayerID = bufferedSource match {
    case Some(source) =>
        if(source.hasNext)
          val startingPlayerID: Int = source.getLines.toList.head.split(" ").head.toInt
          source.close()
          startingPlayerID
        else
          println(s"No player id in config file. Starting from default value: 320411270")
          320411270

    case None =>
      println(s"File $config_db_path does not exists.")
      println(s"Default starting player id: 320411270.")
      320411270

  }


  //println(s"Starting id: $firstPlayerID")

  firstPlayerID

}


object YouthDatabase {

  val DatabasePath: Map[String, String] = Map(
    "678445" -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\FCB_youthPlayerDatabase.csv",
    "2955119" -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\luka_w_systemie_youthPlayerDatabase.csv",
    //"2955119" -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\luka1_w_systemie_youthPlayerDatabase.csv",
    "Polska" -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase.csv",
    "World" -> "C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\World_youthPlayerDatabase.csv"
  )

  def getDatabasePathByDatabaseKey(key: String): String = DatabasePath(key)

  def getDatabaseKeyByDatabasePath(path: String): String = DatabasePath.map(_.swap)(path)

  def updateSeniorPlayer(id: String, line: String): String = {

    val sp = new Senior(Array(s"https://www.hattrick.org/pl/Club/Players/Player.aspx?PlayerID=",id))

    if (sp.exists)
      Senior.UpdateExistingPlayer(sp, line)
    else
      Senior.UpdateNonExistingPlayer(line)

  }

  def addYouthPlayer(yp: Youth/*, b5p: Seq[String], l5p: Seq[Double], line: String*/): String = {
    //320761510
    if (yp.exists)
      //Youth.UpdateExistingPlayer(yp, Seq.empty[String], Seq.empty[Double])
      Youth.UpdateExistingPlayer(yp, Seq.fill(6)("-1.0"),Seq.fill(6)(-1.0))
    else
      Youth.UpdateNonExistingPlayer("")

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

  def createDatabase(playerLine: Seq[String], pathToCsvFile: String, appendFlag: Boolean ): Unit = {

    val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, appendFlag)
    if(!appendFlag)writer.writeRow(headline)
    playerLine.foreach(x => if (x != null) writer.writeRow(Seq(x)))
    writer.close()

  }

  def updateDatabase(pathToCsvFile: String): Unit = {

      val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

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

        val databaseKey: String = getDatabaseKeyByDatabasePath(pathToCsvFile)

        if (databaseKey.matches("\\d+")) {

          val youthClubID: Int = databaseKey.toInt

          println(s"Creating database for Youth Club ID = $youthClubID")

          val playerIDs: Array[String] =
            YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthClubID)

          YouthDatabase.createDatabase(pathToCsvFile, playerIDs)

        } else {
          println(s"Creating database for $databaseKey")

          //pobrać id zawodników "w locie"
          //pobrać pierwsze id z pliku


          val startID = read_config_db

          for(i <- Range(0,10000,1)) {


            val YouthPlayerBuffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
            for (j <- Range(1, 100, 1)) {

              val id = startID + i*100 + j

              val idString = id.toString
              val yp_scan = new QuickScanYouthPlayer(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=", idString))
              val singleYouthPlayerLine: Option[String] = yp_scan.exists && yp_scan.nationality.getOrElse("").equals("Polska") match {

                //val yp = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=", idString))
                //val singleYouthPlayerLine: Option[String] = yp.exists && yp.nationality.getOrElse("").equals("Polska") match{
                case true =>
                  val yp = new Youth(Array(s"https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID=", idString))
                  Some(addYouthPlayer(yp))
                case false => None
              }

              YouthPlayerBuffer += singleYouthPlayerLine.orNull
            }

            //println(read_config_db+20000)

            val youthPlayerLines: Seq[String] = YouthPlayerBuffer.result()

            YouthDatabase.createDatabase(youthPlayerLines, pathToCsvFile, true)

            val file = new File("src/data/config_db.dat")
            val writer = CSVWriter.open(file, append = false)
            val lastID = startID + i*100 + 100
            writer.writeRow(Seq(lastID.toString))
            writer.close()

          }

//320412639

        }




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


/*object createCustomYouthClubDatabase extends App{

  //val youthAcademyId = 678445
  val youthAcademyId = 2955119

  val pathToDatabase: String = YouthDatabase.PlayerDatabasePathByYouthTeamID(youthAcademyId)

  val playerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthAcademyId)

  YouthDatabase.createDatabase(pathToDatabase, playerIDs)

}*/


class YouthAnalysis {

  def this(youthAcademyId: Seq[Int]) = {

    this()


  }


  def this(youthAcademyId: Int) = {

    this()

    val youthAcademyIdAsString = youthAcademyId.toString

    val pathToDatabase: String = YouthDatabase.getDatabasePathByDatabaseKey(youthAcademyIdAsString)

    val currentPlayerIDs: Array[String] = YouthDatabase.getYouthPlayerIDsFromYouthClubID(youthAcademyId)

    YouthDatabase.updateDatabase(pathToDatabase)
    YouthDatabase.addPlayerToDatabase(pathToDatabase, currentPlayerIDs)

  }

  def this(databaseName: String) = {

    this()

    if(databaseName eq "test"){



    }
    else {
      val pathToDatabase: String = YouthDatabase.getDatabasePathByDatabaseKey(databaseName)
      YouthDatabase.updateDatabase(pathToDatabase)
    }



  }




}

object run extends App{

  //new YouthAnalysis(678445)
  new YouthAnalysis(2955119)
  //new YouthAnalysis("Polska")
  //new YouthAnalysis("World")


}


def teamsIDFromLeagueID(leagueID: Int): Seq[String] = {

  val url = s"https://www.hattrick.org/World/Series/?LeagueLevelUnitID=$leagueID"

  val document = JsoupConnection(url)

  val teamsID: Seq[String] = document.select("td.series-table-team").select("a").attr("href").split("&")(1).split("=")(1).split(",").toSeq

  println(s"$teamsID")

  teamsID

}

def youthClubIDFromTeamID(teamID: Int): Int = {

  val url = s"https://www.hattrick.org/Club/?TeamID=$teamID"
  val document = JsoupConnection(url)

  val youthClubID: Int = {
    val youthClubLine: Elements = document.select("div.boxBody").select("a:contains(Overview)")
    if(!youthClubLine.isEmpty)
      youthClubLine.first.attr("href").split("=").last.toInt
    else
      0

  }

  youthClubID

}

object newTest extends App{

  val youthClubIds: Seq[Int] = teamsIDFromLeagueID(59110).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0))

  println(youthClubIds)

  //chcę mieć seq zawierający wszystkie id klubów młodzieżowych
  youthClubIds.foreach(x => {
    val currentPlayerIDs = YouthDatabase.getYouthPlayerIDsFromYouthClubID(x)
    YouthDatabase.addPlayerToDatabase("C:\\Users\\Lukasz\\IdeaProjects\\Scrapper\\src\\data\\Polska_youthPlayerDatabase.csv", currentPlayerIDs)

  })




}

