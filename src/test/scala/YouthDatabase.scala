import com.github.tototoshi.csv.CSVWriter

import java.io.{File, FileNotFoundException}
import com.github.tototoshi.csv.*
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.jsoup.{Connection, Jsoup}

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import scala.collection.mutable
import scala.io.BufferedSource

def JsoupConnection(url: String): Document = {
  val connection: Connection = Jsoup.connect(url)
  connection.get()
}

//ok
def tryBufferedSource(pathToFile: String): Option[BufferedSource] = {

  try {
    Some(io.Source.fromFile(pathToFile))
  } catch {
    case _: FileNotFoundException => None
  }

}

//ok
def writeToFile(path: String, appendFlag: Boolean, headline: Seq[String], lines: Seq[String]): Unit = {

  val file = new File(path)
  val writer = CSVWriter.open(file, appendFlag)
  if(!appendFlag)writer.writeRow(headline)
  lines.foreach(x => if (x != null) writer.writeRow(Seq(x)))
  writer.close()

}

val teamPath = "https://www.hattrick.org/Club/?TeamID="
val databasePath = "src/data/"
val youthTeamPath = "https://www.hattrick.org/Club/Players/YouthPlayers.aspx?YouthTeamID="
val leaguePath = "https://www.hattrick.org/World/Series/?LeagueLevelUnitID="
val seniorPlayerPath = "https://www.hattrick.org/pl/Club/Players/Player.aspx?PlayerID="
val youthPlayerPath = "https://www.hattrick.org/pl/Club/Players/YouthPlayer.aspx?YouthPlayerID="

def headline: Seq[String] = Seq("Player,Player ID,Age,Speciality,Days in Academy,WC X,Stage N,Description,Last Match Date,Season,Week,B_GK,B_CD,B_WB,B_IM,B_W,B_F,L_GK,L_CD,L_WB,L_IM,L_W,L_F,Last Match Details,Country,Last update")

/*def read_config_db: Int = {

  val config_db_path: String = databasePath + "config_db.dat"

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

}*/


object YouthDatabase {

  //ok
  val DatabasePath: Map[String, String] = Map(
    "678445" -> {databasePath + "FCB_youthPlayerDatabase.csv"},
    "2955119" -> {databasePath + "luka_w_systemie_youthPlayerDatabase.csv"},
    "Polska" -> {databasePath + "Polska_youthPlayerDatabase.csv"},
    "World" -> {databasePath + "World_youthPlayerDatabase.csv"},
    "Ligi_1-4" -> {databasePath + "Polska_youthPlayerDatabase1-4L.csv"},
    "5 Liga" -> {databasePath + "Polska_youthPlayerDatabase_5L.csv"},
    "6 Liga 1-256" -> {databasePath + "Polska_youthPlayerDatabase_6L_1_256.csv"},
    "7 Liga 1-256" -> {databasePath + "Polska_youthPlayerDatabase_7L_1_256.csv"},
    "6 Liga 257-512" -> {databasePath + "Polska_youthPlayerDatabase_6L_257_512.csv"},
    "7 Liga 257-512" -> {databasePath + "Polska_youthPlayerDatabase_7L_257_512.csv"},
    "6 Liga 513-768" -> {databasePath + "Polska_youthPlayerDatabase_6L_513_768.csv"},
    "7 Liga 513-768" -> {databasePath + "Polska_youthPlayerDatabase_7L_513_768.csv"},
    "6 Liga 769-1024" -> {databasePath + "Polska_youthPlayerDatabase_6L_769_1024.csv"},
    "7 Liga 769-1024" -> {databasePath + "Polska_youthPlayerDatabase_7L_769_1024.csv"}
  )

  //ok
  def getDatabasePathByDatabaseKey(key: String): String = DatabasePath(key)

  //ok
  def getDatabaseKeyByDatabasePath(path: String): String = DatabasePath.map(_.swap)(path)

  //ok
  def updateSeniorPlayer(id: String, line: String): String = {

    val sp = new Senior(Array(seniorPlayerPath,id))

    if (sp.exists)
      Senior.UpdateExistingPlayer(sp, line)
    else
      //Senior.UpdateNonExistingPlayer(line)
      null

  }

  //ok
  def updateYouthPlayer(id: String, b5p: Seq[String], l5p: Seq[Double], line: String): String = {

    val yp = new Youth(Array(youthPlayerPath,id))

    if(yp.exists)
      Youth.UpdateExistingPlayer(yp, b5p, l5p)
    else
      //Youth.UpdateNonExistingPlayer(line)
      null

    }

  //ok
  def createDatabase(pathToCsvFile: String, ids: Seq[String]): Unit = {

    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
    for(id <- ids) createRecords += updateYouthPlayer(id,Seq.fill(6)("-1.0"),Seq.fill(6)(-1.0),Seq.fill(6)("").mkString(","))
    val createdRecords = createRecords.result()

    writeToFile(pathToCsvFile, false, headline, createdRecords)

  }

  /*def createDatabase(playerLine: Seq[String], pathToCsvFile: String, appendFlag: Boolean ): Unit = {

    writeToFile(pathToCsvFile, appendFlag, headline, playerLine)

  }*/

  //ok
  def updateDatabase(pathToCsvFile: String): Unit = {

      val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

      bufferedSource match {
        case Some(source) =>

          val updateRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
          var counter = 0

          for (line <- source.getLines.drop(1)) {


              val cols: Array[String] = line.split(",").map(_.trim)
              val l5p: Seq[Double] = cols.slice(17, 23).toSeq.map(_.toDouble)
              val b5p: Seq[String] = cols.slice(11, 17).toSeq
              val newRecord: String = if (cols(4).toInt >= 0) updateYouthPlayer(cols(1), b5p, l5p, line) else updateSeniorPlayer(cols(1), line)
              updateRecords += newRecord

            counter += 1
            if(counter % 100 == 0){
              val updatedRecords = updateRecords.result()
              writeToFile("src/data/tttest.csv", true, headline, updatedRecords)
              updateRecords.clear()
            }



            //writeToFile(pathToCsvFile + ".csv", true, headline, updatedRecords)

          }

          val updatedRecords = updateRecords.result()
          writeToFile("src/data/tttest.csv", true, headline, updatedRecords)
          updateRecords.clear()

          source.close()
          /*println(s"File $pathToCsvFile exists.")

          println("--------------------------")
          println(updatedRecords)
          updatedRecords.foreach(println(_))
          println("--------------------------")*/



          /*val file = new File(pathToCsvFile)
          val writer = CSVWriter.open(file, append = false)
          writer.writeRow(headline)
          updatedRecords.foreach(x => writer.writeRow(Seq(x)))
          writer.close()*/

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
          println(s"Cannot update $databaseKey. File does not exist.")

        }

      }

  }

  //ok
  def getYouthPlayerIDsFromYouthClubID(youthAcademyId: Int): Array[String] = {

    val url = youthTeamPath + s"$youthAcademyId"

    val document = JsoupConnection(url)

    val YourNYouthPlayers: String = document.select("head[id]").text()
    val pattern = "\\d+".r
    val nPlayers: Int = pattern.findAllIn(YourNYouthPlayers).map(_.toInt).toArray.head

    val playersID: Array[String] = document.select("td.hidden:not(.left)").text().split(" ")

    if(nPlayers > 0) playersID else Seq("0").toArray

  }

  //ok
  def getYouthPlayerIDsFromYouthDatabase(pathToCsvFile: String): Seq[String] = {

    val bufferedSource: Option[BufferedSource] = tryBufferedSource(pathToCsvFile)

    bufferedSource match {
      case Some(source) =>
        val IDsbuffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
        for (line <- source.getLines) {
          val cols = line.split(",").map(_.trim)
          val youthPlayerId: String = cols(1)
          IDsbuffer += youthPlayerId
        }
        source.close
        val youthPlayerIDs: Seq[String] = IDsbuffer.result()
        youthPlayerIDs
      case None =>
        Seq.empty[String]
    }
  }

  //ok
  def addPlayerToDatabase(
                           pathToCsvFile: String,
                           currentPlayerIDs: Seq[String],
                           storedPlayerIDs: Seq[String]
                         ): Unit = {

    val newPlayers: Seq[String] = currentPlayerIDs.filterNot(p => p.equals("0")).diff(storedPlayerIDs)

    println(s"New players: $newPlayers")

    //////-----------------
    val createRecords: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

    for (id <- newPlayers) createRecords += updateYouthPlayer(id, Seq.fill(6)("-1.0"), Seq.fill(6)(-1.0), Seq.fill(6)("").mkString(","))

    val createdRecords = createRecords.result()

    writeToFile(pathToCsvFile, true, headline, createdRecords)
    writeToFile("src/data/new.csv", true, headline, createdRecords)

    /*val file = new File(pathToCsvFile)
    val writer = CSVWriter.open(file, append = true)
    createdRecords.foreach(x => writer.writeRow(Seq(x)))
    writer.close()*/

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

    val storedPlayerIDs: Seq[String] = YouthDatabase.getYouthPlayerIDsFromYouthDatabase(pathToDatabase)
    YouthDatabase.addPlayerToDatabase(pathToDatabase, currentPlayerIDs, storedPlayerIDs)

  }

  //ok
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

  new YouthAnalysis(678445)
  //new YouthAnalysis(2955119)
  //new YouthAnalysis("Polska")
  //new YouthAnalysis("Ligi_1-4")
  //new YouthAnalysis("5 Liga")
  //new YouthAnalysis("6 Liga 1-256")
  //new YouthAnalysis("6 Liga 257-512")
  //new YouthAnalysis("6 Liga 513-768")
  //new YouthAnalysis("6 Liga 769-1024")
  //new YouthAnalysis("7 Liga 1-256")
  //new YouthAnalysis("7 Liga 257-512")
  //new YouthAnalysis("7 Liga 513-768")
  //new YouthAnalysis("7 Liga 769-1024")
  //new YouthAnalysis("World")
  
}

//ok
def teamsIDFromLeagueID(leagueID: Int): Seq[String] = {

  val url = leaguePath + s"$leagueID"

  val document = JsoupConnection(url)

  val teamsID: Seq[String] = document.select("td.series-table-team").select("a").attr("href").split("&")(1).split("=")(1).split(",").toSeq

  println(s"$teamsID")

  teamsID

}

//ok
def youthClubIDFromTeamID(teamID: Int): Int = {

  val url = teamPath + s"$teamID"
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

  val leagueIDsL1L4: Seq[Int] = Range(3620, 3704, 1)
  //val leagueIDsL5

  //val youthClubIds: Seq[Int] = teamsIDFromLeagueID(3672).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0))

  val youthClubIds: Seq[Int] = leagueIDsL1L4.flatMap(y => teamsIDFromLeagueID(y).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0)))

    println(youthClubIds)
  
  youthClubIds.foreach(x => {
    val currentPlayerIDs = YouthDatabase.getYouthPlayerIDsFromYouthClubID(x)
    val storedPlayerIDs: Seq[String] = YouthDatabase.getYouthPlayerIDsFromYouthDatabase(databasePath)
    YouthDatabase.addPlayerToDatabase(databasePath + "Polska_youthPlayerDatabase.csv", currentPlayerIDs, storedPlayerIDs)

  })

}

//ok
class leagueIDs_DatabasePath {

  def L1: (Range.Inclusive, String) = (3620 to 3620, databasePath + "Polska_youthPlayerDatabase_1L.csv")
  def L2: (Range.Inclusive, String) = (3621 to 3624, databasePath + "Polska_youthPlayerDatabase_2L.csv")
  def L3: (Range.Inclusive, String) = (3641 to 3704, databasePath + "Polska_youthPlayerDatabase_3L.csv")
  def L4: (Range.Inclusive, String) = (3641 to 3704, databasePath + "Polska_youthPlayerDatabase_4L.csv")

  def L1_4: (Range.Inclusive, String) = (3620 to 3704, databasePath + "Polska_youthPlayerDatabase1-4L.csv")

  def L5: (Range.Inclusive, String) = (9383 to 9638, databasePath + "Polska_youthPlayerDatabase_5L.csv")

  def L6_1_256: (Range.Inclusive, String) = (32114 to 32369, databasePath + "Polska_youthPlayerDatabase_6L_1_256.csv")
  def L6_257_512: (Range.Inclusive, String) = (32370 to 32625, databasePath + "Polska_youthPlayerDatabase_6L_257_512.csv")
  def L6_513_768: (Range.Inclusive, String) = (32626 to 32881, databasePath + "Polska_youthPlayerDatabase_6L_513_768.csv")
  def L6_769_1024: (Range.Inclusive, String) = (32882 to 33137, databasePath + "Polska_youthPlayerDatabase_6L_769_1024.csv")

  def L7_1_256: (Range.Inclusive, String) = (58605 to 58860, databasePath + "Polska_youthPlayerDatabase_7L_1_256.csv")
  def L7_257_512: (Range.Inclusive, String) = (58861 to 59116, databasePath + "Polska_youthPlayerDatabase_7L_257_512.csv")
  def L7_513_768: (Range.Inclusive, String) = (59117 to 59372, databasePath + "Polska_youthPlayerDatabase_7L_513_768.csv")
  def L7_769_1024: (Range.Inclusive, String) = (59373 to 59628, databasePath + "Polska_youthPlayerDatabase_7L_769_1024.csv")

}

object addNewPlayersToDatabase extends App{

  val leagueIDs_Path: (Range.Inclusive, String) = new leagueIDs_DatabasePath().L7_769_1024

  val leagueIDs: Seq[Int] = leagueIDs_Path._1
  val pathToCsvFile: String = leagueIDs_Path._2

  leagueIDs.foreach(l_id => {
    teamsIDFromLeagueID(l_id).map(x => youthClubIDFromTeamID(x.toInt)).filter(p => !p.equals(0)).foreach(yc => {

      val currentPlayerIDs = YouthDatabase.getYouthPlayerIDsFromYouthClubID(yc)
      val storedPlayerIDs: Seq[String] = YouthDatabase.getYouthPlayerIDsFromYouthDatabase(pathToCsvFile)
      YouthDatabase.addPlayerToDatabase(pathToCsvFile, currentPlayerIDs, storedPlayerIDs)

    })

    writeToFile(databasePath + "config1_db.dat", false, Seq.empty[String], Seq(l_id.toString))

    /*val file = new File(databasePath + "config1_db.dat")
    val writer = CSVWriter.open(file, append = false)
    writer.writeRow(Seq(l_id.toString))
    writer.close()*/

  })

}

object updateDatabase extends App{}



