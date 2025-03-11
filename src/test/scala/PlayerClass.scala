
import org.jsoup.{Connection, Jsoup}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import java.io.File
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Calendar
import scala.annotation.tailrec
import java.text.Normalizer
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.DayOfWeek
import javax.swing.plaf.DesktopIconUI
import scala.util.matching.Regex

def getTodayDate(pattern: String = "dd.MM.yyyy"): String = {

  lazy val today: Date = Calendar.getInstance().getTime
  lazy val dateFormat = new SimpleDateFormat(pattern)
  dateFormat.format(today)

}

def getYesterdayDate(pattern: String = "dd.MM.yyyy"): String = {

  lazy val yesterday: LocalDate = LocalDate.now().minusDays(1)
  val formatter = DateTimeFormatter.ofPattern(pattern)
  yesterday.format(formatter)

}

def dateToDayOfTheWeek(d: String): String = {

  //println(d)

  val data = d match {
    //default values: "dd.MM.yyyy"
    case "Dzisiaj" => /*println(getTodayDate());*/ getTodayDate()
    case "Wczoraj" => /*println(getYesterdayDate());*/ getYesterdayDate()
    case _         => /*println(d);*/ d
  }

  val dateFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  val date = LocalDate.parse(data, dateFormatter)
  val dayOfWeek = date.getDayOfWeek
  val dayOfWeekText = dayOfWeek.toString

  val engPolDayNames = Map(
    "MONDAY" -> "poniedzialek",
    "TUESDAY" -> "wtorek",
    "WEDNESDAY" -> "sroda",
    "THURSDAY" -> "czwartek",
    "FRIDAY" -> "piatek",
    "SATURDAY" -> "sobota",
    "SUNDAY" -> "niedziela")

  engPolDayNames(dayOfWeekText)

}

object QuickScanYouthPlayer{

    def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title").split(": ")(1)


}

class QuickScanYouthPlayer(args: Array[String]) {

    val url: String = args(0) + args(1)
    val connection: Connection = Jsoup.connect(url)
    val document: Document = connection.get()

    lazy val exists: Boolean = if (document.title.split("»").length <= 2) false else true

    lazy val nationality: Option[String] = if (exists) {
        try {
            Some(Youth.Nationality(document))
        }
        catch
            case _: Throwable =>
                try {
                    Some(Senior.Nationality(document))
                } catch {
                    case _: Throwable => None
                }
    }
    else None
    
}

object PlayerClass{

    def Id(document: Document): Int = {

      document.select("span.idNumber").text().replaceAll("[()]", "").toInt

    }


    def Age(document: Document): (Double, Int, Int) = {

      val wiek = document.select("div.byline").text()
      val years = wiek.split(" ")(0).toInt
      val days = wiek.split(" ")(3).toInt

        (years.toFloat + days.toFloat / 1000.0, years, days)

    }

    def RemoveDiacritics(text: String): String = {
      val normalizedText = Normalizer.normalize(text, Normalizer.Form.NFD)
        normalizedText.replaceAll("ł", "l").replaceAll("Ł", "L").replaceAll("[^\\p{ASCII}]", "")
    }

    def UpdatePreparation(line: String): (Array[String], String) = {

      val cols: Array[String] = line.split(",").map(_.trim)
      val colsDrop5: String = cols.drop(5).mkString(",").replaceAll("\"", "")

        (cols, colsDrop5)

    }

    def UpdateLineForNonExistingPlayer(colscolsDrop5: (Array[String], String), isYouthPlayer: Boolean): String = {

      val (cols, colsDrop5) = colscolsDrop5

      val sign = if(isYouthPlayer) 1 else -1

        if(math.abs(cols(4).toInt).equals(10000)) ""
        else f"${cols.take(4).mkString(",").replaceAll("\"", "")},${sign * 10000}," + colsDrop5

    }

    def AgeFormatLine(age: Double): String = f"$age%2.3f".replace(',', '.')

    def Outlook(document: Document): String = {

      val bodiesNoKits = document.select("img[src^=/Img/Avatar/bodies/]").attr("src").split("/")//
      val bodiesFromKits = document.select("img[src^=//res.hattrick.org/kits/]").attr("src").split("/")//(4).split("\\.").head

      val bodies = if(bodiesNoKits.length > bodiesFromKits.length) bodiesNoKits.last.split("\\.").head.split("_").head
      else bodiesFromKits.last.split("\\.").head.filterNot(Set('o','y').contains)

      val faces: String = document.select("img[src^=/Img/Avatar/faces/]").attr("src").split("/")(4).split("\\.").head.take(2)
      val eyes = document.select("img[src^=/Img/Avatar/eyes/]").attr("src").split("/")(4).split("\\.").head
      val mouths = document.select("img[src^=/Img/Avatar/mouths/]").attr("src").split("/")(4).split("\\.").head
      val noses = document.select("img[src^=/Img/Avatar/noses/]").attr("src").split("/")(4).split("\\.").head
      val hair_tmp: String = document.select("img[src^=/Img/Avatar/hair/]").attr("src").split("/")(4).split("\\.").head.drop(2)
      val hair = if(hair_tmp.length > 2) hair_tmp.dropRight(1) else hair_tmp

      val outlook = Seq(bodies, faces, eyes, mouths, noses, hair).mkString("-")

      outlook

    }





}

class PlayerClass(args: Array[String]) {

  val url: String = args(0) + args(1)
  //println(url)
  val connection: Connection = Jsoup.connect(url)
  val document: Document = connection.get()

  lazy val exists: Boolean = if (document.title.split("»").length <= 2) false else true
  lazy val has_club: Boolean = if (document.title.split("»").length == 4) true else false

  lazy val id: Option[Int] = if (exists) Some(PlayerClass.Id(document)) else None

  lazy val name: Option[String] = if (exists) {
        //document.title.split("»").foreach(println(_))/*.head.trim*/
        Some(PlayerClass.RemoveDiacritics(document.title.split("»").head.replaceAll(",","").trim))
    } else None

  lazy val age: Option[(Double, Int, Int)] = if (exists) Some(PlayerClass.Age(document)) else None
  lazy val link: Option[String] = if (exists) Some(args(0) + args(1)) else None

  lazy val specialityMap: Map[String, String] = Map("gra głową" -> "H", "nieprzewidywalny" -> "U", "techniczny" -> "T", "atletyczny" -> "P", "szybki" -> "Q", "witalny" -> "R", "pomocny" -> "S", "" -> "")
  lazy val speciality: Option[String] = if (exists) Some(specialityMap(document.select("td[colspan]").select("i[title]").attr("title"))) else None

  lazy val outlook: Option[String] = if(exists) Some(PlayerClass.Outlook(document)) else None


}

object Youth{

    def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title").split(": ")(1)
    def Since(document: Document): Int = document.select("span.shy span[dir=ltr]").first.text().split(" ")(2).toInt

    def BestPerformances(document: Document): String = {

        val arrayString: Array[String] = document.select("table.youthPlayerPerformance").select("td.middle").
          text().replaceAll("-", "-1 -").replaceAll("boczny obrońca", "boczny_obrońca").split(" ")

        //arrayString.foreach(println(_))
        val nn: Elements = document.select("table.youthPlayerPerformance").select("td.middle")//.asScala

        //var positionRatingMap = Map[String, String]()
        var result = ""
        nn.forEach { td =>
           // Sprawdź czy obecny element zawiera klasę "right", co oznacza pozycję zawodnika
          if (td.hasClass("right")) {
            //position = td.text() // Pozycja zawodnika
            //result += td.text() + ","
            null
          } else {
            // Jeśli nie, to sprawdź czy jest to element hattrick-rating, jeśli tak, pobierz rating
            if (td.select("hattrick-rating").size() > 0) {
              val rating = td.select("hattrick-rating").attr("rating")
              //positionRatingMap += (position -> rating)
              result += rating + ","
            } else {
            // Jeśli nie, pobierz znak "-"
              //positionRatingMap += (position -> "-")
              result += "-1,"
            }
          }

        }

        // Usuń ostatni przecinek z wyniku
        if (result.nonEmpty) {
          result = result.dropRight(1)
        }

        // Wyświetl wynik
        //ntln(result)

        /*@tailrec
        def addString(str: String, stringToAdd: Array[String], index: Int): String =
            if (index >= stringToAdd.length) str
            else addString(str + "," + stringToAdd(index), stringToAdd, index + 3)

        addString(arrayString(1), arrayString, 4)*/

        result

    }

    def Last5Performances(document: Document): (Seq[Double], String) = {

      val arrayString1: mutable.Buffer[String] = document.select("div.mainBox").select("td.middle").asScala.map(x => x.text().replaceAll("boczny obrońca", "boczny_obrońca"))
      val arrayString2 = document.select("div.mainBox").select("span.stars-full").asScala
      //val arrayString2 = document.select("div.mainBox").select("td.nowrap-middle").asScala
      val arrayString3 = document.select("div.mainBox").select("td.top").asScala

      val nGames = arrayString3.length

      //arrayString1.foreach(println(_))
      //arrayString2.foreach(println(_))
      //arrayString3.foreach(println(_))

      val dictionary = Seq("bramkarz", "stoper", "boczny_obrońca", "pomocnik", "skrzydłowy", "napastnik")
      val dictionaryMap = Map("bramkarz" -> 0, "stoper" -> 1, "boczny_obrońca" -> 2, "pomocnik" -> 3, "skrzydłowy" -> 4, "napastnik" -> 5)

      val lastMatchDetails: String =
            if(nGames > 0)
                PlayerClass.RemoveDiacritics(arrayString1.head.split(" ").head + " " + arrayString1(2))
            else
                "--------"

      val data: Seq[(Int, Double)] = (0 until nGames).map(i => {
        val nPositionsPlayed = dictionary.count(p => arrayString1(2 + i * 3).contains(p))
        val minutesPlayed: Int = if (nPositionsPlayed == 1) arrayString1(2 + i * 3).split(" ").last.replaceAll("[(')]", "").toInt else 0
        val positionPlayed: Int = if (nPositionsPlayed == 1  && minutesPlayed == 90) dictionaryMap(arrayString1(2 + i * 3).split(" ").head) else -1
        //val starsPlayed: Double = if (arrayString3(i).text().nonEmpty) {arrayString3(i).text().toDouble} else -1.0
        val starsPlayed: Double = if (arrayString3(i).select("hattrick-rating").attr("rating").nonEmpty)
        {
          //arrayString3(i).text().toDouble
          arrayString3(i).select("hattrick-rating").attr("rating").toDouble
        }
        else -1.0

            (positionPlayed, starsPlayed)
        })

      val newRecord: Seq[Double] = (0 to 5).map(i => {
        val tmp = data.filter(p => p._1 == i).map(_._2)
        val tmp2 = if (tmp.isEmpty) -1 else tmp.max
            tmp2
        })

        (newRecord,lastMatchDetails)

    }

    def Read_td(worldCupNumber: Int): Elements = {

      val url: String = s"https://hattrickportal.pro/Tracker/U20/U21WC.aspx?WorldCup=$worldCupNumber"
      val connection: Connection = Jsoup.connect(url)
      val document: Document = connection.get()

      //println(document.select("td"))

      document.select("td")
 
    }

    def Read_td_S(worldCupNumber: Int): String = {

      U21_schedule_generator(worldCupNumber).replaceAll("<td>","").replaceAll("</td>","")

    }


    def WorldCupAgeMinMax(worldCupNumber: Int): ((Double, Int, Int), (Double, Int, Int)) = {

      val td: mutable.Seq[Element] = Read_td(worldCupNumber: Int).asScala

      val maxAgeYears = td(15).text().split(" ")(0).toInt //3->15
      val maxAgeDays = td(15).text().split(" ")(3).toInt

      val minAgeYears = td(167).text().split(" ")(0).toInt
      val minAgeDays = td(167).text().split(" ")(3).toInt

      val ageMax = maxAgeYears + maxAgeDays / 1000.0
      val ageMin = minAgeYears + minAgeDays / 1000.0

        ((ageMin, minAgeYears, minAgeDays), (ageMax, maxAgeYears, maxAgeDays))

    }

    def WorldCupAgeMinMax_S(worldCupNumber: Int): ((Double, Int, Int), (Double, Int, Int)) = {

      val td: Array[String] = Read_td_S(worldCupNumber).split("\n")

      //td.foreach(println(_))

      val maxAgeYears: Int = td(15).split(" ")(0).toInt  //3->15; trzeba w tym stringu pr zrobić jakieś eparatory po których tu rozdzielę
      val maxAgeDays: Int = td(15).split(" ")(3).toInt

      val minAgeYears: Int = td(167).split(" ")(0).toInt
      val minAgeDays: Int = td(167).split(" ")(3).toInt

      val ageMax: Double = maxAgeYears + maxAgeDays / 1000.0
      val ageMin: Double = minAgeYears + minAgeDays / 1000.0

      ((ageMin, minAgeYears, minAgeDays), (ageMax, maxAgeYears, maxAgeDays))

    }

    @tailrec
    def WhichWorldCup(playerAge: (Double, Int, Int), worldCupNumber: Int): Int = {

      val ageMin = WorldCupAgeMinMax(worldCupNumber)._1._1
        //val ageMax = WorldCupAgeMinMax(worldCupNumber)._2._1

      val ageCurrent = playerAge._1

        if (ageCurrent >= ageMin) worldCupNumber
        else WhichWorldCup(playerAge, worldCupNumber + 1)

    }

    @tailrec
    def WhichWorldCup_S(playerAge: (Double, Int, Int), worldCupNumber: Int): Int = {

      val ageMin = WorldCupAgeMinMax_S(worldCupNumber)._1._1
      val ageMax = WorldCupAgeMinMax_S(worldCupNumber)._2._1

      val ageCurrent = playerAge._1

      //println(s"___${ageMin},${ageMax},${ageCurrent}___")

      if (ageCurrent >= ageMin) worldCupNumber
      else WhichWorldCup_S(playerAge, worldCupNumber + 1)

    }

    @tailrec
    def f(td: mutable.Buffer[Element], worldCupNumber: Int, playerAge: Double, index: Int): String = {

      val age = td(index).text().split(" ")(0).toDouble + td(index).text().split(" ")(3).toDouble / 1000.0
      val round_id = ((index - 7) / 4.0 + 1.0).toInt
      val round = td(index - 5).text().replaceAll(",", " ")
      val date = td(index - 6).text()
      val week = td(index - 7).text().replaceAll("/", ",")

        if (playerAge > age) s"WC $worldCupNumber --> $round_id --> $round --> $date --> $week" //-1 provides mistake in WC number
        else f(td, worldCupNumber, playerAge, index + 4)

    }

    @tailrec
    def f_S(td: Array[String], worldCupNumber: Int, playerAge: Double, index: Int, round_id_overcounting: Boolean): String = {

      //println(s"${td(index).split(" ")(0).toDouble}, ${td(index).split(" ")(3).toDouble / 1000.0}")

      val age = td(index).split(" ")(0).toDouble + td(index).split(" ")(3).toDouble / 1000.0
      val round_id = if(round_id_overcounting) ((index - 7) / 4.0 + 1.0).toInt + 42; else ((index - 7) / 4.0 + 1.0).toInt

      val round = td(index - 5).replaceAll(",", " ")
      val date = td(index - 6)
      val week = td(index - 7).replaceAll("/", ",")

      if (playerAge > age) s"WC $worldCupNumber --> $round_id --> $round --> $date --> $week" //-1 provides mistake in WC number
      else f_S(td, worldCupNumber, playerAge, index + 4, round_id_overcounting)

    }

    def WhichRoundOfWorldCup(playerAge: (Double, Int, Int), worldCupNumber: Int): String = {

      val td: mutable.Buffer[Element] = Read_td(worldCupNumber).asScala

      val age = WorldCupAgeMinMax(worldCupNumber)

      val ageMin = age._1._1
      val ageMax = age._2._1

        if (playerAge._1 > ageMax) s"WC ${worldCupNumber - 1} --> ,Final,,,"
        else if (playerAge._1 <= ageMin) s"WC $worldCupNumber --> ,Final,,,"
        else f(td, worldCupNumber, playerAge._1, 7)

    }

    def WhichRoundOfWorldCup_S(playerAge: (Double, Int, Int), worldCupNumber: Int): String = {

      val td: Array[String] = Read_td_S(worldCupNumber).split("\n")

      val age: ((Double, Int, Int), (Double, Int, Int)) = WorldCupAgeMinMax_S(worldCupNumber)

      val ageMin = age._1._1
      val ageMax = age._2._1

      /*println(playerAge)
      println(age)*/

      /*if (playerAge._1 > ageMax) s"WC ${worldCupNumber - 1} --> 42,World Cup - Finals,,,"
      else if (playerAge._1 <= ageMin) s"WC $worldCupNumber --> 42,World Cup - Finals,,,"
      else f_S(td, worldCupNumber, playerAge._1, 7)*/

      if (playerAge._1 > ageMax) f_S(td, worldCupNumber-1, playerAge._1, 7, true)
      else f_S(td, worldCupNumber, playerAge._1, 7, false)

    }

    def Availability(age: (Double, Int, Int)): String = {

      val i = 37

      val worldCupNumber = WhichWorldCup_S(age, i)
      //print(worldCupNumber)
      val worldCupRound = WhichRoundOfWorldCup_S(age, worldCupNumber)

        worldCupRound

    }
    
    def UpdateNonExistingPlayer(line: String): String = {

      val cols_colsDrop5 = PlayerClass.UpdatePreparation(line)

        PlayerClass.UpdateLineForNonExistingPlayer(cols_colsDrop5, true)
        
    }


    def UpdateExistingPlayer(yp: Youth, b5p: Seq[String], l5p: Seq[Double]): String = {

      val last5Games: (Seq[Double], String) = yp.last5Performances.getOrElse((Seq(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0), "-----"))
      val lastGame = last5Games._2
      //println(lastGame)
      val bestPerformances: String = yp.bestPerformances.getOrElse(b5p.mkString(","))

      val age: String = PlayerClass.AgeFormatLine(yp.age.get._1)
      val since = yp.since.get
      val specialityStatus = yp.speciality.getOrElse("")
      //  println(s"$specialityStatus")
      val speciality = if (!specialityStatus.equals("")) specialityStatus.concat(s"$since") else specialityStatus

      val outlook: String = yp.outlook.get
      val scoutingDetails: String = yp.scoutingHistory.getOrElse((Seq("------,------,0"),"------"))._2

        println(f"${yp.name.get},${yp.id.get},$age,$speciality,$since,${yp.availability.get.replaceAll(" --> ", ",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,${yp.nationality.get},${getTodayDate()},$outlook,$scoutingDetails")
        f"${yp.name.get},${yp.id.get},$age,$speciality,${yp.since.get},${yp.availability.get.replaceAll(" --> ", ",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,${yp.nationality.get},${getTodayDate()},$outlook,$scoutingDetails"

    }

    def UpdateExistingPlayer1(yp: Youth, b5p: Seq[String], l5p: Seq[Double], line: String): String = {

      val last5Games: (Seq[Double], String) = yp.last5Performances.getOrElse((Seq(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0), "-----"))
      val lastGame = last5Games._2
      val bestPerformances: String = yp.bestPerformances.getOrElse(b5p.mkString(","))

      val age: String = PlayerClass.AgeFormatLine(yp.age.get._1)

      val playerAtributes: Array[String] = line.split(",").map(_.trim)



      val outlook: String = if(playerAtributes.length <= 26) yp.outlook.get
      else playerAtributes(26)

      val scoutingDetails: String = if(playerAtributes.length < 30) { //= if want to add scouting history from all databse to csv file

        val scoutingHistory = yp.scoutingHistory.getOrElse((Seq("------,------,0"),"------"))//._2

        val nationality: String = yp.nationality.getOrElse("unknown")
        //val scoutingHistory: Seq[String] = yp.scoutingHistory.getOrElse((Seq("------,------,0"), "------"))._1

        scoutingHistory._1.foreach(sh => if (!sh.contains("-")) writeToFile(databasePath + "scouting_history.csv", true, Seq.empty[String], Seq(s"$nationality,$sh")))

        scoutingHistory._2

      }
      else playerAtributes.slice(27,30).mkString(",")

      //println(scoutingDetails)

      /*val (name, /*id, */ previousSpecialityStatus, previousSince, availability_wc, availability_num, availability_descr, availability_lastM, availability_lSeason, availability_lWeek, nationality)
        = (playerAtributes(0)/*,playerAtributes(1)*/,playerAtributes(3),playerAtributes(4),playerAtributes(5),playerAtributes(6),playerAtributes(7),playerAtributes(8),playerAtributes(9),playerAtributes(10),playerAtributes(24))
      */

      val (name, /*id, */ previousSpecialityStatus, previousSince, /*availability_wc, availability_num, availability_descr, availability_lastM, availability_lSeason, availability_lWeek, */nationality)
        = (playerAtributes(0)/*,playerAtributes(1)*/,playerAtributes(3),playerAtributes(4)/*,playerAtributes(5),playerAtributes(6),playerAtributes(7),playerAtributes(8),playerAtributes(9),playerAtributes(10)*/,playerAtributes(24))
      
      
      val currentSpecialityStatus: String = yp.speciality.getOrElse("")
      val since = yp.since.get
      val id = yp.id.get.toString

      //val speciality = if(currentSpecialityStatus.equals(previousSpecialityStatus)) currentSpecialityStatus else currentSpecialityStatus.concat(s"$since")
      val speciality = if(currentSpecialityStatus.equals(previousSpecialityStatus.take(1))) previousSpecialityStatus else currentSpecialityStatus.concat(s"$since")

      val last5Games_updated: String = last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")

      lazy val aval: Array[String] = yp.availability.get.split("-->").map(_.trim)

      //val f: String = f"$name,$id,$age,$speciality,$since,$availability_wc,$availability_num,$availability_descr,$availability_lastM,$availability_lSeason,$availability_lWeek,$bestPerformances,$last5Games_updated,$lastGame,$nationality,${getTodayDate()},$outlook,$scoutingDetails"
      val f: String = f"$name,$id,$age,$speciality,$since,${aval(0)},${aval(1)},${aval(2)},${aval(3)},${aval(4)},$bestPerformances,$last5Games_updated,$lastGame,$nationality,${getTodayDate()},$outlook,$scoutingDetails"

        if(!yp.stillInYouthAcademy) {

          val sp = new Senior(Array(seniorPlayerPath,id))

          val character: String = sp.character.get

            /*if(sp.onTL.get) {
              val skills = sp.skills.get
                writeToFile(databasePath + "TL_listed.csv", true, Seq.empty[String], Seq(s"$name,${sp.daysInClub.get},$id,$last5Games_updated,${skills.productIterator.mkString(",")},$scoutingDetails"))
            }*/

            if (sp.onTL.get) {
              val skills = sp.skills.get
              val exp: String = sp.exp.get

              writeToFile(databasePath + "TL_listed_new.csv", true, Seq.empty[String], Seq(s"$name,$id,${sp.age.get._1},${sp.exp.get},${sp.speciality.get},${sp.daysInClub.get},$last5Games_updated,${skills.productIterator.mkString(",")},$character,$scoutingDetails"))
            }

            if(sp.exists){

              writeToFile(databasePath + "characters.csv", true, Seq.empty[String], Seq(s"$name,${sp.daysInClub.get},$id,${outlook.replaceAll("-", ",")},$character"))
              if(sp.nationality.get.equals("Polska")) {
                writeToFile(databasePath + "specialities.csv", true, Seq.empty[String], Seq(s"$name,$id,$speciality,${speciality.take(1)},${speciality.drop(1)},${sp.speciality.get}"))
              }

              //Jak oczyszczę bazę z rekordów z błędną identyfikacją specjałek - ostatni grajek w bazie:
              // Edmund Smuga 333622322
              //val youth_speciality: String = previousSpecialityStatus
              //val senior_speciality: String = sp.speciality.get
              //writeToFile(databasePath + "specialities.csv", true, Seq.empty[String], Seq(s"$youth_speciality,$previousSince,$senior_speciality"))

            }

        }


        println(f)
        //println(s"${currentSpecialityStatus.equals(previousSpecialityStatus)} $previousSpecialityStatus $currentSpecialityStatus $speciality")
        f

        //f"${name/*.replaceAll("\"","")*/},${id},$age,${yp.speciality.getOrElse("-")},${yp.since.get},$availability_wc,$availability_num,$availability_descr,$availability_lastM,$availability_lSeazon,$availability_lWeek,$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,$nationality,${getTodayDate()}"

        //println(f"${yp.name.get},${yp.id.get},$age,${yp.speciality.getOrElse("-")},${yp.since.get},${yp.availability.get.replaceAll(" --> ", ",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,${yp.nationality.get},${getTodayDate()}")
        //f"${yp.name.get},${yp.id.get},$age,${yp.speciality.getOrElse("-")},${yp.since.get},${yp.availability.get.replaceAll(" --> ", ",")},$bestPerformances,${last5Games._1.zip(l5p).map(x => math.max(x._1, x._2)).mkString(",")},$lastGame,${yp.nationality.get},${getTodayDate()}"


    }

    def ScoutingEngine(id: String): (Array[String], Array[String]) = {

      val path = "https://www85.hattrick.org/pl/Club/Players/YouthPlayerHistory.aspx?YouthPlayerID="

      val url: String = path + id
      val connection: Connection = Jsoup.connect(url)
      val document: Document = connection.get()

      val memoryDates: String = document.select("h3.feed").text()
      val memoryHours: String = document.select("span.date").text()
      val memoryItems: String = document.select(".feedItem").text()

      //the oldest records are about scouting, the newest about hattricks, name or face change

      val memoryDates1: Array[String] = memoryDates.split(" ")//.map(x => x.replaceAll("\\.","/"))
      val memoryHours1: Array[String] = memoryHours.split(" ").takeRight(memoryDates1.length)
      val memoryItems1: Array[String] = memoryItems.split("\\.").takeRight(memoryDates1.length)

       /*println(s"$memoryDates ${memoryDates1.length}")
       println(s"$memoryHours ${memoryHours1.length}")
       println(s"$memoryItems ${memoryItems1.length} ")

       memoryDates1.foreach(println(_))
       memoryHours1.foreach(println(_))
       memoryItems1.foreach(println(_))*/


      /*val tableMemoryBool: Array[Boolean] = memoryItems.split("\\.").map(
        mi => List("przyprowadził", "Nie otrzymał").exists(text => mi.contains(text))
      )*/

      val tableMemoryBool: Array[Boolean] = memoryItems1.map(
        mi => List("przyprowadził", "Nie otrzymał").exists(text => mi.contains(text))
      )

      /*println(memoryDates1.nonEmpty)
      memoryDates1.foreach(println(_))
      tableMemoryBool.foreach(println(_))
      println(tableMemoryBool.contains(true))*/

      val memoryModified: Array[String] =
        if (memoryDates1.nonEmpty && memoryDates1.head != "" && tableMemoryBool.contains(true))
          val scoutingLine = (memoryDates1 /*.split(" ")*/.map(x => dateToDayOfTheWeek(x)) zip memoryHours1 /*.split(" ")*/) mkString "-" replaceAll("[()]", "") split "-" zip tableMemoryBool filter (p => p._2) map (_._1)
          scoutingLine
        else
          Array("------,------")

      val scoutingRecord: Array[String] =
        if (memoryDates1.nonEmpty && memoryDates1.head != "" && tableMemoryBool.contains(true))
          val scoutingLine = memoryDates1 zip memoryItems1 mkString "=" replaceAll("[()]", "") split "=" zip tableMemoryBool filter (p => p._2) map (_._1)
          //val scoutingLine = memoryDates1 zip memoryItems1 mkString "-*-+-" replaceAll("[()]","") split "-*-+-" zip tableMemoryBool filter (p => p._2) map (_._1)
          scoutingLine
        else
          Array("------,------")

      (memoryModified,scoutingRecord)

    }

    def ScoutingHistory(id: String): (Seq[String], String) = {

      val memoryModified = ScoutingEngine(id)._1

      /*println("+++")
      memoryModified.foreach(println(_))
      */

      val scoutingHistory: Seq[String] = memoryModified.zipWithIndex.map(x => x._1 + "," + (memoryModified.length - x._2).toString).toSeq

      (scoutingHistory,scoutingHistory.head)

    }

    def RejectionHistory(id: String): Option[Array[String]] = {

    val scoutingRecord = ScoutingEngine(id)._2

    val rejectionHistory: Option[Array[String]] = if (scoutingRecord.length > 1)
      Some(scoutingRecord.drop(1))
    else
      None

    /*println("xxx")
    scoutingRecord.foreach(println(_))
    if(rejectionHistory.getOrElse(Array.empty[String]).nonEmpty) rejectionHistory.get.foreach(println(_))*/

    rejectionHistory

  }

}

class Youth(args: Array[String]) extends PlayerClass(args){

  lazy val stillInYouthAcademy: Boolean = id.getOrElse(false).equals(args(1).toInt)

  //println(s"$stillInYouthAcademy")

  lazy val nationality: Option[String] = if(exists) {
        try {
            Some(Youth.Nationality(document))
        }
        catch
            case _: Throwable =>
                try {
                    Some(Senior.Nationality(document))
                } catch {
                    case _: Throwable => None
                }
    }
    else None

  lazy val since: Option[Int] = if (exists) {
        try {
            Some(Youth.Since(document))
        }
        catch
            case _: Throwable =>
                try {
                    Some(-1) //promoted to senior team
                } catch {
                    case _: Throwable => None
                }
    }
    else None

  lazy val availability: Option[String] = if( exists) Some(Youth.Availability(age.get)) else None

  lazy val bestPerformances: Option[String] = if(stillInYouthAcademy) Some(Youth.BestPerformances(document)) else None
  lazy val last5Performances: Option[(Seq[Double],String)] = if(stillInYouthAcademy) Some(Youth.Last5Performances(document)) else None

  lazy val scoutingHistory: Option[(Seq[String], String)] = if(stillInYouthAcademy) Some(Youth.ScoutingHistory(id.get.toString)) else None
  lazy val rejectionHistory: Option[Option[Array[String]]] = if(stillInYouthAcademy) Some(Youth.RejectionHistory(id.get.toString)) else None
}

object Senior{

    def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title")

    def OnTL(document: Document): Boolean = document.select("div.transferPlayerSkills").asScala.length.equals(1)

    def NewDateFormat(pattern: String = "dd.MM.yyyy") = new SimpleDateFormat(pattern)

    def SinceFrom(document: Document): String = {
      document.select("span.shy span[dir=ltr]").text().split(" ")(1).replace(")", "")
    }

    def DaysInClub(joinDateString: String): Long = {
      val dateFormat = NewDateFormat() //default value = dd.mm.rrrr
      //val dateFormat: SimpleDateFormat = new SimpleDateFormat("dd.MM.yyyy") //default value = dd.mm.rrrr
      val today: Date = Calendar.getInstance().getTime
      val joinDate: Date = dateFormat.parse(joinDateString)
      (today.getTime - joinDate.getTime) / (1000 * 60 * 60 * 24)
    }

    def UpdateNonExistingPlayer(line: String): String = {

      val cols_colsDrop5 = PlayerClass.UpdatePreparation(line)

        PlayerClass.UpdateLineForNonExistingPlayer(cols_colsDrop5, false)
    }
    
    def UpdateExistingPlayer(sp: Senior, line: String): String = {

      val colsDrop5 = PlayerClass.UpdatePreparation(line)._2
      val age: String = PlayerClass.AgeFormatLine(sp.age.get._1)

       if(age.toDouble < 18.0)
         f"${sp.name.get},${sp.id.get},$age,${sp.speciality.getOrElse("-")},-2," + colsDrop5
       else
         null

    }

  def updateSeniorPlayer(id: String, skills: Seq[String]): String = {

    val sp = new Senior(Array(seniorPlayerPath, id))

    if !sp.exists then
      println("nie istnieje")
      null
    else
      val name: String = sp.name.get
      val age = sp.age.get._1
      val nationality = sp.nationality.get
      //tsi,salary,form,condition
      val generalInfo = sp.generalInfo.get
      val (tsi, salary, form, condition, stamina_history) = sp.generalInfo.get //tu jest info o meczach
      //i danych z meczów
      val speciality = sp.speciality.get //OrElse("")
      val exp = sp.exp.get
      val character = sp.character.get
      val skills = sp.skills

      val gk = skills.map(_._5.toString).getOrElse("-")
      val df = skills.map(_._6.toString).getOrElse("-")
      val pm = skills.map(_._7.toString).getOrElse("-")
      val wg = skills.map(_._8.toString).getOrElse("-")
      val pass = skills.map(_._9.toString).getOrElse("-")
      val sco = skills.map(_._10.toString).getOrElse("-")
      val sf = skills.map(_._11.toString).getOrElse("-")


      val f = s"$name,$id,$age,$speciality,$exp,$tsi,$salary,$form,$condition,$stamina_history,$character,$gk,$df,$pm,$wg,$pass,$sco,$sf,${getTodayDate()}"
      //val f = s"$name,$id,$age,$exp"
      println(f)
      f

  }


  //ERROR to ma czytać info a nie info1
  def GeneralInfo(bufferElement: mutable.Buffer[Element], playerId: String): (Int, Int, Int, Int,String) = {
      
      //bufferElement.foreach(println(_))

      val Matchid_buffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
      val PosTime_buffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]
      val ConStar_buffer: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

      val MatchDetailsStars_buffer = Seq.newBuilder[String]
      val MatchDetailsStamina_buffer = Seq.newBuilder[String]
      val MatchDetailsCondition_buffer = Seq.newBuilder[String]
      val MatchDetailsPositions_buffer = Seq.newBuilder[String]
      val MatchDetailsSinglePosition_buffer = Seq.newBuilder[String]

      var counter = 0
    bufferElement.foreach(f = s => if (s.toString.contains("class=\"nowrap middle\"")) { //potrzebne do pozycja minuty
      //println(s)
        counter += 1
        if (counter == 1)
        {
          val matchid = s.toString.split("=")(4).split("&").head
          println(matchid)

          val playerMatchDetails: (String, String, String, String, String, String) = matchPlayerAnalyzer(matchid, playerId)

          Matchid_buffer += s"$matchid "
          MatchDetailsStars_buffer   += s"${playerMatchDetails._1}"
          MatchDetailsStamina_buffer += s"${playerMatchDetails._4}"
          MatchDetailsCondition_buffer += s"${playerMatchDetails._3}"
          MatchDetailsPositions_buffer += s"${playerMatchDetails._5}"
          MatchDetailsSinglePosition_buffer += s"${playerMatchDetails._6}"

        }
        else if (counter == 3 /*% 3 == 1*/) //==1 wypisze wszystko; ==3 wypisze pozycję i minuty
        {
          println(s)

          //dobrze odczytuje pozycje i minuty
          val ss = s.toString.split(">", 2)(1).split("<", 2)(0).split("\\(",2)
          val (pozycja, minuty) = (ss(0).dropRight(2), ss(1).dropRight(3))
          println(s"$pozycja $minuty")

          PosTime_buffer += s"$pozycja $minuty "

          counter = 0
        }

      })

      val matchid: Seq[String] = Matchid_buffer.result()
      val posTime = PosTime_buffer.result()
      val matchDetailsStars: Seq[String] = MatchDetailsStars_buffer.result()
      val matchDetailsStamina = MatchDetailsStamina_buffer.result()
      val matchDetailsCondition = MatchDetailsCondition_buffer.result()
      val matchDetailsPositions = MatchDetailsPositions_buffer.result()
      val matchDetailsSinglePosition: Seq[String] = MatchDetailsSinglePosition_buffer.result()

    val test: Seq[(String, String)] = matchDetailsSinglePosition.zip(matchDetailsStars).filter(p => p._1 == "IM")
    test.foreach(println(_))
    

    counter = 0
    bufferElement.foreach(f = s => if (s.toString.contains("class=\"nowrap middle center\"")) { //potrzebne do pozycja minuty
      counter += 1
      if (counter == 1 /*% 3 == 1*/ ) //==1 wypisze wszystko; ==3 wypisze pozycję i minuty
      {
        //println(s)

        //tu ma czytać gwiazdki i kondycję - na razie nie czyta; czyta ale na koniec meczu, nie 90'; pominać w analizie
        //edit: tak te gwiazdki są złe, i końcowa kondycja też
        val stars = s.toString.split(">", 2)(1).split(">", 2)(0).split(" ", 2)(1).split("\"")(1)
        val end_condition = s.toString.split(">", 2)(1).split(">", 2)(0).split(" ")(2).split("\"")(1)
        //println(end_condition)
        //println(s"stars: $end_condition")

        ConStar_buffer += s"$end_condition $stars"

        counter = 0
      }

    })

    val conStars = ConStar_buffer.result()//to generuje błędne wyniki kondycji i gwiazdek, bo są na koniec meczu, nie 90'



    posTime.foreach(println(_))
    //val matchInfo_tmp: Seq[String] = posTime.zip(conStars).map(x => x._1.concat(x._2))
    val matchInfo: Seq[String] = matchid.zip(posTime).map(x => x._1.concat(x._2))
    val matchInfo1: Seq[String] = matchDetailsStars.zip(matchInfo).map(x => x._1.concat(x._2))
    val matchInfo2: Seq[String] = matchDetailsStamina.zip(matchInfo1).map(x => x._1.concat(x._2))

    println("******")
    matchDetailsStars.foreach(println(_))
    matchDetailsCondition.foreach(println(_))
    println(matchDetailsCondition.mkString("-"))
    matchDetailsStamina.foreach(println(_))
    println(matchDetailsStamina.mkString("-"))
    println(matchDetailsPositions)
    println("++")
    matchInfo.foreach(println(_))
    matchInfo1.foreach(println(_))
    matchInfo2.foreach(println(_))
    println("******")

    val stamina_history: String = matchDetailsStamina.mkString("-")

    val index: Int = bufferElement.indexOf(bufferElement.find(_.text == "TSI").get)

      val TSI = bufferElement(index + 1).select("td").text().replaceAll(" ", "").toInt
      val Salary20 = bufferElement(index + 3).select("td").text().replaceAll(" ", "").replaceAll("zł/tydzień", "")
      val SalaryBASE = bufferElement(index + 3).select("td").select("span").attr("title").split(" ")(0).replaceAll("zł/tydzień", "").trim.replaceAll(" ", "")
      val Salary = (if (SalaryBASE.nonEmpty) SalaryBASE else Salary20).toInt
      val Form = bufferElement(index + 7).select("span.denominationNumber").text().split(" ").head.toInt
      val Condition = bufferElement(index + 10).select("span.denominationNumber").text().split(" ").head.toInt

      (TSI,Salary,Form,Condition,stamina_history)

    }

    def Skills(bufferElement: mutable.Buffer[Element]): (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    = {

      val index: Int = bufferElement.indexOf(bufferElement.find(_.text == "TSI").get)

      val TSI = bufferElement(index + 1).select("td").text().replaceAll(" ", "").toInt

      val Salary20 = bufferElement(index + 3).select("td").text().replaceAll(" ", "").replaceAll("zł/tydzień", "")
      val SalaryBASE = bufferElement(index + 3).select("td").select("span").attr("title").split(" ")(0).replaceAll("zł/tydzień", "").trim.replaceAll(" ", "")
      val Salary = (if (SalaryBASE.nonEmpty) SalaryBASE else Salary20).toInt

      val Form = bufferElement(index + 7).select("span.denominationNumber").text().split(" ").head.toInt
      val Condition = bufferElement(index + 10).select("span.denominationNumber").text().split(" ").head.toInt
      val GK = bufferElement(index + 14).select("span.denominationNumber").text().split(" ").head.toInt
      val DEF = bufferElement(index + 17).select("span.denominationNumber").text().split(" ").head.toInt
      val PM = bufferElement(index + 20).select("span.denominationNumber").text().split(" ").head.toInt
      val WG = bufferElement(index + 23).select("span.denominationNumber").text().split(" ").head.toInt
      val PASS = bufferElement(index + 26).select("span.denominationNumber").text().split(" ").head.toInt
      val SCO = bufferElement(index + 29).select("span.denominationNumber").text().split(" ").head.toInt
      val SP = bufferElement(index + 32).select("span.denominationNumber").text().split(" ").head.toInt

        (TSI, Salary, Form, Condition, GK, DEF, PM, WG, PASS, SCO, SP)

    }

    def Character(document: Document): String = {

    val gentleness = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=gentleness]").text()
    val aggressiveness = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=aggressiveness]").text()
    val honesty = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=honesty]").text()
    
    val leadership = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=leadership]").text()

    val gentlenessMap = Map("złośliwy" -> "0", "kontrowersyjny" -> "1", "przyjemny" -> "2", "sympatyczny" -> "3", "popularny" -> "4", "uwielbiany przez zespół" -> "5")
    val aggressivenessMap = Map("złośliwy" -> "0", "kontrowersyjny" -> "1", "przyjemny" -> "2", "sympatyczny" -> "3", "popularny" -> "4", "uwielbiany przez zespół" -> "5")

    val character: String = Seq(gentleness, aggressiveness, honesty, leadership).mkString(",")

    character

  }

    def Exp(document: Document): String = {

      val experience: String = document.select("a[href^=/pl/Help/Rules/AppDenominations.aspx?lt=skill]").text().split(" ").head

    //println(s"exp: ${experience}")

      experience

  }

    def matchPlayerAnalyzer(matchid: String, playerid: String) = {

      val seniorMatchidPath = "https://www85.hattrick.org/Club/Matches/Match.aspx?matchID="

      val path = seniorMatchidPath + matchid

      val url: String = path
      val connection: Connection = Jsoup.connect(url)
      val document: Document = connection.get()

      val scriptElements: Elements = document.select("script[type=text/javascript]")

      val scriptContent: Array[Array[String]] = scriptElements.asScala(24).html().split("\\{").map(_.split(","))

      val only90: Array[String] = scriptContent.dropWhile(_.head != "\"minute\":90").filter(_.head == "\"playerId\":" + playerid).head

      println("----")
      only90.foreach(println(_))
      println("----")

      val stars: String = only90(1).split(":")(1)

      val position = only90(2).split(":")(1) match {
        case "100" => "GK"
        case "101" => "WB"
        case "102" => "CD"
        case "103" => "CD"
        case "104" => "CD"
        case "105" => "WB"
        case "106" => "W"
        case "107" => "IM"
        case "108" => "IM"
        case "109" => "IM"
        case "110" => "W"
        case "111" => "F"
        case "112" => "F"
        case "113" => "F"
        case _ => "XX"

      }

      val behaviour = only90(3).split(":")(1) match {
        //case "0" => ""
        case "0" => "-N"
        case "1" => "-OFF"
        case "2" => "-DEF"
        case "3" => "-TM"
        case "4" => "-TW"
        case _   => "---"

      }

      val isKicker: String = only90(5).split(":")(1) match {
        case "true" => "-SP"
        case _ => ""
      }

      val condition = "%.0f".format(only90(6).split(":")(1).dropRight(1).toDouble * 100.0).replace(",", ".") + "%"
      val stamina = "%.2f".format(9.0 - (9.9 - only90(6).split(":")(1).dropRight(1).toDouble * 10.0)).replace(",", ".")

      val last_position = position + behaviour + isKicker
      val position_stars = position + behaviour + " " + stars

      println(stars)
      println(last_position)
      println(condition)
      println(stamina)

      (last_position, stars, condition, stamina, position_stars, position)  //2024-02-12


    }
    
}

class Senior(args: Array[String]) extends PlayerClass(args){

  val playerId: String = args(1)

  //println(s"ID: $playerId")
  //println(s"ID: ${PlayerClass.Id(document)}")
    /*var a = 0
    println(s"${a+=1}")*/

  lazy val nationality: Option[String] = if(exists) Some(Senior.Nationality(document)) else None

  lazy val onTL: Option[Boolean] = if(exists) Some(Senior.OnTL(document)) else None

  lazy val info: mutable.Buffer[Element] = document.select("td").asScala
  //lazy val info1: mutable.Buffer[Element] = document.select("hattrick-rating").asScala
  lazy val info1: mutable.Buffer[Element] = document.select("td.nowrap.middle").asScala

  lazy val info2: mutable.Buffer[Element] = document.select("span").asScala

  lazy val sinceFrom: Option[String] = if(exists && has_club)Some(Senior.SinceFrom(document)) else None

  lazy val daysInClub: Option[Long] = if (exists && has_club) {
    Some(Senior.DaysInClub(sinceFrom.get))
  } else None

  lazy val skills: Option[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = if (onTL.getOrElse(false)) Some(Senior.Skills(info)) else None

  lazy val generalInfo: Option[(Int, Int, Int, Int, String)] = Some(Senior.GeneralInfo(info, playerId)) //test: info1 dla czytania gwiazdek i kondycji
  //lazy val generalInfo: Option[(Int, Int, Int, Int)] = Some(Senior.GeneralInfo(info1)) //test: info dla normalnej pracy

  lazy val tsi: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._1) else None
  lazy val salary: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._2) else None
  lazy val form: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._3) else None
  lazy val condition: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._4) else None
  lazy val gk: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._5) else None
  lazy val df: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._6) else None
  lazy val pm: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._7) else None
  lazy val wg: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._8) else None
  lazy val pass: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._9) else None
  lazy val sco: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._10) else None
  lazy val sp: Option[Int] = if (onTL.getOrElse(false)) Some(skills.get._11) else None

  lazy val character: Option[String] = if(exists) Some(Senior.Character(document)) else None
  lazy val exp: Option[String] = if(exists) Some(Senior.Exp(document)) else None

}


//to jest test czytania z pliku tekstowego jak z pliku html

object test11 extends App{

  //test function that read previously written dat file with age schedule for U21 competitions
  def Read_td2(worldCupNumber: Int): Elements = {

    val filename = WC_U21SchedulesPath + "WC" + worldCupNumber + ".dat"
    val file = new File(filename)
    val document: Document = Jsoup.parse(file, "UTF-8")

    document.select("td")

  }

  print(Read_td2(37))

}

//obecny sezon 87 -> WC37 itd
//06.02.2024 mamy wtorek 1 tygodnia sezonu
//tydzień czytam ze strony
//mecze są poniedziałek i piątek

def U21_schedule_generator(n: Int): String = {

  val date0 = "05.02.2024"
  val age0_years = 21
  val age0_days = 111 //21 i 111 dni, w poniedziałek pierwszego dnia nowego sezonu, to jest mak wiek
  //potem odejmujemy od tego
  //42 na 19.03.2023
  val season0 = 87
  //val week = 1
  //val day = 1




  def stringToDate(day: String): LocalDate = {
    val dateFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    val date = LocalDate.parse(day, dateFormatter)

    date
  }





  //println(today)

  @tailrec
  def getAge(age_years: Int, age_days: Int, days: Int): String = {

    //    println(s"${age_years}, ${age_days}, ${days}")

    if (days == 0)
      if (age_days > 111) getAge(age_years + 1, age_days - 112, 0)
      else if (age_days < 0) getAge(age_years - 1, age_days + 112, 0)
      else {
        //println(s"$age_years.$age_days")
        s"$age_years.$age_days"
      }
    else getAge(age_years, age_days + days, 0)


  }

  def getAgeFromString(date: String): Int = {
    /*converts date info days from starting date*/

    /*Updated Comment:
    * 1) 1-st day of a new season a player should have 22 years and 0 days to be able to play in WC finals the previous season
    * 2) x-th day of a new season a player should have 22 years and x days to be able to play in WC finals the previous season
    *
    * so 2) - 1) = todayDate.toEpochDay - DateOf1stDayOfNewSeason.toEpochDay = days from season beginning to now
    *
    * 3) 1-st day of a new season a player should have 22 years and -y days to be able to play in a match taking place y days ofter 1st day of a new season
    *
    * so 1) - 3) = DateOf1stDayOfNewSeason.toEpochDay - DateOfCertainPhaseOfCampaign.toEpochDay = -y days between beginning of a season and certain match date
    *
    * overall: 2) - 1) + 1) - 3) = 2) - 3) = (0 + todayDate.toEpochDay - DateOfCertainPhaseOfCampaign.toEpochDay) days
    *
    * Those are days thay sould be added to 22 years and 0 days to see years and day at certain stage of campaign
    * */

    val days: Long = stringToDate(getTodayDate()).toEpochDay - stringToDate(date).toEpochDay
    //println(s"1) ${getTodayDate()}")
    //println(s"2) ${stringToDate(getTodayDate())}")
    days.toInt

  }

  def getDate(season: Int, week: Int, day: Int): String = {
    /*get date for season-week-day info, adjust date of the certain time of the season (game days)*/

    val date = if (season == 87 & week == 1 & day == 1)
      stringToDate(date0)
    /*else if (season == 87 & week == 1 && day > 1 && day <=7)
    stringToDate(date0).plusDays(day)
  else if (season == 87 & week == 1 && day > 7)
    getDate(season, week + 1, day - 7)
  else if(season == 87 & )*/
    else {
      val days: Int = (season - season0) * 112 + (week - 1) * 7 + day - 1

      //println(days)

      if (days >= 0) stringToDate(date0).plusDays(days)
      else stringToDate(date0).minusDays(days)
    }
    val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    //println(s"${date.format(formatter)}")
    date.format(formatter)
  }

  //val ME = Seq[(1,5,"WC Final -1 training"), (2,5,"WC Final -2 trainings"), (3,1,"Continental Cup round 1")]

  /*def kiedy_22yo(todayDate: String, age: )*/

  def schedule_1st_Season(week: Int, day: Int): String = {

    val ME_2D = Array.ofDim[String](16, 2)

    ME_2D(0)(0) = "World Cup - Finals"
    ME_2D(0)(1) = "World Cup - Finals -1 training"
    ME_2D(1)(0) = "World Cup - Finals -1 training"
    ME_2D(1)(1) = "World Cup - Finals -2 trainings"
    ME_2D(2)(0) = "World Cup - Finals -2 trainings"
    ME_2D(2)(1) = "Continental Championship, Matchday 1"
    ME_2D(3)(0) = "Continental Championship, Matchday 2"
    ME_2D(3)(1) = "Continental Championship, Matchday 3"
    ME_2D(4)(0) = "Continental Championship, Matchday 4"
    ME_2D(4)(1) = "Continental Championship, Matchday 5"
    ME_2D(5)(0) = "Continental Championship, Matchday 5"
    ME_2D(5)(1) = "Continental Championship, Matchday 6"
    ME_2D(6)(0) = "Continental Championship, Matchday 6"
    ME_2D(6)(1) = "Continental Championship, Matchday 7"
    ME_2D(7)(0) = "Continental Championship, Matchday 7"
    ME_2D(7)(1) = "Continental Championship, Matchday 8"
    ME_2D(8)(0) = "Continental Championship, Matchday 8"
    ME_2D(8)(1) = "Continental Championship, Matchday 9"
    ME_2D(9)(0) = "Continental Championship, Matchday 9"
    ME_2D(9)(1) = "Continental Championship, Matchday 10"
    ME_2D(10)(0) = "Continental Championship, Matchday 10"
    ME_2D(10)(1) = "Continental Championship - Quarterfinals"
    ME_2D(11)(0) = "Continental Championship - Semifinals"
    ME_2D(11)(1) = "Continental Championship - Finals"
    ME_2D(12)(0) = "Continental Championship - Finals"
    ME_2D(12)(1) = "World Cup - Round I, Matchday 1"
    ME_2D(13)(0) = "World Cup - Round I, Matchday 2"
    ME_2D(13)(1) = "World Cup - Round I, Matchday 3"
    ME_2D(14)(0) = "World Cup - Round I, Matchday 3"
    ME_2D(14)(1) = "World Cup - Round I, Matchday 4"
    ME_2D(15)(0) = "World Cup - Round I, Matchday 4"
    ME_2D(15)(1) = "World Cup - Round I, Matchday 5"

    ME_2D(week)(day)

  }

  def schedule_2nd_Season(week: Int, day: Int) = {

    val WC_2D = Array.ofDim[String](16, 2)

    WC_2D(0)(0) = "World Cup - Round I, Matchday 5"
    WC_2D(0)(1) = "World Cup - Round I, Matchday 6"
    WC_2D(1)(0) = "World Cup - Round I, Matchday 6"
    WC_2D(1)(1) = "World Cup - Round I, Matchday 7"
    WC_2D(2)(0) = "World Cup - Round I, Matchday 7"
    WC_2D(2)(1) = "World Cup - Round I, Matchday 8"
    WC_2D(3)(0) = "World Cup - Round I, Matchday 8"
    WC_2D(3)(1) = "World Cup - Round I, Matchday 9"
    WC_2D(4)(0) = "World Cup - Round I, Matchday 9"
    WC_2D(4)(1) = "World Cup - Round I, Matchday 10"
    WC_2D(5)(0) = "World Cup - Round I, Matchday 10"
    WC_2D(5)(1) = "World Cup - Round II, Matchday 1"
    WC_2D(6)(0) = "World Cup - Round II, Matchday 2"
    WC_2D(6)(1) = "World Cup - Round II, Matchday 3"
    WC_2D(7)(0) = "World Cup - Round II, Matchday 4"
    WC_2D(7)(1) = "World Cup - Round II, Matchday 5"
    WC_2D(8)(0) = "World Cup - Round II, Matchday 5"
    WC_2D(8)(1) = "World Cup - Round II, Matchday 6"
    WC_2D(9)(0) = "World Cup - Round II, Matchday 6"
    WC_2D(9)(1) = "World Cup - Round III, Matchday 1"
    WC_2D(10)(0) = "World Cup - Round III, Matchday 2"
    WC_2D(10)(1) = "World Cup - Round III, Matchday 3"
    WC_2D(11)(0) = "World Cup - Round III, Matchday 3"
    WC_2D(11)(1) = "World Cup - Round IV, Matchday 1"
    WC_2D(12)(0) = "World Cup - Round IV, Matchday 2"
    WC_2D(12)(1) = "World Cup - Round IV, Matchday 3"
    WC_2D(13)(0) = "World Cup - Round IV, Matchday 3"
    WC_2D(13)(1) = "World Cup - Round V, Matchday 1"
    WC_2D(14)(0) = "World Cup - Round V, Matchday 2"
    WC_2D(14)(1) = "World Cup - Round V, Matchday 3"
    WC_2D(15)(0) = "World Cup - Semifinals"
    WC_2D(15)(1) = "World Cup - Finals"

    WC_2D(week)(day)

  }

  //print(ME_2D(0)(0))


  def last_game(season: Int, week: Int, day: Int): Unit = { //day 0 or 1

    if (week > 0 && week <= 15 && day >= 0 && day <= 1)
      if (season % 2 == 1)
        print(schedule_1st_Season(week - 1, day))
      else
        print(schedule_2nd_Season(week - 1, day))
    else print(s"zły zakres week or day")

  }

  /*print(getDate(88,3,5)) //day 1 = Monday; day 5 = Friday
  print(" ")
  last_game(88,3,1) //day 0 = Monday; day 1 = Friday
  print(" ")
  print(getAge(getDate(88,3,5)))
  print(" ")
  val days: Int = getAge(getDate(88,3,5))
  print(days)
  print(" ")
  val age = getAge(age0_years, age0_days, days)
  print(age0_years, age0_days, days, age)
  print(" ")
  print(" ")

  print(s"$days/$days")*/

  def pr(season: Int, week: Int, day: Int): String = {

    //print(s"<td>$season/${week+1}</td>\n")    //print(s"$season/${week+1}")
    //print(s"<td>${getDate(season,week+1,4*day+1)}</td>\n")    //print(s"${getDate(season,week+1,4*day+1)} ")
    var scheduleString = s"<td>$season/${week + 1}</td>\n" + s"<td>${getDate(season, week + 1, 4 * day + 1)}</td>\n"
    if (season % 2 == 1)
    //print(s"<td>${schedule_1st_Season(week,day)}</td>\n")     //print(s"${schedule_1st_Season(week,day)} ")
      scheduleString += s"<td>${schedule_1st_Season(week, day)}</td>\n"
    else
    //print(s"<td>${schedule_2nd_Season(week,day)}</td>\n")              //print(s"${schedule_2nd_Season(week,day)} ")
      scheduleString += s"<td>${schedule_2nd_Season(week, day)}</td>\n"
    //getAge(age0_years, age0_days, getAge(getDate(season,week+1,4*day+1))).split("\\.").foreach(x => println(s"<td>${x(0)} age and ${x(1)} days</td>"))   //println(getAge(age0_years, age0_days, getAge(getDate(season,week+1,4*day+1))))
    val max_age = getAge(age0_years, age0_days, getAgeFromString(getDate(season, week + 1, 4 * day + 1))).split("\\.")
    //println(s"<td>${max_age(0)} age and ${max_age(1)} days</td>")
    scheduleString += s"<td>${max_age(0)} age and ${max_age(1)} days</td>\n"
    //println(scheduleString.replaceAll("<td>","").replaceAll("</td>",""))
    //println("---")
    scheduleString

  }

  def U21_schedule(campaign: Int): String = {

    val season_0 = 87
    val campaign_0 = 37

    var td = s""

    val delta_campaign = campaign - campaign_0

    val season = season_0 + 2 * delta_campaign

    (season to season + 1).map(s => {
      (0 to 15).map(w => {
        (0 to 1).map(d => {
          if (s % 2 == 1)
            if (w > 0 && d == 0)
              //println(s"${ME_2D(w)(0)} ${ME_2D(w-1)(1)}")
              if (schedule_1st_Season(w, 0) != schedule_1st_Season(w - 1, 1))
                td += s"${pr(s, w, d)}"
              else
                ""
            else td += s"${pr(s, w, d)}"
          else if (w == 0 && d == 0)
            ""
          else if (w > 0 && d == 0)
            if (schedule_2nd_Season(w, 0) != schedule_2nd_Season(w - 1, 1))
              td += s"${pr(s, w, d)}"
            else
              ""
          else td += s"${pr(s, w, d)}"
        })
      })
    })

    //println(td)
    td
  }
  U21_schedule(n)
}


object test_schedule_generator extends App{

  println(U21_schedule_generator(41))

}


object test_Read_td extends App {

  //println(Youth.Read_td(37).asScala)
  println(Youth.Read_td_S(37))

}

object seniorPlayerTest extends App{

  val sp = new Senior(Array(seniorPlayerPath, "485522130"))

  val nationality = sp.nationality.get
  val generalInfo = sp.generalInfo.get
  println(generalInfo)
  println(nationality)
  println(nationality.equals("Polska"))
  println(nationality.eq("Polska"))        //to jest false

}

object youthPlayerTest extends App{

  val yp = new Youth(Array(youthPlayerPath, "348798279"))

  val nationality = yp.nationality.get
  println(nationality)
  println(nationality.equals("Polska"))
  println(nationality.eq("Polska"))        //to jest false
  println(yp.availability.get)
  println(yp.scoutingHistory.get)
  println(yp.bestPerformances.get)
  println(yp.last5Performances.get)

}