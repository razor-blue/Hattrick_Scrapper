
import org.jsoup.{Connection, Jsoup}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Calendar
import scala.annotation.tailrec


object PlayerClass{

    def Age(document: Document): (Double, Int, Int) = {

        val wiek = document.select("div.byline").text()
        val years = wiek.split(" ")(0).toInt
        val days = wiek.split(" ")(3).toInt

        (years.toFloat + days.toFloat / 1000.0, years, days)

    }
}

class PlayerClass(args: Array[String]) {

    val url: String = args(0) + args(1)
    val connection: Connection = Jsoup.connect(url)
    val document: Document = connection.get()

    val exists: Boolean = if (document.title.split("»").length == 1) false else true
    val has_club: Boolean = if (document.title.split("»").length == 4) true else false

    val id: Option[Int] = if (exists) Some(document.select("span.idNumber").text().replaceAll("[()]", "").toInt) else None

    val name: Option[String] = if (exists) Some(document.title.split("»").head.trim) else None

    val age: Option[(Double, Int, Int)] = if (exists) Some(Pla.Age(document)) else None
    val link: Option[String] = if (exists) Some(args(0) + args(1)) else None

    val speciality: Option[String] = if (exists) Some(document.select("td[colspan]").select("i[title]").attr("title")) else None

}

object Youth{

    def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title").split(": ")(1)
    def Since(document: Document): Int = document.select("span.shy span[dir=ltr]").first.text().split(" ")(2).toInt

    def BestPerformances(document: Document): String = {

        val arrayString: Array[String] = document.select("table.youthPlayerPerformance").select("td.middle").
          text().replaceAll("-", "-1 -").replaceAll("boczny obrońca", "boczny_obrońca").split(" ")

        @tailrec
        def addString(str: String, stringToAdd: Array[String], index: Int): String =
            if (index >= stringToAdd.length) str
            else addString(str + "," + stringToAdd(index), stringToAdd, index + 3)

        addString(arrayString(1), arrayString, 4)

    }

    def Last5Performances(document: Document): Seq[Double] = {

        val arrayString1: mutable.Buffer[String] = document.select("div.mainBox").select("td.middle").asScala.map(x => x.text().replaceAll("boczny obrońca", "boczny_obrońca"))
        val arrayString2 = document.select("div.mainBox").select("span.stars-full").asScala
        val arrayString3 = document.select("div.mainBox").select("td.top").asScala

        val nGames = arrayString2.length

        val dictionary = Seq("bramkarz", "stoper", "boczny_obrońca", "pomocnik", "skrzydłowy", "napastnik")
        val dictionaryMap = Map("bramkarz" -> 0, "stoper" -> 1, "boczny_obrońca" -> 2, "pomocnik" -> 3, "skrzydłowy" -> 4, "napastnik" -> 5)


        val data: Seq[(Int, Double)] = (0 until nGames).map(i => {
            val nPositionsPlayed = dictionary.count(p => arrayString1(2 + i * 3).contains(p))
            val positionPlayed: Int = if (nPositionsPlayed == 1) dictionaryMap(arrayString1(2 + i * 3).split(" ").head) else -1
            val starsPlayed: Double = arrayString3(i).text().toDouble

            (positionPlayed, starsPlayed)
        })

        val newRecord: Seq[Double] = (0 to 5).map(i => {
            val tmp = data.filter(p => p._1 == i).map(_._2)
            val tmp2 = if (tmp.isEmpty) -1 else tmp.max
            tmp2
        })

        newRecord

    }

    def Read_td(worldCupNumber: Int): Elements = {

        val url: String = s"https://hattrickportal.pro/Tracker/U20/U21WC.aspx?WorldCup=$worldCupNumber"
        val connection: Connection = Jsoup.connect(url)
        val document: Document = connection.get()

        document.select("td")

    }

    def WorldCupAgeMinMax(worldCupNumber: Int): ((Double, Int, Int), (Double, Int, Int)) = {

        val td = Read_td(worldCupNumber: Int).asScala

        val maxAgeYears = td(3).text().split(" ")(0).toInt
        val maxAgeDays = td(3).text().split(" ")(3).toInt

        val minAgeYears = td(167).text().split(" ")(0).toInt
        val minAgeDays = td(167).text().split(" ")(3).toInt

        val ageMax = maxAgeYears + maxAgeDays / 1000.0
        val ageMin = minAgeYears + minAgeDays / 1000.0

        ((ageMin, minAgeYears, minAgeDays), (ageMax, maxAgeYears, maxAgeDays))

    }

    @tailrec
    def WhichWorldCup(playerAge: (Double, Int, Int), worldCupNumber: Int): Int = {

        val ageMin = WorldCupAgeMinMax(worldCupNumber)._1._1
        val ageMax = WorldCupAgeMinMax(worldCupNumber)._2._1

        val ageCurrent = playerAge._1

        if (ageCurrent >= ageMin) worldCupNumber
        else WhichWorldCup(playerAge, worldCupNumber + 1)

    }

    @tailrec
    def f(td: mutable.Buffer[Element], worldCupNumber: Int, playerAge: Double, index: Int): String = {

        val age = td(index).text().split(" ")(0).toDouble + td(index).text().split(" ")(3).toDouble / 1000.0
        val round = td(index - 5).text()
        val date = td(index - 6).text()
        val week = td(index - 7).text()

        if (playerAge > age) s"WC ${worldCupNumber - 1} --> ${(index - 7) / 4.0 + 1.0} --> $round --> $date --> $week"
        else f(td, worldCupNumber, playerAge, index + 4)

    }

    def WhichRoundOfWorldCup(playerAge: (Double, Int, Int), worldCupNumber: Int): String = {

        val td: mutable.Buffer[Element] = Read_td(worldCupNumber).asScala

        val age = WorldCupAgeMinMax(worldCupNumber)

        val ageMin = age._1._1
        val ageMax = age._2._1

        if (playerAge._1 > ageMax) s"WC ${worldCupNumber - 1} --> Final"
        else if (playerAge._1 <= ageMin) s"WC $worldCupNumber --> Final"
        else f(td, worldCupNumber, playerAge._1, 7)

    }

    def Availability(age: (Double, Int, Int)): String = {

        val i = 37

        val worldCupNumber = WhichWorldCup(age, i)
        val worldCupRound = WhichRoundOfWorldCup(age, worldCupNumber)

        worldCupRound

    }
}

class Youth(args: Array[String]) extends PlayerClass(args){

    val stillInYouthAcademy: Boolean = id.get.equals(args(1).toInt)

    val nationality: Option[String] = if(exists) {
        try {
            Some(Y.Nationality(document))
        }
        catch
            case _: Throwable =>
                try {
                    Some(S.Nationality(document))
                } catch {
                    case _: Throwable => None
                }
    }
    else None

    val since: Option[Int] = if (exists) {
        try {
            Some(Y.Since(document))
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

    val availability: Option[String] = if( exists) Some(Y.Availability(age.get)) else None

    val bestPerformances: Option[String] = if(stillInYouthAcademy) Some(Y.BestPerformances(document))  else None
    val last5Performances: Option[Seq[Double]] = if(stillInYouthAcademy) Some(Y.Last5Performances(document)) else None

}

object Senior{

    def Nationality(document: Document): String = document.select("div.byline").select("img[title]").attr("title")

}

class Senior(args: Array[String]) extends PlayerClass(args){

    val nationality: Option[String] = if(exists) Some(S.Nationality(document)) else None

}