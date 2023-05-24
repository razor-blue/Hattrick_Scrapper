import com.mongodb.MongoClient
import com.mongodb.client.model.Filters
import com.mongodb.client.{FindIterable, ListDatabasesIterable, MongoCollection, MongoCursor, MongoDatabase}
import org.bson.Document
import org.bson.conversions.Bson

import scala.collection.JavaConverters._

import java.{lang, util}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, SECONDS}

object MyMongoApp {
  def main(args: Array[String]): Unit = {

    val mongoClient = new MongoClient("localhost", 27017)

    val database = mongoClient.getDatabase("P")

    //print list of databases
    val databaseList: Array[String] = mongoClient.listDatabaseNames().into(new java.util.ArrayList[String]()).toArray(Array[String]())
    val collectionList: Array[String] = database.listCollectionNames().into(new java.util.ArrayList[String]()).toArray(Array[String]())
    databaseList.foreach(println(_))
    collectionList.foreach(println(_))

    val collection: MongoCollection[Document] = database.getCollection("luka_test")

    /*
    //Te komendy są gorsze od tych poniżej
    val documents: MongoCursor[Document] = collection.find().iterator()
    /*while (documents.hasNext) {
      val document = documents.next()
      println(document.toJson) //{"_id": {"$oid": "646ca1c6e706077748d49240"}, "Player": "Mariusz Czaplak", "Player ID": 325463965, "Age": 15.091, "Speciality": "", "Days in Academy": 10, "WC X": "WC 37", "Stage N": 37, "Description": "World Cup - Round IV  Matchday 3", "Last Match Date": "04/04/2025", "Season": 90, "Week": 13, "B_GK": -1, "B_CD": -1, "B_WB": -1, "B_IM": 3.5, "B_W": 4.5, "B_F": -1, "L_GK": -1.0, "L_CD": -1.0, "L_WB": -1.0, "L_IM": -1.0, "L_W": 4.5, "L_F": -1.0, "Last Match Details": "12.05.2023 skrzydlowy (90')", "Country": "Polska", "Last update": "19.05.2023"}
    }*/

    val fieldLabels: util.HashSet[String] = new util.HashSet[String]() // Kolekcja na unikalne etykiety pól
    while (documents.hasNext) {
      val document = documents.next()
      val keys: util.Set[String] = document.keySet()
      //keys.forEach(println(_))
      val iterator = keys.iterator()
      while (iterator.hasNext) {
        val key: String = iterator.next()
        //println(key)
        fieldLabels.add(key)
      }
    }
    val labelsString: String = fieldLabels.iterator().next()
    val labels = labelsString.split(",")
    //labels.foreach(println(_))

    documents.close()
*/

    /*var r=0
    println("Etykiety pól w bazie danych:")
    val fieldIterator: util.Iterator[String] = fieldLabels.iterator()
    while (fieldIterator.hasNext) {
      val field: String = fieldIterator.next()
      println(s"$r -> $field")
      r+=1
    }*/

    println("------------------------------")
val test_collection: List[Document] = collection.find().into(new java.util.ArrayList[Document]()).asScala.toList
    //Prawidłowo pobiera z prawidłowo zaimportowanej bazy danych
    //********UNIKAĆ " w pliku .csv*************************************
    /*val coursor: MongoCursor[Document] = collection.find().iterator()
    while (coursor.hasNext) {
      val document: Document = coursor.next()
      val age: lang.Double = document.getDouble("Age")
      val nameString: String = document.getString("Player")
      println(document.containsKey("Player"))
      /*val ageDouble = ageString.toDouble
      if (ageDouble < 17.0) {*/
        println(s"$nameString, ${age + 1.0}")
      //}
    }
    coursor.close()*/

    // Pobierz rekordy z bazy danych
    val records: util.ArrayList[Document] = new util.ArrayList[Document]()
    collection.find().into(records)
    //records.forEach(println(_))  //Document{{_id=646ca1c6e706077748d49237, Player=Rafal Gogacz, Player ID=315572051, Age=17.067, Speciality=U, Days in Academy=249, WC X=WC 37, Stage N=1, Description=Friendly, Last Match Date=20/09/2024, Season=89, Week=1, B_GK=2, B_CD=4, B_WB=4, B_IM=3.5, B_W=4, B_F=3.5, L_GK=2.0, L_CD=4.0, L_WB=4.0, L_IM=3.5, L_W=3.5, L_F=-1.0, Last Match Details=19.05.2023 boczny_obronca (90'), Country=Polska, Last update=19.05.2023}}


    // Przetwórz rekordy i wykonaj split
    records.forEach { record =>
      val recordString: String = record.toJson() // przekształć rekord na String
      val fields: Array[String] = recordString.split(",") // wykonaj split
      // Wydrukuj pola rekordu
      //fields.foreach(println) {
        /*"_id": {"$oid": "646ca1c6e706077748d49240"}
        "Player": "Mariusz Czaplak"
        "Player ID": 325463965
        "Age": 15.091
        "Speciality": ""
        "Days in Academy": 10
        "WC X": "WC 37"
        "Stage N": 37
        "Description": "World Cup - Round IV  Matchday 3"
        "Last Match Date": "04/04/2025"
        "Season": 90
        "Week": 13
        "B_GK": -1
        "B_CD": -1
        "B_WB": -1
        "B_IM": 3.5
        "B_W": 4.5
        "B_F": -1
        "L_GK": -1.0
        "L_CD": -1.0
        "L_WB": -1.0
        "L_IM": -1.0
        "L_W": 4.5
        "L_F": -1.0
        "Last Match Details": "12.05.2023 skrzydlowy (90')"
        "Country": "Polska"
        "Last update": "19.05.2023"
      }*/
      //println(fields)          //[Ljava.lang.String;@485966cc
      //println(recordString)  //{"_id": {"$oid": "646ca1c6e706077748d49240"}, "Player": "Mariusz Czaplak", "Player ID": 325463965, "Age": 15.091, "Speciality": "", "Days in Academy": 10, "WC X": "WC 37", "Stage N": 37, "Description": "World Cup - Round IV  Matchday 3", "Last Match Date": "04/04/2025", "Season": 90, "Week": 13, "B_GK": -1, "B_CD": -1, "B_WB": -1, "B_IM": 3.5, "B_W": 4.5, "B_F": -1, "L_GK": -1.0, "L_CD": -1.0, "L_WB": -1.0, "L_IM": -1.0, "L_W": 4.5, "L_F": -1.0, "Last Match Details": "12.05.2023 skrzydlowy (90')", "Country": "Polska", "Last update": "19.05.2023"}
    }

    mongoClient.close()

  }
}


/*
// Scala version of the code for mongodb scala driver library imports
*/

/*import org.mongodb.scala.*
import org.mongodb.scala.bson.collection.immutable.Document
import com.github.tototoshi.csv.* */

/*import com.mongodb.client.{/*MongoClient,*/ MongoClients, MongoCollection, MongoDatabase}
import org.bson.Document

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.{Failure, Success}
import com.mongodb.MongoClient*/

/*object MongoDbConnection extends App {
  val mongoClient = MongoClient("mongodb://localhost:27017")
  val database: MongoDatabase = mongoClient.getDatabase("P")

  val collectionNames: Future[Seq[String]] = database.listCollectionNames().toFuture()

  val names: Seq[String] = Await.result(collectionNames, Duration.create(5,SECONDS))
  names.foreach(println)

}*/

/*object ListDatabasesExample extends App {
  val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017")
  val databaseNames: Observable[String] = mongoClient.listDatabaseNames()

  val databaseNamesFuture: Future[Seq[String]] = databaseNames.toFuture()

  val result: Seq[String] = Await.result(databaseNamesFuture, Duration.Inf)

  println("Available databases:")
  result.foreach(println)

  mongoClient.close()
}*/

/*object MongoDbConnection1 extends App {
  val mongoClient: MongoClient = MongoClient("mongodb://localhost:27017")
  val database: MongoDatabase = mongoClient.getDatabase("P")
  val collection: MongoCollection[Document] = database.getCollection("yp_scan")

  mongoClient.listDatabaseNames().foreach(println(_))


  collection.find().subscribe(
    (document: Document) => {
      println("sss")
      println(document.toJson())
      println("fff")
    },
    (error: Throwable) => println(s"An error occurred: ${error.getMessage}"),
    () => println("Completed")
  )

}*/
