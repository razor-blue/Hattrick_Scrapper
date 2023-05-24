import com.mongodb.MongoClient
import com.mongodb.client.MongoCollection
import org.bson.Document

import scala.jdk.CollectionConverters._

object mongoDB extends App{

  val mongoClient = new MongoClient("localhost", 27017)
  val database = mongoClient.getDatabase("P")
  val collection1: MongoCollection[Document] = database.getCollection("luka_test")
  val collection2: MongoCollection[Document] = database.getCollection("fcb_test")
  val collection3: MongoCollection[Document] = database.getCollection("yp_scan")

  val LD1: List[Document] = collection1.find().into(new java.util.ArrayList[Document]()).asScala.toList
  val LD2: List[Document] = collection2.find().into(new java.util.ArrayList[Document]()).asScala.toList

  //Prepare and add luka and fcb records into yp_scan collection
  collection3.deleteMany(new Document())
  collection3.insertMany(LD1.asJava)
  collection3.insertMany(LD2.asJava)

  mongoClient.close()
}
