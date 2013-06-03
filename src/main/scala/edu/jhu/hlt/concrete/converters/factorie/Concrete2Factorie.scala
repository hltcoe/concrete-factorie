package edu.jhu.hlt.concrete.converters.factorie

import java.util.logging.Logger
import edu.jhu.hlt.concrete.io.ProtocolBufferReader
import edu.jhu.hlt.concrete.Concrete.Communication
import java.io.File
import java.io.FileInputStream
import cc.factorie._
import cc.factorie.app.nlp._
import scala.collection.mutable.ArrayBuffer
import edu.jhu.hlt.concrete.converters.factorie.ImplicitConversions._
import scala.util.control.Breaks
import edu.jhu.hlt.concrete.Concrete.KnowledgeGraph

/**
 * @author tanx
 * 
 */
class TokenId (s:Int=0) extends IntegerVariable(s)
object Concrete2Factorie {
	
	val log = Logger.getLogger(Concrete2Factorie.getClass().getName())
	
	def getFactorieDocs(pathToComm:String): ArrayBuffer[Document]={
		val commFile = new File(pathToComm)
		log.info("Reading protobuf file from: "+commFile.getAbsolutePath())
		val fis = new FileInputStream(commFile)
		val pbr = new ProtocolBufferReader(fis, classOf[Communication])
		val knowledge_graph = pbr.next().asInstanceOf[KnowledgeGraph]
		val docs = new ArrayBuffer[Document]
		val loop = new Breaks;
		loop.breakable{
			do{ // To-Do:no hasnext() method
				var nextMessage = pbr.next()
				log.info(nextMessage.getSerializedSize().toString)
				if(nextMessage ne null) {
					val curComm = nextMessage.asInstanceOf[Communication]
					val document = curComm.asDocument
					docs.append(document)
					log.info("Got document: "+document.name)
				}else loop.break 
			}while(true)
		}
		
		pbr.close()
        fis.close()
        return docs
	}
	
	def main(args: Array[String]): Unit={
		var testDat = System.getProperty("testDat")
		if(testDat eq null) testDat="data/rf-concrete-dump.pb.gz"
		val documents = getFactorieDocs(testDat)
		log.info("Got documents: "+documents.size)
	}
}