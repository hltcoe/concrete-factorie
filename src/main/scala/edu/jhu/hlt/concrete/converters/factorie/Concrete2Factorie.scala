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
import java.util.zip.GZIPInputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.BufferedInputStream

/**
 * @author tanx
 * 
 */
object Concrete2Factorie {
	
	val log = Logger.getLogger(Concrete2Factorie.getClass().getName())
	
	def getFactorieDocs(pathToComm:String): ArrayBuffer[Document]={
		log.info("Reading protobuf file from: "+pathToComm)
		val docs = new ArrayBuffer[Document]
		
		val reader = new BufferedInputStream(new GZIPInputStream(new FileInputStream(pathToComm)))
		val kg = KnowledgeGraph.parseDelimitedFrom(reader)
		log.info("Got knowledgeGraph:\n"+kg.toString())
		val loop = new Breaks;
		loop.breakable{
			do{
				val curComm = Communication.parseDelimitedFrom(reader)
				if(curComm eq null) loop.break
				log.info("Got communication:\n"+curComm.toString())
				val documentWrapper = curComm.asDocument
				//val documentWrapper = curComm.asDocument[SectionSegmentationTheory](new SectionSegmentationTheory("Annotated Gigaword Pipeline"))
				val document = documentWrapper.head
				docs.append(document)
				log.info("Got document: "+document.name+"\nAnnotation: "+document.attr[SentenceSegmentationTheory].value)
			}while(true)
		}
		
        return docs
	}
	
	def main(args: Array[String]): Unit={
		var testDat = System.getProperty("testDat")
		if(testDat eq null) testDat="data/rf-concrete-dump.pb.gz"
		val documents = getFactorieDocs(testDat)
		log.info("Got documents: "+documents.size)
	}
}