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
import com.google.protobuf.InvalidProtocolBufferException
import edu.jhu.hlt.concrete.converters.factorie.utils._
import edu.jhu.hlt.concrete.converters.factorie.utils.Factorie2ConcreteFactorie._

/**
 * @author Tan Xu
 * 
 */
object Concrete2Factorie {
	
	val log = Logger.getLogger(Concrete2Factorie.getClass().getName())
	
	def getFactorieDocs(pathToComm:String): ArrayBuffer[Document]={
		//log.info("Reading protobuf file from: "+pathToComm)
		val docs = new ArrayBuffer[Document]
		
		var reader:BufferedInputStream = null
		if(pathToComm.toLowerCase().endsWith("gz"))
			reader = new BufferedInputStream(new GZIPInputStream(new FileInputStream(pathToComm)))
		else
			reader = new BufferedInputStream(new FileInputStream(pathToComm))
		
		//val kg = KnowledgeGraph.parseDelimitedFrom(reader)
		//log.info("Got knowledgeGraph:\n"+kg.toString())
		val loop = new Breaks;
		loop.breakable{
			try{
				do{
					val curComm = Communication.parseDelimitedFrom(reader)
					if(curComm eq null) loop.break
					//log.info("Got communication:\n"+curComm.toString())
					//val documentWrapper = curComm.asDocument[SectionSegmentationTheory](new SectionSegmentationTheory("Annotated Gigaword Pipeline"))
					//val document = documentWrapper.head
					val document = curComm.asDocument
					docs.append(document)
				}while(true)
			}catch{
				case e:InvalidProtocolBufferException => {
					println("Bad Protobuf File: "+pathToComm+"\n"+e)
				}
			}
		}
		reader.close()
        return docs
	}
	
	def main(args: Array[String]): Unit={
		if(args.length < 1)
			log.severe("Please input a conceret protobuf file.")
		else log.info("Reading concrete protobuf file from"+args(0));
		//var testDat = args(0)
		//if(testDat eq null) testDat="data/rf-concrete-dump.pb.gz"
		val documents = getFactorieDocs(args(0))
		log.info("Got documents: "+documents.size)
		for(document<-documents){
			log.info("Got document: "+document.name+
						"\n SectionSegmentationTheory: "+document.sectionSegmentationTheory.value+
						"\n SentenceSegmentationTheory: "+document.sentenceSegmentationTheory.value+
						"\n TokenizationTheory: "+document.tokenizationTheory.value+
						"\n PosTagTheory: "+document.posTagTheory.value+
						"\n DependencyParseTheory: "+document.dependencyParseTheory.value+
						"\n Sentences: "+document.sentences.size+
						"\n Tokens: "+document.tokens.size)
				println(document.string)
				document.sentences.foreach(sent=>println("Sentence: "+sent.string+"\n"+sent.parse))
				document.foreach(tok=>println(tok.string+"\t"+tok.myPosLabel.categoryValue+"\t"+tok.myNerLabel.categoryValue+"\t"+tok.lemma.value))
		}
	}
}