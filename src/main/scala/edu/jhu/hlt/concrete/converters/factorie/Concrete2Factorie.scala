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

/**
 * @author tanx
 * 
 */
class TokenId (s:Int=0) extends IntegerVariable(s)
object Concrete2Factorie {
	
	val log = Logger.getLogger(Concrete2Factorie.getClass().getName())
	
	def getFactorieDocs(pathToComm:String): ArrayBuffer[Document]={
		val commFile = new File(pathToComm)
		val fis = new FileInputStream(commFile)
		val pbr = new ProtocolBufferReader(fis, classOf[Communication])
		val docs = new ArrayBuffer[Document]
		do{ // To-Do:no hasnext() method
			val curComm = pbr.next().asInstanceOf[Communication]
			docs.append(curComm.asDocument)
		}while(true)
			
		pbr.close()
        fis.close()
        
        return docs
	}
}