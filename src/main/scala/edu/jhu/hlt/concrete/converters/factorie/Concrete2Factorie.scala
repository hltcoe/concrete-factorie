package edu.jhu.hlt.concrete.converters.factorie

import java.util.logging.Logger
import edu.jhu.hlt.concrete.io.ProtocolBufferReader
import edu.jhu.hlt.concrete.Concrete.Communication
import java.io.File
import java.io.FileInputStream
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos._
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

/**
 * @author tanx
 * 
 */
class TokenId (s:Int=0) extends IntegerVariable(s)
object Concrete2Factorie {
	
	val log = Logger.getLogger(Concrete2Factorie.getClass().getName())
	
	def communication2Document(comm:Communication): Document={
		val id = comm.getGuid()
		val text = comm.getText()
		val document = new Document(id.toString(), text)
		
		comm.getSectionSegmentationList().foreach(ss=>{
			ss.getSectionList().foreach(sec=>{
				sec.getSentenceSegmentationList().foreach(sentSeg=>{
					sentSeg.getSentenceList().foreach(sent=>{
						if(sent.hasTextSpan()){
							val sentStart = sent.getTextSpan().getStart()
							val sentEnd = sent.getTextSpan().getEnd()
							val sentence =
								new Sentence(document, sentStart, sentEnd-sentStart+1)
							
							sent.getTokenizationList().foreach(tokenization=>{
								tokenization.getTokenList().foreach(tok=>{
									val token = new Token(sentence, tok.getText())
									token.attr+=new TokenId(tok.getTokenId())
								})
								tokenization.getPosTagsList().foreach(tokTag=>{
									tokTag.getTaggedTokenList().foreach(tagTok=>{
										val tagTokId = tagTok.getTokenId()
										sentence.tokens.foreach(token=>{
											if(token.attr[TokenId]==tagTokId){
												token.attr+=new PosLabel(token, tagTok.getTag())
											}
										})
									})
								})
							})
						}
					})
				})
			})
		})
		
		return document
	}
	
	def getFactorieDocs(pathToComm:String): ArrayBuffer[Document]={
		val commFile = new File(pathToComm)
		val fis = new FileInputStream(commFile)
		val pbr = new ProtocolBufferReader(fis, classOf[Communication])
		val docs = new ArrayBuffer[Document]
		do{ // To-Do:no hasnext() method
			val curComm = pbr.next().asInstanceOf[Communication]
			docs.append(communication2Document(curComm))
		}while(true)
			
		pbr.close()
        fis.close()
        
        return docs
	}
}